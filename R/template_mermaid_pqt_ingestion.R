###############################################################
###### Ingesting Photoquadrat (PQT) into MERMAID  #############
##################### Author: Amkieltiela #####################
###################### Edited by Jeneen #######################

# The steps to ingest photo benthic quadrat (PQT) from CoralNet to MERMAID
# 1. Install & activate libraries
# 2. Download the MERMAID PQT template
# 3. Prepare & reformat the data to match the template
# 4. Ingest data to MERMAID


#### 1. Install & activate libraries ####
remotes::install_github("data-mermaid/mermaidr") # to install the mermaidr package

# activate libraries
library(mermaidr)
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)

#### 2. Download the MERMAID PQT template ####
# STEP 1 - Get MERMAID Project
project <- mermaid_get_my_projects()

# STEP 2 - Filter to select just the project you are working on (and rename as object)
musandam <- project %>% filter(name=="UAE_Musandam_2022")

# STEP 3 - Getting the import template and options
pqt_template <- mermaid_import_get_template_and_options(
  musandam,
  "benthicpqt"
)

# the PQT template consists of: Site *, Management *, Sample date: Year *, Sample date: Month *, Sample date: Day *,
# Sample time, Depth *, Transect number *, Transect label, Transect length surveyed *, Number of quadrats *, Quadrat size *,
# First quadrat number, Number of points per quadrat *, Reef slope, Visibility, Current, Relative depth, Tide, 
# Sample unit notes, Observer emails *, Quadrat *, Benthic attribute *, Growth form, Number of points *

#### 3. Prepare & reformat the data to match the template ####

### Load the data ###
pqt_raw <- read.csv("Data/MERMAID/B_Burt_2022_UAE_Musandam.csv", header = TRUE) 
benthic_attr <- read.csv("Data/MERMAID/labelsetMatched.csv", header = TRUE)
metadata <- read.csv("Data/MERMAID/Sites_UAE_Oman.csv", header = TRUE)

colnames(pqt_raw)

### Reformat the data ###
pqt_mermaid <- subset(pqt_raw, select = c(Site, Date, Transect, Quadrat, Depth, Label))
pqt_mermaid <- pqt_mermaid %>% separate(Date, into = c('Sample date: Day *', 'Sample date: Month *', 'Sample date: Year *'),
                                        sep = '/')
unique(pqt_mermaid$Transect)
pqt_mermaid$Transect <- str_replace_all(pqt_mermaid$Transect, c('A'='1', 'B'='2', 'C'='3', 'D'='4',
                                                                'E'='5', 'F'='6'))
pqt_mermaid[c('Sample date: Year *', 'Sample date: Month *', 'Sample date: Day *', 'Transect')] <- sapply(pqt_mermaid[c('Sample date: Year *', 'Sample date: Month *', 'Sample date: Day *', 'Transect')],
                                                                                                          as.numeric)

pqt_mermaid <- pqt_mermaid %>%
  select(
    `Site *` = Site,
    `Sample date: Year *`,
    `Sample date: Month *`,
    `Sample date: Day *`,
    `Depth *` = Depth,
    `Transect number *` = Transect, 
    `Quadrat *` = Quadrat,
    Label
  )  # reorder and rename the columns


### Summarize data per benthic attribute per quadrat
colnames(pqt_mermaid) # to see the column names

pqt_mermaid <- ddply(pqt_mermaid, c("`Site *`", "`Sample date: Year *`", '`Sample date: Month *`', '`Sample date: Day *`',
                                    '`Depth *`', "`Transect number *`", "`Quadrat *`", "Label"),
                     summarise, `Number of points`= length(Label))

unique(pqt_mermaid$`Site *`)
pqt_mermaid$`Site *`[pqt_mermaid$`Site *` == 'RAKNatReef'] <- 'RAK'

### Adding necessary columns referring to the template
colnames(benthic_attr)

pqt_mermaid <- add_column(pqt_mermaid, 'Benthic attribute *' = benthic_attr$BA_Tiela_Suggestion[match(pqt_mermaid$Label, benthic_attr$Short.Code)], .before = 'Number of points')
unique(pqt_mermaid$`Benthic attribute *`)
pqt_mermaid_NA <- pqt_mermaid[which(!duplicated(pqt_mermaid$Label) & is.na(pqt_mermaid$`Benthic attribute *`)),]
pqt_mermaid <- add_column(pqt_mermaid, 'Growth form' = benthic_attr$GF_Tiela_Suggestion[match(pqt_mermaid$Label, benthic_attr$Short.Code)], .before = 'Number of points')

pqt_mermaid <- ddply(pqt_mermaid, c("`Site *`", "`Sample date: Year *`", '`Sample date: Month *`', 
                                    '`Sample date: Day *`', "`Depth *`", "`Transect number *`", "`Quadrat *`",
                                    "`Benthic attribute *`", "`Growth form`"),
                     summarise, `Number of points *`=sum(`Number of points`))

pqt_mermaid <- add_column(pqt_mermaid, 'Management *' = metadata$Management.name[match(pqt_mermaid$`Site *`, metadata$Name)], .before = 'Sample date: Year *')
unique(pqt_mermaid$`Management *`)
pqt_mermaid_NA <- pqt_mermaid[which(!duplicated(pqt_mermaid$`Site *`) & is.na(pqt_mermaid$`Management *`)),]
pqt_mermaid <- add_column(pqt_mermaid, 'Transect length surveyed *' = 30, .before = 'Quadrat *')
pqt_mermaid <- add_column(pqt_mermaid, 'Number of quadrats *' = 11, .before = 'Quadrat *')
pqt_mermaid <- add_column(pqt_mermaid, 'Quadrat size *' = format(round(((60*41)/10000), 2), nsmall = 2), .before = 'Quadrat *')
pqt_mermaid <- add_column(pqt_mermaid, 'Number of points per quadrat *' = 50, .before = 'Quadrat *')
pqt_mermaid <- add_column(pqt_mermaid, 'Observer emails *' = 'john.burt@nyu.edu', .before = 'Quadrat *')

#### 4. Ingest data to MERMAID ####

# STEP 1 - Address errors and warnings
mermaid_import_check_options(pqt_mermaid, pqt_template, "Site *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Management *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Sample date: Year *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Sample date: Month *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Sample date: Day *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Depth *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Transect number *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Transect length surveyed *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Number of quadrats *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Quadrat size *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Number of points per quadrat *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Observer emails *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Quadrat *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Benthic attribute *")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Growth form")
mermaid_import_check_options(pqt_mermaid, pqt_template, "Number of points *")

# save the clean data
write_csv(pqt_mermaid, "Clean/pqt_musandam_2022_mermaid.csv") # save the clean data

# STEP 2 - Recheck the data
mermaid_import_project_data(
  pqt_mermaid,
  musandam,
  method = "benthicpqt",
  dryrun = TRUE
)

# STEP 3 - Ingest the data
mermaid_import_project_data(
  pqt_mermaid,
  musandam,
  method = "benthicpqt",
  dryrun = FALSE,
  clearexisting = FALSE
)
