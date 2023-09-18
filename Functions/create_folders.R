#use this function to create folder paths for processed images to be sorted


file_path <- "/Users/jh8695/Documents/GitRepos/NortheasternArabiaReefMonitoringNetwork1/Data/Photoquadrats/Test" #Add file path here
site <- "TEST" #Add site name here
date <- "2023-09-18" #Add date here


create_folders <- function(file_path, site, date) {
  # Create "Processed" folder
  processed_path <- file.path(file_path, "Processed")
  if(!dir.exists(processed_path)) {
    dir.create(processed_path)
  }
  
  # Create folder named after the "site" input
  site_path <- file.path(processed_path, site)
  if(!dir.exists(site_path)) {
    dir.create(site_path)
  }
  
  # Create folder named after the "date" input
  date_path <- file.path(site_path, date)
  if(!dir.exists(date_path)) {
    dir.create(date_path)
  }
  
  # Create sub-folders A, B, C, D, E, F
  sub_folders <- c("A", "B", "C", "D", "E", "F")
  for(folder in sub_folders) {
    sub_folder_path <- file.path(date_path, folder)
    if(!dir.exists(sub_folder_path)) {
      dir.create(sub_folder_path)
    }
  }
}

# Example usage:
# file_path <- "/Photoquadrats"
# site <- "TEST"
# date <- "2023-09-18"
 create_folders(file_path, site, date)

 