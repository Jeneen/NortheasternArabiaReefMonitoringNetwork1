library(dplyr)

extract_filepaths <- function(dir_path) {
  # Set the path to the directory
  my_path <- setwd(dir_path)
  
  # List all the image files in the directory
  image_files <- list.files(my_path, pattern = ".jpg|.jpeg|.png|.gif", recursive = TRUE, full.names = TRUE)
  
  # Create a data frame with file paths
  my_df <- data.frame(file_path = image_files)
  
  # Remove the file path before 'Processed' 
  my_df$file_path <- sub("^(?:[^/]*/){7}", "/", my_df$file_path)
  
  # Remove image name
  my_df$file_path <- sub("/[^/]*$", "/", my_df$file_path)
  
  # Deduplicate
  my_df <- my_df %>% distinct()
  
  # Write to CSV
  write.csv(my_df, "surveys_filepaths.csv") # change name here
}

# Example usage:
# extract_filepaths("/Users/jh8695/Documents/GitRepos/NortheasternArabiaReefMonitoringNetwork1/Data/Photoquadrats/Test/Processed/2022-11-11")
