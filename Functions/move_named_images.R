move_named_images <- function() {
  # Set the directory to the specified path
  setwd("/Users/jh8695/Documents/GitRepos/NortheasternArabiaReefMonitoringNetwork1/Data/Photoquadrats/Test/Unsorted")
  
  # Get a list of all the image files
  images <- list.files(recursive = TRUE, pattern = "\\.jpg$", full.names = TRUE)
  
  # Loop through each image file
  for (img in images) {
    # Split the image name based on "_"
    img_parts <- strsplit(basename(img), "_")[[1]]
    
    # Get the folder names based on the image name
    folder1 <- img_parts[1]  # Before the first "_"
    folder2 <- substr(img_parts[4], 1, nchar(img_parts[4]) - 4)  # After the third "_" and remove last 4 characters
    folder3 <- img_parts[2]  # After the first "_"
    
    # Create the nested subfolders if they don't already exist
    nested_folder_path <- file.path(dirname(img), folder1, folder2, folder3)
    dir.create(nested_folder_path, showWarnings = FALSE, recursive = TRUE)
    
    # Move the image to the appropriate subfolder
    file.rename(img, file.path(nested_folder_path, basename(img)))
  }
}

move_named_images()
