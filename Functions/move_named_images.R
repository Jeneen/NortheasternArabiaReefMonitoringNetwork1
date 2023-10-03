move_named_images <- function(base_dir) {
  # Set the directory to the specified path
  setwd(base_dir)
  
  # Create a "Sorted" folder if it doesn't exist
  sorted_folder <- file.path(base_dir, "Sorted")
  if (!dir.exists(sorted_folder)) {
    dir.create(sorted_folder)
  }
  
  # Get a list of all the image files from "Unsorted" folder
  unsorted_folder <- file.path(base_dir, "Unsorted")
  images <- list.files(unsorted_folder, recursive = TRUE, pattern = "\\.jpg$", full.names = TRUE)
  
  # Loop through each image file
  for (img in images) {
    # Split the image name based on "_"
    img_parts <- strsplit(basename(img), "_")[[1]]
    
    # Get the folder names based on the image name
    folder1 <- img_parts[1]  # Before the first "_"
    folder2 <- substr(img_parts[4], 1, nchar(img_parts[4]) - 4)  # After the third "_" and remove last 4 characters
    folder3 <- img_parts[2]  # After the first "_"
    
    # Create the nested subfolders inside "Sorted" if they don't already exist
    nested_folder_path <- file.path(sorted_folder, folder1, folder2, folder3)
    dir.create(nested_folder_path, showWarnings = FALSE, recursive = TRUE)
    
    # Copy the image to the appropriate subfolder in "Sorted"
    file.copy(img, file.path(nested_folder_path, basename(img)))
  }
}

# Example usage:
move_named_images("/Users/jh8695/Documents/GitRepos/NortheasternArabiaReefMonitoringNetwork1/Data/Photoquadrats/Test")
