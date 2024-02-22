move_images_from_subfolders <- function(main_directory) {
  # Define the pattern for image files (adjust as needed)
  image_pattern <- "\\.JPG$|\\.png$|\\.gif$|\\.jpeg$|\\.bmp$|\\.tiff$|\\.jpg$"
  
  # Function to recursively list all files in subdirectories
  list_files_in_subdirs <- function(directory, pattern) {
    subdirs <- list.dirs(directory, full.names = TRUE, recursive = FALSE)
    all_files <- unlist(lapply(subdirs, function(subdir) list.files(subdir, pattern = pattern, full.names = TRUE, recursive = TRUE)))
    return(all_files)
  }
  
  # Find all image files within the subdirectories of the main directory
  image_files <- list_files_in_subdirs(main_directory, image_pattern)
  
  # Move each image file to the main directory
  for (file in image_files) {
    # Extract the file name and create a new path in the main directory
    file_name <- basename(file)
    new_path <- file.path(main_directory, file_name)
    
    # Move file to the main directory
    file.rename(file, new_path)
  }
  
  cat("Image files from subfolders have been moved to the main directory.\n")
}

# Usage example
move_images_to_main_dir("/Users/jh8695/Documents/GitRepos/NortheasternArabiaReefMonitoringNetwork1/Data/Photoquadrats/Test/Sorted/Dhabiya")



delete_empty_subdirs <- function(main_directory) {
  # Function to find subdirectories
  find_subdirs <- function(directory) {
    all_dirs <- list.files(directory, full.names = TRUE)
    return(all_dirs[sapply(all_dirs, function(x) file.info(x)$isdir)])
  }
  
  # Get all subdirectories
  subdirs <- find_subdirs(main_directory)
  
  # Loop through each subdirectory and delete if empty
  for (subdir in subdirs) {
    if (length(list.files(subdir)) == 0) {
      unlink(subdir, recursive = TRUE, force = FALSE)
    }
  }
  
  cat("Empty subdirectories have been deleted.\n")
}

# Usage example
delete_empty_subdirs("/Users/jh8695/Documents/GitRepos/NortheasternArabiaReefMonitoringNetwork1/Data/Photoquadrats/Test/Sorted/Dhabiya")

