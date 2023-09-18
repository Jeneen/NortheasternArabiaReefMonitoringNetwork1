extract_image_titles <- function(dir_path) {
  # Set the path to the directory
  my_path <- setwd(dir_path)
  
  # List all the image files in the directory
  image_files <- list.files(my_path, pattern = ".jpg|.jpeg|.png|.gif", recursive = TRUE, full.names = TRUE)
  
  # Create a data frame with file paths
  my_df <- data.frame(file_path = image_files)
  
  # Remove the file path before the image to get the image name
  my_df$file_path <- sub(".*/", "/", my_df$file_path)
  my_df$file_path <- gsub("/", "", my_df$file_path)
  
  # Write to CSV
  write.csv(my_df, "images_titles.csv") 
}

# Example usage:
# extract_image_titles("/Users/jh8695/Documents/GitRepos/ReefCloud/Processed")
