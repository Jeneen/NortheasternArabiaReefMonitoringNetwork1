library(imager)
library(dplyr)

#change threshold value depending on conditions and frame (0 is black 1 is white)
crop_to_quadrat_frame <- function(input_directory, output_directory, threshold_value = 0.75) { 
  # Get filenames into an array
  files <- list.files(input_directory, full.names = TRUE)
  
  for (file_path in files) {
    # Load and preprocess the image
    image <- load.image(file_path)
    grayscale <- grayscale(image)
    thresh_image <- threshold(grayscale, thr = threshold_value)
    
    # Find contours
    contour_list <- contours(thresh_image, nlevels = 10)  # Adjust nlevels as needed
    
    # Initialize variables to hold the properties of the largest contour
    largest_area <- 0
    quadrat_contour <- NULL
    
    # Loop through each contour to find the one that best matches the quadrat frame
    for (i in seq_along(contour_list)) {
      contour <- contour_list[[i]]
      # Calculate the bounding box
      x_min <- min(contour$x)
      x_max <- max(contour$x)
      y_min <- min(contour$y)
      y_max <- max(contour$y)
      # Calculate area and aspect ratio
      width <- x_max - x_min
      height <- y_max - y_min
      area <- width * height
      aspect_ratio <- width / height
      
      # Filter based on expected aspect ratio and minimum size
      if (area > largest_area && abs(aspect_ratio - 1) < 0.2) {
        largest_area <- area
        quadrat_contour <- i
      }
    }
    
    # If a quadrat_contour is found, crop the image
    if (!is.null(quadrat_contour)) {
      selected_contour <- contour_list[[quadrat_contour]]
      x_min <- min(selected_contour$x)
      x_max <- max(selected_contour$x)
      y_min <- min(selected_contour$y)
      y_max <- max(selected_contour$y)
      
      # Calculate how many pixels to remove from each border
      left_border <- x_min - 1
      right_border <- width(image) - x_max
      top_border <- y_min - 1
      bottom_border <- height(image) - y_max
      
      # Crop the image using crop.borders
      cropped_image <- crop.borders(image, nx = c(left_border, right_border), ny = c(top_border, bottom_border))
      
      # Construct the output filename
      output_filename <- paste0(output_directory, "/", tools::file_path_sans_ext(basename(file_path)), "_cropped.png")
      
      # Save the cropped image
      save.image(cropped_image, output_filename)
    }
  }
}

# Example usage of the function:
crop_to_quadrat_frame("Data/uncroppedImages/", "Data/croppedImages/")


# The contours function returns a list of contour lines
# For demonstration, let's plot the original image and overlay the contours
plot(image)
for (i in 1:length(contour_list)) {
lines(contour_list[[i]]$x, contour_list[[i]]$y, col = "red")}
