# Handles vectorizing a single image if the image exists. Otherwise prints error message. 
# @param i - image number
image.vectorization = function(i){
    library(tiff)
    file.name = paste(directory, '/Updated', i, '.tiff', sep = "")
    
    if (file.exists(file.name)){
      image = readTIFF(file.name)
      image = image[,, 1]
      vec = image[1:(ncol(image) * nrow(image))]
      print(paste("Photo Updated ", i, " vectorized", sep = ""))
      return(vec)
    }
    
    else{
      print(paste("Photo Updated ", i, " not found", sep = ""))
    }
}

# Vectorizes all images and stores them in a matrix. 
# @param directory - where the images come from.
# @param pixel.height - how many columns the matrix should have. Based on height (pixels)
#   of image.
# @param pixel.width - how many rows the matrix should have. Based on width (pixels) of
#   image.
compilation = function(directory, pixel.height, pixel.width){
  matrix = matrix(nrow = 240, ncol = pixel.height * pixel.width)
  counter = 0
  for (i in 1:280){
    file.name = paste(directory, '/Updated', i, '.tiff', sep = "")
    if (file.exists(file.name)){
      counter = counter + 1
      matrix[counter,] = image.vectorization(i)
    }
    
    else {
      print(paste("Photo Updated", i, " not found", sep = ""))
    }
  }
  
  return(matrix)
}