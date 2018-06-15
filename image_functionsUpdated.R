####################################################################################
#   A Set of Functions Used to Featureize the Fingerprint Images
####################################################################################

func_conv_stdev <- function(image, win_size, stride) {
  # DESCRYPTION:
  #    Given a single image file, compute standard deviation of each of the sub_array 
  #    whose size is win_size by win_size with given stride
  #    , and return the mean value of all standard deviations
  
  # ARGUMENTS:
  #    image = array representation of the image with 0-1 scaled real value
  #    win_size = window size
  #    stride = value of the stride (to determine the space between adjacent sub-arrays)
  
  # FUNCTION: 
  
  sd_vec <- NULL
  dim_image <- dim(image)
  row = 1; col = 1;
  while (row + win_size < dim_image[1]) {
    while (col + win_size < dim_image[2]) {
      sub_arr <- image[row:(row + win_size - 1), col:(col + win_size - 1)]
      sd_vec <- c(sd_vec, sd(c(sub_arr)))
      col = col + stride
    }
    row = row + stride
  }
  
  return (mean(sd_vec))
  
}

func_gen_img_qual <- function(N, dir_ = NULL, win_size, stride) {
  # DESCRYPTION:
  
  # ARGUMENTS:
  #    N = number of image files
  #    dir = path of the directory where image files are stored;
  #          do not specify if the files are in current working directory
  
  
  # FUNCTION: 
  if (!is.null(dir_)) {
    dir_ = paste(dir_,"/", sep = "")
  }
  
  image_qual_vec = rep(NA, N)
  
  for (i in 1:N) {
    image_file = paste(dir_, "Updated", i, ".tiff", sep = "")
    
    if (file.exists(image_file)){
      image <- readTIFF(image_file)
      # take only the first slice if image is 3D array
      if (!is.na(dim(image)[3])) {
        image <- image[, , 1]
      }
      
      image_qual_vec[i] <- func_conv_stdev(image, win_size, stride)
    }
  }
  
  return (image_qual_vec)
  
}