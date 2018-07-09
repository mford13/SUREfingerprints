# install required packages
list.of.packages <- c("tiff")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tiff)

# setup working directory
setwd("C:/Users/nmajo/Desktop/CMU SURE")

# import functions
source("image_functionsUpdated.R")
#source("image_functions.R")

####################################################################################
#   Main function
####################################################################################
#win_size = 5
#stride = 5
img_featurized_updated = 
  func_gen_img_qual(280, dir_ = "C:/Users/nmajo/Dropbox/Fingerprints for SURE/Processed (480 x 480)/", 
                    win_size = 100, stride = 10)
print(img_featurized_updated)

setwd("C:/Users/nmajo/Desktop/CMU SURE")
save(img_featurized_updated, file = "image_featurized_updated")
