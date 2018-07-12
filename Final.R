# FINAL

# load basic data
setwd("C:/Users/nmajo/Desktop/CMU SURE")
load("C:/Users/nmajo/Desktop/CMU SURE/printratings_dat.Rda")
load("C:/Users/nmajo/Desktop/CMU SURE/img_featurized.R")

p = cbind(printratings_dat, img_featurized)
attach(p)

# new metrics
# quality: 0 is a low quality print, 1 is a high quality print
p["meanScore"] = (Score.1 + Score.2 + Score.3 + Score.4) / 4
p["quality"] = NA

attach(p)
p$quality[meanScore >= 0 & meanScore <= 2]= 0
p$quality[meanScore > 2 & meanScore <= 4] = 1

attach(p)

# Image Processing

# Function that handles saving a plot as a png. Used in the image.processing function.
# @param plot - plot to be saved as a file.
# @param directoryTo - where the plot should be saved.
# @param image.number - the number of the image; file should be named with number 
#   (numeric) only.
# @param w - desired width of saved plot
# @param h - desired height of saved plot
save = function(plot, directoryTo = "", image.number = 0, w = 480, h = 480){
  library(tiff)
  writeTIFF(filename = paste(directoryTo, "/Updated", image.number, ".tif", sep = ""),
      width = w, height = h, units = "px", pointsize = 1, bg = "transparent")
  print(plot)
  dev.off()
}

# Function that takes a png file, changes the image to black and white, and saves it as a
#   new file.
# @param directoryFrom - folder the images are coming from.
# @param image.number - the number of the image file; file should be named with number
#   (numeric) only.
# @param directoryTo - where the new images should be saved to. 
# @param w - desired width of saved plot. Default is 480 pixels.
# @param h - desired height of saved plot. Default is 480 pixels. 
image.processing = function(directoryFrom = '', image.number = 0, directoryTo = '', 
                            w = 480, h = 480){
  file.name = paste(directoryFrom, '/', image.number, '.png', sep = "")
  if (file.exists(file.name)){
    current.print = load.image(file.name)
    
    save(plot(threshold(grayscale(current.print), "13%"), axes = FALSE), 
         directoryTo, image.number, w, h)
    print(paste("Photo number ", image.number, " saved", sep = ""))
  }
  
  else {
    print(paste("Photo number ", image.number, " not found", sep = ""))
  }
}

# Set these two variables yourselves
directoryFrom = 'C:/Users/nmajo/Dropbox/Fingerprints for SURE/Converted FingerprintsPNG'
directoryTo = "C:/Users/nmajo/Dropbox/Fingerprints for SURE/Processed (640 x 480)"

# for loop
for (i in 1:280){
  image.processing(directoryFrom, i, directoryTo)
}

# PCA 
# Functions
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

# set this yourself
directory = "C:/Users/nmajo/Dropbox/Fingerprints for SURE/Processed (150 x 150)"

matrix = compilation(directory, 150, 150)
pca = prcomp(matrix)
pca.df = data.frame(pca$x[, 1], pca$x[, 2], pca$x[, 3], pca$x[, 4], pca$x[, 5], 
                    pca$x[, 6], pca$x[, 7], pca$x[, 8], pca$x[, 9], pca$x[, 10])
colnames(pca.df) = c("pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10")

# kPCA
library(kernlab)
kpca = kpca(matrix, kernel = rbfdot)
kpca.df = data.frame(kpca@pcv[, 1], kpca@pcv[, 2], kpca@pcv[, 3], kpca@pcv[, 4], 
                     kpca@pcv[, 5], kpca@pcv[, 6], kpca@pcv[, 7], kpca@pcv[, 8], 
                     kpca@pcv[, 9], kpca@pcv[, 10])
colnames(kpca.df) = c("kpc1", "kpc2", "kpc3", "kpc4", "kpc5", "kpc6", "kpc7", "kpc8", 
                      "kpc9", "kpc10")

# Load Image Data
p.images = subset(p, !is.na(img_featurized))
nrow(p.images) == 240 # verification
p.images = cbind(p.images, pca.df, kpca.df)

# Logistic Regression Models

# Base model
library(ResourceSelection)
logbe = glm(quality ~ weight3 + treatment + finger, 
            data = p, family = "binomial")
logbeF = step(logbe, direction = "both")
summary(logbeF)
AIC(logbeF)

library(pROC)
library(tidyverse) 
set.seed(251) # constant random number generation across different models. Can change. 
train_ids1 = sample(1:nrow(p), size = .5 * nrow(p))
train1 = p %>% filter(1:nrow(p) %in% train_ids1)
test1 = p %>% filter(!(1:nrow(p) %in% train_ids1))

logfit1 = glm(quality ~ treatment, 
              data = train1,
              family = "binomial")

pred_logit1 = predict(logfit1, newdata = test1)
pred_prob1  = predict(logfit1, newdata = test1, type = "response")
pred_y1     = 1*(pred_prob1 > .5)


library(plotROC)
vis_data1 = data.frame(truth = test1$quality,
                       prob = pred_prob1)

# BASE MODEL + PCA
# all data
logp = glm(quality ~ weight3 + treatment + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 +
             pc9 + pc10, 
           data = p.images, family = "binomial")
logpF = step(logp, direction = "both")
summary(logpF)
AIC(logpF)

set.seed(251) # can change but make sure this is the same across all models for comparison. 
train_ids2 = sample(1:nrow(p.images), size = .5 * nrow(p.images))
train2 = p.images %>% filter(1:nrow(p.images) %in% train_ids2)
test2 = p.images %>% filter(!(1:nrow(p.images) %in% train_ids2))

logfit2 = glm(quality ~ treatment + pc1 + pc5 + pc9, 
              data = train2,
              family = "binomial")

pred_logit2 = predict(logfit2, newdata = test2)
pred_prob2  = predict(logfit2, newdata = test2, type = "response")
pred_y2     = 1*(pred_prob2 > .5)

vis_data2 = data.frame(truth = test2$quality,
                       prob = pred_prob2)

# BASE MODEL + kPCA
logk = glm(quality ~ weight3 + treatment + kpc1 + kpc2 + kpc3 + kpc4 + kpc5 + kpc6 + kpc7 +
             kpc8 + kpc9 + kpc10, 
           data = p.images, family = "binomial")
logkF = step(logk, direction = "both")
summary(logkF)
AIC(logkF)

set.seed(251)
train_ids3 = sample(1:nrow(p.images), size = .5 * nrow(p.images))
train3 = p.images %>% filter(1:nrow(p.images) %in% train_ids3)
test3 = p.images %>% filter(!(1:nrow(p.images) %in% train_ids3))

logfit3 = glm(quality ~ treatment + kpc1 + kpc2 + kpc3 + kpc5 + kpc6 + kpc7 + kpc8 + kpc10, 
              data = train3,
              family = "binomial")

pred_logit3 = predict(logfit3, newdata = test3)
pred_prob3  = predict(logfit3, newdata = test3, type = "response")
pred_y3     = 1*(pred_prob3 > .5)

vis_data3 = data.frame(truth = test3$quality,
                       prob = pred_prob3)

# BASE MODEL + IMAGE FEATURIZED
logi = glm(quality ~ weight3 + treatment + img_featurized, 
           data = p.images, family = "binomial")
logiF = step(logi, direction = "both")
summary(logiF)
AIC(logiF)

set.seed(251)
train_ids4 = sample(1:nrow(p.images), size = .5 * nrow(p.images))
train4 = p.images %>% filter(1:nrow(p.images) %in% train_ids4)
test4 = p.images %>% filter(!(1:nrow(p.images) %in% train_ids4))

logfit4 = glm(quality ~ treatment + img_featurized, 
              data = train4,
              family = "binomial")

pred_logit4 = predict(logfit4, newdata = test4)
pred_prob4  = predict(logfit4, newdata = test4, type = "response")
pred_y4     = 1*(pred_prob4 > .5)

vis_data4 = data.frame(truth = test4$quality,
                       prob = pred_prob4)

# NEW MODEL
library(png)
df3 = data_frame(R_Intercept = numeric(), R_ID = numeric(), R_ID2 = numeric(), 
                 C_Intercept = numeric(), C_ID = numeric(), C_ID2 = numeric())

# set this yourself. Location of processed pngs.
directory = "C:/Users/nmajo/Dropbox/Fingerprints for SURE/Converted FingerprintsPNG"

# row means
df3 = data_frame(R_Intercept = numeric(), R_ID = numeric(), R_ID2 = numeric())
for (i in 1:280) {
  file.name = paste(directory, '/', i, '.png', sep = "")
  file.exists(file.name)
  if(file.exists(file.name)){
    img <- readPNG(file.name)
    img <- rowMeans(img)
    img <- as.data.frame(img)
    attach(img)
    img$R_ID <- seq.int(nrow(img))
    colnames(img) <- c("RowMean", "ID")
    R_ID2 <- img$R_ID^2
    quadratic.model <- lm(RowMean ~ R_ID + R_ID2, data = img)
    coef(summary(quadratic.model))
    item_1 <- list(coef(summary(quadratic.model))[1])
    item_2 <- list(coef(summary(quadratic.model))[2])
    item_3 <- list(coef(summary(quadratic.model))[3])
    newRow <- data_frame(R_Intercept = item_1, R_ID = item_2, R_ID2 = item_3)
    df1 <- rbind(df3, newRow)
  }
  else{
    newRow <- data.frame(R_Intercept = "NA", R_ID = "NA", R_ID2 = "NA")
    df1 <- rbind(df3, newRow)
    }
}

  # column means
df3 = data_frame(C_Intercept = numeric(), C_ID = numeric(), C_ID2 = numeric())

for (i in 1:280) {
  file.name = paste(directory, '/', i, '.png', sep = "")
  file.exists(file.name)
  if(file.exists(file.name)){
    img <- readPNG(file.name)
    img <- colMeans(img)
    img <- as.data.frame(img)
    attach(img)
    img$C_ID <- seq.int(nrow(img))
    colnames(img) <- c("ColMean", "ID")
    C_ID2 <- img$C_ID^2
    quadratic.model <- lm(ColMean ~ C_ID + C_ID2, data = img)
    coef(summary(quadratic.model))
    item_1 <- list(coef(summary(quadratic.model))[1])
    item_2 <- list(coef(summary(quadratic.model))[2])
    item_3 <- list(coef(summary(quadratic.model))[3])
    newRow <- data_frame(C_Intercept = item_1, C_ID = item_2, C_ID2 = item_3)
    df3 <- rbind(df3, newRow)
  }
  else{
    newRow <- data.frame(C_Intercept = "NA", C_ID = "NA", C_ID2 = "NA")
    df3 <- rbind(df3, newRow)}
}

foo = read.csv("C:/Users/nmajo/Desktop/CMU SURE/completepdat.csv", header = TRUE)
foo = subset(foo, !is.na(img_featurized))

foo["quality"] = NA
attach(foo)

foo$quality[mean.score >= 0 & mean.score <= 1]= 0
foo$quality[mean.score > 1 & mean.score <= 4] = 1

log = glm(quality ~ ninhydrin + mag.powder + DFO...Ninhydrin + DFO + ID + ID2, 
          data = foo, family = "binomial")
logF = step(log, direction = "both")
summary(logF)
AIC(logF)

set.seed(251)
train_ids5 = sample(1:nrow(foo), size = .5 * nrow(foo))
train5 = foo %>% filter(1:nrow(foo) %in% train_ids5)
test5 = foo %>% filter(!(1:nrow(foo) %in% train_ids5))

logfit5 = glm(quality ~ mag.powder + ID + ID2, 
              data = train5,
              family = "binomial")
pred_logit5 = predict(logfit5, newdata = test5)
pred_prob5  = predict(logfit5, newdata = test5, type = "response")
pred_y5     = 1*(pred_prob5 > .5)

vis_data5 = data.frame(truth = test5$quality,
                       prob = pred_prob5)

log2 = glm(quality ~ ninhydrin + mag.powder + DFO...Ninhydrin + DFO + ID + ID2, 
          data = foo, family = "binomial")
log2F = step(log, direction = "both")
summary(log2F)
AIC(log2F)

set.seed(251)
train_ids6 = sample(1:nrow(foo), size = .5 * nrow(foo))
train6 = foo %>% filter(1:nrow(foo) %in% train_ids6)
test6 = foo %>% filter(!(1:nrow(foo) %in% train_ids6))

logfit6 = glm(quality ~ mag.powder + P_ID2 + RP_ID2, 
              data = train6,
              family = "binomial")
pred_logit6 = predict(logfit6, newdata = test6)
pred_prob6  = predict(logfit6, newdata = test6, type = "response")
pred_y6     = 1*(pred_prob6 > .5)

vis_data6 = data.frame(truth = test6$quality,
                       prob = pred_prob6)

summary(logfit6)
cor(P_ID2, RP_ID2)
View(foo)
basics = subset(foo, select = c(P_ID2, RP_ID2))
View(basics)

# overlaid ROC Curves
roc1 = roc(vis_data1$truth, vis_data1$prob)
roc2 = roc(vis_data2$truth, vis_data2$prob)
roc3 = roc(vis_data3$truth, vis_data3$prob)
roc4 = roc(vis_data4$truth, vis_data4$prob)
roc5 = roc(vis_data5$truth, vis_data5$prob)

plot(roc1, col = "red", main = "ROC Curves for All Logistic Models")
plot(roc2, col = "blue", add = TRUE)
plot(roc3, col = "green", add = TRUE)
plot(roc4, col = "orange", add = TRUE)
plot(roc5, col = "purple", add = TRUE)
legend("bottomright", legend = c("Base Model", "Base Model + PCA", "Base Model + kPCA", 
                                 "Base Model + Image Feat.", 
                                 "Base Model + Parabolic"), 
       fill = c("red", "blue", "green", "orange", "purple"))
