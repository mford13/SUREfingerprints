setwd("C:/Users/nmajo/Desktop/CMU SURE")

load("C:/Users/nmajo/Desktop/CMU SURE/printratings_dat.Rda")
load("C:/Users/nmajo/Desktop/CMU SURE/img_featurized.R")
p = cbind(printratings_dat, img_featurized)


library(imager)
library(ggplot2)
library(plyr)
library(dplyr)

# NEW CODE
image.processing = function(directoryFrom = '', image.number = 0){
  current.print = load.image(paste(directoryFrom, image.number, '.png', sep = ""))
  df = as.data.frame(grayscale(current.print))
  m = lm(value ~ x + y, data = df)
  im.f = current.print-fitted(m)
  
  threshold(im.f,"13%") %>% plot
  print ("Processing Complete")
}

save = function(plot, directoryTo = "", image.number = 0){
  tiff(filename = paste(directoryTo, "/Updated", image.number, ".tiff", sep = ""))
  print(plot)
  dev.off()
}

image.cleanup = function(directoryFrom = '', image.number = 0, directoryTo = ''){
  current.print = load.image(paste(directoryFrom, image.number, '.png', sep = ""))
  df = as.data.frame(grayscale(current.print))
  m = lm(value ~ x + y, data = df)
  im.f = current.print-fitted(m)
  print ("Processing Complete")
  
  save(threshold(grayscale(current.print),"13%") %>% plot, directoryTo, image.number)
  print("Saved")
}

image.cleanup('C:/Users/nmajo/Dropbox/Fingerprints for SURE/Converted FingerprintsPNG/', 6, 'C:/Users/nmajo/Desktop')

# END NEW CODE

get.centers = function(im, thr = "99%"){
  dt = imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
  as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% 
    dplyr::summarise(mx = mean(x), my = mean(y))
}

# Compute determinant at scale "scale"
hessdet = function(im, scale = 1) isoblur(im, scale) %>% imhessian %$% { scale^2*(xx*yy - xy^2) }
# Note the scaling (scale^2) factor in the determinant
#plot(hessdet(hub, 1), main = "Determinant of the Hessian at Scale 1")

# function for individual photo
# libraries needed to run function: ggplot2, dplyr, plyr
# parameters
#   - directory: where the image is stored on your computer
#   - image.number: the name of the file. Assumes the name is a number.
image.analysis = function(directory = '', image.number = 0){
  library(ggplot2)
  library(dplyr)
  library(plyr)
  current.print = load.image(paste(directory, image.number, '.png', sep = ""))
  #plot(current.print, main = paste("Image of Fingerprint ", image.number, sep = " "))
  #class(current.print)
  grayscale(current.print) %>% 
    hist(main = paste("Luminance Values of Fingerprint", image.number, sep = " ")) 
  bdf = as.data.frame(current.print)
  gr = imgradient(current.print, "xy")
  #plot(gr, layout = "row")
  
  dx = imgradient(current.print, "x")
  dy = imgradient(current.print, "y")
  grad.mg = sqrt(dx^2 + dy^2)
  #plot(grad.mg)
  
  imhessian(grad.mg)
  Hdet = with(imhessian(grad.mg), (xx*yy - xy^2))
  #plot(Hdet, main = paste("Determinant of Fingerprint", image.number, sep = " "))
  
  threshold(Hdet, "99%") %>% plot(main = "1% Highest Values")
  lab = threshold(Hdet, "99%") %>% label
  #plot(lab, main = "Labelled Regions")
  
  df = as.data.frame(lab) %>% subset(value > 0)
  centers = dplyr::group_by(df,value) %>% dplyr::summarise(mx = mean(x),my = mean(y))
  #plot(grad.mg)
  
  nblobs.denoised = isoblur(grad.mg,2)
  isoblur(grad.mg, 5) %>% get.centers("99.8%") %$% points(mx, my, col = "red")
  
  dat = ldply(c(2,3,4), function(scale) hessdet(grad.mg, scale) %>% as.data.frame %>% 
                mutate(scale=scale))
  p = ggplot(dat,aes(x, y)) + 
    geom_raster(aes(fill = value))+
    facet_wrap(~ scale)+ 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans())
  
  scales = seq(2, 20, l = 10)
  
  d.max = llply(scales, function(scale) hessdet(grad.mg, scale)) %>% parmax
  plot(d.max, main = "Point-Wise Maximum Across Scales")
  
  i.max = llply(scales, function(scale) hessdet(grad.mg,scale)) %>% which.parmax
  plot(i.max, main = "Index of the Point-Wise Maximum \n Across Scales")
  
  # Get a data.frame of labelled regions
  labs = d.max %>% threshold("96%") %>% label %>% as.data.frame
  # Add scale indices
  labs = mutate(labs, index = as.data.frame(i.max)$value)
  regs = dplyr::group_by(labs,value) %>% 
    dplyr::summarise(mx = mean(x), my = mean(y), scale.index = mean(index))
  
  print("Analysis complete")
}

#image.analysis('C:/Users/nmajo/Desktop/CMU SURE/PNGs/', 6)
image.analysis('C:/Users/nmajo/Dropbox/Fingerprints for SURE/Converted FingerprintsPNG/', 6)
bdf = as.data.frame(p6)
View(bdf)

attach(bdf)
lm = lm(value ~ x + y)
summary(lm)

# clean up images
plot(p6)
im.g = grayscale(p6)
plot(im.g)
df = as.data.frame(im.g)
m = lm(value ~ x + y)
threshold(im.g,"13%") %>% plot

image.processing = function(directoryFrom = '', image.number = 0){
  current.print = load.image(paste(directoryFrom, image.number, '.png', sep = ""))
  df = as.data.frame(grayscale(current.print))
  m = lm(value ~ x + y, data = df)
  im.f = current.print-fitted(m)
  
  threshold(im.f,"13%") %>% plot
  print ("Processing Complete")
}

save = function(plot, directoryTo = "", image.number = 0){
  tiff(filename = paste(directoryTo, "/Updated", image.number, ".tiff", sep = ""))
  print(plot)
  dev.off()
}

image.cleanup = function(directoryFrom = '', image.number = 0, directoryTo = ''){
  current.print = load.image(paste(directoryFrom, image.number, '.png', sep = ""))
  df = as.data.frame(grayscale(current.print))
  m = lm(value ~ x + y, data = df)
  im.f = current.print-fitted(m)
  print ("Processing Complete")
  
  save(threshold(grayscale(current.print),"13%") %>% plot, directoryTo, image.number)
  print("Saved")
}

image.cleanup('C:/Users/nmajo/Dropbox/Fingerprints for SURE/Converted FingerprintsPNG/', 6, 'C:/Users/nmajo/Desktop')
