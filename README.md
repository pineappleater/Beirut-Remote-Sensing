# CASA-work (GISS)
#Script for working with raster data from LANNDSAT 5
#import libraries
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(dplyr)
library(dbscan)
library(fpc)
library(sf)
library(tmap)
#read shapefile made in Qgis
beyshape<-readOGR("/Users/Ahmad/Desktop/Beitut Remote Sensing Project/Beirut round buffer/Buffer_beirut.shp")

#read raster files
Leb_files_1_5_1985<-list.files("/Users/Ahmad/Desktop/Beitut Remote Sensing Project/Landsat 5/LT05_L1TP_174037_19850824_20171212_01_T1",pattern=".*B[12345]\\.tif$",ignore.case=TRUE, full.names=TRUE)
#stack raster files so that formatting can apply to all spectral band simultainiously 
bey_stack_1985<-stack(Leb_files_1_5_1985)
#name the bands for easy use (wavelength included)
names(bey_stack_1985)<-c( 'blue 0.45 - 0.52 µm', 'green 0.52 - 0.60 µm', 'red 0.63 - 0.69 µm', 'NIR 0.77 - 0.90 µm', 'SWIR1 1.55 - 1.75 µm') 

#apply the same as the above for the later data
Leb_files_1_5_2011<-list.files("/Users/Ahmad/Desktop/Beitut Remote Sensing Project/Landsat 5/LT05_L1TP_174037_20090607_20180309_01_T1",pattern=".*B[12345]\\.tif$",ignore.case=TRUE, full.names=TRUE)
bey_stack_2011<-stack(Leb_files_1_5_2011)
names(bey_stack_2011)<-c( 'blue 0.45 - 0.52 µm', 'green 0.52 - 0.60 µm', 'red 0.63 - 0.69 µm', 'NIR 0.77 - 0.90 µm', 'SWIR1 1.55 - 1.75 µm') 

#project shapefile in the same CRS as the raster
beyshape<-spTransform(beyshape, CRS(proj4string(bey_stack_1985)))
e<-extent(beyshape)

#crop and mask each raster stack
raster_cropped_1985<-crop(bey_stack_1985,e)
raster_masked_1985<-mask(raster_cropped_1985,beyshape)
raster_cropped_2011<-crop(bey_stack_2011,e)
raster_masked_2011<-mask(raster_cropped_2011,beyshape)

#plot RGB to see if formatting was successful and to also see if there seems to be issues with the data
Bey_rgb_1985<-plotRGB(stack(raster_masked_1985$red.0.63...0.69.µm, raster_masked_1985$green.0.52...0.60.µm, raster_masked_1985$blue.0.45...0.52.µm), axes=FALSE, stretch="lin")
Bey_rgb_2011<-plotRGB(stack(raster_masked_2011$red.0.63...0.69.µm, raster_masked_2011$green.0.52...0.60.µm, raster_masked_2011$blue.0.45...0.52.µm), axes=FALSE, stretch="lin")

mask(crop(raster_red,e),beyshape)

#raster_cropped_1985<-crop(bey_stack_1985,e)
#raster_masked_1985<-mask(crop(bey_stack_1985,e),beyshape)

#creat function for each index
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)}

NDBIfun <- function(SWIR, NIR) {
  NDBI <- (SWIR - NIR) / (NIR + SWIR)
  return(NDBI)}

NDWIfun <- function(SWIR, Green) {
  NDWI <- (Green-SWIR) / (Green + SWIR)
  return(NDWI)}

#calculate indices and their changes
ndvi <- NDVIfun(raster_masked_1985$NIR.0.77...0.90.µm, raster_masked_1985$red.0.63...0.69.µm)
ndvi2 <- NDVIfun(raster_masked_2011$NIR.0.77...0.90.µm, raster_masked_2011$red.0.63...0.69.µm)
ndvi_change<-ndvi2-ndvi

ndbi <- NDBIfun(raster_masked_1985$SWIR1.1.55...1.75.µm, raster_masked_1985$NIR.0.77...0.90.µm)  
ndbi2 <- NDBIfun(raster_masked_2011$SWIR1.1.55...1.75.µm, raster_masked_2011$NIR.0.77...0.90.µm)  
ndbi_change<-ndbi2-ndbi

ndwi<- NDWIfun(raster_masked_1985$SWIR1.1.55...1.75.µm,raster_masked_1985$green.0.52...0.60.µm)
ndwi2<- NDWIfun(raster_masked_2011$SWIR1.1.55...1.75.µm,raster_masked_2011$green.0.52...0.60.µm)
ndwi_change<-ndwi2-ndwi

#create a data frame from the data
data_stack<-stack(ndbi_change,ndvi_change)
random_data_sample<-sampleRandom(data_stack,50000,cells=TRUE)
plot(random_data_sample[,2],random_data_sample[,3]) 
title(main="NDVI Change Vs NDBI Change",ylab="NDVI Change",xlab="NDBI Change")

randomdf=as.data.frame(random_data_sample)
#rename the coloumns
names(randomdf)<-c("cell", "NDBI ", "NDVI ")

#pretty plot
Changes_plot<-ggplot(randomdf,aes(x=randomdf$NDBI,y=randomdf$NDVI )) +
  geom_point(alpha=2, colour = "#51A0D5")+
  labs(x = "Change in NDBI", 
       y = "Change in NDVI",
       title = "Beirut NDVI-NDBI relationship 1984-2011")+
  geom_smooth(method='lm', se=FALSE)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(Changes_plot)

# removing noisy NDBI values
ndbi_corrected<-reclassify(ndbi_change,cbind(-Inf,0.20,NA))
plot(ndbi_corrected)
