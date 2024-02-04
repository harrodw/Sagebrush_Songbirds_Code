#Sagebrush obligate songbird responce to fire
#Comparing correlation among spatial covariates
#last editted 01/24/2023

#start here --------------------------------------------------------------------------------------------------------

#Install packages
library(raster)
library(tidyverse)

#set working directory 
setwd("C:\\Users\\Will\\Desktop\\USU\\SOBs\\GIS\\Project_Folders\\Sobs_Geospatial_Data\\Geoprocessing_Outputs_temp")

#add data 
#sagebrush cover
sage <- raster("sage_cvr_23.tif")
plot(sage)

#shrub cover
shrub <- raster("shrub_cvr_23.tif")
plot(shrub)

#model sage cover as a function of shrub cover
sage_shrub_lm <- lm(sage$sage_cvr_23 ~ shrub$shrub_cvr_23)
summary(sage_shrub_lm)