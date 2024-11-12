#########################################################################################################
# Preparing Raster data 
# Will Harrod
# Utah State University Home Range Lab
# Created 11/12/2024
##########################################################################################################

# 1.0) Prep ##############################################################################################

# Clear Environments
rm(list = ls())

# Add packes 
library(tidyverse)
library(terra)
library(sf)
library(leaflet)
 
# Set a coordinate reference system
utm_12n <- '+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs'

# This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add thee Study Region
study_region <- st_read(paste0(ras_path, "Study_Region.shp")) %>% 
  select(Id)
plot(study_region)

# 2.1) Prepare RAP Cover data ##########################################################################