# Preparing with spatial at the point level data in R
# Creator: Will Harrod
# Created: 11/15/2024
# Start here ---------------------------------------------------------------------------------
rm(list = ls())

# Load packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)
library(RColorBrewer)
library(landscapemetrics)

# 1.1) Prepare points ########################################################################

# Add in the data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#View the data
glimpse(sobs)

# Primary file path for rasters
# This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Set a coordinate reference system (UTM Zone 12N)
target_crs <- "EPSG:32612"

# Define radii to summarize rasters
point_rad <- 125

# Create a buffer around each point
point_buffs <- sobs %>%
  distinct(Full.Point.ID, UTM.X, UTM.Y) %>% 
  st_as_sf(coords = c("UTM.X", "UTM.Y")) %>% 
  st_set_crs(target_crs) %>% 
  st_buffer(dist = point_rad) 
#...and view
point_buffs