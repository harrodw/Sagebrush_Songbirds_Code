# Preparing with spatial data in R
# Creator: Will Harrod
# Created: 11/05/2024
# Start here ---------------------------------------------------------------------------------

# Clear environments
rm(list = ls())

# Load packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)
library(RColorBrewer)

# 1.1) Prepare spatial data ########################################################################

# Primary file path for rasters
# This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

#Set a coordinate reference system
utm_12n <- '+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs'

# Add in raster layers
sage_cvr <- rast(paste0(ras_path, "sage_cvr.tif"))
pern_cvr <- rast(paste0(ras_path, "pern_cvr.tif"))
elevation <- rast(paste0(ras_path, "elevation.tif"))

# Add vector layers
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))
fire_perms <- st_read(paste0(ras_path, "Fire_Perimeters.shp")) %>% 
  select(Year, geometry)

# File path for burn sevarity data
mtbs_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\mtbs_cleaned\\"

# Define which years for which I have fire data
fire_years_df <- read.csv(paste0(mtbs_path, "fire_years.csv")) %>% 
  tibble() %>% 
  dplyr::select(-X) %>% 
  dplyr::rename(Year = "x") %>% 
  arrange(Year)

# Transform to a vector
fire_years <- fire_years_df$Year
#...and view
print(fire_years)

#Define the number of years to loop through
nyears <- length(fire_years)
#...and view
print(nyears)

# Start the rDNBR layer with the first year's burn sevarity
rndbr <- raster(paste0(mtbs_path, "rdnbr_", fire_years[1], ".tif"))

# Loop through each year to extract burn severity
for(i in 1:(nyears-1)){ # Skip 2022. For some reason it doesn't work
  
  # define the year for the current itteration
  year <- fire_years[i]
  
  # read in rdnbr for that year
  rdnbr_temp <- raster(paste0(mtbs_path, "rdnbr_", year, ".tif"))
  
  # Replace negative values
  rdnbr_temp[rdnbr_temp < 0] <- NA
  
  # Merge with more recent fires
  rdnbr <- terra::merge(rdnbr, rdnbr_temp, na.rm = TRUE, first = FALSE)
  
} # end the loop through years

# 