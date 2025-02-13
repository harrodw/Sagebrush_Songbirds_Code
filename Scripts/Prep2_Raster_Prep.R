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
library(elevatr)
library(tmap)
 
# Set a coordinate reference system
# Define the target CRS for UTM zone 12N
target_crs <- "EPSG:32612"

# This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Where the RAP data is stored 
rap_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\RAP\\"

# 2.1) Prepare RAP Cover data ##########################################################################

# Add in the RAP data 
rap <- rast(paste0(rap_path, "RAP_VegCover_2023.tif"))

# Project to UTM 12N
rap_prj <- terra::project(rap, target_crs)

# Shrub cover
shrub <- rap_prj$SHR
# Plot
plot(shrub)
# Perenial forb and grass cover
pfg <- rap_prj$PFG
# Plot
plot(pfg)

# Annual forb and grass cover
afg <- rap_prj$AFG
# Plot
plot(afg)

# Bare Ground cover
bg <- rap_prj$BGR
# Plot
plot(bg)

# Tree Cover
tree <- rap_prj$TRE
# Plot
plot(tree)

# 2.2) Reclassify patches in the rasters ##################################################################

# Matrix for reclassifying shrub cover
shrub_mat <- matrix(c(0, 20, 0,    # Values 0 - 15 are considered not part of the shrub patches
                      20, 100, 1), # Values above 15 are considered shrub patches
                    ncol=3, byrow=TRUE)

# Make a shrub patch raster
shrub_patches <- classify(shrub, shrub_mat, include.lowest=TRUE)
# Plot
plot(shrub_patches)

# Matrix for reclassifying shrub cover
tree_mat <- matrix(c(0, 10, 0,    # Values 0 - 10 are considered not part of the tree patches
                     10, 100, 1), # Values above 10 are considered tree patches
                   ncol=3, byrow=TRUE) 

# Make a tree patch raster
tree_patches <- classify(tree, tree_mat, include.lowest=TRUE)
# Plot
plot(tree_patches)

# 2.3) Clean the elvation data #############################################################################

# Define a path to the DEM's
DEM_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\DEMs"

# List all DEM files in the folder
DEM_list <- list.files(DEM_path, full.names = TRUE)
#...and view
DEM_list

# Count the number of DEM files
num_DEMs <- length(DEM_list)
#...and view
num_DEMs

# Read in the first DEM
elevation <- rast(DEM_list[1])
# First progress message 
message(paste("Added raster 1 out of", num_DEMs))

# Read the rest of the DEMs  and merge them
for(s in 2:num_DEMs){
  # Read in the current raster
  elevation_temp <- rast(DEM_list[s])
  #Project that raster
  # elvation_temp <- project(elevation_temp, utm_12n)
  # Merge that with the existing one
  elevation <- merge(elevation, elevation_temp)
  # Progress Message
  message(paste("Added raster", s, "out of", num_DEMs))
}

# Aggrigate the elevation raster so it has similar reselution to rap
elevation_agg <- terra::aggregate(elevation, fact = 3, fun = "mean")

# Project
elevation_prj <- terra::project(elevation_agg, target_crs)

# 2.4) Make other layers from the DEM raster at multiple scales #######################################

# Calculate Terrain Ruggedness Index in a 40m x 40m window
tri <- terrain(elevation_prj, v = "TRI", neighbors = 4) 
# Plot
plot(tri)

# Calculate Aspect
aspect_cont <- terrain(elevation_prj, v = "aspect", unit = "degrees", neighbors = 4) 
# View aspect
aspect_cont
plot(aspect_cont)

# Make a slope raster
slope <- terrain(elevation_prj, v = "slope", unit = "degrees", neighbors = 4)
# View slope
slope
plot(slope)

# Make a classifyer for flat and not flat raster (15 degrees is the cutoff)
flat_mat <- matrix(c(0, 3, NA,
                      3, 90, 1),
                   ncol = 3, 
                   byrow = TRUE)

# Make the flat not flat raster
flat_rast <- classify(slope, flat_mat, include.lowest = TRUE)
# View the flat raster
flat_rast
plot(flat_rast)

# Only look at aspect where it's hilly
aspect_hill <- mask(x = aspect_cont,
                    mask = flat_rast)
# View the updated aspect
aspect_hill
plot(aspect_hill)

# Matrix for reclassifying Aspect into 8 categories
asp_mat <- matrix(c(0, 22, 1,   
                     22, 67, 2,
                     67, 112, 3,
                     112, 157, 4,
                     157, 202, 5,
                     102, 247, 6,
                     147, 292, 7,
                     292, 337, 8,
                     337, 360, 1),
                  ncol = 3, 
                  byrow = TRUE) 

# Matrix for reclassifying Aspect into 2 categories
south_mat <- matrix(c(0, 22, NA,   
                      22, 67, NA,
                      67, 112, NA,
                      112, 157, 1,
                      157, 202, 1,
                      102, 247, 1,
                      147, 292, NA,
                      292, 337, NA,
                      337, 360, NA),
                    ncol = 3, 
                    byrow = TRUE) 

# Reclassify aspect into 8 catagoriees
aspect_recl <- classify(aspect_hill, asp_mat, include.lowest = TRUE)
# Plot
plot(aspect_recl)
hist(aspect_recl)

# Reclassify aspect into two catagories
south_rast <- classify(aspect_hill, south_mat, include.lowest = TRUE)
# View
south_rast
plot(south_rast)

# 3.0) Export rasters #####################################################################################

# Path to export rasters
# This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Export each one
writeRaster(shrub, paste0(ras_path, "shrub_cvr.tif"), overwrite = TRUE)
writeRaster(pfg, paste0(ras_path, "pfg_cvr.tif"), overwrite = TRUE)
writeRaster(afg, paste0(ras_path, "afg_cvr.tif"), overwrite = TRUE)
writeRaster(bg, paste0(ras_path, "bg_cvr.tif"), overwrite = TRUE)
writeRaster(shrub_patches, paste0(ras_path, "shrub_patches.tif"), overwrite = TRUE)
writeRaster(tree_patches, paste0(ras_path, "tree_patches.tif"), overwrite = TRUE)
writeRaster(elevation_prj, paste0(ras_path, "elevation.tif"), overwrite = TRUE)
writeRaster(tri, paste0(ras_path, "tri.tif"), overwrite = TRUE)
writeRaster(aspect_recl, paste0(ras_path, "aspect.tif"), overwrite = TRUE)
writeRaster(south_rast, paste0(ras_path, "south_facing.tif"), overwrite = TRUE)
