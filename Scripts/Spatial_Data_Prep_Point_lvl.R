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
point_buff <- sobs %>%
  distinct(Full.Point.ID, UTM.X, UTM.Y) %>% 
  st_as_sf(coords = c("UTM.X", "UTM.Y")) %>% 
  st_set_crs(target_crs) %>% 
  st_buffer(dist = point_rad) 
#...and view
point_buff

# Create a storage object for point level covariate information
point_covs <- sobs %>%
  distinct(Full.Point.ID, UTM.X, UTM.Y)
#...and view
glimpse(point_covs)

# 1.2) Add rasters ############################################################################

# Add in raster layers
shrub_cvr <- rast(paste0(ras_path, "shrub_cvr.tif"))
pfg_cvr <- rast(paste0(ras_path, "pfg_cvr.tif"))
afg_cvr <- rast(paste0(ras_path, "afg_cvr.tif"))
bg_cvr <- rast(paste0(ras_path, "bg_cvr.tif"))
shrub_patches <- rast(paste0(ras_path, "shrub_patches.tif"))
tree_patches <- rast(paste0(ras_path, "tree_patches.tif"))
elevation <- rast(paste0(ras_path, "elevation.tif"))
tri <- rast(paste0(ras_path, "tri.tif"))
aspect <- rast(paste0(ras_path, "aspect.tif"))

# Plot the rasters
# plot(shrub_cvr)
# plot(pfg_cvr)
# plot(afg_cvr)
# plot(bg_cvr)
# plot(shrub_patches)
# plot(tree_patches)
# plot(elevation)
# plot(tri)
# plot(aspect)

# 2.1) Extracting values to a 125 radius around each point ##########################################

# summarize shrub cover 
Shrub.Cover <- terra::extract(x = shrub_cvr,
                                   y = point_buff,
                                   fun = function(x) mean(x, na.rm = TRUE))

# Summarize perennial forb and grass cover
Perennial.Cover <- terra::extract(x = pfg_cvr,
                                       y = point_buff,
                                       fun = function(x) mean(x, na.rm = TRUE))
# Summarize annual forb and grass cover
Annual.Cover <- terra::extract(x = afg_cvr,
                                    y = point_buff,
                                    fun = function(x) mean(x, na.rm = TRUE))
# Summarize bare ground cover
Bare.Ground.Cover <- terra::extract(x = bg_cvr,
                                         y = point_buff,
                                         fun = function(x) mean(x, na.rm = TRUE))

# Summarize elevation
Elevation <- terra::extract(x = elevation,
                                 y = point_buff,
                                 fun = function(x) mean(x, na.rm = TRUE)) 

# Summarize ruggedness
TRI <- terra::extract(x = tri,
                           y = point_buff,
                           fun = function(x) mean(x, na.rm = TRUE))


# Add these to the data frame
point_covs$Shrub.Cover <- Shrub.Cover[,2]
point_covs$Perennial.Cover <- Perennial.Cover[,2]
point_covs$Annual.Cover <- Annual.Cover[,2]
point_covs$Bare.Ground.Cover <- Bare.Ground.Cover[,2]
point_covs$Elevation <- Elevation[,2]
point_covs$TRI <- TRI[,2]

# View the new point covariates
glimpse(point_covs)

# 2.2) Patch characteristics for tree and shrub at the 125m point buffer scale #####################################

# Sort the point covariates
point_covs <- point_covs %>% 
  arrange(Full.Point.ID) %>% 
  mutate(Avg.Shrub.Patch.Size = NA,
         n.Shrub.Patches = NA,
         Avg.Tree.Patch.Size = NA,
         n.Tree.Patches = NA)

# List of survey points
npoints <- nrow(point_buff)

# Summarize tree and shrub patches within each point 
for(g in 1:npoints){
  
  # Select a single point
  point <- point_buff[g,] 
  
  # Shrub patch characteristics
  # Crop the shrub patch raster extent
  shrub_patch_crop <- crop(shrub_patches, point)
  # Clip the shrub patch raster to that point buffer
  shrub_patch_clp <- mask(shrub_patch_crop, point)
  # Calculate the average patch area
  avg_area_shrub <- landscapemetrics::lsm_l_area_mn(shrub_patch_clp)
  # Assign average shrub patch area to the point covs 
  point_covs$Avg.Shrub.Patch.Size[g] <- avg_area_shrub$value
  # Calculate the number of shrub patches
  np_shrub <- landscapemetrics::lsm_l_np(shrub_patch_clp)
  # Assign the number of shrub patches to the point covs 
  point_covs$n.Shrub.Patches[g] <- np_shrub$value
  
  # Tree Patch characteristics
  # crop the tree patch raster extent 
  tree_patch_crop <- crop(tree_patches, point)
  # Clip the tree patch raster to that point buffer
  tree_patch_clp <- mask(tree_patch_crop, point)
  # Calculate the average patch area
  avg_area_tree <- landscapemetrics::lsm_l_area_mn(tree_patch_clp)
  # Assign average tree patch area to the point covs 
  point_covs$Avg.Tree.Patch.Size[g] <- avg_area_tree$value
  # Calculate the number of tree patches
  np_tree <- landscapemetrics::lsm_l_np(tree_patch_clp)
  # Assign the number of tree patches to the point covs 
  point_covs$n.Tree.Patches[g] <- np_tree$value
  
  # Progress message
  message(paste("Extracted 125m patch characteristics for point", g, "out of", npoints))
}

# Make a binary Tree presence abcence column
point_covs <- point_covs %>% 
  mutate(Trees.Present = case_when(n.Tree.Patches > 1 ~ 1,
                                    n.Tree.Patches <= 1 ~ 0))


# And view
glimpse(point_covs)

# Export the grid summaries to the current workspace
write.csv(point_covs, "Data\\Outputs\\point_covs.csv")
# And to my box data folder
write.csv(point_covs, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Sagebrush_Songbirds_Code\\Data\\Outputs\\point_covs.csv")
