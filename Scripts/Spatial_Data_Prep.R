# Preparing with spatial data in R
# Creator: Will Harrod
# Created: 02/02/2024
# Start here ---------------------------------------------------------------------------------
rm(list = ls())

# Load packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)
library(RColorBrewer)
library(landscapemetrics)
library(viridis)

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
rad_sm <- 125
rad_med <- 1000
rad_lg <- 5000

# Object for point coordinates
grid_covs <- sobs %>%
  # tidyverse shenanigans 
  group_by(Grid.ID) %>% 
  reframe(Grid.ID,
          Grid.Type,
          Grid.X = mean(UTM.X),
          Grid.Y = mean(UTM.Y)) %>% 
  distinct(Grid.ID, Grid.Type, Grid.X, Grid.Y) 

# Transform to a geographic object
grid_centers <- grid_covs %>% 
  st_as_sf(coords = c("Grid.X", "Grid.Y")) %>% 
  st_set_crs(target_crs)

# Create and merge buffers around each point so that all area within a grid is covered
grid_buff_sm <- sobs %>%
  distinct(Full.Point.ID, Grid.ID, Grid.Type, UTM.X, UTM.Y) %>% 
  st_as_sf(coords = c("UTM.X", "UTM.Y")) %>% 
  st_set_crs(target_crs) %>% 
  st_buffer(dist = rad_sm) %>% 
  group_by(Grid.ID, Grid.Type) %>%
  reframe(geometry = st_union(geometry)) %>% 
  st_as_sf()

# Create medium Buffer
grid_buff_med  <- st_as_sf(grid_centers) %>% 
  st_buffer(dist = rad_med)

# Largest buffer
grid_buff_lg <- st_as_sf(grid_centers) %>% 
  st_buffer(dist = rad_lg)


# Add in the fire perimeters
fire_perms <- st_read(paste0(ras_path, "fire_perimeters.shp")) %>% 
  dplyr::select(geometry) %>% 
  st_transform(target_crs)

# Tmap perameters 
tmap_mode("view")
# tmap_mode("plot")
tmap_options(check.and.fix = TRUE)

# Plot
tm_shape(fire_perms) +
  tm_polygons(col = "darkred", alpha = 0.4) +
  tm_shape(grid_buff_lg) +
  tm_polygons(col = "Grid.Type", palette = c("pink", "lightblue"), 
              alpha = 0.7, title = "125m radius") +
  tm_shape(grid_buff_med) +
  tm_polygons(col = "Grid.Type", palette = c("orange", "turquoise"), 
              alpha = 0.7, title = "1km radius") +
  tm_shape(grid_buff_sm) +
  tm_polygons(col = "Grid.Type", palette = c("red", "blue"), 
              alpha = 0.7, title = "5km radius") 
  
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

# Empty raster template for fires
fire_ras_template <- rast(extent = ext(fire_perms), resolution = c(25, 25), crs = target_crs)

# Burned vs unburned raster
fires_ras <- rasterize(fire_perms, fire_ras_template)

# The study region
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))

# Plot the rasters
terra::plot(shrub_cvr, col = viridis(100))
terra::plot(pfg_cvr, col = magma(100))
terra::plot(afg_cvr)
terra::plot(bg_cvr)
terra::plot(shrub_patches, col = c("cornsilk", "darkcyan"))
terra::plot(tree_patches, col = c("cornsilk", "darkgreen"))
terra::plot(elevation)
terra::plot(tri, col = turbo(100))
terra::plot(aspect)

# 2.1) Extracting values to a 125 radius around each point ##########################################

# summarize shrub cover 
Shrub.Cover.125m <- terra::extract(x = shrub_cvr,
                              y = grid_buff_sm,
                              fun = function(x) mean(x, na.rm = TRUE))

# Summarize perennial forb and grass cover
Perennial.Cover.125m <- terra::extract(x = pfg_cvr,
                                  y = grid_buff_sm,
                                  fun = function(x) mean(x, na.rm = TRUE))
# Summarize annual forb and grass cover
Annual.Cover.125m <- terra::extract(x = afg_cvr,
                                  y = grid_buff_sm,
                                  fun = function(x) mean(x, na.rm = TRUE))
# Summarize bare ground cover
Bare.Ground.Cover.125m <- terra::extract(x = bg_cvr,
                               y = grid_buff_sm,
                               fun = function(x) mean(x, na.rm = TRUE))

# Summarize elevation
Elevation.125m <- terra::extract(x = elevation,
                            y = grid_buff_sm,
                            fun = function(x) mean(x, na.rm = TRUE)) 

# Summarize ruggedness
TRI.125m <- terra::extract(x = tri,
                            y = grid_buff_sm,
                            fun = function(x) mean(x, na.rm = TRUE))
# Summarize aspect
Aspect.125m <- terra::extract(x = aspect,
                             y = grid_buff_sm,
                             fun = function(x) modal(x, na.rm = TRUE))

# Percent of area that burned
Prop.Burned.125m <- terra::extract(x = fires_ras,
                                   y = grid_buff_sm,
                                   fun = function(x) sum(x, na.rm = TRUE))

# Add these to the data frame
grid_covs$Shrub.Cover.125m <- Shrub.Cover.125m[,2]
grid_covs$Perennial.Cover.125m <- Perennial.Cover.125m[,2]
grid_covs$Annual.Cover.125m <- Annual.Cover.125m[,2]
grid_covs$Bare.Ground.Cover.125m <- Bare.Ground.Cover.125m[,2]
grid_covs$Elevation.125m <- Elevation.125m[,2]
grid_covs$TRI.125m <- TRI.125m[,2]
grid_covs$Prop.Burned.125m <- Prop.Burned.125m[,2] 

# View the new point covariates
glimpse(grid_covs)

# 2.2) Extracting values to a 1km radius around each grid ##########################################

# summarize shrub cover 
Shrub.Cover.1km <- terra::extract(x = shrub_cvr,
                              y = grid_buff_med,
                              fun = function(x) mean(x, na.rm = TRUE))

# Summarize perennial forb and grass cover
Perennial.Cover.1km <- terra::extract(x = pfg_cvr,
                                  y = grid_buff_med,
                                  fun = function(x) mean(x, na.rm = TRUE))
# Summarize annual forb and grass cover
Annual.Cover.1km <- terra::extract(x = afg_cvr,
                               y = grid_buff_med,
                               fun = function(x) mean(x, na.rm = TRUE))
# Summarize bare ground cover
Bare.Ground.Cover.1km <- terra::extract(x = bg_cvr,
                                    y = grid_buff_med,
                                    fun = function(x) mean(x, na.rm = TRUE))

# Summarize elevation
Elevation.1km <- terra::extract(x = elevation,
                            y = grid_buff_med,
                            fun = function(x) mean(x, na.rm = TRUE)) 

# Summarize ruggedness
TRI.1km <- terra::extract(x = tri,
                      y = grid_buff_med,
                      fun = function(x) mean(x, na.rm = TRUE))

# Summarize aspect
Aspect.1km <- terra::extract(x = aspect,
                         y = grid_buff_med,
                         fun = function(x) modal(x, na.rm = TRUE))
# Percent of area that burned
Prop.Burned.1km <- terra::extract(x = fires_ras,
                                   y = grid_buff_med,
                                   fun = function(x) sum(x, na.rm = TRUE))

# Add these to the data frame
grid_covs$Shrub.Cover.1km <- Shrub.Cover.1km[,2]
grid_covs$Perennial.Cover.1km <- Perennial.Cover.1km[,2]
grid_covs$Annual.Cover.1km <- Annual.Cover.1km[,2]
grid_covs$Bare.Ground.Cover.1km <- Bare.Ground.Cover.1km[,2]
grid_covs$Elevation.1km <- Elevation.1km[,2]
grid_covs$TRI.1km <- TRI.1km[,2]
grid_covs$Aspect.1km <- Aspect.1km[,2]
grid_covs$Prop.Burned.1km <- Prop.Burned.1km[,2]

# View the new point covariates
glimpse(grid_covs)

# 2.3) Extracting values to a 5km radius around each grid ##########################################

# summarize shrub cover 
Shrub.Cover.5km <- terra::extract(x = shrub_cvr,
                                  y = grid_buff_lg,
                                  fun = function(x) mean(x, na.rm = TRUE))

# Summarize perennial forb and grass cover
Perennial.Cover.5km <- terra::extract(x = pfg_cvr,
                                      y = grid_buff_lg,
                                      fun = function(x) mean(x, na.rm = TRUE))
# Summarize annual forb and grass cover
Annual.Cover.5km <- terra::extract(x = afg_cvr,
                                   y = grid_buff_lg,
                                   fun = function(x) mean(x, na.rm = TRUE))
# Summarize bare ground cover
Bare.Ground.Cover.5km <- terra::extract(x = bg_cvr,
                                        y = grid_buff_lg,
                                        fun = function(x) mean(x, na.rm = TRUE))

# Summarize elevation
Elevation.5km <- terra::extract(x = elevation,
                                y = grid_buff_lg,
                                fun = function(x) mean(x, na.rm = TRUE)) 

# Summarize ruggedness
TRI.5km <- terra::extract(x = tri,
                          y = grid_buff_lg,
                          fun = function(x) mean(x, na.rm = TRUE))
# Percent of area that burned
Prop.Burned.5km <- terra::extract(x = fires_ras,
                                   y = grid_buff_lg,
                                   fun = function(x) sum(x, na.rm = TRUE))

# Add these to the data frame
grid_covs$Shrub.Cover.5km <- Shrub.Cover.5km[,2]
grid_covs$Perennial.Cover.5km <- Perennial.Cover.5km[,2]
grid_covs$Annual.Cover.5km <- Annual.Cover.5km[,2]
grid_covs$Bare.Ground.Cover.5km <- Bare.Ground.Cover.5km[,2]
grid_covs$Elevation.5km <- Elevation.5km[,2]
grid_covs$TRI.5km <- TRI.5km[,2]
grid_covs$Prop.Burned.5km <- 100 * Prop.Burned.5km[,2] / 125663

# View the new point covariates
glimpse(grid_covs)

# 2.5) Patch characteristics for tree and shrub at the 125m point buffer scale #####################################

# Sort the grid covariates
grid_covs <- grid_covs %>% 
  arrange(Grid.ID) %>% 
  mutate(Avg.Shrub.Patch.Size.125m = NA,
         n.Shrub.Patches.125m = NA,
         Avg.Tree.Patch.Size.125m = NA,
         n.Tree.Patches.125m = NA)

# List of survey grids
ngrids <- nrow(grid_buff_sm)

# Summarize tree and shrub patches within each grid 
for(g in 1:ngrids){

  # Select a single grid
  grid <- grid_buff_sm[g,] %>% dplyr::select(Grid.Type, geometry)
  
  # Shrub patch characteristics
  # Crop the shrub patch raster extent
  shrub_patch_crop <- crop(shrub_patches, grid)
  # Clip the shrub patch raster to that grid buffer
  shrub_patch_clp <- mask(shrub_patch_crop, grid)
  # Calculate the average patch area
  avg_area_shrub <- landscapemetrics::lsm_l_area_mn(shrub_patch_clp)
  # Assign average shrub patch area to the grid covs 
  grid_covs$Avg.Shrub.Patch.Size.125m[g] <- avg_area_shrub$value
  # Calculate the number of shrub patches
  np_shrub <- landscapemetrics::lsm_l_np(shrub_patch_clp)
  # Assign the number of shrub patches to the grid covs 
  grid_covs$n.Shrub.Patches.125m[g] <- np_shrub$value
  
  # Tree Patch characteristics
  # crop the tree patch raster extent 
  tree_patch_crop <- crop(tree_patches, grid)
  # Clip the tree patch raster to that grid buffer
  tree_patch_clp <- mask(tree_patch_crop, grid)
  # Calculate the average patch area
  avg_area_tree <- landscapemetrics::lsm_l_area_mn(tree_patch_clp)
  # Assign average tree patch area to the grid covs 
  grid_covs$Avg.Tree.Patch.Size.125m[g] <- avg_area_tree$value
  # Calculate the number of tree patches
  np_tree <- landscapemetrics::lsm_l_np(tree_patch_clp)
  # Assign the number of tree patches to the grid covs 
  grid_covs$n.Tree.Patches.125m[g] <- np_tree$value
  
  # Progress message
  message(paste("Extracted 125m patch characteristics for grid", g, "out of", ngrids))
}

# And view
glimpse(grid_covs)

# 2.5) Patch characteristics for tree and shrub at the 1km grid radius scale #####################################
 
# Sort the grid covariates
grid_covs <- grid_covs %>% 
  arrange(Grid.ID) %>% 
  mutate(Avg.Shrub.Patch.Size.1km = NA,
         n.Shrub.Patches.1km = NA,
         Avg.Tree.Patch.Size.1km = NA,
         n.Tree.Patches.1km = NA)

# List of survey grids
ngrids <- nrow(grid_buff_med)

# Summarize tree and shrub patches within each grid
for(g in 1:ngrids){
  
  # Select a single grid
  grid <- grid_buff_med[g,] %>% dplyr::select(Grid.Type, geometry)
  
  # Shrub patch characteristics
  # Crop the shrub patch raster extent
  shrub_patch_crop <- crop(shrub_patches, grid)
  # Clip the shrub patch raster to that grid buffer
  shrub_patch_clp <- mask(shrub_patch_crop, grid)
  # Calculate the average patch area
  avg_area_shrub <- landscapemetrics::lsm_l_area_mn(shrub_patch_clp)
  # Assign average shrub patch area to the grid covs 
  grid_covs$Avg.Shrub.Patch.Size.1km[g] <- avg_area_shrub$value
  # Calculate the number of shrub patches
  np_shrub <- landscapemetrics::lsm_l_np(shrub_patch_clp)
  # Assign the number of shrub patches to the grid covs 
  grid_covs$n.Shrub.Patches.1km[g] <- np_shrub$value
  
  # Tree Patch characteristics
  # crop the tree patch raster extent 
  tree_patch_crop <- crop(tree_patches, grid)
  # Clip the tree patch raster to that grid buffer
  tree_patch_clp <- mask(tree_patch_crop, grid)
  # Calculate the average patch area
  avg_area_tree <- landscapemetrics::lsm_l_area_mn(tree_patch_clp)
  # Assign average tree patch area to the grid covs 
  grid_covs$Avg.Tree.Patch.Size.1km[g] <- avg_area_tree$value
  # Calculate the number of tree patches
  np_tree <- landscapemetrics::lsm_l_np(tree_patch_clp)
  # Assign the number of tree patches to the grid covs 
  grid_covs$n.Tree.Patches.1km[g] <- np_tree$value
  
  # Progress message
  message(paste("Extracted 1km patch characteristics for grid", g, "out of", ngrids))
}

# And view
glimpse(grid_covs)

# 2.5) Patch characteristics for tree and shrub at the 5km grid scale #####################################

# Sort the grid covariates
grid_covs <- grid_covs %>% 
  arrange(Grid.ID) %>% 
  mutate(Avg.Shrub.Patch.Size.5km = NA,
         n.Shrub.Patches.5km = NA,
         Avg.Tree.Patch.Size.5km = NA,
         n.Tree.Patches.5km = NA)

# List of survey grids
ngrids <- nrow(grid_buff_sm)

# Summarize tree and shrub patches within each grid 
for(g in 1:ngrids){
  
  # Select a single grid
  grid <- grid_buff_lg[g,] %>% dplyr::select(Grid.Type, geometry)
  
  # Shrub patch characteristics
  # Crop the shrub patch raster extent
  shrub_patch_crop <- crop(shrub_patches, grid)
  # Clip the shrub patch raster to that grid buffer
  shrub_patch_clp <- mask(shrub_patch_crop, grid)
  # Calculate the average patch area
  avg_area_shrub <- landscapemetrics::lsm_l_area_mn(shrub_patch_clp)
  # Assign average shrub patch area to the grid covs 
  grid_covs$Avg.Shrub.Patch.Size.5km[g] <- avg_area_shrub$value
  # Calculate the number of shrub patches
  np_shrub <- landscapemetrics::lsm_l_np(shrub_patch_clp)
  # Assign the number of shrub patches to the grid covs 
  grid_covs$n.Shrub.Patches.5km[g] <- np_shrub$value
  
  # Tree Patch characteristics
  # crop the tree patch raster extent 
  tree_patch_crop <- crop(tree_patches, grid)
  # Clip the tree patch raster to that grid buffer
  tree_patch_clp <- mask(tree_patch_crop, grid)
  # Calculate the average patch area
  avg_area_tree <- landscapemetrics::lsm_l_area_mn(tree_patch_clp)
  # Assign average tree patch area to the grid covs 
  grid_covs$Avg.Tree.Patch.Size.5km[g] <- avg_area_tree$value
  # Calculate the number of tree patches
  np_tree <- landscapemetrics::lsm_l_np(tree_patch_clp)
  # Assign the number of tree patches to the grid covs 
  grid_covs$n.Tree.Patches.5km[g] <- np_tree$value

  
  # Progress message
  message(paste("Extracted patch 5km characteristics for grid", g, "out of", ngrids))
}

# And view
glimpse(grid_covs)

# 2.3) fire covariates ########################################################################

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

# Add storage covariates to the point summaries
grid_covs_fire <- grid_covs %>% 
  dplyr::select(Grid.ID) %>% 
  mutate(
    # temporarty storage object covariates
    rdnbr.125m.tmp = NA,
    rdnbr.1km.tmp = NA,
    # permanent covariates
    rdnbr.125m = 0,
    rdnbr.1km = 0,
    Fire.Year = 1800,
    Fire.Count = 0)

# Loop through each year to extract burn severity and number of fires
for(i in 1:(nyears-1)){ # Skip 2022. For some reason it doesn't work
  # run a specific year
  # i <- which(fire_years == 1999)
  
  # define the year for the current itteration
  year <- fire_years[i]
  
  # read in rdnbr for that year
  rdnbr_ras <- rast(paste0(mtbs_path, "rdnbr_", year, ".tif"))
  
  # replace negative values
  rdnbr_ras[rdnbr_ras < 0] <- NA
  
  # reset a temporary points object for the mean rdnbr at the 125m scale
  rdnbr.125m.tmp <- terra::extract(x = rdnbr_ras, y = grid_buff_sm, fun = function(x) mean(x, na.rm = TRUE))
  grid_covs_fire$rdnbr.125m.tmp <- rdnbr.125m.tmp[,2]
  
  # reset a temporary points object for the mean rdnbr at the 1km scale
  rdnbr.1km.tmp <- terra::extract(x = rdnbr_ras, y = grid_buff_sm, fun = function(x) mean(x, na.rm = TRUE))
  grid_covs_fire$rdnbr.1km.tmp <- rdnbr.1km.tmp[,2]
    
  # Pull out the values where there were fires at the point for that year
  grid_covs_fire <- grid_covs_fire %>% 
    mutate(rdnbr.125m = case_when(!is.na(rdnbr.125m.tmp) ~ rdnbr.125m.tmp,
                                 TRUE ~ rdnbr.125m),
           rdnbr.1km = case_when(!is.na(rdnbr.1km.tmp) ~ rdnbr.1km.tmp,
                                  TRUE ~ rdnbr.1km),
           Fire.Count = case_when(! is.na(rdnbr.125m.tmp) ~ Fire.Count + 1,
                                  TRUE ~ Fire.Count),
           Fire.Year = case_when(! is.na(rdnbr.125m.tmp) ~ year,
                                 TRUE ~ Fire.Year))
  
  # finished with one itteration
  message(paste("extracted covariates for year:", year))
  
} # end the loop through years

# Remove the temporary attributes
grid_covs_fire2 <- grid_covs_fire %>% 
  dplyr::select(-rdnbr.125m.tmp, -rdnbr.1km.tmp) %>% 
  # Need to manually add in data for UT-B02 since the fire is too small foor mtbs
  mutate(Fire.Year = case_when(Grid.ID == "UT-B02" ~ 2017, 
                               TRUE ~ Fire.Year),
         rdnbr.125m = case_when(Grid.ID == "UT-B02" ~ mean(grid_covs_fire$rdnbr.125m[which(grid_covs_fire$rdnbr.125m != 0)]), 
                               TRUE ~ rdnbr.125m),
         Fire.Count= case_when(Grid.ID == "UT-B02" ~ 1, 
                               TRUE ~ Fire.Count))
# View the changes
grid_covs_fire2 %>% 
  filter(Grid.ID == "UT-B02")

# Join these to the exisitng covariates
grid_covs_final <- grid_covs %>% 
  left_join(grid_covs_fire2, by = "Grid.ID") %>% 
  # Log transform certain covariates
  mutate(ln.Shrub.Patch.Size.125m = log(Avg.Shrub.Patch.Size.125m),
         ln.Shrub.Patch.Size.1km = log(Avg.Shrub.Patch.Size.1km),
         ln.Shrub.Patch.Size.5km = log(Avg.Shrub.Patch.Size.5km),
         ln.Tree.Patch.Size.125m = log(Avg.Tree.Patch.Size.125m),
         ln.Tree.Patch.Size.1km = log(Avg.Tree.Patch.Size.1km),
         ln.Tree.Patch.Size.5km = log(Avg.Tree.Patch.Size.5km)) %>% 
  # Make a binary variable for whether or not trees are present
  mutate(Trees.Present.125m = case_when(n.Tree.Patches.125m > 1 ~ 1,
                                        n.Tree.Patches.125m <= 1 ~ 0),
         Trees.Present.1km = case_when(n.Tree.Patches.1km > 1 ~ 1,
                                        n.Tree.Patches.1km <= 1 ~ 0),
         Trees.Present.5km = case_when(n.Tree.Patches.5km > 1 ~ 1,
                                        n.Tree.Patches.5km <= 1 ~ 0))

# View all covariates
glimpse(grid_covs_final)

# View the covariate trends
ggplot(grid_covs_final, aes(x = rdnbr.125m, y = Shrub.Cover.125m)) +
  geom_smooth(method = "lm") +
  geom_point()

# Export the grid summaries to the current workspace
write.csv(grid_covs_final, "Data\\Outputs\\grid_covs.csv")
# And to my box data folder. Feel free to comment this out
write.csv(grid_covs_final, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Sagebrush_Songbirds_Code\\Data\\Outputs\\grid_covs.csv")

