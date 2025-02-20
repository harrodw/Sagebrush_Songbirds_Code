# Preparing with spatial data in R
# Creator: Will Harrod
# Created: 02/02/2024
# Start here ---------------------------------------------------------------------------------

# Clear Environments
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
  mutate(Ig.Year = as.numeric(str_extract(Ig_Date, "\\d{4}"))) %>% 
  dplyr::select(geometry, Ig.Year) %>% 
  st_transform(target_crs)

# Tmap perameters 
tmap_mode("view")
# tmap_mode("plot")
tmap_options(check.and.fix = TRUE)

# Plot
# tm_shape(fire_perms) +
#   tm_polygons(col = "darkred", alpha = 0.4) +
#   # tm_shape(grid_buff_lg) +
#   # tm_polygons(col = "Grid.Type", palette = c("pink", "lightblue"),
#   #             alpha = 0.7, title = "125m radius") +
#   # tm_shape(grid_buff_med) +
#   # tm_polygons(col = "Grid.Type", palette = c("orange", "turquoise"),
#   #             alpha = 0.7, title = "1km radius") +
#   tm_shape(grid_buff_sm) +
#   tm_polygons(col = "Grid.Type", palette = c("red", "blue"),
#               alpha = 0.7, title = "5km radius")
  
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
south <- rast(paste0(ras_path, "south_facing.tif"))

# Empty raster template for fires
fire_ras_template <- rast(extent = ext(fire_perms), 
                          resolution = c(24.78859, 24.78859), 
                          crs = target_crs)

# Burned vs unburned raster
fires_ras <- rasterize(fire_perms, fire_ras_template)

# Raster with only value one for calculating proportions
ref_rast <- south 
values(ref_rast) <- 1

# The study region
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))

# Plot the rasters
# terra::plot(shrub_cvr, col = viridis(100))
# terra::plot(pfg_cvr, col = magma(100))
# terra::plot(afg_cvr)
# terra::plot(bg_cvr)
# terra::plot(shrub_patches, col = c("cornsilk", "darkcyan"))
# terra::plot(tree_patches, col = c("cornsilk", "darkgreen"))
# terra::plot(elevation)
# terra::plot(tri, col = turbo(100))
# terra::plot(aspect)
# terra::plot(south)
# terra::plot(ref_rast)

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
                             # Custom "proportion south" function
                             fun = function(x) modal(x, na.rm = TRUE))

# Summarize the number of south aspect cells
Count.South.125m <- terra::extract(x = south,
                            y = grid_buff_med,
                            fun = function(x) sum(x, na.rm = TRUE))

# Percent of area that burned
Count.Burned.125m <- terra::extract(x = fires_ras,
                                   y = grid_buff_sm,
                                   fun = function(x) sum(x, na.rm = TRUE))

# Total number of cells
Cell.Count.125m <- terra::extract(x = ref_rast,
                                  y = grid_buff_med,
                                  fun = function(x) sum(x, na.rm = TRUE))

# Add these to the data frame
grid_covs$Shrub.Cover.125m <- Shrub.Cover.125m[,2]
grid_covs$Perennial.Cover.125m <- Perennial.Cover.125m[,2]
grid_covs$Annual.Cover.125m <- Annual.Cover.125m[,2]
grid_covs$Bare.Ground.Cover.125m <- Bare.Ground.Cover.125m[,2]
grid_covs$Elevation.125m <- Elevation.125m[,2]
grid_covs$TRI.125m <- TRI.125m[,2]
grid_covs$Count.South.125m <- Count.South.125m[,2] 
grid_covs$Count.Burned.125m <- Count.Burned.125m[,2]
grid_covs$Cell.Count.125m <- Cell.Count.125m[,2]

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

# Summarize the number of south aspect cells
Count.South.1km <- terra::extract(x = south,
                            y = grid_buff_med,
                            fun = function(x) sum(x, na.rm = TRUE))

# Percent of area that burned
Count.Burned.1km <- terra::extract(x = fires_ras,
                                   y = grid_buff_med,
                                   fun = function(x) sum(x, na.rm = TRUE))

# Total number of cells
Cell.Count.1km <- terra::extract(x = ref_rast,
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
grid_covs$Count.South.1km <- Count.South.1km[,2]
grid_covs$Cell.Count.1km <- Cell.Count.1km[,2]
grid_covs$Count.Burned.1km <- Count.Burned.1km[,2]

# View the new point covariates
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
  rdnbr.1km.tmp <- terra::extract(x = rdnbr_ras, y = grid_buff_med, fun = function(x) mean(x, na.rm = TRUE))
  grid_covs_fire$rdnbr.1km.tmp <- rdnbr.1km.tmp[,2]
    
  # Pull out the values where there were fires at the point for that year
  grid_covs_fire <- grid_covs_fire %>% 
    mutate(rdnbr.125m = case_when(!is.na(rdnbr.125m.tmp) ~ rdnbr.125m.tmp,
                                 TRUE ~ rdnbr.125m),
           rdnbr.1km = case_when(!is.na(rdnbr.1km.tmp) ~ rdnbr.1km.tmp,
                                  TRUE ~ rdnbr.1km),
           Fire.Count = case_when(! is.na(rdnbr.125m.tmp) ~ Fire.Count + 1,
                                  TRUE ~ Fire.Count),
           Fire.Year = case_when(!is.na(rdnbr.125m.tmp) ~ year,
                                 TRUE ~ Fire.Year))
  
  # finished with one itteration
  message(paste("extracted covariates for year:", year))
  
} # end the loop through years

hist(grid_covs_fire$rdnbr.125m)

# Remove the temporary attributes
grid_covs_fire2 <- grid_covs_fire %>% 
  dplyr::select(-rdnbr.125m.tmp, -rdnbr.1km.tmp) %>% 
  # Need to manually add in data for UT-B02 since the fire is too small for mtbs
  # And ID-C22 picked up a tiny sliver of an unsampled fire that I need to remove 
  mutate(Fire.Year = case_when(Grid.ID == "UT-B02" ~ 2017, 
                               Grid.ID == "ID-C22" ~ 1800,
                               TRUE ~ Fire.Year),
         rdnbr.125m = case_when(Grid.ID == "UT-B02" ~ mean(grid_covs_fire$rdnbr.125m[which(grid_covs_fire$rdnbr.125m > 0)]),
                                Grid.ID == "ID-C22" ~ 0,
                                TRUE ~ rdnbr.125m),
         rdnbr.1km = case_when(Grid.ID == "UT-B02" ~ mean(grid_covs_fire$rdnbr.1km[which(grid_covs_fire$rdnbr.1km > 0)]),
                                Grid.ID == "ID-C22" ~ 0,
                                TRUE ~ rdnbr.1km),
         Fire.Count= case_when(Grid.ID == "UT-B02" ~ 1, 
                               Grid.ID == "ID-C22" ~ 0,
                               TRUE ~ Fire.Count)) 
# View the changes
grid_covs_fire2 %>% 
  filter(Grid.ID == "UT-B02")

# Join these to the exisitng covariates
grid_covs_final <- grid_covs %>% 
  left_join(grid_covs_fire2, by = "Grid.ID") %>% 
# Turn count columns into proportions 
  mutate(Prop.South.125m = 100 * Count.South.125m / Cell.Count.125m,
         Prop.South.1km = 100 * Count.South.1km / Cell.Count.1km,
         Prob.Burned.125m = 100 * Count.Burned.125m / Cell.Count.125m,
         Prob.Burned.1km = 100 * Count.Burned.1km / Cell.Count.1km) %>%
  # Remove the cells with only tiny south aspect slopes
  mutate(Prop.South.125m = case_when(Prop.South.125m < 1 ~ 0,
                                     Prop.South.125m > 1 ~ Prop.South.125m),
         Prop.South.1km = case_when(Prop.South.1km < 1 ~ 0,
                                     Prop.South.1km > 1 ~ Prop.South.1km)) %>% 
  # Create a log-scale version of prop burned 
  mutate(ln.Prop.South.125m = case_when(Prop.South.125m == 0 ~ Prop.South.125m,
                                        Prop.South.125m > 0 ~ log(Prop.South.125m)),
         ln.Prop.South.1km = case_when(Prop.South.1km == 0 ~ Prop.South.1km,
                                        Prop.South.1km > 0 ~ log(Prop.South.1km))) %>% 
  # Remove the columns that are no longer needed
  select(-Count.South.125m, -Count.South.1km, 
         -Cell.Count.125m, -Cell.Count.1km,
         -Count.Burned.125m, -Count.Burned.1km)

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

######################################################################################################
# 3) Pre fire vegetation conditions ##################################################################
######################################################################################################

# Clear Environments
rm(list = ls())

# Load packages
library(tidyverse)
library(terra)
library(sf)
library(ggcorrplot)

# Set a coordinate reference system (UTM Zone 12N)
target_crs <- "EPSG:32612"

# Add in the data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#View the data
glimpse(sobs)

# Create a new covariate storage object for the centroid of each survey grid
# (Can replace this with any set of UTM coordinates)
pre_fire_covs <- sobs %>% 
  group_by(Grid.ID) %>% 
  reframe(Grid.ID,
          Grid.Type,
          Grid.X = mean(UTM.X),
          Grid.Y = mean(UTM.Y)) %>% 
  distinct(Grid.ID, Grid.Type, Grid.X, Grid.Y) %>%
  # Add storage covariates (PF = pre fire)
  mutate(
         # Temporary covariates
         Fire.Pixels.tmp = NA,
         Shrub.Cover.tmp = NA,
         PFG.Cover.tmp = NA,
         AFG.Cover.tmp = NA,
         BG.Cover.tmp = NA, 
         Tree.Cover.tmp = NA,
         # Perminant covariates
         Fire.Pixels = NA,
         Fire.Year = NA,
         Shrub.Cover.PF = NA,
         PFG.Cover.PF = NA,
         AFG.Cover.PF = NA,
         BG.Cover.PF = NA,
         Tree.Cover.PF = NA)
# View covs
glimpse(pre_fire_covs)

# Create a new buffer object (my 125m buffer merged around each point)
pre_fire_buff <- sobs %>%
  distinct(Full.Point.ID, Grid.ID, Grid.Type, UTM.X, UTM.Y) %>% 
  st_as_sf(coords = c("UTM.X", "UTM.Y")) %>% 
  st_set_crs(target_crs) %>% 
  st_buffer(dist = 125) %>% 
  group_by(Grid.ID, Grid.Type) %>%
  reframe(geometry = st_union(geometry)) %>% 
  st_as_sf()

# View buffer
pre_fire_buff 

# Path to the fire perimeters
fire_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add in the fire perimeters
fire_perms <- st_read(paste0(fire_path, "fire_perimeters.shp")) %>% 
  mutate(Ig.Year = as.numeric(str_extract(Ig_Date, "\\d{4}"))) %>% 
  dplyr::select(geometry, Ig.Year) %>% 
  st_transform(target_crs)

# File path for veg cover (all RAP years must be in this folder with no other files)
# (If you open any raster in Arc GIS you'll need to go in and delete the temporary files that Arc Creates)
rap_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\RAP\\"

# List of all Rap data years recursively
rap_files <- list.files(rap_path, recursive = TRUE)
# View files
rap_files 

# Create an object holding all years
rap_years <- sort(as.numeric(str_extract(rap_files, "\\d{4}")))
# View years
rap_years

# Define which years for which I have vegetation data
nyears_rap <- length(rap_years)
# View how many years
nyears_rap 

# Loop through each year to extract pre fire vegetation data
start <- Sys.time()  
start # Start time
for(i in 2:(nyears_rap)){ 
  # run a specific year to test the loop (comment out after testing)
  # i <- which(rap_years == 2017)
  
  # define the year for the current iteration
  year <- rap_years[i]
  
  # Select only the fires from that year
  fires <- fire_perms %>% 
    filter(Ig.Year == year)

  # End the loop if there are no fires
  if (nrow(fires) == 0) {
    message(paste0("No fires in the study region during year:  ", year, 
                   " (", i-1, " of ", nyears_rap-1, " years)"))
    next
  }
  
  # Read in veg cover raster for the previous year
  rap_rast <- rast(paste0(rap_path, "RAP_VegCover_", year - 1, ".tif")) %>% 
    project(target_crs) # Project to the same CRS as the fires
  
  # Clip that raster to the fires from that year
  rap_rast_clp <- mask(rap_rast, fires)
  
  # Create a reference raster so that I can see which grids are truely in the fire
  ref_rast <- rap_rast_clp$SHR
  # Only the values inside the fire are coded as 1
  ref_rast[!is.na(ref_rast)] <- 1 

  # Isolate each layer of the raster
  shrub_rast <- rap_rast_clp$SHR # Shrub cover
  pfg_rast <- rap_rast_clp$PFG   # Perennial for and grass cover
  afg_rast <- rap_rast_clp$AFG   # Annual grass cover
  tree_rast <- rap_rast_clp$TRE  # Tree cover
  bg_rast <- rap_rast_clp$BGR    # Bare ground cover

  # Reset a temporary summary object for the mean value of each covariate within the buffer
  pixel_count <- terra::extract(x = ref_rast, y = pre_fire_buff, fun = function(x) sum(x, na.rm = TRUE)) # Summary of how many raster pixels are inside that buffer
  shrub_cvr <- terra::extract(x = shrub_rast, y = pre_fire_buff, fun = function(x) mean(x, na.rm = TRUE)) # Shrub
  pfg_cvr <- terra::extract(x = pfg_rast, y = pre_fire_buff, fun = function(x) mean(x, na.rm = TRUE)) # Perennial forbs and grass
  afg_cvr <- terra::extract(x = afg_rast, y = pre_fire_buff, fun = function(x) mean(x, na.rm = TRUE)) # Annual grasses
  tree_cvr <- terra::extract(x = tree_rast, y = pre_fire_buff, fun = function(x) mean(x, na.rm = TRUE)) # Trees
  bg_cvr <- terra::extract(x = bg_rast, y = pre_fire_buff, fun = function(x) mean(x, na.rm = TRUE)) # Bare ground
  
  # Assign those covariate summaries to the covariates data frame
  pre_fire_covs$Fire.Pixels.tmp <- pixel_count[,2] # Summary of how many raster pixels are inside that buffer
  pre_fire_covs$Shrub.Cover.tmp <- shrub_cvr[,2] # Shrub
  pre_fire_covs$PFG.Cover.tmp <- pfg_cvr[,2] # Perennial forbs and grass
  pre_fire_covs$AFG.Cover.tmp <- afg_cvr[,2] # Annual grasses
  pre_fire_covs$Tree.Cover.tmp <- tree_cvr[,2] # Trees
  pre_fire_covs$BG.Cover.tmp <- bg_cvr[,2] # Bare grund
  
  # Assine the non-NA temp covariates to the perminant ones
  # 600 cells seems like a good cutoff to define grids that fully burned (about 1/4th in the fire)
  pre_fire_covs <- pre_fire_covs %>% 
    mutate(
           Fire.Pixels = case_when(Fire.Pixels.tmp >= 800 ~ Fire.Pixels.tmp,
                                 TRUE ~ Fire.Year), # Number of pixels inside the burn
           Fire.Year = case_when(!is.na(Shrub.Cover.tmp) & Fire.Pixels.tmp >= 800 ~ year,
                                 TRUE ~ Fire.Year), # Year when the most recent fire affected that grid
           Shrub.Cover.PF = case_when(!is.na(Shrub.Cover.tmp) & Fire.Pixels.tmp >= 800 ~ Shrub.Cover.tmp,
                                      TRUE ~ Shrub.Cover.PF), # Shrub
           PFG.Cover.PF = case_when(!is.na(PFG.Cover.tmp) & Fire.Pixels.tmp >= 800 ~ PFG.Cover.tmp,
                                      TRUE ~ PFG.Cover.PF), # Perennial forbs and grass
           AFG.Cover.PF = case_when(!is.na(AFG.Cover.tmp) & Fire.Pixels.tmp >= 800 ~ AFG.Cover.tmp,
                                      TRUE ~ AFG.Cover.PF), # Annual grass
           Tree.Cover.PF = case_when(!is.na(Tree.Cover.tmp) & Fire.Pixels.tmp >= 800 ~ Tree.Cover.tmp,
                                      TRUE ~ Tree.Cover.PF), # Trees
           BG.Cover.PF = case_when(!is.na(BG.Cover.tmp) & Fire.Pixels.tmp >= 800 ~ BG.Cover.tmp,
                                      TRUE ~ BG.Cover.PF) # Bare ground
           )
  
  # Finished with one itteration
  message(paste0("Extracted pre-fire vegitation data for year: ", year, 
                 " (", i-1, " of ", nyears_rap-1, " years)"))
  
} # end the loop through years
difftime(Sys.time(), start) # End time

# Primary file path for rasters
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add in the elevation and current shrub cover rasters
elevation_rast <- rast(paste0(ras_path, "elevation.tif"))
shrub_rast <- rast(paste0(ras_path, "shrub_cvr.tif"))

# Summarize elevation
elevation_mean <- terra::extract(x = elevation_rast,
                                 y = pre_fire_buff,
                                 fun = function(x) mean(x, na.rm = TRUE))
# Summarize current shrub cover
shrub_mean <- terra::extract(x = shrub_rast,
                                 y = pre_fire_buff,
                                 fun = function(x) mean(x, na.rm = TRUE))

# Add elevation and current shrub cover to the covariates
pre_fire_covs$Elevation <- elevation_mean[,2]
pre_fire_covs$Current.Shrub <- shrub_mean[,2]

# Remove the temporary covariates
pre_fire_covs_final <- pre_fire_covs %>% 
  dplyr::select(Grid.ID, Grid.Type, Elevation, Current.Shrub, Fire.Year, Shrub.Cover.PF, PFG.Cover.PF, 
                AFG.Cover.PF, Tree.Cover.PF, BG.Cover.PF, Fire.Pixels, Grid.X, Grid.Y) %>% 
  # Replace NA's
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  # Log-transform Annual cover and Tree cover
  mutate(ln.AFG.Cover.PF = case_when(AFG.Cover.PF > 0 ~ log(AFG.Cover.PF), 
                                     TRUE ~ 0),
         ln.Tree.Cover.PF = case_when(Tree.Cover.PF > 0 ~ log(Tree.Cover.PF), 
                                      TRUE ~  0))
  
# View
glimpse(pre_fire_covs_final)
pre_fire_covs_final %>% 
  arrange(Grid.Type, Grid.ID) %>% 
  print(n = nrow(pre_fire_covs_final))

#Pull out just the covariates on grids with info
pre_fire_corr <- pre_fire_covs_final %>% 
  filter(Fire.Year > 0) %>%  
  select(Elevation, Shrub.Cover.PF, PFG.Cover.PF, AFG.Cover.PF, Tree.Cover.PF, BG.Cover.PF)
# View
print(pre_fire_corr, n = Inf)
  
# Correlations 
cor_mat <- cor(pre_fire_corr)
cor_mat

# P-value correlations
p_mat <- cor_pmat(pre_fire_corr)
p_mat

# Plot correlations
ggcorrplot(cor_mat, 
           title = "Correlation Matrix for Pre-Fire Vegetation Data", 
           lab = TRUE, 
           lab_size = 4,    
           tl.cex = 10,
           p.mat = p_mat, 
           type = "lower",
           method = "square",
           sig.level = 0.05,
           colors = c("red", "white", "blue")) + 
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text.y = element_text(angle = 0, hjust = 1)  
  )
# Looks like all of these are good to put into the model! 

# Export the pre fire summaries to the current workspace
write.csv(pre_fire_covs_final, "Data\\Outputs\\pre_fire_covs.csv")
# And to my box data folder. Feel free to comment this out
write.csv(pre_fire_covs_final, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Sagebrush_Songbirds_Code\\Data\\Outputs\\pre_fire_covs.csv")

