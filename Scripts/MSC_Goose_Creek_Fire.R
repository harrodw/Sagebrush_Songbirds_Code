# Add packages
library(terra)
library(sf)
library(tidyverse)

# File path for burn sevarity data
mtbs_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\mtbs_cleaned\\"

# Primary file path for rasters
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Read in the 2018 fires
fires_18 <- rast(paste0(mtbs_path, "dnbr_", "2018.tif"))

# Plot the goose creek fire
plot(fires_18)

# Add in the fire perimeters
goose_greek <- st_read(paste0(ras_path, "fire_perimeters.shp")) %>% 
  mutate(Ig.Year = as.numeric(str_extract(Ig_Date, "\\d{4}"))) %>% 
  filter(Incid_Name == "GOOSE CREEK") %>% 
  dplyr::select(geometry, Ig.Year) %>% 
  st_set_crs(crs(fires_18)) 
# View
plot(goose_greek)

# Add shrub cover
shrub_cvr <- rast(paste0(ras_path, "shrub_cvr.tif"))

# clib burn sevarity to that year
gc_rdnbr <- mask(fires_18, goose_greek)
# View
plot(gc_rdnbr)

# Remove negative values
gc_rdnbr[gc_rdnbr < 0] <- NA

# Add in shrub cover
shrub_rast <- rast(paste0(ras_path, "shrub_cvr.tif"))

# Project shrub cover
shrub_prj <- project(shrub_rast, crs(fires_18))
# View
plot(shrub_prj)

# extract shrub cover to the fire parimeter
shrub_msk <- mask(shrub_prj, goose_greek)
# View
plot(shrub_msk)

# Add elevation
elevation <- rast(paste0(ras_path, "elevation.tif"))

# Project elevation
elv_prj <- project(elevation, crs(fires_18))
# View
plot(shrub_prj)

# extract shrub cover to the fire parimeter
elv_msk <- mask(elv_prj, goose_greek)
# View
plot(elv_msk)

# Set a rectangle size (meters)
cell_size <- 1000 

# Number of cells to keep
n_cells <- 300

# Build a fishnet grid
fishnet <- st_make_grid(x = goose_greek,
                          cellsize = cell_size,
                          crs = crs(fires_18),
                          square = TRUE) %>% 
  # Only fully inside fire perimeters
  st_intersection(goose_greek) %>%
  # Switch to an sf object
  st_as_sf() 

# Calculate Area
fishnet$Area = as.numeric(st_area(fishnet))

# # Remove the cells that are only partially in the fires
full_cells <- fishnet[which(fishnet$Area >= 900000),]

# View
plot(full_cells)

# define the number of samples to keep
nsamples <- 200

# Select that many cells
cells <- full_cells %>% 
  st_intersection(goose_greek) %>% 
  slice_sample(n = nsamples)
# View
plot(cells)

# Storage data frame
burn_dat <- data.frame(Shrub.cvr = rep(NA, nsamples),
                       RdNBR = rep(NA, nsamples),
                       Elevation = rep(NA, nsamples))

# Extract elevation
mean_elv <- terra::extract(x = elv_msk,
                            y = cells,
                            fun = function(x) mean(x, na.rm = TRUE))
cells$Elevation <- mean_elv$USGS_13_n42w113_20241031 

# Extract shrub cover
shrub_cvr <- terra::extract(x = shrub_msk,
                                   y = cells,
                                   fun = function(x) mean(x, na.rm = TRUE))
cells$Shrub.cvr <- shrub_cvr$SHR

# Extract burn sevarity
mean_rdnbr <- terra::extract(x = gc_rdnbr,
                            y = cells,
                            fun = function(x) mean(x, na.rm = TRUE))
cells$RdNBR <- mean_rdnbr$dnbr_2018

# Remove the NA cells from nevada
dat <- cells %>% 
  filter(!is.na(RdNBR))

# View the data
dat

# Plot the data
plot(dat)

# View model correlations
rdnbr_lm_low <- dat %>% 
  filter(Elevation < 1800) %>% 
  lm(formula = Shrub.cvr ~ RdNBR)

# Model summary
rdnbr_lm_low_sum <- summary(rdnbr_lm_low)
rdnbr_lm_low_sum

# View model correlations at high elevation
rdnbr_lm_high <- dat %>% 
  filter(Elevation >= 1800) %>% 
  lm(formula = Shrub.cvr ~ RdNBR)

# Model summary at high elevation
rdnbr_lm_high_sum <- summary(rdnbr_lm_high)
rdnbr_lm_high_sum

# Plot correlations 
dat %>% 
  ggplot(aes(x = RdNBR, y = Shrub.cvr)) +
  geom_smooth(method = "lm") +
  geom_jitter()
  