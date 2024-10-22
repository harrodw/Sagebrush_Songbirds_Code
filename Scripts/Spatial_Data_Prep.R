# Preparing with spatial data in R
# Creator: Will Harrod
# Created: 02/02/2024
# Start here ---------------------------------------------------------------------------------
rm(list = ls())

# Load packages
library(tidyverse)
library(raster)
library(sf)
library(tmap)
library(RColorBrewer)

# 1.1) Prepare points ########################################################################

# Add in the data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#View the data
glimpse(sobs)

#Set a coordinate reference system
utm_12n <- '+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs'

# Object for point coordinates
grid_covs <- sobs %>%
  # tidyverse shenanigans 
  group_by(Grid.ID) %>% 
  reframe(Grid.ID,
          Grid.Type,
          Grid.X = mean(UTM.X),
          Grid.Y = mean(UTM.Y)) %>% 
  distinct(Grid.ID, Grid.X, Grid.Y) 

# Transform to a geographic object
grid_centers <- grid_covs %>% 
  st_as_sf(coords = c("Grid.X", "Grid.Y")) %>% 
  st_set_crs(utm_12n)

# View points
grid_centers %>% print(n = Inf)
grid_centers %>% ggplot(aes()) + geom_sf()

#1.2) Add rasters ############################################################################
# Primary file path for rasters
# This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add in raster layers
sage_cvr <- raster(paste0(ras_path, "sage_cvr.tif"))
pern_cvr <- raster(paste0(ras_path, "pern_cvr.tif"))
elevation <- raster(paste0(ras_path, "elevation.tif"))
aspect <- raster(paste0(ras_path, "aspect.tif"))
fire_dist <- raster(paste0(ras_path, "fire_dist.tif"))

# Other rasters I can add in if needed
# shrub_cvr <- raster(paste0(ras_path, "shrub_cvr.tif"))
# tri <- raster(paste0(ras_path, "tri.tif"))
# precip <- raster(paste0(ras_path, "precip.tif"))
# bg_cvr <- raster(paste0(ras_path, "bg_cvr.tif"))
# roads_dist <- raster(paste0(ras_path, "road_dist.tif"))

# Add vector layers
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))

# Function to plot rasters
plot_ras <- function (ras){
  tm_shape(study_region) +  # Add the study region layer
  tm_polygons(fill = "transparent", 
              border.col = "black", 
              alpha = 0.5,  
              title = "Study Region") +
  tm_shape(ras) +  
  tm_raster(palette = brewer.pal(n = 6, name = "YlGnBu"),
            style = "pretty",
            title = NA,  
            colorNA = "transparent", 
            legend.show = TRUE) + 
  tm_layout(frame = FALSE, 
            legend.outside = TRUE, 
            title = "Raster Value")
}

# Make maps of rasters
# shrub_map <- plot_ras(shrub_cvr)
sage_map <- plot_ras(sage_cvr)
pern_map <- plot_ras(pern_cvr)
elevation_map <- plot_ras(elevation)
asp_map <- plot_ras(aspect)
fd_map <- plot_ras(fire_dist)

# Plot all rasters
# tmap_arrange(sage_map, pern_map, elevation_map, asp_map, fd_map)

# 2.1) Extracting values to a radius around each point ##########################################

# Define a radius to summarize rasters
radius <- 657


# summarize sage cover 
grid_covs$Sage.Cover <- raster::extract(x = sage_cvr,
                                              y = grid_centers,
                                              buffer = radius,
                                              fun = function(x) mean(x, na.rm = TRUE))
# Summarize perennial forb and grass cover
grid_covs$Perennial.Cover <- raster::extract(x = pern_cvr,
                                                   y = grid_centers,
                                                   buffer = radius,
                                                   fun = function(x) mean(x, na.rm = TRUE))

# Summarize elevation
grid_covs$Elevation <- raster::extract(x = elevation,
                                             y = grid_centers,
                                             buffer = radius,
                                             fun = function(x) mean(x, na.rm = TRUE)) 

# Summarize aspect
grid_covs$Aspect <- raster::extract(x = aspect,
                                                 y = grid_centers,
                                                 buffer = radius,
                                                 fun = function(x) modal(x, na.rm = TRUE))

# Summarize distance to fires
grid_covs$Fire.Dist <- raster::extract(x = fire_dist,
                                       y = grid_centers,
                                                 buffer = radius,
                                                 fun = function(x) mean(x, na.rm = TRUE))

# View the new point covariates
glimpse(grid_covs)


# 2.2) fire covariates ##########################################################

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
  mutate(# temporarty storage object covariates
    mean.dnbr.tmp = NA,
    mean.rdnbr.tmp = NA,
    sd.dnbr.tmp = NA,
    sd.rdnbr.tmp = NA,
    # permanent covariates
    mean.dnbr = 0,
    mean.rdnbr = 0,
    sd.dnbr = 0,
    sd.rdnbr = 0,
    Fire.Year = 1800,
    Fire.Count = 0)

# Loop through each year to extract burn severity and number of fires
for(i in 1:(nyears-1)){ # Skip 2022. For some reason it doesn't work
  # run a specific year
  # i <- which(fire_years == 1999)
  
  # define the year for the current itteration
  year <- fire_years[i]

  # read in dnbr for that year
  dnbr <- raster(paste0(mtbs_path, "dnbr_", year, ".tif")) 
  
  # replace negative values
  dnbr[dnbr < 0] <- NA
  
  # read in rdnbr for that year
  rdnbr <- raster(paste0(mtbs_path, "rdnbr_", year, ".tif"))
  
  # replace negative values
  rdnbr[rdnbr < 0] <- NA
  
  # reset a temporary points object for the mean dnbr
  grid_covs_fire$mean.dnbr.tmp <- raster::extract(x = dnbr,
                                                   y = grid_centers,
                                                   buffer = radius,
                                                   fun = function(x) mean(x, na.rm = TRUE))
  
  # reset a temporary points object for the sd dnbr
  grid_covs_fire$sd.dnbr.tmp <- raster::extract(x = dnbr,
                                              y = grid_centers,
                                              buffer = radius,
                                              fun = function(x) sd(x, na.rm = TRUE))
  
  # reset a temporary points object for the mean rdnbr
  grid_covs_fire$mean.rdnbr.tmp <- raster::extract(x = rdnbr,
                                                   y = grid_centers,
                                                   buffer = radius,
                                                   fun = function(x) mean(x, na.rm = TRUE))
  
  # reset a temporary points object for the sd rdnbr
  grid_covs_fire$sd.rdnbr.tmp <- raster::extract(x = rdnbr,
                                                 y = grid_centers,
                                                 buffer = radius,
                                                 fun = function(x) sd(x, na.rm = TRUE))
  
  # Pull out the values where there were fires at the point for that year
  grid_covs_fire <- grid_covs_fire %>% 
    mutate(mean.dnbr = case_when(!is.na(mean.dnbr.tmp) ~ mean.dnbr.tmp,
                                 TRUE ~ mean.dnbr),
           sd.dnbr = case_when(!is.na(sd.dnbr.tmp) ~ sd.dnbr.tmp,
                                 TRUE ~ sd.dnbr),
           mean.rdnbr = case_when(!is.na(mean.rdnbr.tmp) ~ mean.rdnbr.tmp,
                                 TRUE ~ mean.rdnbr),
           sd.rdnbr = case_when(!is.na(sd.rdnbr.tmp) ~ sd.rdnbr.tmp,
                               TRUE ~ sd.rdnbr),
           Fire.Count = case_when(!is.na(mean.dnbr.tmp) | ! is.na(mean.rdnbr.tmp) ~ Fire.Count + 1,
                                  TRUE ~ Fire.Count),
           Fire.Year = case_when(!is.na(mean.dnbr.tmp) | ! is.na(mean.rdnbr.tmp) ~ year,
                                 TRUE ~ Fire.Year))
  
  # finished with one itteration
  message(paste("extracted covariates for year:", year))
  
} # end the loop through years

# Remove the temporary attributes
grid_covs_f <- grid_covs_fire %>% 
  dplyr::select(-mean.dnbr.tmp, -mean.rdnbr.tmp, -sd.dnbr.tmp, -sd.rdnbr.tmp) 

# 2.4) Covariates collected on thee ground ###########################################################

# Add in the 2023 point data
points_24_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Points_2024_Raw.csv"))

# Object for renaming 2024 points
points_24_names <- c(Point.ID = "Point..",
                     GlobalID.Survey = "ParentGlobalID",
                     Point.Time = "Start.Time.at.Point",
                     Shrub.Cover = "Percent.of.the.area.within.50m.covered.by.any.shrub.species",
                     Trees.Count = "Count.the.number.of.trees.or.snags.within.50m.of.the.point",
                     Cheatgrass.Cover = "Percent.of.the.area.within.50m.of.the.point.where.cheatgrass.is.present")

# Pull out what I need from the points dataset and rename
points_24 <- points_24_raw %>% 
  dplyr::rename(all_of(points_24_names)) %>% 
  dplyr::select(Point.ID, Point.Time, GlobalID.Survey,
                Shrub.Cover, Trees.Count, Cheatgrass.Cover) %>%  
  mutate(Point.ID = as.character(Point.ID))

# Add in the2024 survey data
surveys_24_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Surveys_2024_Raw.csv"))

#object for renaming 2024 surveys
surveys_24_names <- c(Grid.ID = "Route.ID",
                      GlobalID.Survey = "GlobalID",
                      Visit = "Visit.Number")

#rename and slect useful columns
surveys_24 <- surveys_24_raw %>% 
  dplyr::rename(all_of(surveys_24_names)) %>% 
  dplyr::select(Grid.ID,  Visit, GlobalID.Survey) #standardize some of the variables

# Join the two
dat_24 <- points_24 %>% 
  mutate(Point.ID = as.character(case_when(
    Point.ID == "1" ~ "01",
    Point.ID == "2" ~ "02",
    Point.ID == "3" ~ "03",
    Point.ID == "4" ~ "04",
    Point.ID == "5" ~ "05",
    Point.ID == "6" ~ "06",
    Point.ID == "7" ~ "07",
    Point.ID == "8" ~ "08",
    Point.ID == "9" ~ "09",
    Point.ID == "10" ~ "10",
    Point.ID == "11" ~ "11",
    Point.ID == "12" ~ "12",
    Point.ID == "13" ~ "13",
    Point.ID == "14" ~ "14",
    Point.ID == "15" ~ "15",
    Point.ID == "16" ~ "16", 
    TRUE ~ Point.ID
  ))) %>% 
  left_join(surveys_24, by = "GlobalID.Survey") %>% 
  mutate(Full.Point.ID = paste0(Grid.ID, "-P", Point.ID)) %>% 
  dplyr::select(Grid.ID, Full.Point.ID, Shrub.Cover, Cheatgrass.Cover, Trees.Count, Visit)

#View the cleaned 2024 point data
glimpse(dat_24)

# Average across visits
ground_covs <- dat_24 %>% 
  group_by(Full.Point.ID) %>% 
  reframe(Grid.ID, Full.Point.ID, 
          Cheatgrass.Cover = mean(Cheatgrass.Cover), 
          Trees.Count = mean(Trees.Count)) %>% 
  distinct(Grid.ID, Full.Point.ID, Cheatgrass.Cover, Trees.Count)
#...and view
glimpse(ground_covs)

# Summarize these covariates at the grid level
grid_ground_covs1 <- ground_covs %>% 
  group_by(Grid.ID) %>% 
  reframe(Grid.ID,
          Cheatgrass.Cover = mean(Cheatgrass.Cover),
          Trees.Count = sum(Trees.Count)) %>% 
  distinct(Grid.ID, Cheatgrass.Cover, Trees.Count)
#...and view
glimpse(grid_ground_covs1)

# Swap to binaries
grid_ground_covs <- grid_ground_covs1 %>% 
  mutate(Cheatgrass.Present = case_when(Cheatgrass.Cover <= 1 ~ 0,
                                        Cheatgrass.Cover > 1 ~ 1),
         Trees.Present = case_when(Trees.Count < 1 ~ 0,
                                   Trees.Count >= 1 ~ 1)) %>% 
  dplyr::select(Grid.ID, Cheatgrass.Present, Trees.Present)
#...and view
glimpse(grid_ground_covs)

# Join these to the exisitng covariates
grid_covs <- grid_covs_f %>% 
  left_join(grid_ground_covs, by = "Grid.ID") 

# View the covariate trends
glimpse(grid_covs)
# ggplot(grid_covs, aes(x = mean.dnbr, y = Sage.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()

#export the point summaries
write.csv(grid_covs, "Data\\Outputs\\grid_covs.csv")
# And to my box data folder. Feel free to comment this out
write.csv(sobs, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Sagebrush_Songbirds_Code\\Data\\Outputs\\grid_covs.csv")
