#Practice working with spatial data in R
#Creator: Will Harrod
#Created: 02/02/2024
#Start here ---------------------------------------------------------------------------------
rm(list = ls())

#Load packages
library(tidyverse)
library(raster)
library(sf)
library(tmap)
library(RColorBrewer)

# 1.1) Prepare points ########################################################################

#add in the data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#View the data
glimpse(sobs)

#Set a coordinate reference system
utm_12n <- '+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs'

#Object for point coordinates
points <- sobs %>%
  #tidyverse shenanigans 
  group_by(Full.Point.ID) %>% 
  reframe(Full.Point.ID,
          Route.ID,
          Route.Type,
          UTM.X,
          UTM.Y) %>% 
  distinct(Full.Point.ID, Route.ID, Route.Type, UTM.X, UTM.Y) %>% 
  #transform to a geographic object
  st_as_sf(coords = c("UTM.X", "UTM.Y")) %>% 
  st_set_crs(utm_12n)

#View points
points
points %>% ggplot(aes(col = Route.Type)) + geom_sf()

#1.2 ) Add rasters ############################################################################
#primary file path for rasters
#This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Data\\Spatial\\Geoprocessing_Outputs\\"

#Add in raster layers
sage_cvr <- raster(paste0(ras_path, "sage_cvr.tif"))
pern_cvr <- raster(paste0(ras_path, "pern_cvr.tif"))
elevation <- raster(paste0(ras_path, "elevation.tif"))
fire_dist <- raster(paste0(ras_path, "fire_dist.tif"))
aspect <- raster(paste0(ras_path, "aspect.tif"))
burn_sev <- raster(paste0(ras_path, "burn_sev.tif"))

#Other rasters I can add in if needed
# shrub_cvr <- raster(paste0(ras_path, "shrub_cvr.tif"))
# tri <- raster(paste0(ras_path, "tri.tif"))
# precip <- raster(paste0(ras_path, "precip.tif"))
# bg_cvr <- raster(paste0(ras_path, "bg_cvr.tif"))
# roads_dist <- raster(paste0(ras_path, "road_dist.tif"))

#Add vector layers
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
sage_map <- plot_ras(sage_cvr)
pern_map <- plot_ras(pern_cvr)
elevation_map <- plot_ras(elevation)
asp_map <- plot_ras(aspect)
fd_map <- plot_ras(fire_dist)
burn_sev_map <- plot_ras(burn_sev)

# Plot all rasters
# tmap_arrange(sage_map, pern_map, elevation_map, burn_sev_map, fd_map, asp_map)

# 2.1) Extracting values to a radius around each point ##########################################

# Make an object to store point summaries
point_summaries <- points %>% 
  tibble() %>% 
  dplyr::select(Full.Point.ID, Route.ID, Route.Type)


#Define a radius to summarize rasters
radius <- 125

#summarize sage cover 
point_summaries$Sage.Cover <- raster::extract(x = sage_cvr,
                                              y = points,
                                              buffer = radius,
                                              fun = function(x) mean(x, na.rm = TRUE))
#summarize perennial forb and grass cover
point_summaries$Perennial.Cover <- raster::extract(x = pern_cvr,
                                                   y = points,
                                                   buffer = radius,
                                                   fun = function(x) mean(x, na.rm = TRUE))

#summarize elevation
point_summaries$Elevation <- raster::extract(x = elevation,
                                             y = points,
                                             buffer = radius,
                                             fun = function(x) mean(x, na.rm = TRUE)) 

# #summarize burn sevarity
point_summaries$Burn.Sevarity <- raster::extract(x = burn_sev,
                                                 y = points,
                                                 buffer = radius,
                                                 fun = function(x) modal(x, na.rm = TRUE))

# #summarize aspect
point_summaries$Aspect <- raster::extract(x = aspect,
                                                 y = points,
                                                 buffer = radius,
                                                 fun = function(x) modal(x, na.rm = TRUE))

# #summarize distance to fires
point_summaries$Fire.Dist <- raster::extract(x = fire_dist,
                                                 y = points,
                                                 buffer = radius,
                                                 fun = function(x) mean(x, na.rm = TRUE))

# 2.2) Manual covariates ##########################################################

#define the "refernce" points that actually burned
burn_points_99 <- c("UT-C24-P02",
                    "UT-C24-P03",
                    "UT-C24-P04",
                    "UT-C24-P06",
                    "UT-C24-P07",
                    "UT-C24-P08",
                    "UT-C24-P11",
                    "UT-C24-P12",
                    "UT-C24-P15",
                    "UT-C24-P16",
                    "UT-C25-P01",
                    "UT-C25-P02",
                    "UT-C25-P03",
                    "UT-C25-P04",
                    "UT-C25-P05",
                    "UT-C25-P06",
                    "UT-C25-P07",
                    "UT-C25-P08",
                    "UT-C25-P09",
                    "UT-C25-P11",
                    "UT-C25-P12",
                    "UT-C25-P13",
                    "UT-C25-P14",
                    "UT-C30-P13")

# #Split up the x and y coords
# point_summaries <- point_summaries %>% 
#   mutate(geometry = as.character(geometry)) %>% 
#   mutate(Point.X = str_sub(geometry, start = 3, end = 8)) %>% 
#   mutate(Point.Y = str_sub(geometry, start = 15, end = 21)) %>% 
#   dplyr::select(-geometry) %>% 
#   mutate_at(c('Point.X', 'Point.Y'), as.integer)

#Add in fire name
#add fire names based on the route
point_summaries <- point_summaries %>% 
  mutate(Fire.Name = case_when(Route.ID == 'ID-B04' ~ 'Burnt',
                               Route.ID == 'ID-B07' ~ 'Jim Sage',
                               Route.ID == 'ID-B09' ~ 'Devine Canyon',
                               Route.ID == 'ID-B11' ~ 'Jim Sage',
                               Route.ID == 'ID-B12' ~ 'Emery',
                               Route.ID == 'ID-B13' ~ 'Badger',
                               Route.ID == 'ID-B15' ~ 'Emery',
                               Route.ID == 'ID-B16' ~ 'Cave Canyon',
                               Route.ID == 'ID-B19' ~ 'Cave Canyon',
                               Route.ID == 'ID-B21' ~ 'Black Pine 2',
                               Route.ID == 'ID-B22' ~ 'Burnt',
                               Route.ID == 'ID-B23' ~ 'Deer Hallow',
                               Route.ID == 'ID-B24' ~ 'Burnt',
                               Route.ID == 'ID-B26' ~ 'Cave Canyon',
                               Route.ID == 'ID-B28' ~ 'City of Rocks',
                               Route.ID == 'UT-B01' ~ 'Rosebud',
                               Route.ID == 'UT-B02' ~ 'Playground',
                               Route.ID == 'UT-B05' ~ 'Goose Creek',
                               Route.ID == 'UT-B06' ~ 'Goose Creek', 
                               Route.ID == 'UT-B08' ~ 'Goose Creek',
                               Route.ID == 'UT-B15' ~ 'Wagon Box',
                               Route.ID == 'UT-B16' ~ 'Wagon Box',
                               Route.ID == 'UT-B17' ~ 'Prospect',
                               Route.ID == 'UT-B19' ~ 'City of Rocks',
                               Route.ID == 'UT-B22' ~ 'Goose Creek',
                               Route.ID == 'UT-B24' ~ 'Goose Creek',
                               Route.ID == 'UT-B25' ~ 'Wagon Box',
                               Route.ID == 'UT-B27' ~ 'Dairy Valley',
                               Route.ID == 'UT-B30' ~ 'Dairy Valley',
                               Full.Point.ID %in% burn_points_99 ~ "Dry Mountain")) 

#Add fire year
point_summaries <- point_summaries %>%  
  mutate(Fire.Year = case_when(Fire.Name == "Badger" ~ 2020,
                               Fire.Name == "Black Pine 2" ~ 2007,
                               Fire.Name == "Burnt" ~ 2006,
                               Fire.Name == "Cave Canyon" ~ 2012,
                               Fire.Name == "City of Rocks" ~ 2000,
                               Fire.Name == "Dairy Valley" ~ 2009,
                               Fire.Name == "Deer Hallow" ~ 2012,
                               Fire.Name == "Devine Canyon" ~ 2000,
                               Fire.Name == "Dry Mountain" ~ 1999,
                               Fire.Name == "Emery" ~ 2010,
                               Fire.Name == "Goose Creek" ~ 2018,
                               Fire.Name == "Jim Sage" ~ 2007,
                               Fire.Name == "Playground" ~ 2013,
                               Fire.Name == "Prospect" ~ 2002,
                               Fire.Name == "Rosebud" ~ 2017,
                               Fire.Name == "Wagon Box" ~ 1999))

# Remove zero and negative fire distances
point_summaries <- point_summaries %>%  
  mutate(Fire.Distance = case_when(Fire.Dist > 0 &  Fire.Dist < 10 ~ 10,
                               Fire.Dist < 0 ~ 1000000,
                               TRUE ~ Fire.Dist),
         #Fill in the old fires so there are no miissing values
         Fire.Year = case_when(is.na(Fire.Year) ~ 1850,
                               TRUE ~ Fire.Year)) %>% 
  dplyr::select(-Fire.Dist)

#View again
glimpse(point_summaries)
print(point_summaries, n = Inf)

#export the point summaries
write.csv(point_summaries, "Data\\Outputs\\point_summaries.csv")
#And to my box data folder. Feel free to comment this out
write.csv(sobs, "C:\\Users\\willh\\Box\\Will Harrod MS Project\\Data\\Point_Count\\Cleaned_Data\\point_summaries.csv")

