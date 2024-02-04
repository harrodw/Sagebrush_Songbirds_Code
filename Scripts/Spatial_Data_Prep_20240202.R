#Practice working with spatial data in R
#Creator: Will Harrod
#Created: 02/02/2024
#Start here ---------------------------------------------------------------------------------

#Load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(raster)
library(sf)

#add in the data
sobs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\sobs_data_20240113.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#View the data
glimpse(sobs)

#Create vector data ----------------------------------------------------------
#Set a coordinate reference system
utm_12n <- '+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs'

#Object for point coordinates
points <- sobs %>% 
  group_by(Full.Point.ID) %>% 
  reframe(Full.Point.ID,
          Route.ID,
          Route.Type,
          Point.X = mean(UTM.X , na.rm = TRUE), 
          Point.Y = mean(UTM.Y, na.rm = TRUE)) %>%
  mutate(Point.X = floor(Point.X)) %>% 
  mutate(Point.Y = floor(Point.Y)) %>%
  distinct(Full.Point.ID, Route.ID, Route.Type, Point.X, Point.Y) %>% 
  arrange(Point.X, Point.Y, Route.ID, Route.Type, Full.Point.ID) %>% 
  st_as_sf(coords = c("Point.X", "Point.Y")) %>% 
  st_set_crs(utm_12n)
#View points
points
#points %>% ggplot(aes(col = Route.Type)) + geom_sf()

#Create an object for route centers
route_centers <- sobs %>% 
  group_by(Route.ID) %>% 
  reframe(Route.ID, Route.Type,
          Center.X = mean(UTM.X , na.rm = TRUE), 
          Center.Y = mean(UTM.Y, na.rm = TRUE)) %>%
  mutate(Center.X = floor(Center.X)) %>% 
  mutate(Center.Y = floor(Center.Y)) %>%
  distinct(Route.ID, Route.Type, Center.X, Center.Y) %>% 
  arrange(Center.X, Center.Y, Route.ID, Route.Type) %>% 
  st_as_sf(coords = c("Center.X", "Center.Y")) %>% 
  st_set_crs(utm_12n) %>% 
  arrange(Route.ID)
#View routes
route_centers
#route_centers %>% ggplot(aes(col = Route.Type)) + geom_sf()

#Add rasters ------------------------------------------------------------------------------
#primary file path for rasters
#This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Data\\Spatial\\Geoprocessing_Outputs\\"

#Add in raster layers
sage_cvr <- raster(paste0(ras_path, "sage_cvr.tif"))
anu_cvr <- raster(paste0(ras_path, "anu_cvr.tif"))
perin_cvr <- raster(paste0(ras_path, "herb_cvr.tif"))
shrub_cvr <- raster(paste0(ras_path, "shrub_cvr.tif"))
shrub_hgt <- raster(paste0(ras_path, "shrub_height.tif"))
burn_sev <- raster(paste0(ras_path, "Burn_Sev.tif"))
fire_dist <- raster(paste0(ras_path, "fire_dist.tif"))
elevation <- raster(paste0(ras_path, "elevation.tif"))
aspect <- raster(paste0(ras_path, "aspect.tif"))
tri <- raster(paste0(ras_path, "tri.tif"))
precip <- raster(paste0(ras_path, "precip.tif"))
road_dist <- raster(paste0(ras_path, "road_dist.tif"))

#Create a Topographic ruggedness layer
tri_4x4 <- terrain(x = elevation,
                   opt = "TRI",
                   neighbors = 15)
tri_4x4 #View
tri_5x5 <- terrain(x = elevation,
                   opt = "TRI",
                   neighbors = 15)
tri_5x5 #View
#export tri layers
writeRaster(tri_4x4, 'C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\GIS\\Sobs_Geospatial_Data\\Geoprocessing_Outputs_temp\\tri_4x4.tif')
writeRaster(tri_5x5, 'C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\GIS\\Sobs_Geospatial_Data\\Geoprocessing_Outputs_temp\\tri_5x5.tif')

#make an object to store all of the raster summaries
route_summaries <- route_centers %>% 
                   tibble()
glimpse(route_summaries)

#Extract values to buffers
#summarize Sagebrush cover
route_summaries$Sagebrush.Cover <- raster::extract(x = sage_cvr,
                                                   y = route_centers,
                                                   buffer = 564,
                                                   fun = mean) 
#summarize annual cover
route_summaries$Annual.Cover <- raster::extract(x = anu_cvr,
                                    y = route_centers,
                                    buffer = 564,
                                    fun = mean)

#summarize perennial cover
route_summaries$Perennial.Cover <- raster::extract(x = perin_cvr,
                                             y = route_centers,
                                             buffer = 564,
                                             fun = mean) 
#summarize shrub cover 
route_summaries$Shrub.Cover <- raster::extract(x = shrub_cvr,
                                    y = route_centers,
                                    buffer = 564,
                                    fun = mean)
#summarize shrub height
route_summaries$Shrub.Height <- raster::extract(x = shrub_hgt,
                                    y = route_centers,
                                    buffer = 564,
                                    fun = mean)

#summarize burn sevarity
route_summaries$Burn.Sevarity <- raster::extract(x = burn_sev,
                                                 y = route_centers,
                                                 buffer = 564,
                                                 fun = modal) 
#summarize distance to fire edge
route_summaries$Fire.Distance <- raster::extract(x = fire_dist,
                                                 y = route_centers,
                                                 buffer = 564,
                                                 fun = mean) 
#define inside vs outside of fire
route_summaries <- route_summaries %>% 
  mutate(Fire.Distance = case_when(Route.Type == "B" ~ -Fire.Distance,
                                   TRUE ~ Fire.Distance))

#summarize elevation
route_summaries$Elevation <- raster::extract(x = elevation,
                                             y = route_centers,
                                             buffer = 564,
                                             fun = mean) 
#summarize topographic ruggedness index
route_summaries$TRI <- raster::extract(x = tri,
                                       y = route_centers,
                                       buffer = 564,
                                       fun = mean)
#summarize aspect
route_summaries$Aspect <- raster::extract(x = aspect,
                                          y = route_centers,
                                          buffer = 564,
                                          fun = modal)
#summarize precipitation
route_summaries$Precipitation <- raster::extract(x = precip,
                                             y = route_centers,
                                             buffer = 564,
                                             fun = mean) 
#summarize road distance
route_summaries$Road.Distance <- raster::extract(x = road_dist,
                                             y = route_centers,
                                             buffer = 564,
                                             fun = mean)
#View summaries
glimpse(route_summaries)
print(route_summaries, n = 60)

#add fire information ----------------------------------------------------------
#add fire names based on the route
route_summaries <- route_summaries %>% 
  mutate(Fire.Name = case_when(Route.ID == 'ID-B03' ~ NA,
                               Route.ID == 'ID-B04' ~ 'Burnt',
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
                               Route.ID == 'UT-B30' ~ 'Dairy Valley'))
#view the fire names
glimpse(route_summaries)
unique(route_summaries$Fire.Name)

#Add fire year
route_summaries <- route_summaries %>%  
  mutate(Fire.Year = case_when(Fire.Name == "Badger" ~ 2020,
                               Fire.Name == "Black Pine 2" ~ 2007,
                               Fire.Name == "Burnt" ~ 2006,
                               Fire.Name == "Cave Canyon" ~ 2012,
                               Fire.Name == "City of Rocks" ~ 2000,
                               Fire.Name == "Dairy Valley" ~ 2009,
                               Fire.Name == "Deer Hallow" ~ 2012,
                               Fire.Name == "Devine Canyon" ~ 2000,
                               Fire.Name == "Emery" ~ 2010,
                               Fire.Name == "Goose Creek" ~ 2018,
                               Fire.Name == "Jim Sage" ~ 2007,
                               Fire.Name == "Playground" ~ 2013,
                               Fire.Name == "Prospect" ~ 2002,
                               Fire.Name == "Rosebud" ~ 2017,
                               Fire.Name == "Wagon Box" ~ 1999)) 
#view the fire names
print(route_summaries, n = 30)
unique(route_summaries$Fire.Year)
route_summaries %>%
  ggplot(aes(x = Fire.Year)) +
  geom_bar()

#View the data
glimpse(route_summaries)

#examine spatial data ------------------------------------------------------------------ 

#Plot variables
route_summaries %>% 
  mutate(Aspect = as.factor(Aspect)) %>% 
  ggplot(aes(x = Aspect, y = Shrub.Cover))+ 
  geom_boxplot()

#plot correlation between two specific variables
route_summaries %>% 
  ggplot(aes(x = Shrub.Height, y = Shrub.Cover)) +
  geom_point() +
  geom_smooth() 
 # facet_wrap(~Route.Type)

#test correlation among variables
#First numerically
route_summaries %>%
  filter(Route.Type =="B" & Route.ID != "ID-B03") %>% 
  dplyr::select(-Route.ID, -Route.Type, - geometry, -Fire.Name) %>% 
  cor()

#Then graphically
route_summaries %>%
  filter(Route.Type =="B" & Route.ID != "ID-B03") %>%
  dplyr::select(-Route.ID, -Route.Type, - geometry, -Fire.Name) %>%
  pairs()

#annual and perennial cover are too correlated
#shrub cover and srub height are correlated but more so on reference plots than on burn plots
#precipitation and elevation are too correlated
route_summaries <- route_summaries %>% 
  select(-Shrub.Height, -Precipitation)

#View one last time
glimpse(route_summaries)
  
#looks good. time to export
write.csv(route_summaries, "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv")

#Extract values to 125m buffers around each point -------------------------------------
#make an object to store all of the raster summaries
point_summaries <- points %>% 
  tibble()
glimpse(point_summaries)

#Extract values to buffers
#summarize Sagebrush cover
point_summaries$Sagebrush.Cover <- raster::extract(x = sage_cvr,
                                                   y = points,
                                                   buffer = 125,
                                                   fun = mean) 
#summarize annual cover
point_summaries$Annual.Cover <- raster::extract(x = anu_cvr,
                                                y = points,
                                                buffer = 125,
                                                fun = mean)

#summarize perennial cover
point_summaries$Perennial.Cover <- raster::extract(x = perin_cvr,
                                                   y = points,
                                                   buffer = 125,
                                                   fun = mean) 
#summarize shrub cover 
point_summaries$Shrub.Cover <- raster::extract(x = shrub_cvr,
                                               y = points,
                                               buffer = 125,
                                               fun = mean)
#summarize shrub height
point_summaries$Shrub.Height <- raster::extract(x = shrub_hgt,
                                                y = points,
                                                buffer = 125,
                                                fun = mean)
#summarize burn sevarity
point_summaries$Burn.Sevarity <- raster::extract(x = burn_sev,
                                                 y = points,
                                                 buffer = 125,
                                                 fun = modal) 
#summarize distance to fire edge
point_summaries$Fire.Distance <- raster::extract(x = fire_dist,
                                                 y = points,
                                                 buffer = 125,
                                                 fun = mean) 
#define inside vs outside of fire
point_summaries <- point_summaries %>% 
  mutate(Fire.Distance = case_when(Route.Type == "B" ~ -Fire.Distance,
                                   TRUE ~ Fire.Distance))

#summarize elevation
point_summaries$Elevation <- raster::extract(x = elevation,
                                             y = points,
                                             buffer = 125,
                                             fun = mean) 
#summarize topographic ruggedness index
point_summaries$TRI <- raster::extract(x = tri,
                                       y = points,
                                       buffer = 125,
                                       fun = mean)
#summarize aspect
point_summaries$Aspect <- raster::extract(x = aspect,
                                          y = points,
                                          buffer = 125,
                                          fun = modal)

#summarize road distance
point_summaries$Road.Distance <- raster::extract(x = road_dist,
                                                 y = points,
                                                 buffer = 125,
                                                 fun = mean)
#View summaries
glimpse(point_summaries)
print(point_summaries, n = 60)

#compare variables at the point level --------------------------------------------
#use this if the graphing gets messed up --------------- 
#dev.off()

#Boxplots for catigorical variables
point_summaries %>% 
  mutate(Route.Type = as.factor(Route.Type)) %>% 
  ggplot(aes(x = Route.Type, y = Sagebrush.Cover)) +
  geom_boxplot()

#Plot of a single variable against another
point_summaries %>% 
  ggplot(aes(x = Elevation, y = Sagebrush.Cover, 
             fill = Route.Type, col = Route.Type)) +
  geom_point() +
  geom_smooth() 

#Compare correlation among all variables
point_summaries %>%
  # filter(Route.Type =="B") %>% 
  dplyr::select(-Route.ID, -Route.Type, -Full.Point.ID, -geometry) %>% 
  cor()

#plot corelation among all variables
point_summaries %>% 
  # filter(Route.Type =="B") %>% 
  dplyr::select(-Route.ID, -Route.Type, -Full.Point.ID, -geometry) %>%
  pairs()

#Same as with the rout summaries, I should remove shrub height
#and perennial cover
point_summaries <- point_summaries %>% 
  select(-Shrub.Height)

#export the point summaries
write.csv(point_summaries, "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\point_summaries.csv")
