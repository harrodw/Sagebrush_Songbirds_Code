#Basic occupancy models following methods from WILD 6900

#add in presence absence data --------------------------------------------------
#Load packages
library(tidyverse)

#add in data
sobs <- read.csv("C:\\Users\\Will\\Desktop\\USU\\SOBs\\Data\\sobs_data_20240113.csv") %>% 
  select(-X) %>% 
  tibble()
str(sobs)

#Start Here ---------------------------------------------------------------------------------------
#tranform my songbird counts into presence absence
#Build an object that shows how many possible routes there are
possible_routes <- sobs %>%
  filter(Visit %in% c("V1", "V2")) %>% 
  tidyr::expand(nesting(Full.Point.ID, Year, Visit))
head(possible_routes)
str(possible_routes)

#define a species of interest
soi <- "SATH"

#See which combinations we actually have for one species
presence <- sobs %>% 
  filter(Species == soi) %>% #select only our soi
  expand(nesting(Full.Point.ID, Visit, Year)) %>% #Find all of the combinations that we have
  mutate(Presence = 1) #add a column to indicate presenece 
str(presence)

#Join the two tables to gether to show where we had this species
presence_absence <- possible_routes %>% 
  left_join(presence, by = c("Full.Point.ID", "Year", "Visit")) %>% 
  mutate(Survey.Number = paste(Year, Visit, sep = "")) %>% 
  dplyr::select(Full.Point.ID, Survey.Number, Presence) %>% 
  rename(Point = "Full.Point.ID") %>% 
  pivot_wider(names_from = Survey.Number, values_from = Presence) %>% 
  replace(is.na(.), 0)

#view presence absence
str(presence_absence)

#make an object for point coordinates
point_coords <- sobs %>% 
  group_by(Full.Point.ID, UTM.X, UTM.Y) %>% 
  summarise() %>% 
  ungroup() %>% 
  rename(Point = 'Full.Point.ID', X = 'UTM.X', Y = 'UTM.Y')
str(point_coords)

#link point coords to the presence abcence data
presence_absence <- presence_absence %>% 
  left_join(point_coords, by = 'Point')

#View the updated data
str(presence_absence)

#Add in the spatial data ------------------------------------------------------
library(sf)
library(raster)

#set working directory
setwd("C:\\Users\\Will\\Desktop\\USU\\SOBs\\Data\\R_Spatial_Analysis")

#Load up the my rasters
elevation <- raster("elevation\\elevation.tif")
shrub_cover <- raster("shrub_cover\\shrub_cover.tif")

#load in my vectors
study_region <- shapefile("Study_Region.shp")
survey_points <- shapefile("Point_Coords.shp")

#View coordinate systems
elevation
shrub_cover
survey_points

#Resize elevation so it matches the shrub cover
elevation <- projectRaster(from = elevation, #The starting raster
                           to = shrub_cover, #The resolution that we want
                           res = 25.76616, method="bilinear") #cell size and method

#Extract covariates to points -----------------------------------------------------------
Shrub.Cover <- extract(shrub_cover, presence_absence[, c("X", "Y")]) #shrub cover
str(Shrub.Cover)
Elevation <- extract(elevation, presence_absence[, c("X", "Y")]) #Elevation
str(Elevation)

##Bind these covariates to the points
sath <- cbind(presence_absence, Shrub.Cover, Elevation)
str(sath)

#format this as a model that unmarked can use ------------------------------------------------
library(unmarked)
#pivot the data long so that we can pull make a matrix
sath_long <- sath %>% 
  pivot_longer(cols = 2:5, 
               names_to = "Sampling.Occ", 
               values_to = "Presence") %>% 
  dplyr::select(Point, Sampling.Occ, Presence) 
str(sath_long)

#Make an r by j matrix for unmarked
sath_mat <- tapply(sath_long$Presence, #Use the presence data
                            list(sath_long$Point, sath_long$Sampling.Occ), #matrix of points by sampling days
                            identity) # just return the 1 or 0
str(sath_mat)
head(sath_mat)

#make an object for covariates
site_covs <- sath %>% 
  distinct(Point, Shrub.Cover, Landcover, Elevation) %>% #Unique combinations of points and covs
  arrange(Point) %>% #Arrange in alphabetical order
  mutate(Landcover = factor(Landcover)) %>%  #switch landcover to a factor
  dplyr::select(Shrub.Cover, Landcover, Elevation) 
str(site_covs)
#Everything looks good

#modeling occupancy ------------------------------------------------------------------------------------
#make our unmarked ouccu object
sath_umf <- unmarkedFrameOccu(y = sath_mat, siteCovs = site_covs, obsCovs = NULL)
summary(sath_umf)

#model where detection and ocupancy are constant
occu0 <- occu(formula = ~ 1 ~ 1, data = sath_umf)
#summary
summary(occu0)

#model where occupancy varies with shrub cover
occu1 <- occu(formula = ~ 1 ~ Shrub.Cover, data = sath_umf)
#summary
summary(occu1)

#model where occupancy varies with elevation
occu2 <- occu(formula = ~ 1 ~ Elevation, data = sath_umf)
#summary
summary(occu2)

#model where occupancy varies with shrub cover and elevation
occu3 <- occu(formula = ~ 1 ~ Shrub.Cover, Elevation, data = sath_umf)
#summary
summary(occu3)

#models 1 and 3 had the same AIC score which was the lowest
#elevation does not help to explain sath occupancy
#I'll just look at occu1 from now on

#backtransform results
backTransform(occu1, type = 'det') #probability of detection
#we have a 0.319 of detecting a thrasher on a given site

#create a vector of hypothetical shrub cover to act as the new data
new_data <- data.frame(Shrub.Cover = seq(from = 0, to = 100, by = 1))

#Make predictions
pred_occu1 <- predict(occu1, newdata = new_data, type = 'state')
pred_occu1 <- cbind(pred_occu1, new_data)
pred_occu1 #How likely a cell is to be occupied by a thrasher given it's shrub cover

#Plot occupancy probability as a function of shrub cover
ggplot(data = pred_occu1, aes(x = Shrub.Cover, y = Predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  theme_bw() +
  xlab("% Shrub Cover") +
  ylab(expression("Predicted Sage Thrasher Occupancy " * (psi)))
