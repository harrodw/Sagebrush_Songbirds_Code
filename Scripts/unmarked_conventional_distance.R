#Hierarchical Distance Sampling using the Unmarked package
#Will Harrod
#Utah State University
#Creadted 09/08/2024

# 0) Clear environments and load packages ######################################
rm(list = ls())
library(tidyverse)
library(unmarked)

# 1) Data Prep #################################################################

# 1a) Prepare the data #########################################################

# Add point count data
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) 
# View the data
glimpse(sobs)

# Make a new column for each survey visit
sobs <- sobs %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))

# Define a single species of interest (SOI)
soi <- "BRSP"

# Define a truncation distance
trunc_dist <- 125

#Define distance bin sizes
bin_size <- 25

# filter for only that species
observations_0inf <- sobs %>% 
  filter(Species == soi) %>% 
  select(Visit.ID, Distance) %>% 
  mutate(Distance = as.numeric(Distance)) %>% 
  filter(Distance <= trunc_dist) %>% 
  mutate(Count = 1)
#...and view
glimpse(observations_0inf)

# Define distance bins
dist_breaks <- seq(from = 0, to = trunc_dist, by = bin_size)
#...and view
print(dist_breaks)

# View the histogram of observations bellow the truncation distance
observations_0inf %>% 
  filter(Distance <= trunc_dist) %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(fill = "lightblue", binwidth = bin_size)
# looks good!

# Make a table of all survey visits
points <- sobs %>% 
                   distinct(Visit.ID)

#...and view'
head(points)
glimpse(points)

# Merge with your observations data
observations <- points %>%
  left_join(observations_0inf, 
            by = c("Visit.ID")) %>% 
  mutate(Count = replace_na(Count, 0))
#... and view
glimpse(observations)

#convert the data to the format unmarked likes
dist_dat <- formatDistData(observations, 
                           distCol = "Distance",
                           transectNameCol = "Visit.ID", 
                           dist.breaks = dist_breaks)
#... and view
head(dist_dat, n = 300)
glimpse(dist_dat)
nrow(dist_dat)

# 1b) Covariates ###############################################################

# Add covariate data
covs <- tibble(read.csv("Data/Outputs/point_summaries.csv")) %>%
  dplyr::select(-X, -Point.X, -Point.Y)
#...and view
glimpse(covs)

#View the initial data again
glimpse(sobs)

# Transform the observation data into an object that can receive the covariates
covs_tbl_blank <- sobs %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>% 
  select(Visit.ID, Year, Visit, Full.Point.ID, Route.ID, Observer.ID,
         Ord.Date, MAS, Temp.Start, Wind.Start, Sky.Start,
         UTM.X, UTM.Y) %>% 
  distinct(Visit.ID, Year, Visit, Full.Point.ID, Route.ID, Observer.ID,
         Ord.Date, MAS, Temp.Start, Wind.Start, Sky.Start,
         UTM.X, UTM.Y)
#...and view 
glimpse(covs_tbl_blank)
#make sure this is the same as the actual number of points surveyed
nrow(covs_tbl_blank) == length(unique(covs_tbl_blank$Visit.ID))
# good good

# combine these with the covariates
covs_tbl <- covs_tbl_blank %>% 
  left_join(covs, by = c("Full.Point.ID"))
#...and view
glimpse(covs_tbl)
#Make sure there are still the proper number of points
nrow(covs_tbl) == length(unique(covs_tbl$Visit.ID))

# 1c) Turning these into unmarked objects ######################################

# Build an unmarked distance sampling frame object
umf <- unmarkedFrameDS(y = as.matrix(dist_dat), 
                       siteCovs = covs_tbl,  
                       survey = "point",
                       dist.breaks = dist_breaks, 
                       unitsIn = "m")

# View the unmarked object
summary(umf)

# 2) Model Fitting #############################################################


