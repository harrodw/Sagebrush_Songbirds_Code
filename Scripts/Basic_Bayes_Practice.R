#Practicing Bayesian modeling using my songbird data

#Clear Environments
rm(list = ls())

#add packages
library(R2jags)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(DHARMA)

#Prep ------------------------------------------------------------------------------------

#Add in Data
sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_count_covs.csv") %>% 
  select(-X)
#view the data
glimpse(sobs) 

#Pick out a species of interest
soi <- "BRSP"

#Make a table of all possible route and visit combinations
possible_points <- sobs %>% 
  tidyr::expand(nesting(Full.Point.ID, Year, Visit, Observer.ID, Route.Type,
                        Shrub.Cover, Shrub.Height, Sagebrush.Cover, Annual.Cover, Fire.Distance, 
                        Aspect, Bare.Ground.Cover, Burn.Sevarity, TRI, Elevation, Road.Distance)) %>% 
  mutate_at(c('Route.Type', 'Full.Point.ID', 'Observer.ID', 'Aspect', 'Burn.Sevarity'), 
            as.factor)
#View possible routes
glimpse(possible_points)

#Make a table of how many of the species of interest were observed on each route
count_0inf <- sobs %>% 
  filter(Species == soi) %>%
  group_by(Full.Point.ID, Year, Visit) %>% 
  reframe(Full.Point.ID, Year, Visit, Count = n()) %>% 
  distinct()
#Vieew soi count
glimpse(count_0inf)

#Join the two so I have the zero counts
sobs_count <- left_join(possible_points, count_0inf, 
                        by = c('Full.Point.ID', 'Year', 'Visit')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0,
                               TRUE ~ Count)) 

#View the full count
glimpse(sobs_count)

#Build Models -------------------------------------------------------------------------------------------------------------
 
