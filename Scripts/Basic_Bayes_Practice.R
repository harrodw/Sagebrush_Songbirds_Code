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
  expand(nesting(Full.Point.ID, Year, Visit, Route.Type, Shrub.Cover, Shrub.Height,
                 Sagebrush.Cover, Annual.Cover, Fire.Distance, Aspect,
                 Bare.Ground.Cover, Burn.Sevarity, TRI, Elevation,
                 Road.Distance)) %>% 
  mutate(Route.Type = as.factor(Route.Type))
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

#Frequentest Model --------------------------------------------------------------------------

#Naive Model
sobs_fm1 <- glm(Count ~ Shrub.Cover + Shrub.Height + TRI + Elevation + Road.Distance + Route.Type, 
                data = sobs_count,
                family = poisson)
#View diagnostics plot
par(mfrow =c(2, 2))
plot(sobs_fm1)

#Vieew moddel summary
summary(sobs_fm1)

#shrub cover only model
sobs_fm2 <- glm(Shrub.Cover, 
                data = sobs_count,
                family = poisson)
#View diagnostics plot
par(mfrow =c(2, 2))
plot(sobs_fm2)

#Vieew moddel summary
summary(sobs_fm2)
 
