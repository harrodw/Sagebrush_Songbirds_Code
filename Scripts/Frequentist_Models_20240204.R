#Frequentist models for practice and getting and idea of which variables woill be useful
#Will Harrod
#Created: 02/04/20024
#Start here ---------------------------------------------------------------------

#Add packages
library(tidyverse)
library(tidyr)
library(dplyr)

#Add point count data
sobs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\sobs_data_20240113.csv")) %>% 
        select(-X) #Remove the column that excel generated
#and view the data
glimpse(sobs)

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv"))
#View covariates
glimpse(covs)

#Select a species of interest (SOI)
soi <- "BRSP"

#Make a table of all possible route and visit combinations
possible_routes <- sobs %>% 
  expand(nesting(Route.ID, Year, Visit))
#View possib;e routes
glimpse(possible_routes)

#Make a table of how many of the species of interest were observed on each route
count_0inf <- sobs %>% 
  filter(Species == soi) %>%
  group_by(Route.ID, Year, Visit) %>% 
  reframe(Route.ID, Year, Visit, SOI.Count = n()) %>% 
  ungroup() %>% 
  distinct(Route.ID, Year, Visit, SOI.Count)
#Vieew soi count
glimpse(count_0inf)

#Join the two so I have the zero counts
sobs_count <- left_join(possible_routes, count_0inf, 
          by = c('Route.ID', 'Year', 'Visit'))
#View the full count
glimpse(sobs_count)

#Combine with covariates 
sobs_count <- sobs_count %>% 
  left_join(covs, by = 'Route.ID') 
#View the counts with covariates
glimpse(sobs_count)

#Modeling 
model_no_fire <- glm(data = sobs_count,
                     family = poisson(link = 'log'),
                     formula = SOI.Count ~ Route.Type + Sagebrush.Cover +
                               Annual.Cover + Shrub.Cover + Fire.Distance + 
                               Elevation + TRI + Aspect + Road.Distance,
                     na.action = na.omit)
#View the model
summary(model_no_fire)

#Plot the model
par(mfrow = c(2, 2))
plot(model_no_fire)
par(mfrow = c(1, 1))