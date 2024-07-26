#Bayesian distance sampling model

#Clear Environments
rm(list = ls())

#add packages
library(jagsUI)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)

#Prep ------------------------------------------------------------------------------------
#Add in Data
sobs <- read.csv("Data//Outputs//sobs_data.csv") %>% 
  dplyr::select(-X)
#view the data
glimpse(sobs) 

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("Data//Outputs//point_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
soi <- "BRSP"

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species == soi) %>%
  # filter(Year == "Y2") %>% #I'll remove this later
  group_by(Full.Point.ID, Route.ID, Year, Visit, Date, Species) %>% 
  reframe(Full.Point.ID, Route.ID, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  # filter(Year == "Y2") %>% 
  tidyr::expand(nesting(Full.Point.ID, Route.ID, Year, Sky.Start,
                        Wind.Start, Visit, Date, Observer.ID))

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Full.Point.ID', 'Route.ID', 
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))
#...and view
glimpse(sobs_count)

#Join to route covariates
soi_count <- sobs_count %>% 
  left_join(covs, by = c("Route.ID", "Full.Point.ID"))
#...and view
glimpse(soi_count)

#Change necessary variables to scales and factors
soi_count <- soi_count %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  mutate(Burn.Sevarity = case_when(is.na(Burn.Sevarity) ~ 0,
                                   TRUE ~ Burn.Sevarity)) %>%
  mutate(Burned = factor(Route.Type, levels = c("R", "B"))) %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown',
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>%
  mutate(Sky.Start = factor(Sky.Start, levels = c("Clear", "Partly Cloudy", "Cloudy",
                                                  "Drizzle", "Fog or Smoke"))) %>%
  mutate_at(c("Aspect", "Burn.Sevarity", 
              "Fire.Name", "Observer.ID", "Route.ID"), factor) %>% 
  
  select(-Species)

# Fill in years since fire
for(i in 1:nrow(soi_count)) {
  if(is.na(soi_count$Years.Since.Fire[i])){
    soi_count$Years.Since.Fire[i] <- rnorm(1, 100, 15)
  }
}

#...and view
glimpse(soi_count)

#Create all observations of a single species
soi <- sobs %>% 
  filter(Species == soi) %>% #only one species
  # filter(Year == "Y2") %>% 
  mutate(Burned = factor(Route.Type, levels = c("R", "B"))) %>% 
  dplyr::select(Distance, Minute, How.Detected, 
                Route.ID, Full.Point.ID, Observer.ID, Year, Visit,
                Burned, Ord.Date, 
                # MAS, 
                Wind.Start) %>% 
  left_join(covs, by = c("Route.ID", "Full.Point.ID")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  dplyr::select(-Burn.Sevarity, -Perennial.Cover, - Fire.Distance,
                -Fire.Name, - Fire.Year, - Route.Type, - Aspect) %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))


#Fill in years since fire
for(i in 1:nrow(soi)) {
  if(is.na(soi$Years.Since.Fire[i])){
    soi$Years.Since.Fire[i] <- as.integer(rnorm(1, 150, 30))
  }
}

#... and view
glimpse(soi)

#View the farthest 10% of distances
dist_cutoff <- soi %>% 
  arrange(Distance) %>% 
  slice(as.integer(nrow(soi) * 0.9))

#Pull out the distance
max_dist <- round_any(dist_cutoff$Distance, 10, f = ceiling)
#...and view
max_dist

#Define bin size
bin_size <- max_dist / 5

#Histogram of distances
soi %>% 
  filter(Distance <= max_dist) %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(col = "darkgray", fill = "lightblue")

#Histogram of Distance scaled by radius
soi %>% 
  filter(Distance < max_dist) %>%
  mutate(Dist.Bin = round_any(Distance / bin_size, 1, f = ceiling)) %>% 
  group_by(Dist.Bin) %>% 
  reframe(Dist.Bin, Count = n(), Scaled.Count = n()/ Dist.Bin) %>%
  distinct() %>% 
  mutate(Scaled.Count = Scaled.Count / max(Scaled.Count)) %>% 
  ggplot(aes(x = Dist.Bin, y = Scaled.Count)) +
  geom_col(col = "darkgray", fill = "lightblue") +
  stat_smooth()


# I like these 10m bins. I'll use them in the data for now
soi <- soi %>% 
  mutate(Dist.Bin = round_any(Distance / bin_size, 1, f = ceiling)) %>% 
  mutate(Dist.Bin.Midpoint = case_when(Dist.Bin == 1 ~ 16,
                                       Dist.Bin == 2 ~ 48,
                                       Dist.Bin == 3 ~ 80,
                                       Dist.Bin == 4 ~ 112,
                                       Dist.Bin == 5 ~ 144,))
#...and view
glimpse(soi)

#Plot of detentions by time
soi %>% 
  mutate(Time.Bin = as.factor(case_when(Minute %in% c(1, 2) ~ "1",
                                        Minute %in% c(3, 4) ~ "2",
                                        Minute %in% c(5, 6) ~ "3"))) %>% 
  group_by(Time.Bin) %>% 
  reframe(Time.Bin, Count = n()) %>% 
  distinct() %>% 
  ggplot(aes(x = Time.Bin, y = Count)) +
  geom_col()

#Looks good, I'm adding it to the data
soi_obs <- soi %>% 
  mutate(Time.Bin = as.factor(case_when(Minute %in% c(1, 2) ~ "1",
                                        Minute %in% c(3, 4) ~ "2",
                                        Minute %in% c(5, 6) ~ "3")))
#...and view
glimpse(soi_obs)
glimpse(soi_count)

#Prep the data for N-mixture model
visit_count <- soi_count %>% 
  filter(Year == "Y2") %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown', #change the wind data
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>% 
  dplyr::select(Full.Point.ID, Visit, Count, Shrub.Cover, Wind.Start, Route.Type) %>%  #Pull out only the variables of interest
  pivot_wider(names_from = Visit, values_from = c('Count', 'Shrub.Cover', 'Wind.Start', "Route.Type")) %>%  #Pivot the data so it can be turned into a matrix
  drop_na(Count_V1, Count_V2) %>% #Remove the sites that were only visited once
  select(-Shrub.Cover_V2, - Route.Type_V2) %>% 
  mutate(Route.Type_V1 = factor(Route.Type_V1, levels = c("R", "B")))
#...and view
glimpse(visit_count)
