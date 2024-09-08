#Will Harrod
#Created: 06/18/2024
#Visualizing 2022-2024 songbird point count data 
#Start here ---------------------------------------------------------------------
rm(list = ls())

#Add packages
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(forcats)

#Load fonts
font_import()
loadfonts(device = "win")

#add in the data ---------------------------------------------------------------

#Add point count data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) %>% #Remove the column that excel generated
  tibble()
#and view the data
glimpse(sobs)

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X) %>%  
  tibble() %>% 
  mutate(Burn.Sevarity = case_when(is.na(Burn.Sevarity) ~ 0,
                                   TRUE ~ Burn.Sevarity))
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------
#define relevant species
important_species <- c("BRSP", "SATH", "SABS", "GTTO",
                       "WEME", "HOLA", "VESP")

#Truncate observations beyond a cirtain distance
trunc_dist <- 125

#Count of all target species
n_target_obs <- sobs %>% 
  filter(Species %in% important_species) %>% 
  nrow()

#count of close target species
n_close_target_obs <- sobs %>% 
  filter(Species %in% important_species) %>% 
  filter(Distance <= trunc_dist) %>%
  nrow()

#How many total observations?
print(c(n_target_obs, n_close_target_obs))

#How many observations does this cut?
print(n_target_obs - n_close_target_obs)
#20% of the observations isn't too bad

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  #only relevant species
  filter(Species %in% important_species) %>% 
  #only close observations
  filter(Distance <= trunc_dist) %>%
  #Reorganize
  group_by(Route.ID, Route.Type, Year, Visit, Date, Species, Observer.ID) %>% 
  reframe(Route.ID, Route.Type, Year, Visit, Date, Species, Count = n(), Observer.ID) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  expand(nesting(Route.ID, Route.Type, Year, Visit, Date), Species) %>% 
  filter(Species %in% important_species)
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Route.ID', 'Route.Type', 'Year', 'Visit', 'Date', 'Species')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  left_join(covs, by = c('Route.ID', 'Route.Type')) 
#...and view
glimpse(sobs_count)

#Create the object for relevant species observations
sobs_obs <- sobs %>% 
  filter(Species %in% important_species) %>% 
  filter(Distance <= trunc_dist)
#...and view
glimpse(sobs_obs)

#Plots #####################################################################################################3333

#View how many of each species were seen on each route
sobs_count %>% 
  mutate(Visit.ID = paste(Route.ID, Year, Visit)) %>% 
  group_by(Species, Visit.ID) %>% 
  reframe(Route.Count = sum(Count)) %>% 
  distinct(Species, Visit.ID, Route.Count) %>% 
  mutate(Visit.ID = factor(Visit.ID)) %>% 
  mutate(Visit.ID = fct_reorder(Visit.ID, Route.Count)) %>% 
  ggplot(aes(x = Visit.ID, y = Route.Count)) +
  geom_col() +
  facet_wrap(~Species)

#Look at specific outlines -------------------------------------------
#there are a lot of WEME's on ID-B07 and ID-B22
sobs_count %>% 
  filter(Route.ID== "ID-B22") %>% 
  filter(Species == "WEME") %>% 
  select(Species, Distance, Minute, Full.Point.ID, 
         Observer.ID, Year, Visit, Date) %>%
  ggplot(aes(x = Distance)) +
  geom_histogram() +
  facet_wrap(~Observer.ID)

#How many observations of each species
sobs_count %>% 
  ggplot(aes(x = Count)) + 
  geom_histogram() +
  facet_wrap(~Species)

#Histogram of observations by distance ------------------ ---------------
sobs_obs %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(binwidth = 25, col = "darkgray", fill = "lightblue") +
  facet_wrap(~Species)
  #That's a great looking detection histogram right there

#At what elevation do we start seeing more birds on burned grids?
sobs_count %>% 
  ggplot(aes(x = Elevation, y = Count, col = Route.Type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)

#What if elevation was binary?
sobs_count %>% 
  mutate(Sagebrush.Type = case_when(Elevation > 1800 ~ "Mountain",
                                    Elevation <= 1800 ~ "Wyoming")) %>% 
  mutate(Sagebrush.Type = factor(Sagebrush.Type, levels = c("Wyoming", "Mountain"))) %>% 
  ggplot(aes(x = Sagebrush.Type, y = Count, col = Route.Type)) +
  geom_boxplot() +
  facet_wrap(~Species)

#View time since fire
sobs_count %>% 
  filter(!is.na(Fire.Year)) %>%
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>%
  ggplot(aes(x = Years.Since.Fire, y = Count, col = "darkred")) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)

#And the interaction
sobs_count %>% 
  filter(!is.na(Fire.Year)) %>%
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>%
  mutate(Sagebrush.Type = case_when(Elevation > 1800 ~ "Mountain",
                                    Elevation <= 1800 ~ "Wyoming")) %>% 
  mutate(Sagebrush.Type = factor(Sagebrush.Type, levels = c("Wyoming", "Mountain"))) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Count, col = Sagebrush.Type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)

#Now Precipitation
sobs_count %>% 
  ggplot(aes(x = Precipitation, y = Count, col = Route.Type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)

#How does observer affect the count?
sobs_count %>% 
  filter(Count > 0) %>% 
  # mutate(Observer.ID = str_sub(Observer.ID, start = 1, end = 2)) %>% 
  ggplot(aes(x = Observer.ID, y = Count)) +
  geom_boxplot() +
  facet_wrap(~Species)

#tests for normality ---------------------------------
#View the covariates
names(sobs_obs)

#MAS -------------------------------
sobs_obs %>% 
  ggplot(aes(x = MAS)) +
  geom_histogram() +
  ggtitle("Observations by mintues after sunrise") +
  xlim(0, 300) +
  facet_wrap(~Species)

#proportion of observations by minute
obs_prop <- sobs %>% 
  expand(nesting(Full.Point.ID, Date, Minute))
