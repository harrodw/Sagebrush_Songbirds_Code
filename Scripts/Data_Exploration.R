#Exploratory analysis of songbird data from 2022 and 2023
#Created: 12/28/2023

#Load packages
library(tidyverse)

#add in the data
sobs <- tibble(read.csv("C:\\Users\\Will\\Desktop\\USU\\SOBs\\R Projects\\Tri_State_Sagebrush_Songbirds\\Data\\Outputs\\sobs_data.csv")) %>% 
  select(-X) #Remove the column that excel generated

#View the data
glimpse(sobs)

#list of all the routes
possible_routes <- sobs %>%
  filter(Visit %in% c("V1", "V2")) %>% 
  select(Route.ID, Visit, Year) %>% 
  expand(Route.ID, Visit, Year)
#View all routes
glimpse(possible_routes)

#Pull out focal species
sobs <- sobs %>% 
  filter(Visit %in% c("V1", "V2")) %>%
  filter(Species %in% c('BRSP', 'SATH', 'SABS', 'GTTO',
                        'WEME', 'VESP', 'HOLA', 'GRFL', 'NOSH'))
#View focal species
glimpse(sobs)

#pull out only the species that I will look at and see how many were seen during each visit
counts_0Inf <- sobs %>% 
  group_by(Route.ID, Year, Visit, Species) %>%
  summarise(Count = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species,
              values_from = Count)
  
#view species counts
glimpse(counts_0Inf)

#combine the two tables to show zero counts
counts <- possible_routes %>%
  left_join(counts_0Inf, by = c("Route.ID", "Year", "Visit")) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  arrange(Route.ID)
#View the full count dataset
glimpse(counts)

#Graph of number of individuals by plot--------------------------------
counts %>% 
  mutate(Visit.ID = paste(Route.ID, Year, Visit, sep ="-")) %>% 
  select(Visit.ID, BRSP, SATH, SABS, GTTO, VESP, WEME, HOLA, GRFL) %>% 
  pivot_longer(cols = 2:9,
               names_to = 'Species',
               values_to = 'Count') %>% 
  ggplot(aes(x = Visit.ID, y = Count)) +
  geom_point(col = 'black') +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  facet_wrap(~Species)
#the only outlier is that ID-B24 has a lot of HOLA's

#plot observations a single species -----------------------------------------
counts %>% 
  mutate(Visit.ID = paste(Route.ID, Year, Visit, sep ="-")) %>% 
  select(Visit.ID, WEME) %>% 
  ggplot(aes(x = Visit.ID, y = WEME, label = Visit.ID)) +
  geom_point(col = 'black') +
  geom_text(hjust = 0, vjust = 0, size = 2.4) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

#Look at specific outlines -------------------------------------------
#there are a lot of WEME's on ID-B07 and ID-B22
sobs %>% 
  filter(Route.ID== "ID-B22") %>% 
  filter(Species == "WEME") %>% 
  filter(Year == "Y2") %>%
  select(Species, Distance, Minute, Full.Point.ID, 
         Observer.ID, Year, Visit, Date) %>%
  ggplot(aes(x = Distance)) +
  geom_histogram() +
  facet_wrap(~Observer.ID)

#Histogram of observations by distance ------------------ ---------------
hist(sobs$Distance)
sobs %>% 
  filter(Distance <= 300) %>% 
  arrange(Distance) %>% 
  ggplot(aes(x = Distance)) +
    geom_histogram() + 
    xlim(0, 300) +
    ggtitle("Observations by Distance") +
    facet_wrap(~Species)

#test for normality ---------------------------------
#View the covariates
glimpse(sobs)

#MAS
sobs %>% 
  ggplot(aes(x = MAS)) +
  geom_histogram() +
  ggtitle("Observations by mintues after sunrise") +
  xlim(0, 300) +
  facet_wrap(~Species)

#proportion of observations by minute
obs_prop <- sobs %>% 
  expand(nesting(Full.Point.ID, Date, Minute))
