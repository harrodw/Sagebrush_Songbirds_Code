#Distance Sampling using the Unmarked package
#Will Harrod
#Utah State University
#Creadted 09/08/2024

# 0) Clear environments and load packages ######################################
rm(list = ls())
library(tidyverse)
library(unmarked)

# 1) Prepare the data ##########################################################

# Add point count data
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# View the data
glimpse(sobs)

# Make a new column for each survey visit
sobs <- sobs %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))

# Make a table of all survey visits
points <- sobs %>% 
  distinct(Visit.ID)
#...and view'
glimpse(points)

# Define a single species of interest (SOI)
soi <- "BRSP"

# filter for only that species
observations <- sobs %>% 
  filter(Species == soi) %>% 
  mutate(Distance.Bin = NA) # Add a cloumn for distance bin
#...and view
glimpse(observations)

# View histogram of all observed distances
observations %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(fill = "lightblue")

# Define a truncation distance
trunc_dist <- 125

#Define distance bin sizes
bin_size <- 25

# Define distance bins
dist_bins <- seq(from = 0, to = trunc_dist, by = bin_size)
#...and view
print(dist_bins)

# View the histogram of observations bellow the truncation distance
observations %>% 
  filter(Distance <= trunc_dist) %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(fill = "lightblue", binwidth = bin_size)
# looks good!

# Add the distance bins to the observation data 
for(j in 2:length(dist_bins)){
  observations <- observations %>% 
    filter(Distance <= trunc_dist) %>% 
    mutate(Distance.Bin = case_when(Distance > dist_bins[j -1] & Distance <= dist_bins[j] # Distance is within the current bin
                                    ~ paste0("[", dist_bins[j -1], ", ", dist_bins[j], "]"), # Bin name in the format that unmarked likes
                                    TRUE ~ Distance.Bin))         # Otherwise keep it the same
}

#Now view the result
observations %>% 
  select(Distance, Distance.Bin) %>% 
  print(n = 50)

#See if there are any NA's
observations %>% 
  select(Distance, Distance.Bin) %>%
  filter(is.na(Distance.Bin))
#looks good!

#

