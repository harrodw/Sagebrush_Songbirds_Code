# Will Harrod
# Created 01/23/2025

# Add packages
library(tidyverse)

#clear environments
rm(list = ls())

################################################################################
# 1.0) Data Prep  ################################################################
################################################################################

# 1.1) Read in data ################################################################
# Add in Data from local drive
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# #or from github
# sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_data.csv") %>%
#   dplyr::select(-X) %>%
#   tibble()

#view the data
glimpse(sobs)

# 1.2) Prepare the count level data ################################################################

# List of Study Species
all_species <- c("BRSP", "SATH", "SABS", "GTTO", "VESP", "WEME", "HOLA")

# Loop over all species
for(s in 1:length(all_species)){
  
# Define single species
study_species <- all_species[s]

# Define a truncation distance (km)
trunc_dist <- 0.125

# Function to find the mode of catigorical variables in the model
find_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]  
}


# Make a table of important species sightings by visit
counts_0inf <- sobs %>%
  # Switch from m to km
  mutate(Distance = Distance/1000) %>% 
  # Only observations closer than the truncation distance
  filter(Distance <= trunc_dist) %>% 
  # Only one species
  filter(Species  == study_species) %>% 
  # Unque ID for each visit
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Year, Visit.ID, Ord.Date) %>% 
  # Number of individuals per visit
  reframe(Grid.ID, Year, Visit.ID, Ord.Date, Count = n()) %>% 
  distinct()
#...and view
# glimpse(counts_0inf)

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Grid.Type, Year, Ord.Date, MAS, Observer.ID, Year, Visit) %>% 
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
         # Each visit should be treated separately
         Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Grid.Type, Year, Visit.ID, Ord.Date) %>% 
  reframe(Grid.ID, Visit.ID, 
          # Average minutes after sunrise on the survey
          Mean.MAS = mean(MAS),
          # Observer who submitted the most points
          Observer.ID = find_mode(Observer.ID),
          n.Points = n()) %>% 
  distinct()

#...and view
# glimpse(visit_count)

#Join the two, add zeros and average observations within year
counts_temp <-  visit_count %>% 
  left_join(counts_0inf, by = c("Grid.ID", "Year", "Visit.ID", "Ord.Date")) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))

#...and view
# glimpse(counts_temp)
# print(counts_temp, n = Inf)

# View the surveys that weren't done
counts_temp %>% 
  group_by(Grid.ID) %>% 
  filter(n() < 6)
# ID-C11, Y1-V1 and ID-C22, Y1-V1 weren't recorded

# For now I'm going to propagate these miissing surveys using existing data
new_dat <- tibble(Grid.ID = c("ID-C11", "ID-C22"),
                  Grid.Type = c("R", "R"),
                  Year = c("Y1", "Y1"),
                  Visit.ID = c("Y1-V1", "Y1-V1"),
                  # Saying Rory filled in the data
                  Observer.ID = c("Rory", "Rory"),
                  # Average ordianal date across visits
                  Ord.Date = c(floor(mean(counts_temp$Ord.Date[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$Ord.Date[which(counts_temp$Grid.ID == "ID-C22")]))),
                  # Average survey time across visits
                  Mean.MAS = c(mean(counts_temp$Mean.MAS[which(counts_temp$Grid.ID == "ID-C11")]),
                               mean(counts_temp$Mean.MAS[which(counts_temp$Grid.ID == "ID-C22")])),
                  # Average count across visits
                  Count = c(floor(mean(counts_temp$Count[which(counts_temp$Grid.ID == "ID-C11")])),
                            floor(mean(counts_temp$Count[which(counts_temp$Grid.ID == "ID-C22")]))),
                  # Average number of points surevyed
                  n.Points = c(floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C22")]))))


# Bind these to the existing data 
counts_temp2 <- bind_rows(counts_temp, new_dat)
#...and view
# glimpse(counts_temp2)

# Save the counts to a csv
write.csv(counts_temp2, paste0("Data/Outputs/", study_species, "_Grid_Counts.csv"))

# 1.3) Prepare the observation level data ################################################################

# Define the number of bins
nbins <- 5

# Define a bin size
bin_size <- trunc_dist / nbins

# Define the bins
bins <- seq(from = bin_size, to = bin_size * nbins, by = bin_size)

# View the bins
# bins

# Create an object with all observations of a single species for the detection function
observations_temp <- sobs %>% 
  # Switch from m to km
  mutate(Distance = Distance / 1000) %>%  
  # Only one species
  filter(Species == study_species) %>%   
  # Same order as the counts
  arrange(Year, Visit, Grid.ID) %>%
  # Only close observations
  filter(Distance <= trunc_dist) %>%          
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis)
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
         # A unique ID for each visit
         Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  dplyr::select(Distance, Minute, Grid.ID, Observer.ID, Year, Visit.ID) %>% 
  # Calculate Distance Bins
  mutate(Dist.Bin = case_when(Distance >= 0 & Distance <= bins[1] ~ bins[1],
                              Distance > bins[1] & Distance <= bins[2] ~ bins[2],
                              Distance > bins[2] & Distance <= bins[3] ~ bins[3],
                              Distance > bins[3] & Distance <= bins[4] ~ bins[4],
                              Distance > bins[4] & Distance <= bins[5] ~ bins[5],
                              TRUE ~ NA)) %>% 
  # Calculate Midpoints
  mutate(Dist.Bin.Midpoint = Dist.Bin - bin_size/2) %>% 
  # Change to a numeric factor
  mutate(Dist.Bin = case_when(Dist.Bin == bins[1] ~ 1,
                              Dist.Bin == bins[2] ~ 2,
                              Dist.Bin == bins[3] ~ 3,
                              Dist.Bin == bins[4] ~ 4,
                              Dist.Bin == bins[5] ~ 5,
                              TRUE ~ NA)) %>% 
  # Asign each observation to a two minute time interval
  mutate(Time.Interval = case_when(Minute %in% c(1, 2) ~ 1,
                                   Minute %in% c(3, 4) ~ 2,
                                   Minute %in% c(5, 6) ~ 3)) %>% 
  dplyr::select(Grid.ID, Observer.ID, Distance, Dist.Bin, Dist.Bin.Midpoint, Time.Interval, Year, Visit.ID)


# View the whole object
# glimpse(observations_temp)
# sort(unique(observations_temp$Dist.Bin))
# sort(unique(observations_temp$Dist.Bin.Midpoint))
# sort(unique(observations_temp$Time.Interval))

# Save the Observation Data to a csv
write.csv(observations_temp, paste0("Data/Outputs/", study_species, "_Observations.csv"))

# Say that an itteration is finsihed
message(paste("Created count and observation data for", study_species))

} # End loop over species
