# Will Harrod
# Created 01/23/2025

# Add packages
library(tidyverse)

#clear environments
rm(list = ls())

##################################################################################
# 1.0) Data Prep  ################################################################
##################################################################################

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

# View all observers
sobs %>% 
  distinct(Observer.ID) %>% 
  arrange(Observer.ID)

# Define the observers who have conducted a previous point count season
prev_season <- c("Rory", "Ruger", "Thea", "Trey", "Will")
no_prev_season <- c("Aidan", "Alex", "Andrew", "Anna", "Austin", "Ben", "Devin",
                    "Eliza", "Emily", "Eoin", "Holden", "Keramie", "Thomas", "Two-Observers")

# Function to find the mode of catigorical variables in the model
find_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]  
}

# 1.2) Prepare the count level data ################################################################

# List of Study Species
all_species <- c("BRSP", "SATH", "SABS", "GTTO", "VESP", "WEME", "HOLA")

# Loop over all species
for(s in 1:length(all_species)){
  
# Define single species
# study_species <- all_species[1]
study_species <- all_species[s]

# Define a truncation distance (km)
trunc_dist <- 0.125

# Find out how many total observations there were for the grid plus visit
#I only need a few columns
indv_counts_tmp <- sobs %>%
  # Remove very far observations
  filter(Distance <= 200) %>% 
  # Remove NOBI's
  filter(Species != "NOBI" & Species != study_species) %>% 
  # Column for each distinct visit
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  #Reframe the data to total observations per grid
  group_by(Grid.ID, Visit.ID) %>% 
  reframe(Grid.ID, Visit.ID, Total.Indv = n()) %>% 
  distinct()

# Impute the missing surveys
missing_indv_counts <- indv_counts_tmp %>% 
  filter(Grid.ID %in% c("ID-C11", "ID-C22") & Visit.ID == "Y1-V2") %>% 
  mutate(Visit.ID = "Y1-V1")

# Add these to the data
indv_counts <- indv_counts_tmp %>% 
  bind_rows(missing_indv_counts)
# View
# glimpse(indv_counts)



# Make a table of important species sightings by visit
counts_0inf <- sobs %>%
  # Switch from m to km
  mutate(Distance = Distance/1000) %>% 
  # Only observations closer than the truncation distance
  filter(Distance <= trunc_dist) %>% 
  # Only one species
  filter(Species == study_species) %>% 
  # Unque ID for each visit
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Year, Visit.ID, Ord.Date) %>% 
  # Number of individuals per visit
  reframe(Grid.ID, Year, Visit.ID, Ord.Date, Count = n()) %>% 
  distinct()
#...and view
# glimpse(counts_0inf)

# Find the number of observers who surveys each grid
obsv_count <- sobs %>% 
  distinct(Grid.ID, Year, Visit, Observer.ID) %>% 
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
         # Each visit should be treated separately
         Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Visit.ID) %>%
  reframe(Grid.ID, Visit.ID,
          # Number of observers who surveyed
          n.Observers = n()) %>%
  distinct() 
# and view
# glimpse(obsv_count)

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Grid.Type, Ord.Date, MAS, Observer.ID, Year, Visit) %>% 
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
         # Each visit should be treated separately
         Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Grid.Type, Visit.ID, Ord.Date) %>%
  reframe(Grid.ID, Visit.ID, Year, Observer.ID,
          # Average minutes after sunrise on the survey
          Mean.MAS = mean(MAS),
          #  Number of points surveyed
          n.Points = n(),
          Observer.ID = find_mode(Observer.ID)) %>%
  distinct() 
#...and view
# glimpse(visit_count)

#Join all datasets, add zeros and average observations within year
counts_temp <-  visit_count %>% 
  left_join(counts_0inf, by = c("Grid.ID", "Year", "Visit.ID", "Ord.Date")) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  left_join(obsv_count, by = c("Grid.ID", "Visit.ID")) %>% 
  # Make a "new observer" for the grids that were surveyed by two people
  # mutate(Observer.ID = case_when(n.Observers == 1 ~ Observer.ID,
  #                                n.Observers > 1 ~ "Two-Observers",
  #                                TRUE ~ NA)) %>% 
  # Split up Observers By Year
  mutate(Observer.Year = paste(Observer.ID, Year)) %>% 
  # # Make an observer.Experience column
  mutate(Observer.Experience = case_when(Observer.ID %in% no_prev_season ~ 1,
                                         Observer.ID %in% prev_season ~ 2))
# View
# glimpse(counts_temp)

# For now I'm going to impute these miissing surveys using existing data
new_counts <- counts_temp %>% 
  #Select the necessary data 
  filter(Grid.ID %in% c("ID-C11", "ID-C22") & Visit.ID == "Y1-V2") %>%
  # Change these to a first visit 
  mutate(Visit.ID = "Y1-V1") %>% 
  # Create a column to show that this is imputed data
  mutate(Imputed = TRUE)
# View the imputed data
# glimpse(new_counts)

# Bind these to the existing data 
counts_temp2 <- counts_temp %>% 
  # Create a column to show that this is not imputed data
  mutate(Imputed = FALSE) %>% 
  bind_rows(new_counts) %>% 
  arrange(Grid.ID, Year, Visit.ID) %>% 
  # Add the total observations per grid
  left_join(indv_counts, by = c("Grid.ID", "Visit.ID"))
#...and view
# glimpse(counts_temp2)
# new_dat%>% 
#   print()

# Conferm that everything is in the dataset now
# counts_temp2 %>%
#   filter(Grid.ID %in% c("ID-C11", "ID-C22")) %>%
#   print(n = Inf)

# Save the cinfert# Save the counts to a csv
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
  # Make an observer.Experience column
  mutate(Observer.Experience = case_when(Observer.ID %in% no_prev_season ~ 1,
                                         Observer.ID %in% prev_season ~ 2)) %>% 
  dplyr::select(Grid.ID, Observer.ID, Observer.Experience, Distance, 
                Dist.Bin, Dist.Bin.Midpoint, Time.Interval, Year, Visit.ID) %>% 
  left_join(obsv_count, by = c("Grid.ID", "Visit.ID")) %>% 
  # # Make an observer.Experience column
  mutate(Observer.Experience = case_when(Observer.ID %in% no_prev_season ~ 1,
                                         Observer.ID %in% prev_season ~ 2)) 

# Propegrate the existing data for the icomplete surveys (ID-C11 and ID-C22 Y1-V1)
# ID-C11
incomp_surveys <- observations_temp %>% 
  #Select the nessesary data 
  filter(Grid.ID %in% c("ID-C11", "ID-C22") & Visit.ID == "Y1-V2") %>%
  # Change these to a first visit 
  mutate(Visit.ID = "Y1-V1") %>% 
  # Create a column to show that this is imputeddata
  mutate(Imputed = TRUE)
# View
# glimpse(incomp_surveys)
# incomp_surveys %>% 
#   print()

# Join these to the data 
observations_temp2 <- observations_temp %>% 
  # Create a column to show that this is not imputed data
  mutate(Imputed = FALSE) %>% 
  # Join to imputed data
  bind_rows(incomp_surveys)
#View
# glimpse(observations_temp2)

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

