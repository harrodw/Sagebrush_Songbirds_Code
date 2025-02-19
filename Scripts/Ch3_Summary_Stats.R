# Summary stats and supplamental materials 

# Will Harrod
# Created 02/19/2025

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

######################################################################################
# 2.0) Summary Stats  ################################################################
######################################################################################

# 2.1) Total species by grid type and year
species_counts <- sobs %>% 
  # Remove No bird observations and unknowns
  filter(!Species %in% c("NOBI", "UNGR", "UNWO", "UNBB", "UNFI", "UNSW", "UNJA", "UNFA",
                         "UNBB", "UNBI", "UNFL", "UNHA", "UNHU", "UNOW", "UNSP", "UKBB")) %>% 
  # Rename year and grid type
  mutate(Year = case_when(Year == "Y1" ~ "2022",
                          Year == "Y2" ~ "2023",
                          Year == "Y3" ~ "2024",
                          TRUE ~ NA),
         Grid.Type = case_when(Grid.Type == "B" ~ "Burn",
                               Grid.Type == "R" ~ "Reference",
                               TRUE ~ NA)) %>% 
  # Combine year and grid type
  mutate(Treatment.Year = paste(Year, Grid.Type,  sep = " ")) %>% 
  # Rearrage based on species
  group_by(Treatment.Year, Species) %>% 
  reframe(Treatment.Year, Species, Number.Detected = n()) %>% 
  distinct() %>% 
  # Remove the species that we saw less than six of
  filter(Number.Detected > 6) %>% 
  # Pivot to a wider format (Just one row per species)
  pivot_wider(names_from = Treatment.Year, values_from = Number.Detected) %>% 
  # Replace NA's with zeros
  replace(is.na(.), 0) %>% 
  # Sort alphabetically
  arrange(Species)

# View 
glimpse(species_counts)
print(species_counts, n = Inf)

# Save this dataset
write.csv(species_counts, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Point_Count\\Tables\\species_summaries.csv")
