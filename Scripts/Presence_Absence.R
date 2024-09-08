#Presence absence data based on my songbird surveys

#Clear Objects
rm(list = ls())

#Load tidyverse
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

#add in data
sobs <- read.csv("Data\\Outputs\\sobs_data.csv") %>% 
  tibble() %>% 
  select(-X)
#... and view
glimpse(sobs)

#Start Here ---------------------------------------------------------------------------------------
#define a species of interest
soi <- "SATH"

#Build an object that shows how many possible routes there are
possible_routes <- sobs %>%
  filter(Visit %in% c("V1", "V2")) %>% 
  dplyr::select(Full.Point.ID, Visit, Year) %>% 
  tidyr::expand(Full.Point.ID, Visit, Year)
possible_routes
str(possible_routes)

#See which combinations we actually have for one species
presence <- sobs %>% 
  filter(Species == soi) %>% #select only our soi
  dplyr::select(Full.Point.ID, Visit, Year) %>% #only need these columns
  expand(nesting(Full.Point.ID, Visit, Year)) %>% #Find all of the combinations that we have
  mutate(Presence = 1) #add a column to indicate presenece 
str(presence)

#Join the two tables to gether to show where we had this species
presence_absence <- possible_routes %>% 
  left_join(presence, by = c("Full.Point.ID", "Visit", "Year")) %>% 
  mutate(Presence = replace_na(Presence, 0)) %>% 
  mutate(Survey.Number = paste(Year, Visit, sep = "")) %>% 
  dplyr::select(Full.Point.ID, Survey.Number, Presence) %>% 
  rename(Point = "Full.Point.ID") %>% 
  arrange(Point)
str(presence_absence)
tail(presence_absence)

#Pivot the dataframe wider so each visit is it's own column
presence_absence_table <- presence_absence %>% 
  pivot_wider(names_from = Survey.Number,
               values_from = Presence) %>% 
  mutate(Observed = case_when(Y1V1 == 1 ~ 1,
                              Y1V2 == 1 ~ 1,
                              Y2V1 == 1 ~ 1,
                              Y2V2 == 1 ~ 1,
                              .default = 0))
str(presence_absence_table)
head(presence_absence_table)

#how many points what that species seen at
count_points_observed <- sum(presence_absence_table$Observed)
count_points_observed

#Percent of points where that species was seen
percent_points_observed <- 100 * (sum(presence_absence_table$Observed) / 
                                    nrow(presence_absence_table))
percent_points_observed
  

#define an object for naming this datafram
filename <- paste(soi, "pres_abs_table_2023.csv", sep = "_")
filename

#export presence absence table
write.csv(presence_absence_table, 
          paste(data_path,
                "\\Outputs\\",
                filename,
                sep = ""))
       
#Show how many points this species was seen at
print(paste(soi, "was seen at", count_points_observed, "points", 
            "which was", round(percent_points_observed, 1), 
            "percent of all points surveyed",
            sep = " "))