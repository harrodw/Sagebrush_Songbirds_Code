#A script to print csv's of an observers data for the week so that they can fix any errors

#clear environments
rm(list = ls())

#Add packages'
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

#Add this weeks data ###################################################################

#Observations
obs <- read.csv("Data\\Inputs\\2024_Surveys\\Downloads\\observations_4.csv") %>%
  rename(GlobalID.Obs = GlobalID,
         GlobalID.Point = ParentGlobalID,
         Species = X4.Letter.Species.Code,
         Distance = Radial.Distance..m.,
         How.Detected = How.Detected.,
         Song.Also = Song.Also.,
         Group.Size = Number.of.Birds.in.Group
         )
#and view
glimpse(obs)

#Points 
points <- read.csv("Data\\Inputs\\2024_Surveys\\Downloads\\Point_1.csv") %>% 
  rename(GlobalID.Point = GlobalID,
         GlobalID.Survey = ParentGlobalID,
         Point.ID = Point..,
         Start.Time = Start.Time.at.Point,
         Shrub.Cover = Percent.of.the.area.within.50m.covered.by.any.shrub.species,
         Trees.Count = Count.the.number.of.trees.within.50m.of.the.point,
         Cheatgrass.Cover = Percent.of.the.area.within.50m.of.the.point.where.cheatgrass.is.present,
         X.Coord = x,
         Y.Coord = y
         )
#and view
glimpse(points)

#Observations
surveys <- read.csv("Data\\Inputs\\2024_Surveys\\Downloads\\survey_0.csv") %>% 
  rename(GlobalID.Survey = GlobalID,
         Observer.ID = Observer.Name,
         Visit = Visit.Number,
         Temp.Start = Start.Temperature..Celcius.,
         Temp.End = End.Temperature..Celcius.
         ) %>% 
  mutate(Date = str_sub(Date.and.Start.Time, start = 1, end = 9)) %>% #change date
  mutate(Date = str_remove_all(Date, " ")) %>% 
  mutate(Date = mdy(Date)) %>%  #switch it to a date object
  mutate(Date = as.character(Date)) %>% 
  mutate(Date = str_remove_all(Date, "-"))
#and view
glimpse(surveys)

#Join
point_counts_all <- obs %>% 
  left_join(points, by = "GlobalID.Point") %>% 
  left_join(surveys, by = "GlobalID.Survey") %>% 
  select(#Route level variables
         Observer.ID,
         Route.ID,
         Year,
         Visit,
         Date,
         #Point level variables
         Point.ID,
         Start.Time,
         Shrub.Cover,
         Cheatgrass.Cover,
         Trees.Count,
         #Observation level variables
         Species,
         Minute,
         Distance,
         How.Detected,
         Song.Also,
         Direction,
         Sex,
         Group.Size,
         #Covariates
         Temp.Start,
         Temp.End,
         Wind.Start,
         Wind.End,
         Sky.End,
         Sky.End,
         X.Coord,
         Y.Coord,
         #Notes
         Point.Notes,
         Route.Notes) 
#...and view
glimpse(point_counts_all)

#Fix some things
point_counts_all <- point_counts_all %>% 
  mutate(Notes = paste(Route.Notes, Point.Notes, sep = ". ")) %>% 
  select(-Route.Notes, -Point.Notes) %>% 
  mutate(Observer.ID = str_replace_all(Observer.ID, " ", "_")) %>% 
  mutate(Observer.ID = str_to_lower(Observer.ID)) %>% 
  mutate(Point.ID = case_when(Point.ID == 1 ~ "01",
                              Point.ID == 2 ~ "02",
                              Point.ID == 3 ~ "03",
                              Point.ID == 4 ~ "04",
                              Point.ID == 5 ~ "05",
                              Point.ID == 6 ~ "06",
                              Point.ID == 7 ~ "07",
                              Point.ID == 8 ~ "08",
                              Point.ID ==  9 ~ "09",
                              TRUE ~ as.character(Point.ID)))
#...and view
glimpse(point_counts_all)

#One observers routes for the specified time period ############################################################################

#Define all the observers
observers <- point_counts_all %>% 
  distinct(Observer.ID)
#...and view
glimpse(observers)

#Pick a date -----
date <- "20240422"

#loop over all observers and export the results
for(o in 1:nrow(observers)){
#pull out a single observer
observer <- observers$Observer.ID[o]
#Filter the survey
point_counts <- point_counts_all %>% 
  filter(Observer.ID == observer &
           ymd(Date) == ymd(date))
#Export 
write.csv(point_counts, paste0("Data\\Inputs\\2024_Surveys\\Unedited\\surveys_", observer, "_", date, ".csv"))
} #end loop


#Load the cleaned surveys back in #################################################################################################

#Build a table of all survey dates
visit_tbl <- point_counts_all %>% 
  distinct(Date, Observer.ID)
#...and view
glimpse(visit_tbl)

#create a blank data frame with the right column names
surveys_cleaned <- point_counts_all %>% 
  filter(Species == "Andian Cock-of-the-Rock") #Just a place holder so that we get an object with all the column names but no info
#...and view
glimpse(surveys_cleaned)

#combine the cleaned surveyes
for(k in 1:nrow(visit_tbl)){
  #Pull out date
  date <- visit_tbl$Date[k]
  #pull out observer
  observer <- visit_tbl$Observer.ID[k]
  #add in that survey
  survey_temp <- read.csv(paste0("Data\\Inputs\\2024_Surveys\\Edited\\surveys_", observer, "_", date, ".csv")) %>% 
    mutate(Date = as.character(Date))
  #Combine with other surveys
  surveys_cleaned <- bind_rows(list(surveys_cleaned, survey_temp))
}
#...and view the result
glimpse(surveys_cleaned)

#How many total Surveys?
surveys_cleaned %>% 
  distinct(Route.ID, Year, Visit)
  count(Route.ID)
  
#How many points?
surveys_cleaned %>% 
  distinct(Route.ID, Point.ID, Year, Visit) %>% 
  count(Rounte.ID, Point.ID)