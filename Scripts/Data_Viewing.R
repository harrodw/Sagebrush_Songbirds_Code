#A script to print csv's of an observers data for the week so that they can fix any errors

#clear environments
rm(list = ls())

#Add packages'
library(tidyr)
library(dplyr)
library(lubridate)

#Add this weeks data ###################################################################

#Observations
obs <- read.csv("Data\\Inputs\\Sobs_Observations_Raw_2023.csv") %>%
  rename(GlobalID.Obs = GlobalID,
          GlobalID.Point = ParentGlobalID)
#and view
glimpse(obs)

#Points 
points <- read.csv("Data\\Inputs\\Sobs_Points_Raw_2023.csv") %>% 
  rename(GlobalID.Point = GlobalID,
         GlobalID.Survey = ParentGlobalID)
#and view
glimpse(points)

#Observations
surveys <- read.csv("Data\\Inputs\\Sobs_Surveys_Raw_2023.csv") %>% 
  rename(GlobalID.Survey = GlobalID) %>% 
  mutate(Date = str_sub(Date.and.Start.Time, start = 1, end = 9)) %>% #change date
  mutate(Date = str_remove_all(Date, " ")) %>% 
  mutate(Date = mdy(Date)) #switch it to a date object
#and view
glimpse(surveys)

#Join
point_counts_all <- obs %>% 
  left_join(points, by = "GlobalID.Point") %>% 
  left_join(surveys, by = "GlobalID.Survey") %>% 
  select(#Route level variables
         Observer.Name,
         Route.ID,
         Year,
         Visit.Number,
         Date,
         Start.Temperature..Celcius.,
         End.Temperature..Celcius.,
         Wind.Start,
         Wind.End,
         Sky.End,
         Sky.End,
         #Point level variables
         Point..,
         Start.Time.at.Point,
         #Observation level variables
         X4.Letter.Species.Code,
         Radial.Distance..m.,
         How.Detected.,
         Direction,
         Sex,
         Number.of.Birds.in.Group,
         #Notes
         Point.Notes,
         Route.Notes,
         x.x,
         y.x)
#...and view
glimpse(point_counts_all)


#One observers routes for the specified time period ############################################################################

#Define parameters 
observers <- unique(point_counts_all$Observer.Name)
observer <- obs
date <- "20230522"

#loop over all observers and export the results
for(o in 1:length(observers)){
#pull out a single observer
observer <- observers[o]
  
#Filter the survey
point_counts <- point_counts_all %>% 
  filter(Observer.Name == observer &
         Date >= ymd(date))

#Export 
write.csv(point_counts, paste0("Data\\Viewing_Surveys\\surveys_", observer, "_", date, ".csv"))
} #end loop
