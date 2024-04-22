#A script to print csv's of an observers data for the week so that they can fix any errors

#clear environments
rm(list = ls())

#Add packages'
library(tidyr)
library(dplyr)
library(lubridate)

#Add this weeks data ###################################################################

#Observations
obs <- read.csv("obs.file.path") %>% 
  rename(GlobalID = GlobalID.Obs,
         Parent.GlobalID = GlobalID.Point)
#and view
glimpse(obs)

#Points 
points <- read.csv("point.file.path") %>% 
  rename(GlobalID = GlobalID.Point,
         Parent.GlobalID = GlobalID.Survey)
#and view
glimpse(points)

#Observations
surveys <- read.csv("surveys.file.path") %>% 
  rename(GlobalID = GlobalID.Survey) %>% 
  mutate(Date = str_sub(Date.Time.Raw, start = 1, end = 9)) %>% #change date
  mutate(Date = str_remove_all(Date, " ")) %>% 
  mutate(Date = mdy(Date)) #switch it to a date object
#and view
glimpse(surveys)

#Join
point_counts <- obs %>% 
  left_join(Points, by = "GlobalID.Point") %>% 
  left_join(Points, by = "GlobalID.Survey")
#...and view
glimpse(point_counts)

#Define parameters ############################################################################
observer <- "Will Harrod"
start_date <- mdy("05/22/2023")
end_date <- mdy("05/26/2023")
week <- 1

#Filter the survey
