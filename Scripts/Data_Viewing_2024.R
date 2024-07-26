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
obs <- read.csv("C:\\Users\\Will\\Desktop\\USU\\SOBs\\R_Projects\\Tri_State_Sagebrush_Songbirds\\Data\\Inputs\\2024_Surveys\\Downloads\\observations_4.csv") %>%
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
points <- read.csv("C:\\Users\\Will\\Desktop\\USU\\SOBs\\R_Projects\\Tri_State_Sagebrush_Songbirds\\Data\\Inputs\\2024_Surveys\\Downloads\\Point_1.csv") %>% 
  rename(GlobalID.Point = GlobalID,
         GlobalID.Survey = ParentGlobalID,
         Point.ID = Point..,
         Point.Start.Time = Start.Time.at.Point,
         Shrub.Cover = Percent.of.the.area.within.50m.covered.by.any.shrub.species,
         Shrub.Height = Average.height.of.shrubs.within.50m.of.the.point..cm.,
         Trees.Count = Count.the.number.of.trees.or.snags.within.50m.of.the.point,
         Cheatgrass.Cover = Percent.of.the.area.within.50m.of.the.point.where.cheatgrass.is.present,
         X.Coord = x,
         Y.Coord = y
         )
#and view
glimpse(points)

#Observations
surveys <- read.csv("C:\\Users\\Will\\Desktop\\USU\\SOBs\\R_Projects\\Tri_State_Sagebrush_Songbirds\\Data\\Inputs\\2024_Surveys\\Downloads\\survey_0.csv") %>% 
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
         Point.Start.Time,
         Shrub.Cover,
         Shrub.Height,
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

#Fix Aidan and Alex's Observer ID's #########################################################

#Combine route ID and visit number
point_counts_all <- point_counts_all %>% 
  mutate(Visit.ID = paste0(Route.ID, "-", Visit))
#...and view
glimpse(point_counts_all)

#List all of Aidan's surveys
aidan_surveys <- c('ID-B26-V1',
                   'ID-B07-V2',
                   'ID-B09-V2',
                   'ID-C19-V2',
                   'ID-C04-V2',
                   'UT-C05-V2',
                   'UT-C08-V2',
                   'UT-B15-V2',
                   'UT-C19-V2')

#List both of Alex's surveys
alex_surveys <- c('ID-C26-V2',
                  'UT-C15-V2')

#Update the observer ID's including Emily's last name
point_counts_all <- point_counts_all %>% 
  mutate(Observer.ID = case_when(Visit.ID %in% aidan_surveys ~ "aidan_richer",
                                 Visit.ID %in% alex_surveys ~ "alex_blanche",
                                 Observer.ID == "emily_hughie" ~ "emily_hugie",
                                 TRUE ~ Observer.ID)) %>% 
  select(-Visit.ID) #I no longer need this column

#and view
point_counts_all %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Point.ID) %>% 
  group_by(Route.ID, Visit, Date, Observer.ID) %>% 
  reframe(Route.ID, Visit, Date, Observer.ID, Points.Surveyed = n()) %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Points.Surveyed) %>% 
  arrange(Route.ID, Visit) %>% 
  # filter(Observer.ID == "will_harrod") %>% 
  print(n = Inf)


#Fix some random Things ###############################################################################

#There are too many ID-B19's. View them 
point_counts_all %>% 
  distinct(Observer.ID, Route.ID, Date) %>% 
  filter(Route.ID == "ID-B19")

#Change Holden's ID-B19 to UT-B19
point_counts_all <- point_counts_all %>% 
  mutate(Route.ID = case_when(Route.ID == "ID-B19" & Date == '20240610' 
                              ~ "UT-B19",
                              TRUE ~ Route.ID))

#There are too many ID-C24's. View them 
point_counts_all %>% 
  distinct(Observer.ID, Route.ID, Visit, Date) %>% 
  filter(Route.ID == "ID-C24")

#Change V1 ID-C24 to UT-C24
point_counts_all <- point_counts_all %>% 
  mutate(Route.ID = case_when(Observer.ID == "ben_zimmermann" & Date == "20240516" 
                   ~ "UT-C24",
                   TRUE ~ Route.ID
                     ))

#Change the points on a few of my surveys. First I need to see them
point_counts_all %>% 
  filter(Observer.ID == "will_harrod" &
           Route.ID %in% c("ID-C09", "ID-B22", "ID-C28", "UT-B08", "UT-C25")) %>% 
  distinct(Route.ID, Point.ID, Point.Start.Time) %>% 
  head(n = Inf)

#Change the wrong points that I put down
point_counts_all <- point_counts_all %>% 
  mutate(Point.ID = case_when(Observer.ID == "will_harrod" & 
                                Route.ID == "ID-B22" & 
                                Point.Start.Time == "07:23" 
                              ~ "11",
                              Observer.ID == "will_harrod" & 
                                Route.ID == "ID-B22" & 
                                Point.Start.Time == "07:52" 
                              ~ "04",
                              Observer.ID == "will_harrod" & 
                                Route.ID == "UT-B08" & 
                                Point.Start.Time == "07:05" 
                              ~ "12",
                              Observer.ID == "will_harrod" & 
                                Route.ID == "ID-C28" & 
                                Point.Start.Time == "06:01" 
                              ~ "16",
                              Observer.ID == "will_harrod" & 
                                Route.ID == "ID-C28" & 
                                Point.Start.Time == "08:50" 
                              ~ "13",
                              Observer.ID == "will_harrod" & 
                                Route.ID == "UT-C25" & 
                                Point.Start.Time == "08:37" 
                              ~ "14",
                              Observer.ID == "will_harrod" & 
                                Route.ID == "UT-C25" & 
                                Point.Start.Time == "07:03"
                              ~ "01",
                              TRUE ~ Point.ID)) %>% 
  mutate(Visit = case_when(Route.ID == "ID-B22" & Observer.ID == "will_harrod" 
                           ~ "V1",
                           Route.ID == "ID-C09" & Observer.ID == "will_harrod" 
                           ~ "V1",
                           TRUE ~ Visit))
#and view
point_counts_all %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Point.ID) %>% 
  group_by(Route.ID, Visit, Date, Observer.ID) %>% 
  reframe(Route.ID, Visit, Date, Observer.ID, Points.Surveyed = n()) %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Points.Surveyed) %>% 
  arrange(Route.ID, Visit) %>% 
  filter(Observer.ID == "will_harrod") %>% 
  print(n = Inf)

#Clear out a few extra surveys #######################################################

#View all the survey dates
point_counts_all %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Point.ID) %>% 
  group_by(Route.ID, Visit, Date, Observer.ID) %>% 
  reframe(Route.ID, Visit, Date, Observer.ID, Points.Surveyed = n()) %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Points.Surveyed) %>% 
  arrange(Route.ID, Date, Visit) %>% 
  print(n = Inf)
#Emily's UT-C16 survey on June 14th and Ben/Aidan's ID-C09 survey on May 24th should be removed

#Define and remove those surveys
point_counts_all <- point_counts_all %>% 
  mutate(Remove = case_when(Route.ID == "ID-B09" & Date == "20240524" ~ "Y",
                            Route.ID == "UT-C16" & Date == "20240614" ~ "Y",
                            TRUE ~ "N")) %>% 
  filter(Remove != "Y") %>% 
  select(-Remove)

#View all the survey dates again
point_counts_all %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Point.ID) %>% 
  group_by(Route.ID, Visit, Date, Observer.ID) %>% 
  reframe(Route.ID, Visit, Date, Observer.ID, Points.Surveyed = n()) %>% 
  distinct(Route.ID, Visit, Date, Observer.ID, Points.Surveyed) %>% 
  arrange(Route.ID, Date, Visit) %>% 
  print(n = Inf)

#make a CSV for  each individual survey #########################################

#change the formating
point_counts_all <- point_counts_all %>% 
  mutate(Route.ID.CSV = str_replace_all(Route.ID, "-", ""))
#...and view
glimpse(point_counts_all)

# #Pick a date -----
# date <- "20240511"

#Define all the observers
surveys <- point_counts_all %>% 
  # filter(Date == date) %>% 
  distinct(Observer.ID, Route.ID.CSV, Visit, Date)
#...and view
glimpse(surveys)

#Define the dates 
dates <- point_counts_all %>% 
  distinct(Date)
#...and view
glimpse(dates)

#initialize the count
csv_count <- 0

#loop over all the dates #####
for(i in 1:nrow(dates)){
  #pull out a single date 
  date <- dates$Date[i]
  #look at only the surveys from that date
  surveys_temp <- surveys %>% 
    filter(Date == date)
  
#loop over all surveys and export the results
for(j in 1:nrow(surveys_temp)){
#pull out a single observer
observer <- surveys_temp$Observer.ID[j]
#pull out that route.ID
route_id <- surveys_temp$Route.ID.CSV[j]
#Pull out the Visit Number
visit <- surveys_temp$Visit[j]
#Filter the survey
point_counts <- point_counts_all %>% 
  filter(Observer.ID == observer &
           ymd(Date) == ymd(date))
#Export 
write.csv(point_counts, paste0("C:\\Users\\Will\\Desktop\\USU\\SOBs\\R_Projects\\Tri_State_Sagebrush_Songbirds\\Data\\Inputs\\2024_Surveys\\Unedited\\", 
                               observer, 
                               "_", 
                               route_id,
                               "_",
                               visit,
                               "_",
                               date, 
                               ".csv"))
#count how many were exported 
csv_count <- csv_count + 1
#see how many were exported
print(c(route_id, visit, csv_count))
  } #end surveys loop
} #end date loop

#View a single species ########################
point_counts_all %>% 
  filter(Species == "HORA") %>% 
  select(Route.ID, Point.ID, Visit, Observer.ID, Species, Distance)

#Load the cleaned surveys back in #################################################################################################

# #Build a table of all survey dates
# visit_tbl <- point_counts_all %>% 
#   distinct(Date, Observer.ID)
# #...and view
# glimpse(visit_tbl)
# 
# #create a blank data frame with the right column names
# surveys_cleaned <- point_counts_all %>% 
#   filter(Species == "Andian Cock-of-the-Rock") #Just a place holder so that we get an object with all the column names but no info
# #...and view
# glimpse(surveys_cleaned)
# 
# #combine the cleaned surveyes
# for(k in 1:nrow(visit_tbl)){
#   #Pull out date
#   date <- visit_tbl$Date[k]
#   #pull out observer
#   observer <- visit_tbl$Observer.ID[k]
#   #add in that survey
#   survey_temp <- read.csv(paste0("Data\\Inputs\\2024_Surveys\\Edited\\surveys_", observer, "_", date, ".csv")) %>% 
#     mutate(Date = as.character(Date))
#   #Combine with other surveys
#   surveys_cleaned <- bind_rows(list(surveys_cleaned, survey_temp))
# }
# #...and view the result
# glimpse(surveys_cleaned)
# 
# #How many total Surveys?
# surveys_cleaned %>% 
#   distinct(Route.ID, Year, Visit)
#   count(Route.ID)
#   
# #How many points?
# surveys_cleaned %>% 
#   distinct(Route.ID, Point.ID, Year, Visit) %>% 
#   count(Rounte.ID, Point.ID)