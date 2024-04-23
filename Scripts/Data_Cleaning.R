# SOBS Data Cleaning ----
# Will Harrod
# sagebrush songbird Point count project
# This version of the data cleaning script explores each year's data separately
# Last edited 01/07/2024

#Load tidyverse
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

#Start here ------------------------------------------------------------

#Add in observation Data
#2022 Observation data 
obs_22_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Observations_2022_Raw.csv"))
#2023 observation data
obs_23_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Observations_Raw_2023.csv"))

#View the two datasets
glimpse(obs_22_raw)
glimpse(obs_23_raw)

#Rename 2022 observations
#View the 2022 names
names(obs_22_raw)

#rename the two datasets so they can be joined
#storage object for new names for 2022 
obs_22_names <- c(Minute.Raw= "Minute", 
                  Species.Raw = "Species_Code", 
                  Distance.Raw = "Radial_Distance",      
                  How.Detected.Raw = "How_Detected.",       
                  Song.Also.Raw = "Song_Also.",           
                  Sex.Raw = "Sex",                  
                  Visual.Raw = "Visual_Detection.",   
                  Migrant.Raw = "Migrating.",
                  Obs.Notes = "Point_Notes",
                  Obs.Date = "CreationDate",        
                  Creator = "Creator",              
                  Edit.Date = "EditDate",            
                  Editor = "Editor",
                  Within.Burn.Raw = "Is_bird_within_burn.",
                  Direction.Raw = "Direction",
                  Global.ID.Obs = "GlobalID",
                  Global.ID.Point = "ParentGlobalID")

#rename and remove the columns that I don't need
obs_22 <- obs_22_raw %>% 
  dplyr::rename(all_of(obs_22_names)) %>% 
  dplyr::select(Species.Raw, Distance.Raw, Minute.Raw, How.Detected.Raw, #key observation information
         Sex.Raw, Visual.Raw, Migrant.Raw, Song.Also.Raw, 
         Direction.Raw, Within.Burn.Raw,  #additional information
         Obs.Date, Obs.Notes, #Notes
         Global.ID.Obs, Global.ID.Point) %>% #join fields
mutate(Distance.Raw = as.character(Distance.Raw)) #Make distance a character

#View the updated dataframe
str(obs_22) #2022 observations look good

#Rename 2023 observations
#View 2023 names
names(obs_23_raw)

#storage object for 2022 observanetions new names  
obs_23_names <- c(Minute.Raw = "Minute", 
                  Species.Raw = "X4.Letter.Species.Code", 
                  Distance.Raw = "Radial.Distance..m.",      
                  How.Detected.Raw = "How.Detected.",       
                  Song.Also.Raw = "Song.Also.",           
                  Sex.Raw = "Sex",                  
                  Visual.Raw = "Visual.ID",   
                  Migrant.Raw = "Migrant",
                  Obs.Date = "CreationDate",        
                  Creator = "Creator",              
                  Edit.Date = "EditDate",            
                  Editor = "Editor",
                  Next.Observation  = "Next.Observation",
                  Within.Burn.Raw = "Is.bird.within.burn.",
                  Group.Size.Raw = "Number.of.Birds.in.Group",
                  Direction.Raw = "Direction",
                  Global.ID.Obs = "GlobalID",
                  Global.ID.Point = "ParentGlobalID")

#rename and remove the columns that I don't need
obs_23 <- obs_23_raw %>% 
  dplyr::rename(all_of(obs_23_names)) %>% 
  dplyr::select(Species.Raw, Distance.Raw, Minute.Raw, How.Detected.Raw, #key observation information
         Sex.Raw, Visual.Raw, Migrant.Raw, Song.Also.Raw, Direction.Raw, Within.Burn.Raw, #additional information
         Group.Size.Raw,
         Obs.Date, Global.ID.Obs, Global.ID.Point) %>% #join fields
  mutate(Distance.Raw = as.character(Distance.Raw)) #Make distance a character

#Vew the 2023 observations
str(obs_23)

#Combine the two years of data
obs_cleaned <- bind_rows(obs_22, obs_23) %>% 
  dplyr::relocate(Species.Raw, Distance.Raw, Minute.Raw, How.Detected.Raw, #key observation information
          Sex.Raw, Visual.Raw, Migrant.Raw, Song.Also.Raw, 
          Direction.Raw, Within.Burn.Raw, #additional information
          Group.Size.Raw, Obs.Notes,
          Obs.Date, Global.ID.Point, Global.ID.Obs) %>% 
  tibble()

#View the full dataframe
str(obs_cleaned) #Looks good

#Cleaning up Observation Datasheet ----------------------------------------------                                                  
#First the species data

#View the unique species observed
obs_cleaned %>% 
  dplyr::count(Species.Raw) %>% 
  base::print(n = Inf)

#View specific observations
obs_cleaned %>% 
  filter(Species.Raw == "")  #Change this based on which one I want to look at

#Capitalize Species Code
obs_cleaned  <- obs_cleaned %>% 
  mutate(Species = str_to_upper(Species.Raw)) %>% 
  mutate(Species = str_remove_all(Species, "_")) %>% #remove "_, "
  mutate(Species = str_remove_all(Species, " "))

#...and view
obs_cleaned %>% 
  dplyr::count(Species) %>% 
  base::print(n = Inf)
  
#Mutate incorrect species codes to corrected ones  
obs_cleaned <- obs_cleaned %>% 
  mutate(Species = case_when(
    Species %in% c("VEDP",
                   "VESO",
                   "VSSP",
                   "CESP",
                   "117",
                   "19",
                   "324"
    ) ~ "VESP",
    Species == "53" & Distance.Raw == "VESP" ~ "VESP",
    Species %in% c("MEWE",
                   "MELA",
                   "MEME",
                   "WEMD",
                   "WEMR",
                   "WEMW",
                   "WENE",
                   "WRME",
                   "WEMA",
                   "WTMA",
                   "132",
                   "222",
                   "60"
    ) ~ "WEME",
    Species %in% c("13HOLA'S",
                   "HOL",
                   "HORNEDLARK",
                   "107"
    ) ~ "HOLA",
    Species %in% c("RNPH",
                   "RPHE"
    ) ~ "RNEP",
    Species %in% c("ROWE",
                   "RORW",
                   "ROQR",
                   "ROHE",
                   "ROWR?"
    ) ~ "ROWR",
    Species %in% c("MOBI",
                   "NONE",
                   ""
    ) ~ "NOBI",
    Species %in% c("LSSP",
                   "LASL",
                   "LARS"
    ) ~ "LASP",
    Species %in% c("CONH",
                   "NIHA",
                   "NIGHTHAWK",
                   "152"
    ) ~ "CONI",
    Species %in% c("MOSO",
                   "MIDO"
    ) ~ "MODO",
    Species %in% c("BLMA",
                   "BBMP",
                   "BTMJ",
                   "BBMQ",
                   "MAPI",
                   "MTMP"
    ) ~ "BBMA",
    Species %in% c("BRS",
                   "BRAP",
                   "BRSL",
                   "BRSO",
                   "BESP"
    ) ~ "BRSP",
    Species %in% c("GRTO",
                   "GTRO",
                   "GTTI",
                   "GTTP",
                   "20"
    ) ~ "GTTO",
    Species %in% c("SAGT",
                   "SATG",
                   "151"
    ) ~ "SATH",
    Species %in% c("SPOTTEDTOWHEE",
                   "SPOT",
                   "SPRO"
    ) ~ "SPTO",
    Species == "53" & Distance.Raw == "SPTO" ~ "SPTO",
    Species %in% c("SAGS",
                   "SASP",
                   "SBSP"
    ) ~ "SABS",
    Species %in% c("GRPA",
                   "GRAG",
                   "HUPA"
    ) ~ "GRAP",
    Species %in% c("LABU",
                   "LAZP",
                   "LAZULIBUNTING",
                   "LUBU"
    ) ~ "LAZB",
    Species %in% c("UN",
                   "UNKNOWN",
                   "BLTH",
                   "283",
                   "GFCA"
    ) ~ "UNBI",
    Species %in% c("WHSP",
                   "WCDP"
    ) ~ "WCSP",
    Species %in% c("GRAYFLYCATCHER",
                   "GFLC",
                   "GFLC",
                   "GRFC"
    ) ~ "GRFL",
    Species %in% c("CHUKAR",
                   "CHUKAR?",
                   "CHUC",
                   "CHUCK"
    ) ~ "CHUK",
    Species %in% c("ATFC",
                   "ASFL"
    ) ~ "ATFL",
    Species %in% c("TURKEY",
                   'WITA'
    ) ~ "WITU",
    Species %in% c("BRCO",
                   "BHCB"
    ) ~ "BHCO",
    Species %in% c("SAGR",
                   "GREATERSAGEGROUSE",
                   "GSAG"
    ) ~ "GRSG",
    Species %in% c("BRBB",
                   "10BRBB'S"
    ) ~ "BRBL",
    Species %in% c("BGGC",
                   "BGNA",
                   "BLGN"
    ) ~ "BGGN",
    Species %in% c("AMRO",
                   "AMRA") ~ "AMRO",
    Species ==   "ECDO" ~ "EUCD",
    Species == "NORTHERNFLICKER" ~ "NOFL",
    Species == "KIDE" ~ "KILL",
    Species == "SAND" ~ "SACR",
    Species == "NORTHERNHARRIER" ~ "NOHA",
    Species == "SPOTTEDFALCON" ~ "PRFA",
    Species == "FLYCATCHER" ~ "UNFL",
    Species == "LOCU" ~ "LBCU",
    Species == "UNKNOWNOWL" ~ "UNOW",
    Species == "BLGN" ~ "BGGN",
    Species == "BUSHTIT" ~ "BUSH",
    Species == "BASW" ~ "BARS",
    Species == "CHDP" ~ "CHSP",
    Species == "REHA" ~ "RTHA", 
    Species ==  "MAKE" ~ "AMKE",
    Species == "LHSH" ~ "LOSH",
    Species == "TRSW" ~ "TRES",
    Species == "GHSP" ~ "GRSP",
    Species == "Coha" ~ "COHA",
    Species == "WTSW" ~ "WTSP",
    Species == "ANGO" ~ "AMGO",
    Species == "BBHU" ~ "BTHU",
    Species == "BAOW" ~ "BUOW",
    Species == "PINJ" ~ "PIJA",
    Species == "BLSP" ~ "BTSP",
    Species == "AUWA" ~ "YRWA",
    Species == "CAWR" ~ "CANW",
    Species == "CEWA" ~ "CEDW",
    Species == "DEFL" ~ "DUFL",
    Species == "RTHU" ~ "UNHU",
    Species == "FERH" ~ "FEHA",
    Species == "GOFI" ~ "UNFI",
    Species == "MAWA" ~ "MGMA",
    Species == "RWBB" ~ "RWBL",
    Species == "YECH" ~ "YBCH",
    Species == "YHBK" ~ "YHBL",
    Species == "UNBB" ~ "UNBL",
    TRUE ~ as.character(Species)
  ))

#check for any missing corrections
obs_cleaned %>% 
  dplyr::count(Species) %>% 
  base::print(n = Inf)

#check for blank species
obs_cleaned %>% 
  filter(is.na(Species)) 
#none :)

str(obs_cleaned)
#I think this looks good for now

#Fix Distances ----------------------------------------------------
#View raw distances
obs_cleaned %>% 
  dplyr::count(Distance.Raw) %>% 
  base::print(n = Inf)

#View specific distances
obs_cleaned %>% 
  filter(Distance.Raw == "0NEST")

#Make a clean distance column
obs_cleaned <- obs_cleaned %>% 
  mutate(Distance = str_remove_all(Distance.Raw, "_")) %>% # remove _
  mutate(Distance = str_remove_all(Distance, "[a-z]")) %>% #remove lower case letters
  mutate(Distance = str_remove_all(Distance, "[A-Z]")) %>% #Remove capitol letters
  mutate(Distance = case_when(str_detect(Distance, ">") ~ "999", #I'm making the decision to 
                              str_detect(Distance, "<") ~ "999", #Treat these observations
                              str_detect(Distance, "21") ~ "21", #There's a 21 with a quotation mark
                              Distance == "" ~ NA,               #as far away
                              TRUE ~ as.character(Distance)))

#View clean distances
obs_cleaned %>% 
  dplyr::count(Distance) %>%
  base::print(n = Inf)

#Convert distance to numeric
obs_cleaned <- obs_cleaned %>% 
  mutate(Distance = as.numeric(Distance))

#make sure the NA's carried over correctly
obs_cleaned %>% 
  filter(is.na(Distance)) %>% 
  dplyr::count(Distance.Raw, Distance)

#plot distances
# obs_cleaned %>% 
#   ggplot(aes(x = Distance)) +
#   geom_histogram(col = "black", fill = "gray") +
#   theme_bw()

#View the data
str(obs_cleaned)

#remove the nest observation
#check how many rows we start with
nrow(obs_cleaned)

#create an object for all raw distances
obs_cleaned <- obs_cleaned %>% 
  filter(! Distance.Raw %in% c("0NEST"))

#how many rows after I remove the nest?
nrow(obs_cleaned)

#Fix direction codes ------------------------------------------------------
#View all raw directions
obs_cleaned %>% 
  dplyr::count(Direction.Raw) %>% 
  base::print(n = Inf)

#Make a clean direction column
obs_cleaned <- obs_cleaned %>% 
  mutate(Direction = case_when(
    Direction.Raw == "n" ~ "N",
    Direction.Raw == "North" ~ "N",
    Direction.Raw == "ne" ~ "NE",
    Direction.Raw == "Norteast" ~ "NE",
    Direction.Raw == "e" ~ "E",
    Direction.Raw == "East" ~ "E",
    Direction.Raw == "se" ~ "SE",
    Direction.Raw == "Southeast" ~ "SE",
    Direction.Raw == "s" ~ "S",
    Direction.Raw == "South" ~ "S",
    Direction.Raw == "sw" ~ "SW",
    Direction.Raw == "Southwest" ~ "SW",
    Direction.Raw == "w" ~ "W",
    Direction.Raw == "West" ~ "W",
    Direction.Raw == "nw" ~ "NW",
    Direction.Raw == "Northwest" ~ "NW"
  ))

#View the new directions
obs_cleaned %>% 
  distinct(Direction)

# Fix song also ----------------------------------------------
#view song also
obs_cleaned %>% 
  dplyr::count(Song.Also.Raw) %>% 
  base::print(n = Inf)

#View specific songs
obs_cleaned %>% 
  filter(Song.Also.Raw == "(begging)")

#make a clean song also column
obs_cleaned <- obs_cleaned %>% 
  mutate(Song.Also = case_when(
    Song.Also.Raw %in% c("(begging)",
                     "Call",
                     "n",
                     "no",
                     "No",
                     "no_call",
                     "No,_a_juvenile",
                     "No,_on_nest",
                     "N"
                     
    ) ~ "N",
    Song.Also.Raw %in% c("yes",
                     "Yes",
                     "y",
                     "yew",
                     "YES",
                     "u",
                     "Ye",
                     "Short song",
                     "s",
                     "Y"
                     
    ) ~ "Y",
    Song.Also.Raw == "" & Species != "NOBI" ~ "N",
    Song.Also.Raw == "" & Species == "NOBI" ~ NA))

#View clean song
obs_cleaned %>% 
  dplyr::count(Song.Also) %>% 
  base::print(n = Inf)

#Fix detection type -----------------------------------------
#View detection type
obs_cleaned %>% 
  dplyr::count(How.Detected.Raw) %>% 
  base::print(n = Inf)

#view specific detections
obs_cleaned %>% 
  filter(How.Detected.Raw == "D")

#then clean
obs_cleaned <- obs_cleaned %>%
  mutate(How.Detected = case_when(
    How.Detected.Raw == "OA" ~ "O",
    How.Detected.Raw == "D" & Species != "NOFL" ~ NA,
    TRUE ~ as.character(How.Detected.Raw)))

#View clean detection type
obs_cleaned %>% 
  dplyr::count(How.Detected) %>% 
  base::print(n = Inf)

#fix sex ---------------------------------------------
#view raw sex data
obs_cleaned %>% 
  dplyr::count(Sex.Raw) %>% 
  base::print(n = Inf)

#None of these need to be fixed yet
obs_cleaned <- obs_cleaned %>% 
  mutate(Sex = Sex.Raw)

#fix migration -------------------------------------------------------------------

#View raw migration data
obs_cleaned %>% 
  dplyr::count(Migrant.Raw) %>% 
  base::print(n = Inf)

#create a clean migrant column
obs_cleaned <- obs_cleaned %>% 
  mutate(Migrant = case_when(
    Migrant.Raw == "m" ~ "Y",
    Migrant.Raw == "Yes" ~ "Y",
    Migrant.Raw == "" & Species != "NOBI" ~ "N",
    TRUE ~ NA
  ))

#View clean migration data
obs_cleaned %>% 
  dplyr::count(Migrant) %>% 
  base::print(n = Inf)

#fix group size -------------------------------------------------------------------------
#View raw group size data
obs_cleaned %>% 
  dplyr::count(Group.Size.Raw) %>% 
  base::print(n = Inf)

#clean group size
obs_cleaned <- obs_cleaned %>% 
  mutate(Group.Size = case_when(Species.Raw == "10_BRBB's" ~ 10, #Replace wierd observations
                                Species.Raw == "13_HOLA's" ~ 13,
                                TRUE ~ as.integer(Group.Size.Raw))) %>%  #default is normal values
  mutate(Group.Size = replace_na(Group.Size, 1)) %>% 
  mutate(Group.Size = case_when(Species == "NOBI" ~ NA, #Replace NOBI obs with na's for this
                                TRUE ~ Group.Size))

#View clean group size data
obs_cleaned %>% 
  dplyr::count(Group.Size) %>% 
  base::print(n = Inf)

#Fix Visual ID
#View raw visual id
obs_cleaned %>% 
  dplyr::count(Visual.Raw) %>% 
  base::print(n = Inf)

#Change visual
obs_cleaned <- obs_cleaned %>% 
  mutate(Visual.ID = case_when(Visual.Raw %in% c("Yes", "v") ~ "Y",
                              Visual.Raw == "No" ~ "N",
                              TRUE ~ "UK")) %>%           #The default is we don't know 
  mutate(Visual.ID = case_when(Species == "NOBI" ~ NA,
                               TRUE ~ Visual.ID)) #whether they saw the bird or not

#View clean visual id
obs_cleaned %>% 
  dplyr::count(Visual.ID) %>% 
  base::print(n = Inf)

#Fix Minute ---------------------------------------
#View minutes
obs_cleaned %>%
  distinct(Minute.Raw)


#they look good, I just need to turn them into characters
obs_cleaned <- obs_cleaned %>% 
  mutate(Minute = case_when(Minute.Raw == 1 ~ "01",
                             Minute.Raw == 2 ~ "02",
                             Minute.Raw == 3 ~ "03",
                             Minute.Raw == 4 ~ "04",
                             Minute.Raw == 5 ~ "05",
                             Minute.Raw == 6 ~ "06",
                             is.na(Minute.Raw) ~ NA))


#View cleaned minutes
obs_cleaned %>%
  dplyr::count(Minute)

#change blank cells to NA and select useful columns ---------------------------------
observations <- obs_cleaned %>% 
  dplyr::select(Species, Distance, Minute, How.Detected, Song.Also, #only keep useful columns
         Direction, Song.Also, Sex, Migrant, Group.Size, Visual.ID,
         Obs.Date, Obs.Notes, Global.ID.Obs, Global.ID.Point) %>% 
  mutate_if(is.character, ~ na_if(., "")) #change blanks to NA's

#View the cleaned data
glimpse(observations)
#Finished cleaning the observations data --------------------------------------

#Cleaning the Point data --------------------------------------
#Add in point data
#2022 point data
points_22_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Points_2022_Raw.csv"))
#2023 point data
points_23_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Points_Raw_2023.csv"))

#View the two datasets
glimpse(points_22_raw)
glimpse(points_23_raw)

#Rename 2022 points
#View the 2022 names
names(points_22_raw)

#object for renaming 2022 points
points_22_names <- c(Point.Time = "Start.Time.at.Point",
                     Point.ID.Raw = "Point..",
                     Cheatgrass = "Cheatgrass.Present.",
                     Global.ID.Point = "GlobalID",
                     Global.ID.Survey = "ParentGlobalID")

#Pull out what I need from the points dataset and rename
points_22 <- points_22_raw %>% 
  dplyr::rename(all_of(points_22_names)) %>% 
  dplyr::select(Point.ID.Raw, Point.Time, 
         Point.Notes, Global.ID.Point, Global.ID.Survey,
         x, y)

#View the cleaned 2022 point data
glimpse(points_22)

#rename the 2023 points
#view 2023 names
names(points_23_raw)

#object for renaming 2022 points
points_23_names <- c(Point.Time = "Start.Time.at.Point",
                     Point.ID.Raw = "Point..",
                     Cheatgrass = "Cheatgrass.Present.",
                     Percent.Area.Burned = "Percent.of.Area.Burned",
                     Global.ID.Point = "GlobalID",
                     Global.ID.Survey = "ParentGlobalID")

#Pull out what I need from the points dataset and rename
points_23 <- points_23_raw %>% 
  dplyr::rename(all_of(points_23_names)) %>% 
  dplyr::select(Point.ID.Raw, Point.Time, 
         Point.Notes, Global.ID.Point, Global.ID.Survey,#make sure the point id fields match
         x, y) %>%  
  mutate(Point.ID.Raw = as.character(Point.ID.Raw))
  
#View the cleaned 2023 point data
glimpse(points_23)

#combine the two years of point data
points_cleaned <- bind_rows(points_22, points_23)

#View the full raw point data
glimpse(points_cleaned)

#Standardize point numbers -------------------------------
#view point numbers
points_cleaned %>% 
  dplyr::count(Point.ID.Raw) %>% 
  base::print(n = Inf)

#clean point numbers
points_cleaned <- points_cleaned %>% 
  mutate(Point.ID = as.character(case_when(
    Point.ID.Raw == "1" ~ "01",
    Point.ID.Raw == "2" ~ "02",
    Point.ID.Raw == "3" ~ "03",
    Point.ID.Raw == "4" ~ "04",
    Point.ID.Raw == "5" ~ "05",
    Point.ID.Raw == "6" ~ "06",
    Point.ID.Raw == "7" ~ "07",
    Point.ID.Raw == "8" ~ "08",
    Point.ID.Raw == "9" ~ "09",
    Point.ID.Raw == "10" ~ "10",
    Point.ID.Raw == "11" ~ "11",
    Point.ID.Raw == "12" ~ "12",
    Point.ID.Raw == "13" ~ "13",
    Point.ID.Raw == "14" ~ "14",
    Point.ID.Raw == "15" ~ "15",
    Point.ID.Raw == "16" ~ "16", 
    TRUE ~ Point.ID.Raw
  )))

#View the clean point id's
points_cleaned %>% 
  dplyr::count(Point.ID) %>% 
  base::print(n = Inf)

#Remove the nest points ------------------------------------------
#check how long the point tibble is before the change
#it should only shrink by 2 rows
nrow(points_cleaned)

#remove nest points
points_cleaned <- points_cleaned %>% 
  filter(Point.ID %in% c("01", "02", "03", "04", "05", "06", "07", "08",
                         "09", "10", "11", "12", "13", "14", "15", "16"))

#check length again
nrow(points_cleaned)


#Select on the point columns that I will need --------------------------------
points <- points_cleaned %>% 
  dplyr::select(Point.ID, Point.Time,
         Point.Notes, Global.ID.Point, Global.ID.Survey, x, y) %>% 
  mutate_if(is.character, ~ na_if(., "")) #change blanks to NA's

#Final look at the point data
glimpse(points)
#Finished cleaning the point data ----------------------------------------------------

#Clean survey data -----------------------------------------------------------
#Add in survey data
#2022 survey data
surveys_22_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Surveys_2022_Raw.csv"))
#2023 survey data
surveys_23_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Surveys_Raw_2023.csv"))

#View the two datasets
glimpse(surveys_22_raw)
glimpse(surveys_23_raw)

#rename surveys and combine data ----------------------------------------------------
#Rename 2022 surveys
#View the 2022 names
names(surveys_22_raw)

#object for renaming 2022 surveys
surveys_22_names <- c(Route.ID.Raw = "Transect.ID",
                      Visit.Raw = "Visit..",
                      Observer.ID.Raw = "Observer.Name",
                      Temp.Start.Raw = "Start.Temperature",
                      Temp.End.Raw = "End.Temperature",
                      Wind.Start.Raw = "Wind.Start",
                      Wind.End.Raw = "Wind.End",
                      Sky.Start.Raw = "Sky.Start",
                      Sky.End.Raw= "Sky.End",
                      Date.Time.Raw = "Date.and.Start.Time",
                      Global.ID.Survey = "GlobalID")

#rename and select useful columns
surveys_22 <- surveys_22_raw %>% 
  dplyr::rename(all_of(surveys_22_names)) %>% 
  dplyr::select(Route.ID.Raw, Visit.Raw, Date.Time.Raw,
         Observer.ID.Raw,
         Sky.Start.Raw, Sky.End.Raw, Temp.Start.Raw, 
         Temp.End.Raw, Wind.Start.Raw, Wind.End.Raw,
         Date.Time.Raw, Global.ID.Survey) %>% 
  mutate_at(c("Visit.Raw", "Sky.Start.Raw", 
              "Sky.End.Raw", "Wind.Start.Raw", "Wind.End.Raw", 
              "Temp.Start.Raw", "Temp.End.Raw"), 
            as.character) #standardize some of the variables

#view the cleaned 2022 surveys
glimpse(surveys_22)

#Rename 2023 surveys
#View the 2023 names
str(surveys_23_raw)
names(surveys_23_raw)

#object for renaming 2023 surveys
surveys_23_names <- c(Route.ID.Raw = "Route.ID",
                      Year.Raw = "Year",
                      Visit.Raw = "Visit.Number",
                      Observer.ID.Raw = "Observer.Name",
                      Second.Observer.Raw = "Other.Observer.Name",
                      Temp.Start.Raw = "Start.Temperature..Celcius.",
                      Temp.End.Raw = "End.Temperature..Celcius.",
                      Wind.Start.Raw = "Wind.Start",
                      Wind.End.Raw = "Wind.End",
                      Sky.Start.Raw = "Sky.Start",
                      Sky.End.Raw= "Sky.End",
                      Survey.End.Raw = "End.of.Survey",
                      Date.Time.Raw = "Date.and.Start.Time",
                      Global.ID.Survey = "GlobalID")

#rename and slect useful columns
surveys_23 <- surveys_23_raw %>% 
  dplyr::rename(all_of(surveys_23_names)) %>% 
  dplyr::select(Route.ID.Raw, Year.Raw, Visit.Raw, Date.Time.Raw,
         Observer.ID.Raw, Route.Notes,
         Sky.Start.Raw, Sky.End.Raw, Temp.Start.Raw, 
         Temp.End.Raw, Wind.Start.Raw, Wind.End.Raw,
         Date.Time.Raw, Survey.End.Raw, Global.ID.Survey) %>% 
  mutate_at(c("Visit.Raw", "Sky.Start.Raw", 
              "Sky.End.Raw", "Wind.Start.Raw", "Wind.End.Raw", 
              "Temp.Start.Raw", "Temp.End.Raw"), 
              as.character) #standardize some of the variables

#View the 2023 surveys
str(surveys_23)

#Merge the two survey years
surveys_cleaned <- bind_rows(surveys_22, surveys_23)

#view the full raw surveys
str(surveys_cleaned)

#Fix Date ------------------------------------
#turn the raw date into a ymd object
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Date = str_sub(Date.Time.Raw, start = 1, end = 9)) %>% #pull out the first 9 characters
  mutate(Date = str_remove_all(Date, " ")) %>% #Remove the spaces
  mutate(Date = mdy(Date)) #switch it to a date object

#View the new date column
surveys_cleaned %>% 
  dplyr::count(Date) %>% 
  base::print(n= Inf)

#Make an ordinal date column
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Ord.Date = case_when(Date <= ymd("2022-12-31") ~ Date - ymd("2022-01-01"), #for 2022 dates
                              Date > ymd("2022-12-31") ~ Date - ymd("2023-01-01"))) %>% #for 2023 dates
  mutate(Ord.Date = as.numeric(Ord.Date)) #switch ordinal date to just the number

#view the new ordinal date
str(surveys_cleaned)

#Fix Temp --------------------------------------------------------
#view start temp
surveys_cleaned %>% 
  distinct(Temp.Start.Raw) %>% 
  base::print(n = Inf)
#view end temp
surveys_cleaned %>% 
  distinct(Temp.Start.Raw) %>% 
  base::print(n = Inf)

#Switch temp to numeric
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Temp.Start = as.numeric(Temp.Start.Raw)) %>% 
  mutate(Temp.End = as.numeric(Temp.End.Raw))

#View what was lost in start temp
surveys_cleaned %>% 
  filter(! c(Temp.Start.Raw %in% Temp.Start))

#View what was lost in end temp
surveys_cleaned %>% 
  dplyr::select(Route.ID.Raw, Temp.End.Raw, Temp.End) %>% 
  filter(! c(Temp.End.Raw %in% Temp.End))
#idk what the h was supposed  to be so I am okay with it

#View a plot of starting and ending temperatures
# surveys_cleaned %>%
#   dplyr::select(Temp.Start, Temp.End) %>% 
#   pivot_longer(cols = c(Temp.Start, Temp.End),
#     names_to = "Time") %>% 
#   ggplot(aes(x = value, fill = Time)) +
#   geom_histogram()
#a lot of these values look really high
#people probably recorded them in F not C

#view all the start temps
surveys_cleaned %>% 
  dplyr::count(Temp.Start) %>% 
  base::print(n = Inf)

#View all the end temps
surveys_cleaned %>% 
  dplyr::count(Temp.End) %>% 
  base::print(n = Inf)
#I'll probably have to link climate data instead of using these
#I'm not sure how to piece apart the real C's from the fake ones

#Fix Wind ---------------------------------------------------------
#view start wind
surveys_cleaned %>% distinct(Wind.Start.Raw)

#view end wind
surveys_cleaned %>% distinct(Wind.Start.Raw)

#Recod wind based on how it was in survey123
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Wind.Start = case_when(Wind.Start.Raw == 0 ~ "<1 mph",
                                Wind.Start.Raw == 1 ~ "1-3 mph",
                                Wind.Start.Raw == 2 ~ "4-7 mph",
                                Wind.Start.Raw == 3 ~ "8-12 mph",
                                Wind.Start.Raw == 4 ~ "13-18 mph",
                                TRUE ~ as.character(Wind.Start.Raw))) %>% 
  mutate(Wind.End = case_when(Wind.End.Raw == 0 ~ "<1 mph",
                              Wind.End.Raw == 1 ~ "1-3 mph",
                              Wind.End.Raw == 2 ~ "4-7 mph",
                              Wind.End.Raw == 3 ~ "8-12 mph",
                              Wind.End.Raw == 4 ~ "13-18 mph",
                              TRUE ~ as.character(Wind.End.Raw)))

#view start wind after the fix
surveys_cleaned %>% distinct(Wind.Start)

#view end wind after the fix
surveys_cleaned %>% distinct(Wind.Start)

#Fix sky ---------------------------------------------------------
#view start sky
surveys_cleaned %>% distinct(Sky.Start.Raw)

#view end sky
surveys_cleaned %>% distinct(Sky.Start.Raw)

#Recod sky based on how it was in survey123
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Sky.Start = case_when(Sky.Start.Raw == 0 ~ "Clear",
                                Sky.Start.Raw == 1 ~ "Partly Cloudy",
                                Sky.Start.Raw == 2 ~ "Cloudy",
                                Sky.Start.Raw == 3 ~ "Fog or Smoke",
                                Sky.Start.Raw == 4 ~ "Drizzle",
                               TRUE ~ as.character(Sky.Start.Raw))) %>% 
  mutate(Sky.End = case_when(Sky.End.Raw == 0 ~ "Clear",
                              Sky.End.Raw == 1 ~ "Partly Cloudy",
                              Sky.End.Raw == 2 ~ "Cloudy",
                              Sky.End.Raw == 3 ~ "Fog or Smoke",
                              Sky.End.Raw == 4 ~ "Drizzle",
                             TRUE ~ as.character(Sky.End.Raw)))

#view start sky after the fix
surveys_cleaned %>% distinct(Sky.Start)

#view end sky after the fix
surveys_cleaned %>% distinct(Sky.Start)

#Fix Year ---------------------------------------------------------
surveys_cleaned %>% 
  dplyr::count(Year.Raw) %>% 
  base::print(n = Inf)

#Why are there three blanks in year?
surveys_cleaned %>% 
  filter(Year.Raw == "")
#They're all y2

#Where are the NA's?
surveys_cleaned %>% 
  filter(is.na(Year.Raw)) %>%  
  dplyr::select(Route.ID.Raw, Date, Year.Raw) %>% 
  arrange(Date) %>% 
  base::print(n = Inf)
#All 2022's

#Make a new Year column
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Year = case_when(Year.Raw %in% c("", "y2") ~ "Y2",
                          is.na(Year.Raw) ~ "Y1",
                          TRUE ~ NA))

#View the clean year
surveys_cleaned %>% 
  dplyr::select(Year, Date) %>% 
  base::print(n = Inf)

#Fix route ID's -------------------------------------------------
#view all the route id's
surveys_cleaned %>% 
  dplyr::count(Route.ID.Raw) %>% 
  base::print(n = Inf)

#View a specific route id
surveys_cleaned %>% 
  filter(Route.ID.Raw == "B-4")

#Clean up transect codes
surveys_cleaned <- surveys_cleaned %>%
  mutate(Route.ID = case_when( 
    Route.ID.Raw %in% c("UT-B1", "B1", "UT-B01") ~ "UT-B01", #some of these came from
    Route.ID.Raw %in% c("UT-C1", "UT-C01") ~ "UT-C01",       #ruling out other possibilities in
    Route.ID.Raw %in% c("UT-B2", "UT-B02") ~ "UT-B02",       #the dataset. Others came from
    Route.ID.Raw %in% c("UT-C2", "C2") ~ "UT-C02",       #looking at coordinates in arc
    Route.ID.Raw %in% c("UT-B5", "B5") ~ "UT-B05",
    Route.ID.Raw %in% c("UT-C5", "C5") ~ "UT-C05",
    Route.ID.Raw %in% c("UT-B6", "UT-B6-P1") ~ "UT-B06",
    Route.ID.Raw %in% c("UT-C6", "UT-C06") ~ "UT-C06",
    Route.ID.Raw %in% c("UT-B8") ~ "UT-B08",
    Route.ID.Raw %in% c("UT-C8", "C8", "UT-C08") ~ "UT-C08",
    Route.ID.Raw %in% c("B27") ~ "UT-B27",
    Route.ID.Raw %in% c("UT_C15") ~ "UT-C15",
    Route.ID.Raw %in% c("B15") ~ "UT-B15",
    Route.ID.Raw %in% c("UT_C16") ~ "UT-C16",
    Route.ID.Raw %in% c("B16") ~ "UT-B16",
    Route.ID.Raw %in% c("B17") ~ "UT-B17",
    Route.ID.Raw %in% c("UT-C") ~ "UT-C19",
    Route.ID.Raw %in% c("B19") ~ "UT-B19",
    Route.ID.Raw %in% c("B22") ~ "UT-B22",
    Route.ID.Raw %in% c("C24") ~ "UT-C24",
    Route.ID.Raw %in% c("UT_C25") ~ "UT-C25",
    Route.ID.Raw %in% c("ID-B3", "ID_B3") ~ "ID-B03",
    Route.ID.Raw %in% c("ID-C3", "ID_C3", "C3") ~ "ID-C03",
    Route.ID.Raw %in% c("ID-B4") ~ "ID-B04",
    Route.ID.Raw %in% c("ID-C4", "C4", "B-4") ~ "ID-C04", #The B-4 obs was incorrectly labeled
    Route.ID.Raw %in% c("ID-B7") ~ "ID-B07",
    Route.ID.Raw %in% c("ID-C7", "ID C7") ~ "ID-C07",
    Route.ID.Raw %in% c("ID-B9") ~ "ID-B09",
    Route.ID.Raw %in% c("ID-C9") ~ "ID-C09",
    Route.ID.Raw %in% c("ID_B19") ~ "ID-B19",
    Route.ID.Raw %in% c("ID_B21") ~ "ID-B21",
    Route.ID.Raw %in% c("ID_B26") ~ "ID-B26",
    Route.ID.Raw %in% c("ID_B28") ~ "ID-C07",
    Route.ID.Raw %in% c("ID_C12") ~ "ID-C12",
    Route.ID.Raw %in% c("ID_C15") ~ "ID-C15",
    Route.ID.Raw %in% c("ID_C19") ~ "ID-C19",
    Route.ID.Raw %in% c("ID_C26") ~ "ID-C26",
    Route.ID.Raw %in% c("ID_") ~ "ID-B19",
    TRUE ~ as.character(Route.ID.Raw)
  ))

#view cleaned route id's
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf)
#There are a couple incorrect routes

#Fix plots where Plot ID doesn't line up with plot type ---------------------------------------------

#Start changing the incorrect routes or visits one by one
#First ID-B04
#Look at specific surveys
surveys_cleaned %>%
  filter(Route.ID == "UT-C24") %>% #Only care about the one route
  dplyr::select(Observer.ID.Raw, Date.Time.Raw, Visit.Raw, Route.ID) # only need a few columns

#One of that route's observations was labeled ID-B24
#I know from arc that the 6/28 one is incrrectly labeled
surveys_cleaned <- surveys_cleaned %>%
  mutate(Route.ID = case_when(Route.ID == "ID-B24" & Date.Time.Raw == "6/28/2022 12:38:00 PM"
                              ~ "ID-B04",
                                     TRUE ~ as.character(Route.ID)))
#view cleaned route id's
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf)
#Those two routes look good

#View UT-B01
surveys_cleaned %>%
  filter(Route.ID == "UT-B01" & Year == "Y1") #Only care about the one route
#I will treat everything on 6/2/2022 as training. I am removing those 

#make sure nothing else happened on 6/2/2022
surveys_cleaned %>%
  filter(Date == '2022-06-2')

#View surveys before the change
nrow(surveys_cleaned)

#Remove the surveys from training days
surveys_cleaned <- surveys_cleaned %>%
  filter(Date != '2022-06-2')
#I will treat everything on 6/2/2022 as training. I am removing those 

#View surveys after the change
nrow(surveys_cleaned)

#Fix UT-C24
surveys_cleaned %>%
  filter(Route.ID == "UT-C24") %>% #View UT-B24 routes
  dplyr::select(Observer.ID.Raw, Date.Time.Raw, Visit.Raw, Route.ID) # only need a few columns
#Austin's UT-B24 survey should be UT-C24

#Make sure nothing else had that start time
surveys_cleaned %>% 
  filter(Date.Time.Raw == "6/10/2022 12:29:00 PM")

#change that observation
surveys_cleaned <- surveys_cleaned %>%
  mutate(Route.ID = case_when(Date.Time.Raw == "6/10/2022 12:29:00 PM" ~ "UT-C24",
                              TRUE ~ Route.ID))

#View surveys after the change
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf)

#Fix UT-B24
#One is recorded as UT-B08, the other as UT-B25
#View those routes
surveys_cleaned %>% 
  filter(Route.ID == "UT-B08" | 
           Route.ID == "UT-B25") %>% 
  dplyr::select(Route.ID, Date.Time.Raw, Observer.ID.Raw)

#View the date and time of the incrrect ones
surveys_cleaned %>% 
  filter(Date.Time.Raw == "6/10/2022 1:00:00 PM" | 
           Date.Time.Raw == "5/25/2023 11:58:00 AM") %>% 
  dplyr::select(Route.ID, Date.Time.Raw, Observer.ID.Raw)

#switch those over to the correct route
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Route.ID = case_when(Date.Time.Raw == "6/10/2022 1:00:00 PM" ~ "UT-B24",
           Date.Time.Raw == "5/25/2023 11:58:00 AM"~ "UT-B24",
           TRUE ~ Route.ID))

#View surveys after the change
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf)
#looks good 

#Fix UT-B08
#One of the UT-B08's is labeled UT-C08
#View those routes
surveys_cleaned %>% 
  filter(Route.ID == "UT-B08" | 
           Route.ID == "UT-C08") %>% 
  dplyr::select(Route.ID, Date.Time.Raw, Observer.ID.Raw)

#View the date and time of the incorrect one
surveys_cleaned %>% 
  filter(Date.Time.Raw == "6/10/2022 1:43:00 PM") %>% 
  dplyr::select(Route.ID, Date.Time.Raw, Observer.ID.Raw)

#switch those over to the correct route
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Route.ID = case_when(Date.Time.Raw == "6/10/2022 1:43:00 PM" ~ "UT-B08",
                              TRUE ~ Route.ID))

#View surveys after the change
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf) 
#looks good 

#Fix UT-C16
#One of the UT-C16's is labeled UT-C30
#View the date and time of the incorrect one
surveys_cleaned %>% 
  filter(Date.Time.Raw == "7/12/2022 1:08:00 PM") %>% 
  dplyr::select(Route.ID, Date.Time.Raw, Observer.ID.Raw)
#There are two with this date and time

#switch one over to the correct route
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Route.ID = case_when(Date.Time.Raw == "7/12/2022 1:08:00 PM" &
                                Route.ID == "UT-C30" ~ "UT-C16",
                              TRUE ~ Route.ID))

#View surveys after the change
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf) 

#Fix UT-C22
#One of the surveys is labeled ID-C22

#View the incorrect survey
surveys_cleaned %>% 
  filter(Date.Time.Raw == "6/12/2023 12:22:00 PM") %>% 
  dplyr::select(Route.ID, Date.Time.Raw, Observer.ID.Raw)

#switch it over to the correct route
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Route.ID = case_when(Date.Time.Raw == "6/12/2023 12:22:00 PM" ~ "UT-C22",
                              TRUE ~ Route.ID))

#View surveys after the change
surveys_cleaned %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf) 

#That's all of the ones that I know how to fix
#IT-C11 and ID-C22 might just only have three surveys and that's okay

#fix observer name ----------------------------------------------------------
#observer name
surveys_cleaned %>%
  distinct(Observer.ID.Raw)

surveys_cleaned <- surveys_cleaned %>%
  mutate(Observer.ID = case_when( 
    Observer.ID.Raw == "thea_mills" ~ "Thea Mills",
    Observer.ID.Raw == "devon_erwin" ~ "Devin Erwin",
    Observer.ID.Raw %in% c("Austin_Heitzman", 
                           "Austin") ~ "Austin Heitzman",
    Observer.ID.Raw == "will_harrod" ~ "Will Harrod",
    Observer.ID.Raw == "andrew_zilka" ~ "Andrew Zilka",
    Observer.ID.Raw %in% c("Eoin_Ohearn", 
                           "Eoin") ~ "Eoin Ohearn",
    Observer.ID.Raw == "anna_mumford" ~ "Anna Mumford",
    Observer.ID.Raw == "trey_mccuen" ~ "Trey Mccuen",
    Observer.ID.Raw %in% c("Eliza_Wesemann", 
                         "Eliza_Wesemann ",
                         "eliza wesemann ",
                         "eliza wesemann",
                         "Eliza wesemann") ~ "Eliza Wesemann",
    Observer.ID.Raw %in% c("Rory_Eggleston",
                         "rory_eggleston") ~ "Rory Eggleston",
    Observer.ID.Raw %in% c("Ruger_Carter",
                         "ruger_carter") ~ "Ruger Carter",
    Observer.ID.Raw %in% c("Keramie_Hamby_",
                         "Keramie_Hamby",
                         "keramie_hamby",
                         "Keramie Hamby ") ~ "Keramie Hamby",
    TRUE ~ as.character(Observer.ID.Raw)
  ))

#View observer ID after the change
surveys_cleaned %>%
  distinct(Observer.ID)

#Add a column for plot type ------------------------------------------------------------
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Route.Type = case_when(str_detect(Route.ID, "C") ~ "R",
                                str_detect(Route.ID, "B") ~ "B",
                                TRUE ~ NA))

#Make sure it worked
surveys_cleaned %>% 
  dplyr::select(Route.ID, Route.Type) %>% 
  base::print(n = Inf)

#Fix Visit Number ------------------------------------------------------------------------
surveys_cleaned %>%
  dplyr::count(Visit.Raw)

#View the V3's
surveys_cleaned %>% 
  filter(Route.ID %in% c("UT-C24", "UT-C08")) %>% 
  dplyr::select(Route.ID, Visit.Raw, Date) %>% 
  arrange(Route.ID)
#Not sure why they're here

#Standardize visit number 
surveys_cleaned <- surveys_cleaned %>%
  mutate (Visit = case_when(
    Visit.Raw %in% c("1", "01", "V1") ~ "V1",
    Visit.Raw %in% c("2", "02", "V2") ~ "V2",
    Visit.Raw == "3" ~ "V3",
    TRUE ~ Visit.Raw))

#View the cleaned visit number 
surveys_cleaned %>%
  group_by(Year, Visit) %>% 
  dplyr::summarise(Count = n()) %>% 
  ungroup()

#How many of each visit did each route get
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#Fix individual Visits -------------------------------------------------------
#ID-B04 had 2 V2's and no V1 in Y2--
surveys_cleaned %>% 
  filter(Route.ID == "ID-B04") %>% 
  dplyr::select(Route.ID, Year, Observer.ID, Visit, Date, Date.Time.Raw)

#Update the 2nd visit from Y2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/8/2023 12:10:00 PM"
                           & Route.ID == "ID-B04" ~ "V1", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#ID-B11 had 2 V1's in Y2--
surveys_cleaned %>% 
  filter(Route.ID == "ID-B11") %>% 
  dplyr::select(Route.ID, Year, Observer.ID, Visit, Date, Date.Time.Raw)
#Update the 1st visit from Y2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/6/2023 12:47:00 PM"
                           & Route.ID == "ID-B11" ~ "V2", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#ID-B12 had 2 V1's in Y2--
surveys_cleaned %>% 
  filter(Route.ID == "ID-B12") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#Update the 2nd visit from Y2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/12/2023 11:50:00 AM"
                           & Route.ID == "ID-B12" ~ "V2", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#ID-B19 had 2 V1's in Y1--
surveys_cleaned %>% 
  filter(Route.ID == "ID-B19") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#Those are true V1's no need to change anything

#ID-C07 had 2 V1's in Y2 --
surveys_cleaned %>% 
  filter(Route.ID == "ID-C07") %>% 
  dplyr::select(Route.ID, Year, Observer.ID, Visit, Date, Date.Time.Raw)
#Update the 2nd visit from Y2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/6/2023 12:44:00 PM"
                           & Route.ID == "ID-C07" ~ "V2", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#ID-C09 had 3 V2's in Y2 --
surveys_cleaned %>% 
  filter(Route.ID == "ID-C09") %>% 
  dplyr::select(Route.ID, Year, Observer.ID, Visit, Date, Date.Time.Raw)
#Update the 1st visit from Y2
#The other is a real second V2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/19/2023 12:20:00 PM"
                           & Route.ID == "ID-C09" ~ "V1", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#ID-C11 had 2 V1's in Y2 --
surveys_cleaned %>% 
  filter(Route.ID == "ID-C11") %>% 
  dplyr::select(Route.ID, Year, Observer.ID, Visit, Date, Date.Time.Raw)
#Update the 2nd visit from Y2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/6/2023 12:38:00 PM"
                           & Route.ID == "ID-C11" ~ "V2", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#UT-B15 had 2 V2's in Y1 --
surveys_cleaned %>% 
  filter(Route.ID == "UT-B15") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#There were two V2's so no need to change anything

#UT-B16 had 2 V2's in Y2 --
surveys_cleaned %>% 
  filter(Route.ID == "UT-B16") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#The extra Y1 visit is a real extra visit
#Update the first visit from Y2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/9/2023 12:37:00 PM"
                           & Route.ID == "UT-B16" ~ "V1", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#UT-B30 had 2 V2's in Y1 --
surveys_cleaned %>% 
  filter(Route.ID == "UT-B30") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#The extra Y2 visit is a real extra visit

#UT-C19 had 3 V1's in Y2 --
surveys_cleaned %>% 
  filter(Route.ID == "UT-C19") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#There were two V2's in Y2 but they were both labeled V1
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw %in% c("6/6/2023 11:42:00 AM", 
                                                "6/6/2023 11:45:00 AM") 
                           & Route.ID == "UT-C19" ~ "V2", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#UT-C30 had 2 V1's in Y2 --
surveys_cleaned %>% 
  filter(Route.ID == "UT-C30") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#Switch the second visit in Y2 to V2
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw == "6/6/2023 11:30:00 AM"
                           & Route.ID == "UT-C30" ~ "V2", 
                           TRUE ~ Visit))
#Did that fix it
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>% 
  base::print(n = Inf)

#ID-C07 has an extra Y1 V1
surveys_cleaned %>% 
  filter(Route.ID == "ID-C07") %>% 
  dplyr::select(Route.ID, Observer.ID, Year, Visit, Date, Date.Time.Raw)
#Fix the incorrect one
surveys_cleaned <- surveys_cleaned %>% 
  mutate(Visit = case_when(Date.Time.Raw %in% c("6/27/2022 12:57:00 PM", 
                                                "6/27/2022 1:01:00 PM") 
                           & Route.ID == "ID-C07" ~ "V2", 
                           TRUE ~ Visit))

#Make sure I didn't miss anything
#Do they all have the right number of unique visits?
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>%
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf)

#How many visits are doubled?
#All doubbles 
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>%
  filter(Visit.Count != 1) %>% 
  base::print(n = Inf)
#Date doubles
surveys_cleaned %>% 
  group_by(Route.ID, Year, Visit, Date) %>% 
  dplyr::summarise(Visit.Count = n()) %>% 
  ungroup() %>%
  filter(Visit.Count != 1) %>% 
  base::print(n = Inf)
#Those are all real doubles

#Pull out only the columns that I will need
names(surveys_cleaned)
surveys <- surveys_cleaned %>% 
  dplyr::select(Route.ID, Route.Type, Year, Visit, Date, Ord.Date, Observer.ID, 
         Temp.Start, Temp.End, Sky.Start, Sky.End, Wind.Start, Wind.End,
         Route.Notes, Global.ID.Survey) %>% 
  mutate_if(is.character, ~ na_if(., "")) #change blanks to NA's

#View the survey data one last time
glimpse(surveys)
#Finished cleaning the survey data -----------------------------------------

#join surveys and points, then export --------------------------------------------
#How many points before the join?
glimpse(points)

#join points with surveys
survey_points <- surveys %>% #start with surveys
  full_join(points, by = "Global.ID.Survey")  

#View joined survey points
glimpse(survey_points)

#Export for Arc cleaning --------------------------------
survey_points %>% 
  mutate(Full.Point.ID = paste0(Route.ID, "-", "P", Point.ID),
         Survey.ID = paste(Full.Point.ID, Year, Visit, sep = "-"),
         Time.Stamp = paste(Point.ID, Visit, Point.Time, sep = "-")) %>% 
  write.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\GIS\\Sobs_Geospatial_Data\\Geoprocessing_Outputs_temp\\surveyed_points.csv")

#Add Observation data
sobs <- survey_points %>% 
  left_join(observations, by = "Global.ID.Point")

#View the full joined data
glimpse(sobs)

#How many times was each route surveyed in the final dataset
sobs %>% 
  dplyr::count(Route.ID, Year, Visit) %>% 
  dplyr::count(Route.ID, Year, Visit) %>% 
  base::print(n = Inf)

#View the route NA
sobs %>% 
  filter(is.na(Route.ID))

#Can drop this 
sobs <- sobs %>% 
  drop_na(Route.ID)
#...and view
glimpse(sobs)

#Make a new column that combines point id and route id
sobs <- sobs %>% 
  mutate(Full.Point.ID = paste(Route.ID, "-", "P", Point.ID, sep = ""))

#View it to make sure the data frame looks good
glimpse(sobs)

#141 observations were lost
sobs %>% distinct(Full.Point.ID) %>% base::print(n = Inf)

#How many times does each point appear on each route?
visit_count <- sobs %>% 
  filter(Visit %in% c("V1", "V2")) %>% 
  group_by(Full.Point.ID, Route.ID, Year, Visit) %>% 
  dplyr::summarise(Observations = n()) %>% 
  ungroup() %>% 
  mutate(Full.Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  dplyr::select(Full.Point.ID, Route.ID, Full.Visit.ID, Observations) %>% 
  pivot_wider(names_from = Full.Visit.ID,
              values_from = Observations)

#view how many observations were recorded at each point
head(visit_count, n = 20)

#Combine Notes
sobs <- sobs %>% 
  mutate(Notes = paste(Route.Notes, Point.Notes, Obs.Notes, sep = " ")) %>% 
  mutate(Notes = str_remove_all(Notes, "NA")) %>% 
  mutate(Notes = case_when(Notes %in% c("", " ", "  ", "   ") ~ NA,
                           TRUE ~ Notes)) %>% 
  mutate(Notes = str_replace_all(Notes, "_", " "))

#View the updated notes
glimpse(sobs)
sobs %>% 
  dplyr::distinct(Notes)

#Pull out the columns that I will actually need
#I will probably end up tweaking which of these are included
names(sobs)
sobs <- sobs %>% 
  dplyr::select(Species, Distance, Minute, Direction, How.Detected, Song.Also, Group.Size,
         Sex, Visual.ID, Route.ID, Route.Type, Full.Point.ID, Year, Visit,  
         Date, Ord.Date, Point.Time, Point.ID, Observer.ID, 
         Temp.Start, Sky.Start, Wind.Start, Temp.End, Sky.End, Wind.End,
         Notes)

#View the new sobs data
glimpse(sobs)

##########################################################################################
sobs_point_fix <- sobs 

#Fix the incorrect point names
#The naming systems for the first year was different 
sobs_temp <- sobs_point_fix %>% 
  mutate(Full.Point.ID = case_when(#ID-B13-V1
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P10"
                                   ~ "ID-B13-P01",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P11"
                                   ~ "ID-B13-P02", 
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P14"
                                   ~ "ID-B13-P03",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P15"
                                   ~ "ID-B13-P04",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P09"
                                   ~ "ID-B13-P05",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P12"
                                   ~ "ID-B13-P06",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P13"
                                   ~ "ID-B13-P07",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P16"
                                   ~ "ID-B13-P08",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P08"
                                   ~ "ID-B13-P09",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P05"
                                   ~ "ID-B13-P10",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P04"
                                   ~ "ID-B13-P11",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P01"
                                   ~ "ID-B13-P12",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P07"
                                   ~ "ID-B13-P13",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P06"
                                   ~ "ID-B13-P14",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P03"
                                   ~ "ID-B13-P15",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B13-P02"
                                   ~ "ID-B13-P16",
                                   #ID-B13-V2
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P08"
                                   ~ "ID-B13-P02",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P03"
                                   ~ "ID-B13-P03",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P02"
                                   ~ "ID-B13-P04",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P07"
                                   ~ "ID-B13-P06",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P04"
                                   ~ "ID-B13-P07",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P01"
                                   ~ "ID-B13-P08",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P06"
                                   ~ "ID-B13-P10",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P05"
                                   ~ "ID-B13-P11",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P16"
                                   ~ "ID-B13-P12",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P13"
                                   ~ "ID-B13-P14",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P14"
                                   ~ "ID-B13-P15",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B13-P15"
                                   ~ "ID-B13-P16",
                                   #ID-C13-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C13-P10"
                                   ~ "ID-C13-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P11"
                                   ~ "ID-C13-P02",
                                   Year == "Y1"  & Full.Point.ID == "ID-C13-P16"
                                   ~ "ID-C13-P03",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P01"
                                   ~ "ID-C13-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P09"
                                   ~ "ID-C13-P05",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P12"
                                   ~ "ID-C13-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P15"
                                   ~ "ID-C13-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C13-P02"
                                   ~ "ID-C13-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P08"
                                   ~ "ID-C13-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P13"
                                   ~ "ID-C13-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P14"
                                   ~ "ID-C13-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C13-P03"
                                   ~ "ID-C13-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-C13-P07"
                                   ~ "ID-C13-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C13-P06"
                                   ~ "ID-C13-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C13-P05"
                                   ~ "ID-C13-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C13-P04"
                                   ~ "ID-C13-P16",
                                   #ID-B26-V1
                                   Year == "Y1" & Full.Point.ID == "ID-B26-P08"
                                   ~ "ID-B26-P05",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P08"
                                   ~ "ID-B26-P05",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P07"
                                   ~ "ID-B26-P06",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P06"
                                   ~ "ID-B26-P07",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P05"
                                   ~ "ID-B26-P08",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P16"
                                   ~ "ID-B26-P13",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P15"
                                   ~ "ID-B26-P14",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P14"
                                   ~ "ID-B26-P15",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B26-P13"
                                   ~ "ID-B26-P16",
                                   #ID-B26-V2
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P16"
                                   ~ "ID-B26-P13",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P08"
                                   ~ "ID-B26-P05",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P05"
                                   ~ "ID-B26-P08",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P13"
                                   ~ "ID-B26-P16",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P14"
                                   ~ "ID-B26-P15",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P06"
                                   ~ "ID-B26-P07",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P07"
                                   ~ "ID-B26-P06",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B26-P15"
                                   ~ "ID-B26-P14",
                                   #ID-B19-V1
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P03"
                                   ~ "ID-B19-P05",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P04"
                                   ~ "ID-B19-P09",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P05"
                                   ~ "ID-B19-P13",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P06"
                                   ~ "ID-B19-P14",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P07"
                                   ~ "ID-B19-P10",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P08"
                                   ~ "ID-B19-P06",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P09"
                                   ~ "ID-B19-P03",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P11"
                                   ~ "ID-B19-P07",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P12"
                                   ~ "ID-B19-P11",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P13"
                                   ~ "ID-B19-P15",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P14"
                                   ~ "ID-B19-P12",
                                   Year == "Y1" & Visit == "V1" & Full.Point.ID == "ID-B19-P15"
                                   ~ "ID-B19-P08",
                                   #ID-B19-V2
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P02"
                                   ~ "ID-B19-P05",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P03"
                                   ~ "ID-B19-P09",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P04"
                                   ~ "ID-B19-P13",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P05"
                                   ~ "ID-B19-P13",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P06"
                                   ~ "ID-B19-P10",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P07"
                                   ~ "ID-B19-P06",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P10"
                                   ~ "ID-B19-P11",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P12"
                                   ~ "ID-B19-P15",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P13"
                                   ~ "ID-B19-P16",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P14"
                                   ~ "ID-B19-P12",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P15"
                                   ~ "ID-B19-P08",
                                   Year == "Y1" & Visit == "V2" & Full.Point.ID == "ID-B19-P16"
                                   ~ "ID-B19-P04",
                                   #ID-C19-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P01"
                                   ~ "ID-C19-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P02"
                                   ~ "ID-C19-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P03"
                                   ~ "ID-C19-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P04"
                                   ~ "ID-C19-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P05"
                                   ~ "ID-C19-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P07"
                                   ~ "ID-C19-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P08"
                                   ~ "ID-C19-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P09"
                                   ~ "ID-C19-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P10"
                                   ~ "ID-C19-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P11"
                                   ~ "ID-C19-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-C19-P12"
                                   ~ "ID-C19-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C19-P13"
                                   ~ "ID-C19-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-C19-P14"
                                   ~ "ID-C19-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-C19-P15"
                                   ~ "ID-C19-P12",
                                   #ID-B16-Y1 & V2
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P01"
                                   ~ "ID-B16-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P02"
                                   ~ "ID-B16-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P03" & Point.Time == "07:39"
                                   ~ "ID-B16-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P03" & Point.Time == "10:22"
                                   ~ "ID-B16-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P03" & Point.Time == "10:36"
                                   ~ "ID-B16-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P04"
                                   ~ "ID-B16-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P05"
                                   ~ "ID-B16-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P06"
                                   ~ "ID-B16-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P07"
                                   ~ "ID-B16-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P08"
                                   ~ "ID-B16-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P09"
                                   ~ "ID-B16-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P10"
                                   ~ "ID-B16-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P11"
                                   ~ "ID-B16-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-B16-P12"
                                   ~ "ID-B16-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B16-P13"
                                   ~ "ID-B16-P03",
                                   Year == "Y1" &  Full.Point.ID == "ID-B16-P14"
                                   ~ "ID-B16-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-B16-P15"
                                   ~ "ID-B16-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-B16-P16"
                                   ~ "ID-B16-P12",
                                   #ID-C16-Y1 & V2
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P01"
                                   ~ "ID-C16-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P02"
                                   ~ "ID-C16-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P03"
                                   ~ "ID-C16-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P04"
                                   ~ "ID-C16-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P05"
                                   ~ "ID-C16-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P06"
                                   ~ "ID-C16-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P07"
                                   ~ "ID-C16-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P08"
                                   ~ "ID-C16-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P09"
                                   ~ "ID-C16-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P10"
                                   ~ "ID-C16-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P11"
                                   ~ "ID-C16-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-C16-P12"
                                   ~ "ID-C16-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C16-P13"
                                   ~ "ID-C16-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C16-P14"
                                   ~ "ID-C16-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-C16-P15"
                                   ~ "ID-C16-P11",
                                   Year == "Y1" &  Full.Point.ID == "ID-C16-P16"
                                   ~ "ID-C16-P12",
                                   #ID-C26-Y1 & V2
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P01"
                                   ~ "ID-C26-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P02"
                                   ~ "ID-C26-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P03"
                                   ~ "ID-C26-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P04"
                                   ~ "ID-C26-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P05"
                                   ~ "ID-C26-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P06"
                                   ~ "ID-C26-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P07"
                                   ~ "ID-C26-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P08"
                                   ~ "ID-C26-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P09"
                                   ~ "ID-C26-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P10"
                                   ~ "ID-C26-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P11"
                                   ~ "ID-C26-P11",
                                   Year == "Y1" &  Full.Point.ID == "ID-C26-P12"
                                   ~ "ID-C26-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C26-P13"
                                   ~ "ID-C26-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-C26-P14"
                                   ~ "ID-C26-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-C26-P15"
                                   ~ "ID-C26-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-C26-P16"
                                   ~ "ID-C26-P04",
                                   #ID-C15-Y1 & V2
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P01"
                                   ~ "ID-C15-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P02"
                                   ~ "ID-C15-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P03"
                                   ~ "ID-C15-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P04"
                                   ~ "ID-C15-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P05"
                                   ~ "ID-C15-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P06"
                                   ~ "ID-C15-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P07"
                                   ~ "ID-C15-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P08"
                                   ~ "ID-C15-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P09"
                                   ~ "ID-C15-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P10"
                                   ~ "ID-C15-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P11"
                                   ~ "ID-C15-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-C15-P12"
                                   ~ "ID-C15-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C15-P13"
                                   ~ "ID-C15-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-C15-P14"
                                   ~ "ID-C15-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-C15-P15"
                                   ~ "ID-C15-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-C15-P16"
                                   ~ "ID-C15-P08",
                                   #ID-C12-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P01" & Point.Time == "06:59"
                                   ~ "ID-C12-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P01" & Point.Time == "08:12"
                                   ~ "ID-C12-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P02" & Visit == "V1"
                                   ~ "ID-C12-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P03" & Visit == "V1"
                                   ~ "ID-C12-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P04" & Visit == "V1"
                                   ~ "ID-C12-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P05" & Visit == "V1"
                                   ~ "ID-C12-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P06" & Visit == "V1"
                                   ~ "ID-C12-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P08" & Visit == "V1"
                                   ~ "ID-C12-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P09" & Visit == "V1"
                                   ~ "ID-C12-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P10" & Visit == "V1"
                                   ~ "ID-C12-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P11" & Visit == "V1"
                                   ~ "ID-C12-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P12" & Visit == "V1"
                                   ~ "ID-C12-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P13" & Visit == "V1"
                                   ~ "ID-C12-P11",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P14" & Visit == "V1"
                                   ~ "ID-C12-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P15" & Visit == "V1"
                                   ~ "ID-C12-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P16" & Visit == "V1"
                                   ~ "ID-C12-P13",
                                   #ID-C12-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P01" & Point.Time == "06:23"
                                   ~ "ID-C12-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P02" & Visit == "V2"
                                   ~ "ID-C12-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P03" & Visit == "V2"
                                   ~ "ID-C12-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P04" & Visit == "V2"
                                   ~ "ID-C12-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P05" & Visit == "V2"
                                   ~ "ID-C12-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P06" & Visit == "V2"
                                   ~ "ID-C12-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P07" & Visit == "V2"
                                   ~ "ID-C12-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P08" & Visit == "V2"
                                   ~ "ID-C12-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P09" & Visit == "V2"
                                   ~ "ID-C12-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P10" & Visit == "V2"
                                   ~ "ID-C12-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P11" & Visit == "V2"
                                   ~ "ID-C12-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P12" & Visit == "V2"
                                   ~ "ID-C12-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C12-P13" & Visit == "V2"
                                   ~ "ID-C12-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P14" & Visit == "V2"
                                   ~ "ID-C12-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P15" & Visit == "V2"
                                   ~ "ID-C12-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-C12-P16" & Visit == "V2"
                                   ~ "ID-C12-P14",
                                   #ID-B12-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P01" 
                                   ~ "ID-B12-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P02" 
                                   ~ "ID-B12-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P03" 
                                   ~ "ID-B12-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P04"
                                   ~ "ID-B12-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P05"
                                   ~ "ID-B12-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P06"
                                   ~ "ID-B12-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P07"
                                   ~ "ID-B12-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P08"
                                   ~ "ID-B12-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P09"
                                   ~ "ID-B12-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P10"
                                   ~ "ID-B12-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P11" 
                                   ~ "ID-B12-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-B12-P12"
                                   ~ "ID-B12-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B12-P13" 
                                   ~ "ID-B12-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-B12-P14"
                                   ~ "ID-B12-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-B12-P15"
                                   ~ "ID-B12-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B12-P16"
                                   ~ "ID-B12-P05",
                                   #ID-C09-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P01" 
                                   ~ "ID-C09-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P02" 
                                   ~ "ID-C09-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P03" 
                                   ~ "ID-C09-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P04"
                                   ~ "ID-C09-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P05"
                                   ~ "ID-C09-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P06"
                                   ~ "ID-C09-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P07"
                                   ~ "ID-C09-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P08"
                                   ~ "ID-C09-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P09"
                                   ~ "ID-C09-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P10"
                                   ~ "ID-C09-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P11" 
                                   ~ "ID-C09-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-C09-P12"
                                   ~ "ID-C09-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C09-P13" 
                                   ~ "ID-C09-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-C09-P14"
                                   ~ "ID-C09-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-C09-P15"
                                   ~ "ID-C09-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C09-P16"
                                   ~ "ID-C09-P05",
                                   #ID-B09-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P01" 
                                   ~ "ID-B09-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P02" 
                                   ~ "ID-B09-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P03" 
                                   ~ "ID-B09-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P04"
                                   ~ "ID-B09-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P05"
                                   ~ "ID-B09-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P06"
                                   ~ "ID-B09-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P07"
                                   ~ "ID-B09-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P08"
                                   ~ "ID-B09-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P09"
                                   ~ "ID-B09-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P10"
                                   ~ "ID-B09-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P11" 
                                   ~ "ID-B09-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-B09-P12"
                                   ~ "ID-B09-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B09-P13" 
                                   ~ "ID-B09-P13",
                                   Year == "Y1" &  Full.Point.ID == "ID-B09-P14"
                                   ~ "ID-B09-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-B09-P15"
                                   ~ "ID-B09-P05",
                                   Year == "Y1" &  Full.Point.ID == "ID-B09-P16"
                                   ~ "ID-B09-P01",
                                   #ID-B28-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P01"
                                   ~ "ID-B28-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P02" 
                                   ~ "ID-B28-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P03" 
                                   ~ "ID-B28-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P04"
                                   ~ "ID-B28-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P05"
                                   ~ "ID-B28-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P06"
                                   ~ "ID-B28-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P07"
                                   ~ "ID-B28-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P08"
                                   ~ "ID-B28-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P09"
                                   ~ "ID-B28-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P10"
                                   ~ "ID-B28-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P11" 
                                   ~ "ID-B28-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-B28-P12"
                                   ~ "ID-B28-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B28-P13" 
                                   ~ "ID-B28-P13",
                                   Year == "Y1" &  Full.Point.ID == "ID-B28-P14"
                                   ~ "ID-B28-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-B28-P15"
                                   ~ "ID-B28-P05",
                                   Year == "Y1" &  Full.Point.ID == "ID-B28-P16"
                                   ~ "ID-B28-P01",
                                   #ID-C07-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P01"
                                   ~ "ID-C07-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P02" 
                                   ~ "ID-C07-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P03" 
                                   ~ "ID-C07-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P04"
                                   ~ "ID-C07-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P05"
                                   ~ "ID-C07-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P06"
                                   ~ "ID-C07-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P07"
                                   ~ "ID-C07-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P08"
                                   ~ "ID-C07-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P09"
                                   ~ "ID-C07-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P10"
                                   ~ "ID-C07-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P11" 
                                   ~ "ID-C07-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-C07-P12"
                                   ~ "ID-C07-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C07-P13" 
                                   ~ "ID-C07-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-C07-P14"
                                   ~ "ID-C07-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-C07-P15"
                                   ~ "ID-C07-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-C07-P16"
                                   ~ "ID-C07-P14",
                                   #ID-C11-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P01"
                                   ~ "ID-C11-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P02" 
                                   ~ "ID-C11-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P03" 
                                   ~ "ID-C11-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P04"
                                   ~ "ID-C11-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P05"
                                   ~ "ID-C11-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P06"
                                   ~ "ID-C11-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P07"
                                   ~ "ID-C11-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P08"
                                   ~ "ID-C11-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P09"
                                   ~ "ID-C11-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P10"
                                   ~ "ID-C11-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P11" 
                                   ~ "ID-C11-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-C11-P12"
                                   ~ "ID-C11-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C11-P13" 
                                   ~ "ID-C11-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-C11-P14"
                                   ~ "ID-C11-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C11-P15"
                                   ~ "ID-C11-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-C11-P16"
                                   ~ "ID-C11-P08",
                                   #ID-B07-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P01"
                                   ~ "ID-B07-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P02" 
                                   ~ "ID-B07-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P03" 
                                   ~ "ID-B07-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P04"
                                   ~ "ID-B07-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P05"
                                   ~ "ID-B07-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P06"
                                   ~ "ID-B07-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P07"
                                   ~ "ID-B07-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P08"
                                   ~ "ID-B07-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P09"
                                   ~ "ID-B07-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P10"
                                   ~ "ID-B07-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P11" 
                                   ~ "ID-B07-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B07-P12"
                                   ~ "ID-B07-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B07-P13" 
                                   ~ "ID-B07-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-B07-P14"
                                   ~ "ID-B07-P05",
                                   Year == "Y1" &  Full.Point.ID == "ID-B07-P15"
                                   ~ "ID-B07-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-B07-P16"
                                   ~ "ID-B07-P13",
                                   #ID-B11-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P01"
                                   ~ "ID-B11-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P02" 
                                   ~ "ID-B11-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P03" 
                                   ~ "ID-B11-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P04"
                                   ~ "ID-B11-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P05"
                                   ~ "ID-B11-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P06"
                                   ~ "ID-B11-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P07"
                                   ~ "ID-B11-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P08"
                                   ~ "ID-B11-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P09"
                                   ~ "ID-B11-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P10"
                                   ~ "ID-B11-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P11" 
                                   ~ "ID-B11-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-B11-P12"
                                   ~ "ID-B11-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B11-P13" 
                                   ~ "ID-B11-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-B11-P14"
                                   ~ "ID-B11-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B11-P15"
                                   ~ "ID-B11-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-B11-P16"
                                   ~ "ID-B11-P14",
                                   #ID-B03-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P01" & Point.Time == "09:41"
                                   ~ "ID-B03-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P02" 
                                   ~ "ID-B03-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P03" 
                                   ~ "ID-B03-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P04"
                                   ~ "ID-B03-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P05"
                                   ~ "ID-B03-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P06"
                                   ~ "ID-B03-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P07"
                                   ~ "ID-B03-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P08"
                                   ~ "ID-B03-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P01" & Point.Time == "06:54"
                                   ~ "ID-B03-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P10"
                                   ~ "ID-B03-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P11" 
                                   ~ "ID-B03-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B03-P12"
                                   ~ "ID-B03-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B03-P13" 
                                   ~ "ID-B03-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-B03-P14"
                                   ~ "ID-B03-P02",
                                   Year == "Y1" &  Full.Point.ID == "ID-B03-P15"
                                   ~ "ID-B03-P03",
                                   Year == "Y1" &  Full.Point.ID == "ID-B03-P16"
                                   ~ "ID-B03-P04",
                                   #ID-C03-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P01"
                                   ~ "ID-C03-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P02" 
                                   ~ "ID-C03-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P03" 
                                   ~ "ID-C03-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P04"
                                   ~ "ID-C03-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P05"
                                   ~ "ID-C03-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P06"
                                   ~ "ID-C03-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P07"
                                   ~ "ID-C03-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P08"
                                   ~ "ID-C03-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P09"
                                   ~ "ID-C03-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P10"
                                   ~ "ID-C03-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P11" 
                                   ~ "ID-C03-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-C03-P12"
                                   ~ "ID-C03-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C03-P13" 
                                   ~ "ID-C03-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-C03-P14"
                                   ~ "ID-C03-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-C03-P15"
                                   ~ "ID-C03-P14",
                                   Year == "Y1" &  Full.Point.ID == "ID-C03-P16"
                                   ~ "ID-C03-P13",
                                   #ID-B23-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P01"
                                   ~ "ID-B23-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P02" 
                                   ~ "ID-B23-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P03" 
                                   ~ "ID-B23-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P04"
                                   ~ "ID-B23-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P05"
                                   ~ "ID-B23-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P06"
                                   ~ "ID-B23-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P07"
                                   ~ "ID-B23-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P08"
                                   ~ "ID-B23-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P09"
                                   ~ "ID-B23-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P10"
                                   ~ "ID-B23-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P11" 
                                   ~ "ID-B23-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-B23-P12"
                                   ~ "ID-B23-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B23-P13" 
                                   ~ "ID-B23-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-B23-P14"
                                   ~ "ID-B23-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-B23-P15"
                                   ~ "ID-B23-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-B23-P16"
                                   ~ "ID-B23-P14",
                                   #ID-C23-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P01"
                                   ~ "ID-C23-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P02" 
                                   ~ "ID-C23-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P03" 
                                   ~ "ID-C23-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P04"
                                   ~ "ID-C23-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P05"
                                   ~ "ID-C23-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P06"
                                   ~ "ID-C23-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P07"
                                   ~ "ID-C23-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P08"
                                   ~ "ID-C23-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P09"
                                   ~ "ID-C23-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P10"
                                   ~ "ID-C23-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P11" 
                                   ~ "ID-C23-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C23-P12"
                                   ~ "ID-C23-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C23-P13" 
                                   ~ "ID-C23-P14",
                                   Year == "Y1" &  Full.Point.ID == "ID-C23-P14"
                                   ~ "ID-C23-P13",
                                   Year == "Y1" &  Full.Point.ID == "ID-C23-P15"
                                   ~ "ID-C23-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-C23-P16"
                                   ~ "ID-C23-P05",
                                   #ID-B22-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P01"
                                   ~ "ID-B22-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P02" 
                                   ~ "ID-B22-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P03" 
                                   ~ "ID-B22-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P04"
                                   ~ "ID-B22-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P05"
                                   ~ "ID-B22-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P06"
                                   ~ "ID-B22-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P07"
                                   ~ "ID-B22-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P08"
                                   ~ "ID-B22-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P09"
                                   ~ "ID-B22-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P10"
                                   ~ "ID-B22-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P11" 
                                   ~ "ID-B22-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-B22-P12"
                                   ~ "ID-B22-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B22-P13" 
                                   ~ "ID-B22-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-B22-P14"
                                   ~ "ID-B22-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B22-P15"
                                   ~ "ID-B22-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-B22-P16"
                                   ~ "ID-B22-P14",
                                   #ID-C22-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P01"
                                   ~ "ID-C22-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P02" 
                                   ~ "ID-C22-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P03" 
                                   ~ "ID-C22-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P04"
                                   ~ "ID-C22-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P05"
                                   ~ "ID-C22-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P06"
                                   ~ "ID-C22-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P07"
                                   ~ "ID-C22-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P08"
                                   ~ "ID-C22-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P09"
                                   ~ "ID-C22-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P10"
                                   ~ "ID-C22-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P11" 
                                   ~ "ID-C22-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C22-P12"
                                   ~ "ID-C22-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C22-P13" 
                                   ~ "ID-C22-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-C22-P14"
                                   ~ "ID-C22-P05",
                                   Year == "Y1" &  Full.Point.ID == "ID-C22-P15"
                                   ~ "ID-C22-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-C22-P16"
                                   ~ "ID-C22-P13",
                                   #ID-B04-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P01"
                                   ~ "ID-B04-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P02" 
                                   ~ "ID-B04-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P03" 
                                   ~ "ID-B04-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P04"
                                   ~ "ID-B04-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P05"
                                   ~ "ID-B04-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P06"
                                   ~ "ID-B04-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P07"
                                   ~ "ID-B04-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P08"
                                   ~ "ID-B04-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P09"
                                   ~ "ID-B04-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P10"
                                   ~ "ID-B04-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P11" 
                                   ~ "ID-B04-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B04-P12"
                                   ~ "ID-B04-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B04-P13" 
                                   ~ "ID-B04-P14",
                                   Year == "Y1" &  Full.Point.ID == "ID-B04-P14"
                                   ~ "ID-B04-P13",
                                   Year == "Y1" &  Full.Point.ID == "ID-B04-P15"
                                   ~ "ID-B04-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-B04-P16"
                                   ~ "ID-B04-P05",
                                   #ID-C04-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P01"
                                   ~ "ID-C04-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P02" 
                                   ~ "ID-C04-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P03" 
                                   ~ "ID-C04-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P04"
                                   ~ "ID-C04-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P05"
                                   ~ "ID-C04-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P06"
                                   ~ "ID-C04-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P07"
                                   ~ "ID-C04-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P08"
                                   ~ "ID-C04-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P16" & Point.Time == "09:08"
                                   ~ "ID-C04-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P10"
                                   ~ "ID-C04-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P11" 
                                   ~ "ID-C04-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C04-P12"
                                   ~ "ID-C04-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-C04-P13" 
                                   ~ "ID-C04-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-C04-P14"
                                   ~ "ID-C04-P05",
                                   Year == "Y1" &  Full.Point.ID == "ID-C04-P15"
                                   ~ "ID-C04-P09",
                                   Year == "Y1" &  Full.Point.ID == "ID-C04-P16" & Point.Time == "06:38"
                                   ~ "ID-C04-P13",
                                   Year == "Y1" &  Full.Point.ID == "ID-C04-P16" & Point.Time == "09:40"
                                   ~ "ID-C04-P13",
                                   #ID-B24-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P01" & Visit == "V1"
                                   ~ "ID-B24-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P02" & Visit == "V1"
                                   ~ "ID-B24-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P03" & Visit == "V1"
                                   ~ "ID-B24-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P04" & Visit == "V1"
                                   ~ "ID-B24-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P05" & Visit == "V1"
                                   ~ "ID-B24-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P06" & Visit == "V1"
                                   ~ "ID-B24-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P07" & Visit == "V1"
                                   ~ "ID-B24-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P08" & Visit == "V1"
                                   ~ "ID-B24-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P09" & Visit == "V1"
                                   ~ "ID-B24-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P10" & Visit == "V1"
                                   ~ "ID-B24-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P11" & Visit == "V1"
                                   ~ "ID-B24-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P12" & Visit == "V1"
                                   ~ "ID-B24-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P13" & Visit == "V1"
                                   ~ "ID-B24-P01",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P14" & Visit == "V1"
                                   ~ "ID-B24-P02",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P15" & Visit == "V1"
                                   ~ "ID-B24-P03",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P16" & Visit == "V1"
                                   ~ "ID-B24-P04",
                                   #ID-B24-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P01" & Visit == "V2"
                                   ~ "ID-B24-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P02" & Visit == "V2"
                                   ~ "ID-B24-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P03" & Visit == "V2"
                                   ~ "ID-B24-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P04" & Visit == "V2"
                                   ~ "ID-B24-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P05" & Visit == "V2"
                                   ~ "ID-B24-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P06" & Visit == "V2"
                                   ~ "ID-B24-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P07" & Visit == "V2"
                                   ~ "ID-B24-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P08" & Visit == "V2"
                                   ~ "ID-B24-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P09" & Visit == "V2"
                                   ~ "ID-B24-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P10" & Visit == "V2"
                                   ~ "ID-B24-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P11" & Visit == "V2"
                                   ~ "ID-B24-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P12" & Visit == "V2"
                                   ~ "ID-B24-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-B24-P13" & Visit == "V2"
                                   ~ "ID-B24-P07",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P14" & Visit == "V2"
                                   ~ "ID-B24-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P15" & Visit == "V2"
                                   ~ "ID-B24-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-B24-P16" & Visit == "V2"
                                   ~ "ID-B24-P03",
                                   #ID-B21-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P01" & Visit == "V1"
                                   ~ "ID-B21-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P02" & Visit == "V1"
                                   ~ "ID-B21-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P03" & Visit == "V1"
                                   ~ "ID-B21-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P04" & Visit == "V1"
                                   ~ "ID-B21-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P05" & Visit == "V1"
                                   ~ "ID-B21-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P06" & Visit == "V1"
                                   ~ "ID-B21-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P07" & Visit == "V1"
                                   ~ "ID-B21-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P08" & Visit == "V1"
                                   ~ "ID-B21-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P09" & Visit == "V1"
                                   ~ "ID-B21-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P10" & Visit == "V1"
                                   ~ "ID-B21-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P11" & Visit == "V1" 
                                   ~ "ID-B21-P14",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P12" & Visit == "V1"
                                   ~ "ID-B21-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P13" & Visit == "V1" 
                                   ~ "ID-B21-P11",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P14" & Visit == "V1"
                                   ~ "ID-B21-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P15" & Visit == "V1"
                                   ~ "ID-B21-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P16" & Visit == "V1"
                                   ~ "ID-B21-P12",
                                   #ID-B21-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P01" & Visit == "V2" & Point.Time == "09:03"
                                   ~ "ID-B21-P04",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P01" & Visit == "V2" & Point.Time == "08:52"
                                   ~ "ID-B21-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P03" & Visit == "V2"
                                   ~ "ID-B21-P02",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P04" & Visit == "V2"
                                   ~ "ID-B21-P08",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P05" & Visit == "V2"
                                   ~ "ID-B21-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P06" & Visit == "V2"
                                   ~ "ID-B21-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P07" & Visit == "V2"
                                   ~ "ID-B21-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P08" & Visit == "V2"
                                   ~ "ID-B21-P09",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P09" & Visit == "V2"
                                   ~ "ID-B21-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P10" & Visit == "V2"
                                   ~ "ID-B21-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P12" & Visit == "V2" 
                                   ~ "ID-B21-P06",
                                   Year == "Y1" & Full.Point.ID == "ID-B21-P13" & Visit == "V2" 
                                   ~ "ID-B21-P10",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P14" & Visit == "V2" & Point.Time == "07:36"
                                   ~ "ID-B21-P15",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P14" & Visit == "V2" & Point.Time == "07:25"
                                   ~ "ID-B21-P16",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P15" & Visit == "V2"
                                   ~ "ID-B21-P12",
                                   Year == "Y1" &  Full.Point.ID == "ID-B21-P16" & Visit == "V2"
                                   ~ "ID-B21-P11",
                                   #ID-C21-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P01" & Point.Time == "06:28"
                                   ~ "ID-C21-P01",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P02" 
                                   ~ "ID-C21-P05",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P03" 
                                   ~ "ID-C21-P03",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P01" & Point.Time == "06:58"
                                   ~ "ID-C21-P13",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P05"
                                   ~ "ID-C21-P14",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P06"
                                   ~ "ID-C21-P15",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P07"
                                   ~ "ID-C21-P16",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P08"
                                   ~ "ID-C21-P12",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P09"
                                   ~ "ID-C21-P11",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P10"
                                   ~ "ID-C21-P10",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P11" 
                                   ~ "ID-C21-P06",
                                   Year == "Y1" &  Full.Point.ID == "ID-C21-P12"
                                   ~ "ID-C21-P07",
                                   Year == "Y1" & Full.Point.ID == "ID-C21-P13" 
                                   ~ "ID-C21-P08",
                                   Year == "Y1" &  Full.Point.ID == "ID-C21-P15" & Point.Time == "08:37"
                                   ~ "ID-C21-P04",
                                   Year == "Y1" &  Full.Point.ID == "ID-C21-P15" & Point.Time %in% c("07:25", "08:48")
                                   ~ "ID-C21-P03",
                                   Year == "Y1" &  Full.Point.ID == "ID-C21-P16"
                                   ~ "ID-C21-P02",
                                   #UT-B19-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P01" & Point.Time %in% c("06:31", "10:12")
                                   ~ "UT-B19-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P01" & Point.Time == "09:39"
                                   ~ "UT-B19-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P02" 
                                   ~ "UT-B19-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P03" 
                                   ~ "UT-B19-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P04"
                                   ~ "UT-B19-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P05"
                                   ~ "UT-B19-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P06"
                                   ~ "UT-B19-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P07"
                                   ~ "UT-B19-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P08"
                                   ~ "UT-B19-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P09"
                                   ~ "UT-B19-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P10"
                                   ~ "UT-B19-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P11" 
                                   ~ "UT-B19-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B19-P12"
                                   ~ "UT-B19-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B19-P13" 
                                   ~ "UT-B19-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-B19-P14"
                                   ~ "UT-B19-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B19-P15"
                                   ~ "UT-B19-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B19-P16"
                                   ~ "UT-B19-P04",
                                   #UT-C19-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P01" & Visit == "V1"
                                   ~ "UT-C19-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P02" & Visit == "V1"
                                   ~ "UT-C19-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P03" & Visit == "V1"
                                   ~ "UT-C19-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P04" & Visit == "V1"
                                   ~ "UT-C19-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P05" & Visit == "V1"
                                   ~ "UT-C19-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P06" & Visit == "V1"
                                   ~ "UT-C19-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P07" & Visit == "V1"
                                   ~ "UT-C19-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P08" & Visit == "V1"
                                   ~ "UT-C19-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P09" & Visit == "V1"
                                   ~ "UT-C19-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P10" & Visit == "V1"
                                   ~ "UT-C19-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P11" & Visit == "V1" 
                                   ~ "UT-C19-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P12" & Visit == "V1"
                                   ~ "UT-C19-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P13" & Visit == "V1" 
                                   ~ "UT-C19-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P14" & Visit == "V1"
                                   ~ "UT-C19-P11",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P15" & Visit == "V1"
                                   ~ "UT-C19-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P16" & Visit == "V1"
                                   ~ "UT-C19-P03",
                                   #UT-C19-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P01" & Visit == "V2"
                                   ~ "UT-C19-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P02" & Visit == "V2"
                                   ~ "UT-C19-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P03" & Visit == "V2"
                                   ~ "UT-C19-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P04" & Visit == "V2"
                                   ~ "UT-C19-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P05" & Visit == "V2"
                                   ~ "UT-C19-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P06" & Visit == "V2"
                                   ~ "UT-C19-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P07" & Visit == "V2"
                                   ~ "UT-C19-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P08" & Visit == "V2"
                                   ~ "UT-C19-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P09" & Visit == "V2"
                                   ~ "UT-C19-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P10" & Visit == "V2"
                                   ~ "UT-C19-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P11" & Visit == "V2" 
                                   ~ "UT-C19-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P12" & Visit == "V2"
                                   ~ "UT-C19-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C19-P13" & Visit == "V2" 
                                   ~ "UT-C19-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P14" & Visit == "V2"
                                   ~ "UT-C19-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P15" & Visit == "V2"
                                   ~ "UT-C19-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-C19-P16" & Visit == "V2"
                                   ~ "UT-C19-P04",
                                   #UT-B17-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P01" 
                                   ~ "UT-B17-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P2"
                                   ~ "UT-B17-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P03"
                                   ~ "UT-B17-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P04"
                                   ~ "UT-B17-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P05"
                                   ~ "UT-B17-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P06"
                                   ~ "UT-B17-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P07"
                                   ~ "UT-B17-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P08" 
                                   ~ "UT-B17-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-B17-P09"
                                   ~ "UT-B17-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B17-P10" 
                                   ~ "UT-B17-P09",
                                   Year == "Y1" &  Full.Point.ID == "UT-B17-P11"
                                   ~ "UT-B17-P07",
                                   #UT-C17-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P03" & Visit == "V1"
                                   ~ "UT-C17-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P04" & Visit == "V1"
                                   ~ "UT-C17-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P05" & Visit == "V1"
                                   ~ "UT-C17-P03",
                                   #UT-C17-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P10" & Visit == "V2" & Point.Time == "09:17"
                                   ~ "UT-C17-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P04" & Visit == "V2"
                                   ~ "UT-C17-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P05" & Visit == "V2"
                                   ~ "UT-C17-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P06" & Visit == "V2"
                                   ~ "UT-C17-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P07" & Visit == "V2"
                                   ~ "UT-C17-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P08" & Visit == "V2"
                                   ~ "UT-C17-P09",
                                   Year == "Y1" &  Full.Point.ID == "UT-C17-P09" & Visit == "V2"
                                   ~ "UT-C17-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C17-P10" & Visit == "V2" & Point.Time == "07:36"
                                   ~ "UT-C17-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-C17-P11" & Visit == "V2"
                                   ~ "UT-C17-P11",
                                   #UT-C01-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P01" 
                                   ~ "IUT-C01-P13",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P02"
                                   ~ "IUT-C01-P14",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P03"
                                   ~ "IUT-C01-P15",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P04"
                                   ~ "IUT-C01-P16",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P05"
                                   ~ "IUT-C01-P12",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P06"
                                   ~ "IUT-C01-P11",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P07"
                                   ~ "IUT-C01-P10",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P08"
                                   ~ "IUT-C01-P09",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P09"
                                   ~ "IUT-C01-P05",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P10"
                                   ~ "IUT-C01-P06",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P11" 
                                   ~ "IUT-C01-P07",
                                   Year == "Y1" &  Full.Point.ID == "IUT-C01-P12"
                                   ~ "IUT-C01-P08",
                                   Year == "Y1" & Full.Point.ID == "IUT-C01-P13"
                                   ~ "IUT-C01-P04",
                                   Year == "Y1" &  Full.Point.ID == "IUT-C01-P14"
                                   ~ "IUT-C01-P03",
                                   Year == "Y1" &  Full.Point.ID == "IUT-C01-P15"
                                   ~ "IUT-C01-P02",
                                   Year == "Y1" &  Full.Point.ID == "IUT-C01-P16"
                                   ~ "IUT-C01-P01",
                                   #UT-B01-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P01" 
                                   ~ "UT-B01-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P02"
                                   ~ "UT-B01-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P03"
                                   ~ "UT-B01-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P04"
                                   ~ "UT-B01-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P05"
                                   ~ "UT-B01-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P06"
                                   ~ "UT-B01-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P07"
                                   ~ "UT-B01-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P08"
                                   ~ "UT-B01-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P09"
                                   ~ "UT-B01-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P10"
                                   ~ "UT-B01-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P11" 
                                   ~ "UT-B01-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B01-P12"
                                   ~ "UT-B01-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B01-P13"
                                   ~ "UT-B01-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-B01-P14"
                                   ~ "UT-B01-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B01-P15"
                                   ~ "UT-B01-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B01-P16"
                                   ~ "UT-B01-P04",
                                   #UT-C02-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P01" & Point.Time %in% c("07:50", "06:44")
                                   ~ "UT-C02-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P01" & Point.Time == "08:34"
                                   ~ "UT-C02-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P02"
                                   ~ "UT-C02-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P03"
                                   ~ "UT-C02-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P04"
                                   ~ "UT-C02-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P05"
                                   ~ "UT-C02-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P06"
                                   ~ "UT-C02-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P07"
                                   ~ "UT-C02-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P08"
                                   ~ "UT-C02-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P09"
                                   ~ "UT-C02-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C02-P10"
                                   ~ "UT-C02-P10",
                                   #UT-B02-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B02-P10" & Point.Time %in% c("08:00", "08:19")
                                   ~ "UT-B02-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B02-P10" & Point.Time == "08:06"
                                   ~ "UT-B02-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B02-P02"
                                   ~ "UT-B02-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B02-P04"
                                   ~ "UT-B02-P02",
                                   #UT-B05-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P01" 
                                   ~ "UT-B05-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P02"
                                   ~ "UT-B05-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P03"
                                   ~ "UT-B05-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P04"
                                   ~ "UT-B05-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P05"
                                   ~ "UT-B05-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P06"
                                   ~ "UT-B05-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P07"
                                   ~ "UT-B05-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P08"
                                   ~ "UT-B05-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P09"
                                   ~ "UT-B05-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P10"
                                   ~ "UT-B05-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P11" 
                                   ~ "UT-B05-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-B05-P12"
                                   ~ "UT-B05-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B05-P13"
                                   ~ "UT-B05-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-B05-P14"
                                   ~ "UT-B05-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B05-P15"
                                   ~ "UT-B05-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B05-P16"
                                   ~ "UT-B05-P01",
                                   #UT-C05-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P01" 
                                   ~ "UT-C05-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P02"
                                   ~ "UT-C05-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P03"
                                   ~ "UT-C05-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P04"
                                   ~ "UT-C05-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P05"
                                   ~ "UT-C05-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P06"
                                   ~ "UT-C05-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P07"
                                   ~ "UT-C05-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P08"
                                   ~ "UT-C05-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P09"
                                   ~ "UT-C05-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P10"
                                   ~ "UT-C05-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P11" 
                                   ~ "UT-C05-P11",
                                   Year == "Y1" &  Full.Point.ID == "UT-C05-P12"
                                   ~ "UT-C05-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C05-P13"
                                   ~ "UT-C05-P16",
                                   Year == "Y1" &  Full.Point.ID == "UT-C05-P14"
                                   ~ "UT-C05-P12",
                                   Year == "Y1" &  Full.Point.ID == "UT-C05-P15"
                                   ~ "UT-C05-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-C05-P16"
                                   ~ "UT-C05-P04",
                                   #UT-B06-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P01" & Visit == "V2"
                                   ~ "UT-B06-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P02" & Visit == "V2"
                                   ~ "UT-B06-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P03" & Visit == "V2"
                                   ~ "UT-B06-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P04" & Visit == "V2"
                                   ~ "UT-B06-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P05" & Visit == "V2"
                                   ~ "UT-B06-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P06" & Visit == "V2"
                                   ~ "UT-B06-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P07" & Visit == "V2"
                                   ~ "UT-B06-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P08" & Visit == "V2"
                                   ~ "UT-B06-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P09" & Visit == "V2"
                                   ~ "UT-B06-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P10" & Visit == "V2"
                                   ~ "UT-B06-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P11" & Visit == "V2"
                                   ~ "UT-B06-P11",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P12" & Visit == "V2"
                                   ~ "UT-B06-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P13" & Visit == "V2"
                                   ~ "UT-B06-P16",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P14" & Visit == "V2"
                                   ~ "UT-B06-P12",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P15" & Visit == "V2"
                                   ~ "UT-B06-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P16" & Visit == "V2"
                                   ~ "UT-B06-P04",
                                   #UT-B06-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P01" & Visit == "V1"
                                   ~ "UT-B06-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P02" & Visit == "V1"
                                   ~ "UT-B06-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P03" & Visit == "V1"
                                   ~ "UT-B06-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P04" & Visit == "V1"
                                   ~ "UT-B06-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P05" & Visit == "V1"
                                   ~ "UT-B06-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P06" & Visit == "V1"
                                   ~ "UT-B06-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P07" & Visit == "V1"
                                   ~ "UT-B06-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P08" & Visit == "V1"
                                   ~ "UT-B06-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P09" & Visit == "V1"
                                   ~ "UT-B06-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P10" & Visit == "V1"
                                   ~ "UT-B06-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P11" & Visit == "V1"
                                   ~ "UT-B06-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P12" & Visit == "V1"
                                   ~ "UT-B06-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B06-P13" & Visit == "V1"
                                   ~ "UT-B06-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P14" & Visit == "V1"
                                   ~ "UT-B06-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P15" & Visit == "V1"
                                   ~ "UT-B06-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B06-P16" & Visit == "V1"
                                   ~ "UT-B06-P04",
                                   #UT-C06-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P01" 
                                   ~ "UT-C06-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P02"
                                   ~ "UT-C06-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P03"
                                   ~ "UT-C06-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P04"
                                   ~ "UT-C06-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P05"
                                   ~ "UT-C06-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P06"
                                   ~ "UT-C06-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P07"
                                   ~ "UT-C06-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P08"
                                   ~ "UT-C06-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P09"
                                   ~ "UT-C06-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P10"
                                   ~ "UT-C06-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P11"
                                   ~ "UT-C06-P11",
                                   Year == "Y1" &  Full.Point.ID == "UT-C06-P12"
                                   ~ "UT-C06-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C06-P13"
                                   ~ "UT-C06-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-C06-P14"
                                   ~ "UT-C06-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-C06-P15"
                                   ~ "UT-C06-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-C06-P16"
                                   ~ "UT-C06-P12",
                                   #UT-B22-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P01" 
                                   ~ "UT-B22-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P02"
                                   ~ "UT-B22-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P03"
                                   ~ "UT-B22-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P04"
                                   ~ "UT-B22-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P05"
                                   ~ "UT-B22-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P06"
                                   ~ "UT-B22-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P07"
                                   ~ "UT-B22-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P08"
                                   ~ "UT-B22-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P09"
                                   ~ "UT-B22-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P10"
                                   ~ "UT-B22-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B22-P11"
                                   ~ "UT-B22-P09",
                                   Year == "Y1" &  Full.Point.ID == "UT-B22-P12"
                                   ~ "UT-B22-P12",
                                   #UT-C22-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P01" & Visit == "V1"
                                   ~ "UT-C22-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P02" & Visit == "V1"
                                   ~ "UT-C22-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P03" & Visit == "V1"
                                   ~ "UT-C22-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P04" & Visit == "V1"
                                   ~ "UT-C22-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P05" & Visit == "V1"
                                   ~ "UT-C22-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P06" & Visit == "V1"
                                   ~ "UT-C22-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P07" & Visit == "V1"
                                   ~ "UT-C22-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P08" & Visit == "V1"
                                   ~ "UT-C22-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P09" & Visit == "V1"
                                   ~ "UT-C22-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P10" & Visit == "V1"
                                   ~ "UT-C22-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P11" & Visit == "V1"
                                   ~ "UT-C22-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-C22-P12" & Visit == "V1"
                                   ~ "UT-C22-P03",
                                   #UT-C22-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P01" & Visit == "V2"
                                   ~ "UT-C22-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P03" & Visit == "V2"
                                   ~ "UT-C22-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P04" & Visit == "V2"
                                   ~ "UT-C22-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P05" & Visit == "V2"
                                   ~ "UT-C22-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P06" & Visit == "V2"
                                   ~ "UT-C22-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P07" & Visit == "V2"
                                   ~ "UT-C22-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P08" & Visit == "V2"
                                   ~ "UT-C22-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P09" & Visit == "V2"
                                   ~ "UT-C22-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P10" & Visit == "V2"
                                   ~ "UT-C22-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C22-P11" & Visit == "V2"
                                   ~ "UT-C22-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-C22-P12" & Visit == "V2"
                                   ~ "UT-C22-P12",
                                   #UT-C27-Y1-V2&V2
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P01" & Point.Time %in% c("06:32", "09:54")
                                   ~ "UT-C27-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P02" 
                                   ~ "UT-C27-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P03"
                                   ~ "UT-C27-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P04" 
                                   ~ "UT-C27-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P05" 
                                   ~ "UT-C27-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P06" 
                                   ~ "UT-C27-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P07" 
                                   ~ "UT-C27-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P08" 
                                   ~ "UT-C27-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P09" 
                                   ~ "UT-C27-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P01"  & Point.Time == "08:26"
                                   ~ "UT-C27-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P10"  
                                   ~ "UT-C27-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P11" 
                                   ~ "UT-C27-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-C27-P12" 
                                   ~ "UT-C27-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C27-P13" 
                                   ~ "UT-C27-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-C27-P14" 
                                   ~ "UT-C27-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-C27-P15" 
                                   ~ "UT-C27-P05",
                                   Year == "Y1" &  Full.Point.ID == "UT-C27-P16" 
                                   ~ "UT-C27-P09",
                                   #UT-C16-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P01"
                                   ~ "UT-C16-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P02" 
                                   ~ "UT-C16-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P03"
                                   ~ "UT-C16-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P04" 
                                   ~ "UT-C16-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P05" 
                                   ~ "UT-C16-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P06" 
                                   ~ "UT-C16-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P07" 
                                   ~ "UT-C16-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P08" 
                                   ~ "UT-C16-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P09" 
                                   ~ "UT-C16-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P10"  
                                   ~ "UT-C16-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P11" 
                                   ~ "UT-C16-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-C16-P12" 
                                   ~ "UT-C16-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C16-P13" 
                                   ~ "UT-C16-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-C16-P14" 
                                   ~ "UT-C16-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-C16-P15" 
                                   ~ "UT-C16-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-C16-P16" 
                                   ~ "UT-C16-P02",
                                   #UT-B30-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P01"
                                   ~ "UT-B30-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P02" 
                                   ~ "UT-B30-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P03"
                                   ~ "UT-B30-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P04" 
                                   ~ "UT-B30-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P05" 
                                   ~ "UT-B30-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P06" 
                                   ~ "UT-B30-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P07" 
                                   ~ "UT-B30-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P08" 
                                   ~ "UT-B30-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P09" 
                                   ~ "UT-B30-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P10"  
                                   ~ "UT-B30-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P11" 
                                   ~ "UT-B30-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P12" 
                                   ~ "UT-B30-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P13" 
                                   ~ "UT-B30-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P14" 
                                   ~ "UT-B30-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P15" 
                                   ~ "UT-B30-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P16" 
                                   ~ "UT-B30-P04",
                                   #UT-B30-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P01" & Visit == "V1"
                                   ~ "UT-B30-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P02" & Visit == "V1"
                                   ~ "UT-B30-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P03" & Visit == "V1"
                                   ~ "UT-B30-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P04" & Visit == "V1"
                                   ~ "UT-B30-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P05" & Visit == "V1"
                                   ~ "UT-B30-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P06" & Visit == "V1"
                                   ~ "UT-B30-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P07" & Visit == "V1"
                                   ~ "UT-B30-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P08" & Visit == "V1"
                                   ~ "UT-B30-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P09" & Visit == "V1"
                                   ~ "UT-B30-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P10" & Visit == "V1"
                                   ~ "UT-B30-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P11" & Visit == "V1"
                                   ~ "UT-B30-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P12" & Visit == "V1"
                                   ~ "UT-B30-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P13" & Visit == "V1"
                                   ~ "UT-B30-P13",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P14" & Visit == "V1"
                                   ~ "UT-B30-P14",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P15" & Visit == "V1"
                                   ~ "UT-B30-P15",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P16" & Visit == "V2"
                                   ~ "UT-B30-P16",
                                   #UT-B30-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P01" & Visit == "V2"
                                   ~ "UT-B30-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P02" & Visit == "V2"
                                   ~ "UT-B30-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P03" & Visit == "V2"
                                   ~ "UT-B30-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P04" & Visit == "V2"
                                   ~ "UT-B30-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P05" & Visit == "V2"
                                   ~ "UT-B30-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P06" & Visit == "V2"
                                   ~ "UT-B30-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P07" & Visit == "V2"
                                   ~ "UT-B30-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P08" & Visit == "V2"
                                   ~ "UT-B30-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P09" & Visit == "V2"
                                   ~ "UT-B30-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P10" & Visit == "V2"
                                   ~ "UT-B30-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P11" & Visit == "V2"
                                   ~ "UT-B30-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P12" & Visit == "V2"
                                   ~ "UT-B30-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B30-P13" & Visit == "V2"
                                   ~ "UT-B30-P13",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P14" & Visit == "V2"
                                   ~ "UT-B30-P09",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P15" & Visit == "V2"
                                   ~ "UT-B30-P05",
                                   Year == "Y1" &  Full.Point.ID == "UT-B30-P16" & Visit == "V2"
                                   ~ "UT-B30-P01",
                                   #UT-B16-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P01"
                                   ~ "UT-B16-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P02" 
                                   ~ "UT-B16-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P03"
                                   ~ "UT-B16-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P04" 
                                   ~ "UT-B16-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P05" 
                                   ~ "UT-B16-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P06" 
                                   ~ "UT-B16-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P07" 
                                   ~ "UT-B16-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P08" 
                                   ~ "UT-B16-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P09" 
                                   ~ "UT-B16-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P10"  
                                   ~ "UT-B16-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P11" 
                                   ~ "UT-B16-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-B16-P12" 
                                   ~ "UT-B16-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B16-P13" 
                                   ~ "UT-B16-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-B16-P14" 
                                   ~ "UT-B16-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-B16-P15" 
                                   ~ "UT-B16-P12",
                                   Year == "Y1" &  Full.Point.ID == "UT-B16-P16" 
                                   ~ "UT-B16-P16",
                                   #UT-B25-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P01"
                                   ~ "UT-B25-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P02" 
                                   ~ "UT-B25-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P03"
                                   ~ "UT-B25-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P04" 
                                   ~ "UT-B25-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P05" 
                                   ~ "UT-B25-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P06" 
                                   ~ "UT-B25-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P07" 
                                   ~ "UT-B25-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P08" 
                                   ~ "UT-B25-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P09" 
                                   ~ "UT-B25-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P10"  
                                   ~ "UT-B25-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P11" 
                                   ~ "UT-B25-P10",
                                   Year == "Y1" &  Full.Point.ID == "UT-B25-P12" 
                                   ~ "UT-B25-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P13" 
                                   ~ "UT-B25-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-B25-P14" 
                                   ~ "UT-B25-P03",
                                   #UT-B15-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P01"
                                   ~ "UT-B15-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P02" 
                                   ~ "UT-B15-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P03"
                                   ~ "UT-B15-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P04" 
                                   ~ "UT-B15-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P05" 
                                   ~ "UT-B15-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P06" 
                                   ~ "UT-B15-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P07" 
                                   ~ "UT-B15-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P08" 
                                   ~ "UT-B15-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P09" 
                                   ~ "UT-B15-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P10"  
                                   ~ "UT-B15-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P11" 
                                   ~ "UT-B15-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-B15-P12" 
                                   ~ "UT-B15-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B15-P13" 
                                   ~ "UT-B15-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-B15-P14" 
                                   ~ "UT-B15-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B15-P15" 
                                   ~ "UT-B15-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B15-P16" 
                                   ~ "UT-B15-P01",
                                   #UT-C15-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P01"
                                   ~ "UT-C15-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P02" 
                                   ~ "UT-C15-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P03"
                                   ~ "UT-C15-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P04" 
                                   ~ "UT-C15-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P05" 
                                   ~ "UT-C15-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P06" 
                                   ~ "UT-C15-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P07" 
                                   ~ "UT-C15-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P08" 
                                   ~ "UT-C15-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P09" & Point.Time %in% c("09:08", "08:30")
                                   ~ "UT-C15-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P09" & Point.Time == "08:56"
                                   ~ "UT-C15-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P10"  
                                   ~ "UT-C15-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P11" 
                                   ~ "UT-C15-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-C15-P12" 
                                   ~ "UT-C15-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C15-P13" 
                                   ~ "UT-C15-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-C15-P14" 
                                   ~ "UT-C15-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-C15-P15" 
                                   ~ "UT-C15-P12",
                                   Year == "Y1" &  Full.Point.ID == "UT-C15-P16" 
                                   ~ "UT-C15-P16",
                                   #UT-B25-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P01"
                                   ~ "UT-B25-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P02" 
                                   ~ "UT-B25-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P03"
                                   ~ "UT-B25-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P04" 
                                   ~ "UT-B25-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P05" 
                                   ~ "UT-B25-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P06" 
                                   ~ "UT-B25-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P07" 
                                   ~ "UT-B25-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P08" 
                                   ~ "UT-B25-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P09" 
                                   ~ "UT-B25-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P10"  
                                   ~ "UT-B25-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B25-P11" 
                                   ~ "UT-B25-P14",
                                   Year == "Y1" &  Full.Point.ID == "UT-B25-P12" 
                                   ~ "UT-B25-P10",
                                   #UT-C30-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P01"
                                   ~ "UT-C30-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P02" 
                                   ~ "UT-C30-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P03"
                                   ~ "UT-C30-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P04" 
                                   ~ "UT-C30-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P05" 
                                   ~ "UT-C30-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P06" 
                                   ~ "UT-C30-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P07" 
                                   ~ "UT-C30-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P08" 
                                   ~ "UT-C30-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P09" 
                                   ~ "UT-C30-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P10"  
                                   ~ "UT-C30-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P11" 
                                   ~ "UT-C30-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-C30-P12" 
                                   ~ "UT-C30-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C30-P13" 
                                   ~ "UT-C30-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-C30-P14" 
                                   ~ "UT-C30-P05",
                                   Year == "Y1" &  Full.Point.ID == "UT-C30-P15" 
                                   ~ "UT-C30-P09",
                                   Year == "Y1" &  Full.Point.ID == "UT-C30-P16" 
                                   ~ "UT-C30-P13",
                                   #UT-B24-Y1-V1
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P01" & Visit == "V1"
                                   ~ "UT-B24-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P02" & Visit == "V1"
                                   ~ "UT-B24-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P03" & Visit == "V1"
                                   ~ "UT-B24-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P04" & Visit == "V1"
                                   ~ "UT-B24-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P05" & Visit == "V1"
                                   ~ "UT-B24-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P06" & Visit == "V1"
                                   ~ "UT-B24-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P07" & Visit == "V1"
                                   ~ "UT-B24-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P08" & Visit == "V1"
                                   ~ "UT-B24-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P09" & Visit == "V1"
                                   ~ "UT-B24-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P10" & Visit == "V1"
                                   ~ "UT-B24-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P11" & Visit == "V1"
                                   ~ "UT-B24-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P12" & Visit == "V1"
                                   ~ "UT-B24-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P13" & Visit == "V1"
                                   ~ "UT-B24-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P14" & Visit == "V1"
                                   ~ "UT-B24-P02",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P15" & Visit == "V1"
                                   ~ "UT-B24-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P16" & Visit == "V2"
                                   ~ "UT-B24-P04",
                                   #UT-B24-Y1-V2
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P01" & Visit == "V2"
                                   ~ "UT-B24-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P02" & Visit == "V2"
                                   ~ "UT-B24-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P03" & Visit == "V2"
                                   ~ "UT-B24-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P04" & Visit == "V2"
                                   ~ "UT-B24-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P05" & Visit == "V2"
                                   ~ "UT-B24-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P06" & Visit == "V2"
                                   ~ "UT-B24-P07",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P07" & Visit == "V2"
                                   ~ "UT-B24-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P08" & Visit == "V2"
                                   ~ "UT-B24-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P09" & Visit == "V2"
                                   ~ "UT-B24-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P10" & Visit == "V2"
                                   ~ "UT-B24-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P11" & Visit == "V2"
                                   ~ "UT-B24-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P12" & Visit == "V2"
                                   ~ "UT-B24-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B24-P13" & Visit == "V2"
                                   ~ "UT-B24-P01",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P14" & Visit == "V2"
                                   ~ "UT-B24-P05",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P15" & Visit == "V2"
                                   ~ "UT-B24-P09",
                                   Year == "Y1" &  Full.Point.ID == "UT-B24-P16" & Visit == "V2"
                                   ~ "UT-B24-P13",
                                   #UT-B08-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P01"
                                   ~ "UT-B08-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P02" 
                                   ~ "UT-B08-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P03"
                                   ~ "UT-B08-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P04" 
                                   ~ "UT-B08-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P05" 
                                   ~ "UT-B08-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P06" 
                                   ~ "UT-B08-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P07" 
                                   ~ "UT-B08-P16",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P08" 
                                   ~ "UT-B08-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P09" 
                                   ~ "UT-B08-P08",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P10"  
                                   ~ "UT-B08-P04",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P11" 
                                   ~ "UT-B08-P03",
                                   Year == "Y1" &  Full.Point.ID == "UT-B08-P12" 
                                   ~ "UT-B08-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-B08-P13" 
                                   ~ "UT-B08-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-B08-P14" 
                                   ~ "UT-B08-P11",
                                   Year == "Y1" &  Full.Point.ID == "UT-B08-P15" 
                                   ~ "UT-B08-P06",
                                   Year == "Y1" &  Full.Point.ID == "UT-B08-P16" 
                                   ~ "UT-B08-P10",
                                   #UT-C08-Y1-V1&V2
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P01"
                                   ~ "UT-C08-P13",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P02" 
                                   ~ "UT-C08-P09",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P03"
                                   ~ "UT-C08-P05",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P04" 
                                   ~ "UT-C08-P01",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P05" 
                                   ~ "UT-C08-P02",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P06" 
                                   ~ "UT-C08-P06",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P07" 
                                   ~ "UT-C08-P10",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P08" 
                                   ~ "UT-C08-P14",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P09" 
                                   ~ "UT-C08-P15",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P10"  
                                   ~ "UT-C08-P11",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P11" 
                                   ~ "UT-C08-P07",
                                   Year == "Y1" &  Full.Point.ID == "UT-C08-P12" 
                                   ~ "UT-C08-P03",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P13" 
                                   ~ "UT-C08-P04",
                                   Year == "Y1" &  Full.Point.ID == "UT-C08-P14" 
                                   ~ "UT-C08-P08",
                                   Year == "Y1" &  Full.Point.ID == "UT-C08-P15" 
                                   ~ "UT-C08-P12",
                                   Year == "Y1" & Full.Point.ID == "UT-C08-P16" 
                                   ~ "UT-C08-P16",
                                   ###### Fix a few Y2's #######################
                                   Year == "Y2" & Full.Point.ID == "ID-B13-P09" & Point.Time == "06:23" 
                                   ~ "ID-B13-P16",
                                   Year == "Y2" & Full.Point.ID == "ID-B13-P16" & Visit == "V1" 
                                   ~ "ID-B13-P15",
                                   Year == "Y2" & Full.Point.ID == "ID-B13-P15" & Visit == "V1" 
                                   ~ "ID-B13-P14",
                                   Year == "Y2" & Full.Point.ID == "ID-B13-P13" & Visit == "V1" 
                                   ~ "ID-B13-P13",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P10" & Visit == "V1" 
                                   ~ "ID-B12-P09",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P11" & Visit == "V1" 
                                   ~ "ID-B12-P10",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P12" & Visit == "V1" 
                                   ~ "ID-B12-P11",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P13" & Visit == "V1" 
                                   ~ "ID-B12-P12",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P14" & Visit == "V1" 
                                   ~ "ID-B12-P13",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P15" & Visit == "V1" 
                                   ~ "ID-B12-P14",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P16" & Visit == "V1" 
                                   ~ "ID-B12-P15",
                                   Year == "Y2" & Full.Point.ID == "ID-B12-P09" & Visit == "V1" 
                                   ~ "ID-B12-P16",
                                   Year == "Y2" & Full.Point.ID == "UT-B16-P13" & Visit == "V2" 
                                   ~ "UT-B16-P14",
                                   Year == "Y2" & Full.Point.ID == "UT-B16-P14" & Visit == "V2" 
                                   ~ "UT-B16-P13",
                                   #UT-B25
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P02" & Visit == "V1" 
                                   ~ "UT-B25-P15",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P03" & Visit == "V1" 
                                   ~ "UT-B25-P02",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P04" & Visit == "V1" 
                                   ~ "UT-B25-P03",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P05" & Visit == "V1" 
                                   ~ "UT-B25-P04",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P06" & Visit == "V1" 
                                   ~ "UT-B25-P05",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P07" & Visit == "V1" 
                                   ~ "UT-B25-P06",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P08" & Visit == "V1" 
                                   ~ "UT-B25-P07",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P09" & Visit == "V1" 
                                   ~ "UT-B25-P08",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P10" & Visit == "V1" 
                                   ~ "UT-B25-P09",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P11" & Visit == "V1" 
                                   ~ "UT-B25-P10",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P12" & Visit == "V1" 
                                   ~ "UT-B25-P11",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P13" & Visit == "V1" 
                                   ~ "UT-B25-P12",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P14" & Visit == "V1" 
                                   ~ "UT-B25-P13",
                                   Year == "Y2" & Full.Point.ID == "UT-B25-P15" & Visit == "V1" 
                                   ~ "UT-B25-P14",
                                   #UT-C25
                                   Year == "Y2" & Full.Point.ID == "UT-C25-P11" & Point.Time == "08:25"
                                   ~ "UT-C25-P12",
                                   Year == "Y2" & Full.Point.ID == "UT-C25-P12" & Visit == "V1"
                                   ~ "UT-C25-P13",
                                   Year == "Y2" & Full.Point.ID == "UT-C25-P13" & Visit == "V1"
                                   ~ "UT-C25-P14",
                                   Year == "Y2" & Full.Point.ID == "UT-C25-P13" & Point.Time == "09:13"
                                   ~ "UT-C25-P10",
                                   #Need to continue the Goose creek plots
                                   TRUE ~  Full.Point.ID))


#View the UT-B25 points
glimpse(sobs_temp)
sobs_temp %>% 
  filter(Route.ID == "UT-B25") %>% 
  dplyr::count(Full.Point.ID)

#Fix UT-B25. It has one extra point (now P15)
sobs_temp <- sobs_temp %>% 
  filter(Full.Point.ID != "UT-B25-P15") %>% 
  #IDK what to do with the V3's so I'm dropping them
  filter(Visit %in% c("V1", "V2"))

#View how many of each point we have after the fix
sobs_temp %>% 
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  distinct(Full.Point.ID, Visit.ID) %>% 
  count(Full.Point.ID, Visit.ID) %>% 
  arrange(Full.Point.ID) %>% 
  pivot_wider(names_from = Visit.ID, values_from = n) %>% 
  print(n = Inf)

#Now in a more condenced format
sobs_temp %>% 
  distinct(Full.Point.ID, Year, Visit) %>% 
  count(Full.Point.ID) %>% 
  arrange(Full.Point.ID) %>% 
  print(n = Inf)

#View the dataset to make sure everything looks good
glimpse(sobs_temp)


#After learning more about fire history in this area I have realized that 
#UT-C24 and UT-C25 burned in 1999 and ID-B03 did not burn

#Redefine the incorrect route types
sobs<- sobs_temp %>% 
  mutate(Route.Type = case_when(Route.ID == "UT-C24" ~ "B",
                                Route.ID == "UT-C25" ~ "B",
                                Route.ID == "UT-C30" ~ "R",
                                Route.ID == "ID-B03" ~ "R",
                                TRUE ~ Route.Type))

#Many of the counts do not have NOBI for minutes where no birds were detected
#Add in NOBI Observations ---------------------------------------------------
#View some example NOBI observations
sobs %>% 
  filter(Species == "NOBI") %>% 
  glimpse()

#make an object of all NOBI observations
nobi <- sobs %>% 
  expand(nesting(Route.ID, Route.Type, Full.Point.ID, Observer.ID, #Nested variables at the 
                 Year, Visit, Date, Ord.Date, Temp.Start, Wind.Start,
                 Sky.Start, Temp.End, Wind.End, Sky.End, Notes),  #point or route level
         Minute) %>% #The only non nested variable
  mutate(Species = "NOBI")
#View
glimpse(nobi)

#Join the two and delete duplicates
sobs_nobi <- sobs %>%  
  bind_rows(nobi) %>% 
  mutate(Sort = case_when(Species == "NOBI" ~ 2,
                          TRUE ~ 1)) %>% 
  arrange(Date, Full.Point.ID, Minute, Sort) %>% 
  mutate(Remove = NA) %>% 
  filter(!is.na(Minute))

#Viw the combined results and compare to the original
glimpse(sobs_nobi)
base::print(sobs_nobi, n = 400)

#Don't remove the first row
sobs_nobi$Remove[1] <- F 
#For loop to remove duplicates and NOBI's on rows that 
for(i in 2:nrow(sobs_nobi)) {
  sobs_nobi$Remove[i] <- ifelse(sobs_nobi$Species[i] == "NOBI" &
                                  sobs_nobi$Minute[i] == sobs_nobi$Minute[i - 1],
                                T, F)
}

#And view
glimpse(sobs_nobi)
sobs_nobi %>% 
  dplyr::select(Full.Point.ID, Species, Distance, Minute, Remove, Sort, Date,
                Ord.Date, Temp.Start, Wind.Start, Sky.Start, 
                Temp.End, Wind.End, Sky.End, Notes) %>% 
  base::print(n = 200)

#remove the tagged rows
sobs <- sobs_nobi %>% 
  filter(Remove == F) %>% 
  dplyr::select(- Remove, - Sort)  

#view
glimpse(sobs)

#Add in point coordinates --------------------------------------------
#Convert coordinates to numeric

#read in point corrdinates
point_cords <- tibble(read.csv("Data\\Inputs\\Point_Cords_Raw.csv"))
#View point ID's
point_cords %>% 
  distinct(Point_ID)

#Standardize point numbers
point_cords <- point_cords %>% 
  mutate(Point.ID = as.character(case_when(
    Point_ID == "1" ~ "01",
    Point_ID == "2" ~ "02",
    Point_ID == "3" ~ "03",
    Point_ID == "4" ~ "04",
    Point_ID == "5" ~ "05",
    Point_ID == "6" ~ "06",
    Point_ID == "7" ~ "07",
    Point_ID == "8" ~ "08",
    Point_ID == "9" ~ "09",
    Point_ID == "10" ~ "10",
    Point_ID == "11" ~ "11",
    Point_ID == "12" ~ "12",
    Point_ID == "13" ~ "13",
    Point_ID == "14" ~ "14",
    Point_ID == "15" ~ "15",
    Point_ID == "16" ~ "16"
  )))
#View the cleaned points
point_cords %>% 
  distinct(Point.ID)

#Replace "_" with "-" in route name
point_cords <- point_cords %>% 
  mutate(Route.ID = str_replace_all(RouteName, "_", "-")) %>% 
  arrange(Route.ID)

#View the cleaned Routes and how many points they have
point_cords %>% 
  dplyr::count(Route.ID) %>% 
  base::print(n = Inf)

#Add column for full point id
#Make a new column that combines point id and route id
point_cords <- point_cords %>% 
  mutate(Full.Point.ID = paste0(Route.ID, 
                                "-", "P", 
                                Point.ID, 
                                collapse = NULL)) %>% 
  dplyr::rename(UTM.X = "UTM_X", UTM.Y = "UTM_Y",
         Geo.X = "Geo_X", Geo.Y = "Geo_Y") %>% 
  dplyr::select(Full.Point.ID, Point.ID, Route.ID, UTM.X, UTM.Y, Geo.X, Geo.Y) 

#View point coordinates  
glimpse(point_cords) 
point_cords %>% 
  dplyr::count(Full.Point.ID) %>% 
  base::print(n = Inf)

#Remove route type
point_cords <- point_cords %>% 
  dplyr::select(-Point.ID)

#Compare the points present in each dataset
glimpse(point_cords) 

#points
coords_tbl <- point_cords %>% 
  dplyr::count(Route.ID, Full.Point.ID) %>% 
  dplyr::count(Route.ID)

#observations
surveys_tbl <- sobs %>% 
  dplyr::count(Route.ID, Full.Point.ID) %>% 
  dplyr::count(Route.ID)

#compare the two
full_join(surveys_tbl, coords_tbl, by = "Route.ID") %>% 
  filter(n.x != n.y) %>% 
  base::print(n = Inf)
#Great! None match

#Select only the columns that I will need 
point_cords <- point_cords %>% 
  dplyr::select(Full.Point.ID, UTM.X, UTM.Y, Geo.X, Geo.Y)

#Join coordinates to point count data
sobs <- sobs %>% 
  as_tibble() %>% 
  left_join(point_cords, by = "Full.Point.ID")

#View updated point count data
glimpse(sobs)
sobs %>% 
  distinct(Full.Point.ID, UTM.X, UTM.Y)

#A lot of the temps were recorded in Fahrenheit instead of Celsius -------------------------

#View all start temps
sobs %>% 
  distinct(Full.Point.ID, Visit, Year, Temp.Start) %>% 
  arrange(Temp.Start) %>% 
  group_by(Temp.Start) %>% 
  reframe(Temp.Start, Count = n()) %>% 
  distinct(Temp.Start, Count) %>% 
  ggplot(aes(x = Temp.Start, y = Count)) +
  geom_col() +
  geom_text(aes(label = Temp.Start))

#View all the end temps
sobs %>% 
  distinct(Full.Point.ID, Visit, Year, Temp.End) %>% 
  arrange(Temp.End) %>% 
  group_by(Temp.End) %>% 
  reframe(Temp.End, Count = n()) %>% 
  distinct(Temp.End, Count) %>% 
  ggplot(aes(x = Temp.End, y = Count)) +
  geom_col() +
  geom_text(aes(label = Temp.End))

#View the NA's
sobs %>% 
  select(Full.Point.ID, Year, Visit, Observer.ID, Temp.Start, Temp.End) %>% 
  filter(is.na(Temp.Start) | is.na(Temp.End)) %>% 
  print(n = 250)

#I'll use 20 for the start temp cutoff and 40 for the end temp cutoff
sobs <- sobs %>% 
  mutate(Temp.Start = case_when(Temp.Start > 20 & Temp.Start <= 100 ~ floor((Temp.Start -32) / 1.8),
                                Temp.Start > 100 ~ NA,
                                TRUE ~ Temp.Start)) %>% 
  mutate(Temp.End = case_when(Temp.End > 35  ~ floor((Temp.End -32) / 1.8),
                                TRUE ~ Temp.End))

#View all start temps after the fix
sobs %>% 
  distinct(Full.Point.ID, Visit, Year, Temp.Start) %>% 
  arrange(Temp.Start) %>% 
  group_by(Temp.Start) %>% 
  reframe(Temp.Start, Count = n()) %>% 
  distinct(Temp.Start, Count) %>% 
  ggplot(aes(x = Temp.Start, y = Count)) +
  geom_col() +
  geom_text(aes(label = Temp.Start))

#View all the end temps after the fix
sobs %>% 
  distinct(Full.Point.ID, Visit, Year, Temp.End) %>% 
  arrange(Temp.End) %>% 
  group_by(Temp.End) %>% 
  reframe(Temp.End, Count = n()) %>% 
  distinct(Temp.End, Count) %>% 
  ggplot(aes(x = Temp.End, y = Count)) +
  geom_col() +
  geom_text(aes(label = Temp.End))
#Looks good :)

#calculate minutes after sunrise ------------------------------------------------

#Install bioRAD package for working with sunrises
#install.packages("bioRad")
library(bioRad)

#claculate minutes after sunrise (MAS)
sobs <- sobs %>% 
  mutate(Loc.SunR = sunrise(date = Date, #Date comes from the date column and is already read a a date
                            lon = Geo.X, #The decimal degrees longitude
                            lat = Geo.Y, #the decimal degrees latitude
                            elev = -0.268, #default sun elevation
                            tz = "America/Denver", #our time zone (MDT)
                            force_tz = TRUE)) %>% #forcing the function to use MDT
  mutate(Loc.SunR = as.character(Loc.SunR)) %>% #Make it a character so Lubridate knows what to read
  mutate(Loc.SunR = str_sub(Loc.SunR, start = 12, end = 19)) %>% #pull out the time section
  mutate(Loc.SunR = lubridate::hms(Loc.SunR)) %>%  # read sunrise time into lubridate
  mutate(Time.lbr = lubridate::hm(Point.Time)) %>% #read start time into lubridate
  mutate(MAS = as.duration(Time.lbr - Loc.SunR + 1800)) %>%  # find the time in seconds from 30 mins before sunrise to the start of the survey
  mutate(MAS = as.numeric(MAS)) %>%  #switch mas to a number 
  mutate(MAS = (MAS / 60)) %>%  #switch from seconds to munutes
  mutate(MAS = as.integer(MAS)) %>% #Round to the nearest whole number
  drop_na(MAS)

#View the data after the calculation
glimpse(sobs)
sobs %>% 
  dplyr::select(Full.Point.ID, Time.lbr, Loc.SunR, MAS) %>% 
  head(n = 6)

#view the NA's for MAS
sobs %>% 
  filter(is.na(MAS)) %>% 
  dplyr::select(Species, Full.Point.ID, Time.lbr, Loc.SunR, MAS)
#there are none

#view the points that are more than 5.5 hours after sunrise
sobs %>% 
  dplyr::select(Full.Point.ID, Date, Point.Time, Time.lbr, Loc.SunR, MAS) %>% 
  filter(MAS > 330) %>% 
  base::print(n = 50)

#View the points that were more than 30 minutes before sunrise'
sobs %>% 
  dplyr::select(Full.Point.ID, Observer.ID, Visit, Date, Time.lbr, Loc.SunR, MAS) %>% 
  filter(MAS < 0) %>% 
  base::print(n = 50)

#Remove excess columns and reorder everything
sobs <- sobs %>% 
   dplyr::select(Species, Distance, Minute, How.Detected, 
          # Song.Also, Group.Size, Sex, 
          Visual.ID,
          Route.ID, Route.Type, Full.Point.ID, 
          Observer.ID, Year, Visit, Date, Ord.Date,
          Point.Time, MAS,
          Temp.Start, Wind.Start, Sky.Start, 
          Temp.End, Wind.End, Sky.End,
          Notes, UTM.X, UTM.Y, Geo.X, Geo.Y) %>% 
  arrange(by = Date)
glimpse(sobs)

#Last check ----------------------------------------------------------
#View one last time
glimpse(sobs)
base::print(sobs, n = 200)

#Save the cleaned data as a csv
write.csv(sobs, "Data\\Outputs\\sobs_data.csv")

# Notes and next steps ------------------------
#I might need to link external temp data
