#Top of the code block ----
#Basic Single Species detection functions

#clear objects
rm(list = ls())

#Load packages
#Load Required Packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(Distance)
library(dsm)

#Load in Data
sobs <- read.csv("Data\\Outputs\\sobs_data.csv") %>% 
  dplyr::select(-X) #Remove the column that excel generated

#view our data
glimpse(sobs)                      

#Add in an Effort Column to the count data
sobs <- sobs %>% 
mutate(Effort = case_when(Year == 'Y1' & Visit == 'V1' ~ 1,
                          Year == 'Y1' & Visit == 'V2' ~ 1,
                          Year == 'Y2' & Visit == 'V1' ~ 1,
                          Year == 'Y2' & Visit == 'V2' ~ 1,
                          TRUE ~ NA))
#View the data again  
glimpse(sobs)
sobs %>% 
  filter(is.na(Effort))

#Remove those surveys
sobs <- sobs %>% 
  filter(!is.na(Effort))

#Change the orgional tibble so distance likes it
sobs <- sobs %>% 
  #rename columns so distance can understand them
  rename(distance = "Distance") %>% 
  #Each point within each year is a "Sample"
  mutate(Sample.Label = paste(Full.Point.ID, Year, sep = "-")) %>% 
  #Each route within each year is a "region"
  mutate(Region.Label = paste(Route.ID, Year, sep = "-")) %>% 
  #Make Distance Continuous
  mutate(distance = as.numeric(distance))
#...and view
glimpse(sobs)

#View and combine some of the detection level covariates -----------------------------
#Start Sky
sobs %>% count(Sky.Start)
sobs <- sobs %>% 
  mutate(Sky.Start = case_when(Sky.Start %in% c('Cloudy',
                                                'Drizzle',
                                                'Fog or Smoke') ~ 'Poor',
                              TRUE ~ Sky.Start))
#...and view 
count(sobs, Sky.Start)

#Start Wind
sobs %>% count(Wind.Start)
sobs <- sobs %>% 
  mutate(Wind.Start = case_when(Wind.Start %in% c('8-12 mph', '13-18 mph') ~ '> 7 mph',
                               TRUE ~ Wind.Start))
#...and view
count(sobs, Wind.Start)

#Transform the covariates we need to factors
sobs <- sobs %>%
  mutate_at(c("Minute", "Year", "Sky.Start", "Wind.Start", "Route.Type", 
              "Observer.ID", "Visit", "How.Detected", "Sex", 
              "Visual.ID"), 
            factor)

#Change Some Other covariates to scale
sobs <- sobs %>%
  mutate_at(c("MAS", "Ord.Date", "Temp.Start"), scale)

#and make sure all the rows are correct
glimpse(sobs)

#Build a sample table --------------------------------------------------------

#Calculate effort
sample_table <- sobs %>% 
  expand(nesting(Region.Label, Sample.Label, Full.Point.ID, Visit)) %>% 
  group_by(Sample.Label) %>% 
  reframe(Region.Label, Sample.Label, Full.Point.ID, Effort = n()) %>% 
  distinct(Region.Label, Sample.Label, Full.Point.ID, Effort) %>% 
  relocate(Region.Label, Full.Point.ID, Sample.Label, Effort)
#And view
glimpse(sample_table)

#Add site level covariates
point_covs <- read.csv("Data\\Outputs\\point_summaries.csv") %>% 
  select(-X, -Route.ID) 
#...and view
glimpse(point_covs)

#Join to sample Table
sample_table <- left_join(sample_table, point_covs, by = "Full.Point.ID") %>% 
  select(-Full.Point.ID)
#View
glimpse(sample_table)

 #Region Table ---------------------------------------------------------------------
#Define a storage object to look at the data for a single species of interest (SOI)
SOI <- "BRSP"

#arrange data by distance for our SOI
soi_obs <- sobs %>%
  filter(Species == SOI) %>%
  arrange(distance)
#and view
glimpse(soi_obs)

#Storage object for how many of this species we saw
soi_count <- nrow(soi_obs)
#View how many of that species we saw
print(soi_count)

#find the observations that are right at the top ten percent of distances
SOI_trunc_dist <- soi_obs$distance[(0.9 * soi_count)]
print(SOI_trunc_dist)

#Build an object for the area of a single point
point_area <- (0.000001 * pi * (SOI_trunc_dist ^ 2)) # Area of a circle in km^2 
print(point_area)
#with a radius equal to the approximate SOI cutoff distance

#Show the number of points by route
n_points <- sobs %>% 
  expand(nesting(Region.Label, Full.Point.ID)) %>%
  group_by(Region.Label) %>% 
  reframe(Region.Label, N.Points = n()) %>% 
  distinct(Region.Label, N.Points)
#View
glimpse(n_points)

#Number of visits that each route had
route_visits <- sobs %>% 
  expand(nesting(Region.Label, Year, Visit)) %>%
  group_by(Region.Label) %>% 
  reframe(Region.Label, N.Visits = n()) %>% 
  distinct(Region.Label, N.Visits)
#And view
glimpse(route_visits)

#Join number of points and visit number together to build an object for Region label
region_table <- left_join(n_points, route_visits, by = 'Region.Label')
#And view
glimpse(region_table)

#Calculate point area and remove excess columns
region_table <- region_table %>% 
  mutate(Area = N.Points * N.Visits * point_area) %>% 
  dplyr::select(Region.Label, Area)

#view our finalized region table
glimpse(region_table)

#Build an object unit for conversion
conv <- convert_units("Meter", NULL, "Square kilometer")

#Final data exploration --------------------------------------------
print(SOI) #Check which species we're looking at

#Table of which Plots have SOI
sobs %>%
  filter(Species == SOI) %>% 
  count(Region.Label, Year, Visit) %>% 
  select(Region.Label, Visit, n) %>% 
  pivot_wider(names_from = Visit, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  print(n = 60)

#A storage object for the plot title
SOI_count_chart_title <- paste(SOI, "Observed by Distance", sep = " ")

#Plot of the distribution of observed distances
sobs %>%
  filter(Species == SOI) %>%
  ggplot(aes(x = distance)) +
  geom_histogram() + 
  facet_wrap(~ Year + Route.Type) +
  labs(title = SOI_count_chart_title, x="Radial Distance (m)", y="Observed Count") +
  theme_bw()

#view na's
sobs %>% 
  filter(Species == "BRSP") %>% 
  dplyr::select(distance, Observer.ID, MAS,  How.Detected, Ord.Date) %>% 
  filter_all(any_vars(is.na(.)))

#View juvenile observations


#view data one more time
glimpse(sobs)

#Time to model!!! -----------------------------------------
#naive model ----------------------------------------------
SOI_model_obs <- sobs %>%
  filter(Species == SOI) %>%         #for starters, only look at our SOI
  drop_na(distance, How.Detected) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
    transect = "point",                            #point transect
    formula = ~ Observer.ID,          #observer is the most important covariate
    key = "hr",                                    #starting with a hazard rate detection function
    convert_units = conv,                          #Not converting units yet
    region_table = region_table,                   #Stratify observations by plot type
    sample_table = sample_table
    )

#View the data from our basic detection model -----------------------------
#Using the basic model, this is how many SOI are on burned verses unburned plots
print(SOI) #So we know our species of interest
print(SOI_model_obs$dht$individuals$summary) #effort and area covered
print(SOI_model_obs$dht$individuals$D) #individuals and density
summary(SOI_model_obs$ddf)
par(mfrow = c(1,1))
plot(SOI_model_obs, main = SOI) #plot of detection function ----

#QQ Plot for initial SOI Model ----
gof_ds(SOI_model_obs, main="QQ plot Haz-Observer for SOI")

#Plot of SOI density by plot type ----
#Build an object to store the density outputs
SOI_Density <- tibble(SOI_model_obs$dht$individuals$D)
print(SOI_Density)

#Add a column for plot type to this new object
SOI_Density <- SOI_Density %>%
  rename(Route.ID = 'Label', #Rename columns so they make more sense
         SOI.Per.km2 = 'Estimate') %>%
  filter(Route.ID != 'Total') %>% #Remove the total estimate
  mutate(Route.Type = if_else(str_detect(Route.ID, "B"), #Add back in plot type
                             "Burn", "Reference")) %>% 
  mutate(Species = SOI) %>% 
  dplyr::select(Species, Route.ID, Route.Type, SOI.Per.km2,
         se, lcl, ucl) #Remove the columns I no longer need
  
#...And view
glimpse(SOI_Density)

#Exploratory analysis of the data -----------------------------------------
#build a storage object for y axis so it will change with species
SOI_density_chart_ylab <- paste("Estimated number of", SOI, "per km^2", sep = " ") 

#storage object so plot title will change with species
SOI_density_chart_title <- paste(SOI, "Density by Plot Type using a Basic Hazard Rate Detection Function", 
                                 sep = " ")
#storage object so we can change colors
SOI_Colors <- c("brown2", "dodgerblue3", "darkorchid4") #The colors we will use for each 

#Density in burn vs unburned plots -----
SOI_Density_BoxP <- SOI_Density %>%
  ggplot(aes(x = Route.Type, y = SOI.Per.km2, fill = Route.Type)) +
  geom_boxplot() +
  ggtitle(SOI_density_chart_title) +                   
  ylab(SOI_density_chart_ylab) +  
  theme(plot.title = element_text(hjust = 0.3, size = 14, face = "bold"), #Set the plot title
        axis.title.y = element_text(size = 13),       #change the size of the y axis title
        axis.text.y = element_text(size = 12), #change the size of the y-axis values
        axis.text.x = element_blank(),                 #remove x axis labels
        axis.ticks.x = element_blank(),                #remove x axis ticks
        axis.title.x = element_blank(),                #remove x-axis title 
        legend.text = element_text(size = 12),         #Change the size of the legend   
        legend.title = element_blank())        #remove the legend title  
SOI_Density_BoxP

#That model looks pretty good but let's try some more -----------------------------------
#view covariates so I know the options
glimpse(sobs)

#observer + mas
SOI_model_obs_mas <- sobs %>%
  filter(Species == SOI) %>%         #for starters, only look at our SOI
  drop_na(distance) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
     transect = "point",                            #point transect
     formula = ~ Observer.ID + MAS,                       #plug in a lot of covariates
     key = "hr",                                    #starting with a hazard rate detection function
     convert_units = conv,                          #Not converting units yet
     region_table = region_table,                   #Stratify observations by plot type
     sample_table = sample_table)

#Observer plus minutes after sunrise and ordinal date
SOI_model_obs_mas_date <- sobs %>%
  filter(Species == SOI) %>%         #for starters, only look at our SOI
  drop_na(distance) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
     transect = "point",                            #point transect
     formula = ~ Observer.ID + MAS + Ord.Date,                       #plug in a lot of covariates
     key = "hr",                                    #starting with a hazard rate detection function
     convert_units = conv,                          #Not converting units yet
     region_table = region_table,                   #Stratify observations by plot type
     sample_table = sample_table)

#observer plus ord date
SOI_model_obs_date<- sobs %>%
  filter(Species == SOI) %>%         #for starters, only look at our SOI
  drop_na(distance) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
     transect = "point",                            #point transect
     formula = ~ Observer.ID + Ord.Date ,                       #plug in a lot of covariates
     key = "hr",                                    #starting with a hazard rate detection function
     convert_units = conv,                          #Not converting units yet
     region_table = region_table,                   #Stratify observations by plot type
     sample_table = sample_table)

#observer plus how detected
SOI_model_obs_detect<- sobs %>%
  filter(Species == SOI) %>%         #for starters, only look at our SOI
  drop_na(distance) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
     transect = "point",                            #point transect
     formula = ~ Observer.ID + How.Detected,        #plug in a lot of covariates
     key = "hr",                                    
     convert_units = conv,                          #Not converting units yet
     region_table = region_table,                   #Stratify observations by plot type
     sample_table = sample_table)

#all detection level covariates
SOI_model_naive <- sobs %>%
  filter(Species == SOI) %>%         #for starters, only look at our SOI
  drop_na(distance) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
     transect = "point",                            #point transect
     formula = ~ Observer.ID + MAS + Ord.Date + 
       Sky.Start, Wind.Start + Temp.Start,                       #plug in a lot of covariates
     key = "hr",                                    #starting with a hazard rate detection function
     convert_units = conv,                          #Not converting units yet
     region_table = region_table,                   #Stratify observations by plot type
     sample_table = sample_table)

#Compare models
summarize_ds_models(SOI_model_obs, SOI_model_obs_mas, SOI_model_obs_date,
                    SOI_model_obs_mas_date, SOI_model_obs_detect, SOI_model_naive)
