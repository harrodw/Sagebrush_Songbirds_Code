#Top of the code block ----
#Basic Single Species detection functions

#Load packages
#Load Required Packages
library(tidyverse)
library(RSQLite)
library(Distance)

#Load in Data
sobs <- read.csv("C:\\Users\\Will\\Desktop\\USU\\SOBs\\R Projects\\Tri_State_Sagebrush_Songbirds\\Data\\Outputs\\sobs_data_20240113.csv") %>% 
  select(-X) #Remove the column that excel generated

#view our data
str(sobs)                      

#Build and object for effort
effort <- 4

#Add in an Effort Column
sobs <- sobs %>% 
mutate(Effort = 1)
  
#View the data again  
str(sobs)

#rename columns so distance can understand them
#storage object for new names
dist_names <- c(Region.Label = "Route.ID", 
                Sample.Label = "Full.Point.ID",
                distance = "Distance")

#now the actual renaming
sobs <- sobs %>% 
  rename(all_of(dist_names))
str(sobs)

# #Make Distance Continuous
sobs <- sobs %>% 
  mutate(distance = as.numeric(distance))
str(sobs)

#Transform the covariates we need to factors
sobs <- sobs %>%
  mutate_at(c("Minute", "Year", "Region.Label", "Sample.Label", "Sky.Start", "Wind.Start", 
              "Route.Type", "Observer.ID", "Visit", "How.Detected", "Sex", "Visual.ID"), 
            factor)

#Change Some Other covariates to scale
sobs <- sobs %>%
  mutate_at(c("MAS", "Ord.Date", "Temp.Start"), scale)

#and make sure all the rows are correct
str(sobs)

#Define a storage object to look at the data for a single species of interest (SOI) ----
SOI <- "BRSP"

#A storage object for the plot title
SOI_count_chart_title <- paste(SOI, "Observed by Distance", sep = " ")

#Table of which Plots have SOI
sobs %>%
  filter(Species == SOI) %>%
  count(Region.Label)

#Plot of the distribution of observed distances
sobs %>%
  filter(Species == SOI) %>%
  ggplot(aes(x = distance)) +
  geom_histogram() + 
  facet_wrap(~Route.Type) +
  labs(title = SOI_count_chart_title, x="Radial Distance (m)", y="Observed Count") +
  theme_bw()

#arrange data by distance for our SOI
soi_obs <- sobs %>%
  filter(Species == SOI) %>%
  arrange(distance)

#How many total observations?
str(soi_obs)

#Storage object for how many of this species we saw
soi_count <- round(nrow(soi_obs), 0)
print(soi_count)

#find the observations that are right at the top ten percent of distances
SOI_trunc_dist <- (soi_obs[(0.9 * soi_count), 2])
print(SOI_trunc_dist)

#Table showig how many individuals we saw on each route
sobs %>%
  filter(Species == SOI) %>%
  with(table(Species, Region.Label))

#Sample table that stores our effort
sample_table <- data.frame(Sample.Label = unique(sobs$Sample.Label)) #one row per point two sets of points for the two visits
sample_table <- sample_table %>% 
  mutate(Region.Label = str_remove_all(Sample.Label, "-P[01][123456789]")) # define burn or reference plots
sample_table$Effort <- rep((effort), nrow(sample_table)) # add effort

#View our sample table
str(sample_table)
head(sample_table)
tail(sample_table)

#Build an object for conversion
conv <- convert_units("Meter", NULL, "Square kilometer")

#Build an object for the area of a single point
point_area <- (0.000001 * pi * (SOI_trunc_dist ^ 2)) # Area of a circle in km^2 
print(point_area)
#with a radius equal to the approximate SOI cutoff distance

#AN object to store how all of the unique region labels
unique_regions <- data.frame(
  Region.Label = unique(sobs$Region.Label))
str(unique_regions)

#Build an object for Region label
region_table <- data.frame(
  Region.Label = Unique_Regions) %>%
  mutate(Area = case_when(Region.Label %in% c("UT-B02",
                                              "UT-C02") ~ (effort * 10 * point_area),
                          Region.Label %in% c("UT-B17",
                                              "UT-C17") ~ (effort * 11 * point_area),
                          Region.Label %in% c("UT-B22",
                                              "UT-C22") ~ ( effort * 12 * point_area),
                          Region.Label %in% c("UT-B25",
                                              "UT-C25") ~ (effort *14 * point_area),
                          TRUE ~ (effort *16 * point_area)
                          ))

#view our region table
str(region_table)

#Build a hazard rate detection function with a simple polynomial adjustment ----------------
print(SOI) #Check which species we're looking at
sobs %>% #See how many total observations we're dealing with
  filter(Species == SOI, ) %>%
  with(table(Species, Region.Label))

#view na's
sobs %>% 
  filter(Species == "BRSP") %>% 
  select(distance, Observer.ID, MAS,  How.Detected) %>% 
  tibble() %>% 
  summarise_all(funs(sum(is.na(.))))

#view data one more time
str(sobs)

#naive model ----------------------------------------------
SOI_model_naive <- sobs %>%
  filter(Species== SOI) %>%         #for starters, only look at our SOI
  drop_na(distance) %>% 
  ds(truncation = "10%",                           #truncate the 10% #of distances
    transect = "point",                            #point transect
    formula = ~ Observer.ID,       #plug in a lot of covariates
    key = "hr",                                    #starting with a hazard rate detection function
    convert_units = conv,                          #Not converting units yet
    region_table = region_table,                   #Stratify observations by plot type
    sample_table = sample_table)

#View the data from our basic detection model
#Using the basic model, this is how many SOI are on burned verses unburned plots --------------
print(SOI) #So we know our species of interest
print(SOI_model_naive$dht$individuals$summary) #effort and area covered
print(SOI_model_naive$dht$individuals$D) #individuals and density
summary(SOI_model_naive$ddf)
par(mfrow = c(1,1))
plot(SOI_model_naive, main = SOI) #plot of detection function ----

#QQ Plot for initial SOI Model ----
gof_ds(SOI_model_naive, main="QQ plot Haz-Observer for SOI")

#Plot of SOI density by plot type ----
#Build an object to store the density outputs
SOI_Density <- SOI_model_naive$dht$individuals$D
print(SOI_Density)

#Add a column for plot type to this new object
SOI_Density <- SOI_Density %>%
  mutate(Plot.Type = if_else(str_detect(Label, "B"), "Burn", "Reference"))
print(SOI_Density)


#build a storage object for y axis so it will change with species
SOI_density_chart_ylab <- paste("Estimated number of", SOI, "per km^2", sep = " ") 

#storage object so plot title will change with species
SOI_density_chart_title <- paste(SOI, "Density by Plot Type using a Basic Hazard Rate Detection Function", 
                                 sep = " ")
#storage object so we can change colors
SOI_Colors <- c("brown2", "dodgerblue3", "darkorchid4") #The colors we will use for each 

#Remove the total Estimate
SOI_Density <- SOI_Density[-61,]
  
#Boxplot
SOI_Density_BoxP <- SOI_Density %>%
  ggplot(aes(x = Plot.Type, y = Estimate, fill = Plot.Type)) +
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

#Plot of species density by plot type
SOI_Density_Bar <- SOI_Density %>% # the column for the detection object that we want                         
  ggplot(aes(x = Plot.Type, y = Estimate, fill = Plot.Type)) + #define the axes
  geom_bar(stat = "identity") +                        #We'll use a bar graph
  geom_text(aes(label = Estimate), stat = "identity", 
            vjust = 1.5, col = "white", size = 5) +    #Text inside the bars
  scale_fill_manual(values = SOI_Colors) +             #change the bar colors
  ggtitle(SOI_density_chart_title) +                   
  ylab(SOI_density_chart_ylab) +                       #y-axis label that changes with species
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), #Set the plot title
        axis.title.y = element_text(size = 14),       #change the size of the y axis title
        axis.text.y = element_text(size = 12, face = "bold"), #change the size of the y-axis values
        axis.text.x = element_blank(),                 #remove x axis labels
        axis.ticks.x = element_blank(),                #remove x axis ticks
        axis.title.x = element_blank(),                #remove x-axis title 
        legend.text = element_text(size = 12),         #Change the size of the legend   
        legend.title = element_blank())        #remove the legend title    

#Print the plot        
SOI_Density_Bar

#Add a column for fire year
SOI_Density <- SOI_Density%>% 
  mutate(Years.Since.Fire = as.numeric(case_when(Label == "ID-B04" ~ 16,
                                                 Label == "ID-B07" ~ 15,
                                                 Label == "ID-B09" ~ 22,
                                                 Label == "ID-B11" ~ 15,
                                                 Label == "ID-B12" ~ 15,
                                                 Label == "ID-B13" ~ 2,
                                                 Label == "ID-B15" ~ 12, 
                                                 Label == "ID-B16" ~ 10,
                                                 Label == "ID-B19" ~ 10,
                                                 Label == "ID-B21" ~ 15,
                                                 Label == "ID-B22" ~ 16,
                                                 Label == "ID-B23" ~ 10,
                                                 Label == "ID-B24" ~ 16,
                                                 Label == "ID-B26" ~ 10,
                                                 Label == "ID-B28" ~ 22,
                                                 Label == "UT-B01" ~ 19,
                                                 Label == "UT-B02" ~ 5,
                                                 Label == "UT-B05" ~ 4,
                                                 Label == "UT-B06" ~ 4,
                                                 Label == "UT-B08" ~ 4,
                                                 Label == "UT-B15" ~ 23,
                                                 Label == "UT-B16" ~ 23,
                                                 Label == "UT-B17" ~ 20,
                                                 Label == "UT-B19" ~ 22,
                                                 Label == "UT-B22" ~ 4,
                                                 Label == "UT-B24" ~ 4,
                                                 Label == "UT-B25" ~ 23,
                                                 Label == "UT-B27" ~ 15,
                                                 Label == "UT-B30" ~ 15)))
print(SOI_Density) #Looks good

#Species density by fire year
SOI_Density %>%
  filter(Plot.Type == "Burn" & Label != "ID-B03") %>%
  ggplot(aes(x = Years.Since.Fire, y = Estimate)) +
  geom_point()
#Looks all over the place

#Try out linear models -------------------------
#Plot Type
treatment_density_lm <- lm(Estimate ~ Plot.Type,
                      data = SOI_Density)
summary(treatment_density_lm)
par(mfrow = c(2, 2))
plot(treatment_density_lm)

#Fire year Linear Model
#Object fr burn out desnities
SOI_Burn_Density <- SOI_Density %>%
  filter(Plot.Type == "Burn" & Label != "ID-B03")
print(SOI_Burn_Density)

fire_density_lm <- lm(Estimate ~ Years.Since.Fire,
  data = SOI_Burn_Density)
summary(fire_density_lm)
par(mfrow = c(2, 2))
plot(fire_density_lm)

#Density by fire year ------------------------------------------------------------

#Define SOI
SOI <- "WEME"

#Make an object for only burn plots
str(point_count_data)
burn_count_data <- point_count_data %>%
  filter(Route.Type == "B" & Region.Label != "ID-B03")

#Rename Route ID and Point ID
new_names <- c(Label = "Region.Label", Full.Point.ID = "Sample.Label")
burn_count_data <- rename(burn_count_data, all_of(new_names))
str(burn_count_data)

#Looks Good
unique(burn_count_data$Route.ID)

#Add in Columns for fire year and name
burn_count_data <- burn_count_data %>% #Fire name
  mutate(Fire.Name = as.factor(case_when(Route.ID == "ID-B04" ~ "Burnt",
                               Route.ID == "ID-B07" ~ "Jim Sage",
                               Route.ID == "ID-B09" ~ "Devine Canyon",
                               Route.ID == "ID-B11" ~ "Jim Sage",
                               Route.ID == "ID-B12" ~ "Emery",
                               Route.ID == "ID-B13" ~ "Badger",
                               Route.ID == "ID-B15" ~ "Emery", 
                               Route.ID == "ID-B16" ~ "Cave Canyon",
                               Route.ID == "ID-B19" ~ "Cave Canyon",
                               Route.ID == "ID-B21" ~ "Black Pine",
                               Route.ID == "ID-B22" ~ "Burnt",
                               Route.ID == "ID-B23" ~ "Deer Hallow",
                               Route.ID == "ID-B24" ~ "Burnt",
                               Route.ID == "ID-B26" ~ "Cave Canyon",
                               Route.ID == "ID-B28" ~ "City of Rocks",
                               Route.ID == "UT-B01" ~ "Playground",
                               Route.ID == "UT-B02" ~ "Rosebud",
                               Route.ID == "UT-B05" ~ "Goose Creek",
                               Route.ID == "UT-B06" ~ "Goose Creek",
                               Route.ID == "UT-B08" ~ "Goose Creek",
                               Route.ID == "UT-B15" ~ "Wagon Box",
                               Route.ID == "UT-B16" ~ "Wagon Box",
                               Route.ID == "UT-B17" ~ "Prospect",
                               Route.ID == "UT-B19" ~ "City of Rocks",
                               Route.ID == "UT-B22" ~ "Goose Creek",
                               Route.ID == "UT-B24" ~ "Goose Creek",
                               Route.ID == "UT-B25" ~ "Wagon Box",
                               Route.ID == "UT-B27" ~ "Winecup",
                               Route.ID == "UT-B30" ~ "Winecup")))
unique(burn_count_data$Fire.Name) #looks good

#Fire name
burn_count_data <- burn_count_data %>% 
  mutate(Fire.Year = as.numeric(case_when(Route.ID == "ID-B04" ~ 2006,
                                          Route.ID == "ID-B07" ~ 2007,
                                          Route.ID == "ID-B09" ~ 2000,
                                          Route.ID == "ID-B11" ~ 2007,
                                          Route.ID == "ID-B12" ~ 2007,
                                          Route.ID == "ID-B13" ~ 2020,
                                          Route.ID == "ID-B15" ~ 2010, 
                                          Route.ID == "ID-B16" ~ 2012,
                                          Route.ID == "ID-B19" ~ 2012,
                                          Route.ID == "ID-B21" ~ 2007,
                                          Route.ID == "ID-B22" ~ 2006,
                                          Route.ID == "ID-B23" ~ 2012,
                                          Route.ID == "ID-B24" ~ 2006,
                                          Route.ID == "ID-B26" ~ 2012,
                                          Route.ID == "ID-B28" ~ 2000,
                                          Route.ID == "UT-B01" ~ 2013,
                                          Route.ID == "UT-B02" ~ 2017,
                                          Route.ID == "UT-B05" ~ 2018,
                                          Route.ID == "UT-B06" ~ 2018,
                                          Route.ID == "UT-B08" ~ 2018,
                                          Route.ID == "UT-B15" ~ 1999,
                                          Route.ID == "UT-B16" ~ 1999,
                                          Route.ID == "UT-B17" ~ 2002,
                                          Route.ID == "UT-B19" ~ 2000,
                                          Route.ID == "UT-B22" ~ 2018,
                                          Route.ID == "UT-B24" ~ 2018,
                                          Route.ID == "UT-B25" ~ 1999,
                                          Route.ID == "UT-B27" ~ 2007,
                                          Route.ID == "UT-B30" ~ 2007)))
unique(burn_count_data$Fire.Year) #Looks good

#Change to years since fire
burn_count_data <- burn_count_data %>%
  mutate(Years.Since.Fire = (2022 - Fire.Year))
#Looks Good
unique(burn_count_data$Years.Since.Fire)

#Change to classes
burn_count_data <- burn_count_data %>%
  mutate(Years.Since.Fire.Class = factor(case_when(Years.Since.Fire < 5
                                        ~ "0-4",
                                        Years.Since.Fire < 10 &  
                                          Years.Since.Fire >= 5
                                        ~ "5-9",
                                        Years.Since.Fire < 15 &  
                                          Years.Since.Fire >= 10
                                        ~ "10-14",
                                        Years.Since.Fire < 20 &  
                                          Years.Since.Fire >= 15
                                        ~ "15-19",
                                        Years.Since.Fire >= 20
                                        ~ "20 or More"),
                                  levels = c("0-4", "5-9", "10-14", "15-19", "20 or More")))
#View the New Row
str(burn_count_data$Years.Since.Fire.Class) #Looks good
unique(burn_count_data$Years.Since.Fire.Class) #looks good

#Sample and Region Labels for Fire Year ----
#Define New Names
str(burn_count_data)
new_names <- c(Region.Label = "Years.Since.Fire.Class",
                   Sample.Label = "Full.Point.ID")

#Change Row names so distance can understand sample and region labels
burn_count_data <- rename(burn_count_data, all_of(new_names))
str(burn_count_data)

#Store SOI count data
SOI_Burn_Count <- burn_count_data %>%
  filter(Species == SOI) %>%
  arrange(distance)
head(SOI_Burn_Count$distance)

#Storage object for how many of this species we saw
SOI_Burn_OBS_Number <- round(nrow(SOI_Burn_Count), 0)
print(SOI_Burn_OBS_Number)

#find the observations that are right at the top ten percent of distances
SOI_Burn_Trunc_Dist <- SOI_Burn_Count[(0.9 * SOI_Burn_OBS_Number), 2]
print(SOI_Burn_Trunc_Dist)

#Fire year sample table that stores our effort ----
sample_table <- data.frame(Sample.Label = as.factor(unique(burn_count_data$Sample.Label)))
                           #one row per point two sets of points for the two visits
str(sample_table) #looks good so far

#Create a row for point ID
sample_table <- sample_table %>% # define burn or reference plots
  mutate(Route.ID = str_remove(Sample.Label, "-P[01234567890][0123456789]")) 
str(sample_table) #still looking good
unique(sample_table$Route.ID)

#View which routes are in each class
burn_count_data %>%
with(table(Route.ID, Region.Label))

#Create Specific lables for each burn class
sample_table <- sample_table %>%
  mutate(Region.Label = factor(case_when(Route.ID %in% c("ID-B13",
                                                         "UT-B05",
                                                         "UT-B06",
                                                         "UT-B08",
                                                         "UT-B22",
                                                         "UT-B24"
                                                ) ~ "0-4",
                                           Route.ID %in% c("UT-B01",
                                                           "UT-B02"
                                                ) ~ "5-9",
                                           Route.ID %in% c("ID-B15",
                                                           "ID-B16",
                                                           "ID-B19",
                                                           "ID-B22",
                                                           "ID-B23",
                                                           "ID-B26"
                                                ) ~ "10-14",
                                           Route.ID %in% c("ID-B04",
                                                           "ID-B07",
                                                           "ID-B11",
                                                           "ID-B12",
                                                           "ID-B21",
                                                           "ID-B22",
                                                           "ID-B24",
                                                           "UT-B27",
                                                           "UT-B30"
                                                ) ~ "15-19",
                                           Route.ID %in% c("ID-B09",
                                                           "ID-B28",
                                                           "UT-B15",
                                                           "UT-B16",
                                                           "UT-B17",
                                                           "UT-B19",
                                                           "UT-B25"
                                                ) ~ "20 or More")))
#View the sample table
str(sample_table)

#Addd Effort Column
sample_table$Effort <- rep((effort), nrow(sample_table)) # add effort

#RemoveRoute.ID
sample_table <- sample_table %>%
  select(Sample.Label, Region.Label, Effort)

#View Sample Table
str(sample_table)
unique(sample_table$Region.Label)

#Region Table ------
#Build an object for conversion
conv <- convert_units("Meter", NULL, "Square kilometer")

#Build an object for the area of a single point
point_area <- (0.000001 * pi * (SOI_Burn_Trunc_Dist ^ 2)) # Area of a circle in km^2 
print(point_area)
#with a radius equal to the approximate SOI cutoff distance

#See how many points are in each year class
unique(sample_table$Region.Label)

ysf_0_4_points <- sample_table %>%
  filter(Region.Label == "0-4")

ysf_5_9_points <- sample_table %>%
  filter(Region.Label == "5-9")

ysf_10_14_points <- sample_table %>%
  filter(Region.Label == "10-14")

ysf_15_19_points <- sample_table %>%
  filter(Region.Label == "15-19")

ysf_20_points <- sample_table %>%
  filter(Region.Label == "20 or More")


#Build an object for Region label
region_table <- data.frame(
  Region.Label = unique(sample_table$Region.Label))
print(region_table)
region_table <- region_table %>%
  mutate(Area = case_when(Region.Label == "0-4" ~ (nrow(ysf_0_4_points)* point_area),
                          Region.Label == "5-9" ~ (nrow(ysf_5_9_points)* point_area),
                          Region.Label == "10-14" ~ (nrow(ysf_10_14_points)* point_area),
                          Region.Label == "15-19" ~ (nrow(ysf_15_19_points)* point_area),
                          Region.Label == "20 or More" ~ (nrow(ysf_20_points)* point_area),
                          .default = NA))

#view our region table
str(region_table)
unique(region_table$Area)

#Fire year to the dection function ---------------
print(SOI) #Check which species we're looking at
SOI_Model_Fire_Year <- burn_count_data %>%
  filter(Species == SOI & Year == "Y2") %>%         #for starters, only look at the second year
  ds(                                              #Detection Function
    truncation = "20%",                            #truncate the 10% #of distances
    transect = "point",                            #point transect
    formula = ~ Observer + DAS,  #I know that OBS also affects the detection function
    key = "hr",                #starting with a hazard rate detection function
    convert_units = conv,                          #Not converting units yet
    region_table = region_table,                    #No region table
    sample_table = sample_table
  )

#View how fire year affects density ------------
print(SOI) #So we know our species of interest
summary(SOI_Model_Fire_Year$ddf) #Overall Summary
print(SOI_Model_Fire_Year$dht$individuals$summary) #effort and area covered
print(SOI_Model_Fire_Year$dht$individuals$D) #individuals and density
plot(SOI_Model_Fire_Year, main = SOI) #plot of detection function

#QQ Plot for initial SOI Model ----
gof_ds(SOI_Model_Fire_Year,
       main="QQ plot Haz-Observer for SOI")
