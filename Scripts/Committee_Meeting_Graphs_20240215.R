#Basic Graphs to show what I have to my committee 
#Will Harrod
#Created: 02/15/2024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

#add in the data --------------------------------------------------

#Add point count data
sobs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#and view the data
glimpse(sobs)

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
important_species <- c("BRSP", "SATH", "SABS", "GTTO",
                       "WEME", "HOLA", "VESP", "GRFL")

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species %in% important_species) %>% 
  group_by(Route.ID, Route.Type, Year, Visit, Date, Species) %>% 
  reframe(Route.ID, Route.Type, Year, Visit, Date, Species, Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zreo counts
visit_count <- sobs %>% 
  expand(nesting(Route.ID, Route.Type, Year, Visit, Date), Species) %>% 
  filter(Species %in% important_species)
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Route.ID', 'Route.Type', 'Year', 'Visit', 'Date', 'Species')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  left_join(covs, by = c('Route.ID', 'Route.Type'))
#...and view
glimpse(sobs_count)

#Start plotting --------------------------------------------------------------------------

#Define colors for boxplot
count_cols <- c("darkorange3", "deepskyblue1", "red3", "dodgerblue3")

#Average number of observations between burned and unburned plots 
sobs_count %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn",
                                Route.Type == "R" ~ "Reference")) %>% 
  mutate(Year = case_when(Year == "Y1" ~ "Year 1",
                          Year == "Y2" ~ "Year 2")) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Count, fill = Year.Type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual("",
    values = count_cols) +
  labs(y = "Average Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(),
        legend.position = c(0.85, 0.19)) +
  facet_wrap(~Species)

#Total number of observations between burned and unburned plots 
sobs_count %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn",
                                Route.Type == "R" ~ "Reference")) %>% 
  mutate(Year = case_when(Year == "Y1" ~ "Year 1",
                          Year == "Y2" ~ "Year 2")) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Count, fill = Year.Type)) +
  geom_col() +
  theme_bw() +
  scale_fill_manual("",
                    values = count_cols) +
  labs(y = "Total Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(),
        legend.position = c(0.85, 0.19)) +
  facet_wrap(~Species)

#Species counts and shrub cover
sobs_count %>% 
  ggplot(aes(x = Shrub.Cover, y = Count)) +
  geom_point(col = "cadetblue4", size = 1) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Percent Shrub Cover",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~Species) 

#Species counts and ruggedness
sobs_count %>% 
  ggplot(aes(x = TRI, y = Count)) +
  geom_point(col = "cadetblue4", size = 1) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Topographic Ruggedness Index",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~Species) 

#Species counts and fire year
sobs_count %>% 
  filter(Route.Type == "B" & Route.ID != "ID-B03") %>% 
  mutate(Years.Since.Fire = 2023 - Fire.Year) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Count)) +
  geom_point(col = "cadetblue4", size = 1) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Years Since Fire",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~Species) 

#Species counts and fire sevarety 
sobs_count %>% 
  filter(Route.Type == "B") %>% 
  filter(!is.na(Burn.Sevarity)) %>% 
  mutate(Burn.Sevarity = as.factor(Burn.Sevarity)) %>% 
  ggplot(aes(x = Burn.Sevarity, y = Count)) +
  geom_boxplot(col = "red3") +
  labs(x = "Average Burn Sevarity",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~Species) 
