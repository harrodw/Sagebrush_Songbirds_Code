#Ciomparing species diversity between burned and unburned plots
#Will Harrod
#Created: 03/27/20024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)
library(forcats)

#Remove objects
rm(list = ls())

#Add in Data
sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_count_covs.csv") %>% 
  select(-X)
#view the data
glimpse(sobs) 

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/route_summaries.csv")) %>% 
  select(-X)
#View covariates
glimpse(covs)

#remove unused rows
covs <- covs %>% 
  slice(-c(58, 60, 62)) #Remove the info for route that straddle a burn
#...and view
print(covs, n = Inf)

#Find how many of each species were observed across the whole data set 
obs_count <- sobs %>% 
  group_by(Species) %>% 
  reframe(Species, Count = n()) %>% 
  filter(!Species %in% c("NOBI", "UNBI")) %>% 
  distinct(Species, Count) %>% 
  arrange(desc(Count)) %>%
  mutate(Species = factor(Species, levels = Species)) %>% 
  filter(Count >= 60)
#...and view
print(obs_count, n = Inf)
#...and plot
obs_count %>% 
  ggplot(aes(x = Species, y = Count)) +
  geom_col(fill = "lightblue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 7))
  

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
important_species <- c("BRSP", "SATH", "SABS", "GTTO",
                       "WEME", "HOLA", "VESP", "LASP", 
                       "GRFL", "LAZB", "CHSP",
                       "ROWR", "SPTO", "BBMA", "MOBL",
                       "SAVS", "AMRO", "BRBL", "NOFL",
                       )

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species %in% important_species) %>% 
  group_by(Route.ID, Year, Visit, Date, Species) %>% 
  reframe(Route.ID, Year, Visit, Date, Species, Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  filter(Species %in% important_species) %>% 
  expand(nesting(Route.ID, Year, Visit, Date), Species)
  
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Route.ID', 'Year', 'Visit', 'Date', 'Species')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))
#...and view
glimpse(sobs_count)

#Calculate Shannon diversity index --------------------------------------------------------------------------------------

#Storage object for the number of route visits
route_diversity <- sobs_count %>% 
  distinct(Route.ID, Year, Visit) %>% 
  mutate(Diversity = NA)
#...and view
glimpse(route_diversity)

#for loop to calculate the Shannon diversity index for each species across all route visits
for(k in 1:nrow(route_diversity)) {
  #Define the route for eacch iteration
  route <- route_diversity$Route.ID[k]
  #Define the year for eacch iteration
  year <- route_diversity$Year[k]
  #Define the visit for eacch iteration
  visit <- route_diversity$Visit[k]
  #filter the counts to that visit
  visit_count <- sobs_count %>% 
    filter(Route.ID == route,
           Year == year,
           Visit == visit)
  #Number of each species observed
  freq <- visit_count %>%
    select(Count) %>% 
    filter(Count != 0)
  #find the total number of observations
  n_obs <- sum(freq$Count)
  # What proportion of total observations was made up of that species
  pi <- freq / n_obs
  #Find the log of p_i
  ln_pi <-log(pi)
  #find the Shannon diversity
  H <- -1 * sum(pi * ln_pi)
  #assign it to a route visit
  route_diversity$Diversity[k] <- H
}
#...and view
glimpse(route_diversity)

#Join to route covariates
route_diversity <- route_diversity %>% 
  left_join(covs, by = "Route.ID")
#...and view
glimpse(route_diversity)

#Plot
route_diversity %>% 
  mutate(Route.Type = as.factor(Route.Type)) %>% 
  mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
  ggplot(aes(x = Route.ID, y = Diversity, col = Route.Type)) +
  geom_boxplot() +
  theme_classic()

#Model Diversity ---------------------------------------------------------------------------

#Change nesessary variavles to scales and factors
route_diversity <- route_diversity %>% 
  mutate_at(c("Route.Type"), factor)
#...and view
glimpse(route_diversity)

#model 1: Burn vs unburned
m_burn <- glm(data = route_diversity,
              formula = Diversity ~ Route.Type,
              family = Gamma(link = "inverse"))

#View model diagnostics
par(mfrow = c(2, 2))
plot(m_burn)

#View model output
summary(m_burn)

