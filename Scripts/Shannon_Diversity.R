#Ciomparing species diversity between burned and unburned plots
#Will Harrod
#Created: 03/27/20024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(gridExtra)
library(glmmTMB)
library(DHARMa)
library(lme4)

#Remove objects
rm(list = ls())

#Add in Data
sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_count_covs.csv") %>% 
  dplyr::select(-X)
#view the data
glimpse(sobs) 

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/route_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

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
                       "SAVS", "AMRO", "BRBL", "NOFL"
                       # , "CORA", "MODO", "CHUK", "PIJA", "RTHA" 
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
  tidyr::expand(nesting(Route.ID, Year, Visit, Date, Observer.ID), Species)
  
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Route.ID', 'Year', 'Visit', 'Date', 'Species')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))
#...and view
glimpse(sobs_count)

#View the routes done by two observers
two_obs_routes <- sobs %>% 
  tidyr::expand(nesting(Route.ID, Year, Visit, Observer.ID)) %>% 
  group_by(Route.ID, Year, Visit) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  mutate(Keep = NA)
#...and view
print(two_obs_routes, n = Inf)

#Pick every other row to drop
for(i in 1:nrow(two_obs_routes)){
  if(i %% 2 == 0){
  two_obs_routes$Keep[i] <- T
  } else if(i %% 2 != 0) {
    two_obs_routes$Keep[i] <- F
  } #end if else
} # end loop
#...and view
print(two_obs_routes, n = Inf)

#remove those
two_obs_routes_drop <- two_obs_routes %>% 
  filter(Keep == F) %>% 
  mutate(Visit.ID = paste(Route.ID, Year, Visit, Observer.ID, sep = "-"))
#...and view
glimpse(two_obs_routes_drop)
   
#Remove from the counts
sobs_count <- sobs_count %>% 
  mutate(Visit.ID = paste(Route.ID, Year, Visit, Observer.ID, sep = "-")) %>% 
  filter(!Visit.ID %in% two_obs_routes_drop$Visit.ID) %>% 
  select(-Visit.ID)
#...and view
glimpse(sobs_count)

 #Calculate Shannon diversity index --------------------------------------------------------------------------------------

#Storage object for the number of route visits
route_diversity <- sobs_count %>% 
  distinct(Route.ID, Year, Visit, Observer.ID) %>% 
  mutate(Diversity = NA) %>% 
  mutate(Richness = NA)
#...and view
glimpse(route_diversity)

#for loop to calculate the Shannon diversity index for each species across all route visits
for(k in 1:nrow(route_diversity)) {
  #Define the route for each iteration
  route <- route_diversity$Route.ID[k]
  #Define the year for each iteration
  year <- route_diversity$Year[k]
  #Define the visit for each iteration
  visit <- route_diversity$Visit[k]
  #filter the counts to that visit
  visit_count <- sobs_count %>% 
    filter(Route.ID == route,
           Year == year,
           Visit == visit)
  #Number of each species observed
  freq <- visit_count %>%
    dplyr::select(Count) %>% 
    filter(Count != 0)
  #find the total number of observations
  n_obs <- sum(freq$Count)
  # What proportion of total observations was made up of that species
  pi <- freq / n_obs
  #Find the log of p_i
  ln_pi <- log(pi)
  #find the Shannon diversity
  H <- -1 * sum(pi * ln_pi)
  #assign it to a route visit
  route_diversity$Diversity[k] <- H
  #calculate species richness
  richness <- visit_count %>% 
    mutate(Sp.Present = case_when(Count > 0 ~ 1,
                                  Count == 0 ~ 0))
  route_diversity$Richness[k] <- sum(richness$Sp.Present)
}
#...and view
glimpse(route_diversity)

#Join to route covariates
route_diversity <- route_diversity %>% 
  left_join(covs, by = "Route.ID")
#...and view
glimpse(route_diversity)


#Change necessary variables to scales and factors
route_diversity <- route_diversity %>% 
  mutate_at(c("Route.Type", "Aspect", "Burn.Sevarity", "Fire.Name", "Observer.ID"), factor) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year))
#...and view
glimpse(route_diversity)

#Plot interesting comparisons -------------------------------------------------------

#Plot diversity
H <- route_diversity %>% 
        mutate(Route.Type = as.factor(Route.Type)) %>% 
        mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
        ggplot(aes(x = Route.ID, y = Diversity, col = Route.Type)) +
        geom_boxplot() +
        theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 6))

#plot species richness
S <- route_diversity %>% 
        mutate(Route.Type = as.factor(Route.Type)) %>% 
        mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
        ggplot(aes(x = Route.ID, y = Richness, col = Route.Type)) +
        geom_boxplot() +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, size = 6))

#View plots
grid.arrange(H, S, ncol = 2)

#Diversity and richness by fire year
#diversity
H_fy <- route_diversity %>% 
  drop_na(Years.Since.Fire, Burn.Sevarity) %>% 
  mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Diversity, col = Burn.Sevarity)) +
  geom_point() +
  geom_smooth() +
  theme_classic() 
#species richness
S_fy <- route_diversity %>% 
  drop_na(Years.Since.Fire, Burn.Sevarity) %>% 
  mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Richness, col = Burn.Sevarity)) +
  geom_point() +
  geom_smooth() +
  theme_classic() 
#View plots
grid.arrange(H_fy, S_fy, ncol = 2)

#Diversity by shrub cover
H_shrb <- route_diversity %>% 
  mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
  ggplot(aes(x = Shrub.Cover, y = Diversity, col = Route.Type)) +
  geom_point() +
  geom_smooth() +
  theme_classic() 
#View shrub cover plot
H_shrb

#Diversity by elevation
H_elv <- route_diversity %>% 
  mutate(Route.ID = reorder(Route.ID, as.numeric(Route.Type))) %>% 
  ggplot(aes(x = Elevation, y = Diversity, col = Route.Type)) +
  geom_point() +
  geom_smooth() +
  theme_classic() 
#view elevation plot
H_elv

 #Model Diversity ---------------------------------------------------------------------------

#View covariate distributions
par(mfrow = c(1,1))
hist(route_diversity$Sagebrush.Cover)
hist(route_diversity$Annual.Cover)
hist(route_diversity$Shrub.Cover)
hist(route_diversity$Shrub.Height)
plot(route_diversity$Diversity ~ route_diversity$Burn.Sevarity)
route_diversity %>% 
  ggplot(aes(x = Observer.ID, y = Diversity)) +
  geom_boxplot(fill = "lightgreen") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
hist(route_diversity$Fire.Distance)
hist(route_diversity$Fire.Year)
hist(route_diversity$Elevation)
hist(route_diversity$TRI)
hist(route_diversity$Aspect)
hist(route_diversity$Road.Distance)

#model 1: Burn vs unburned ----------------------------------------------------------------------
H_burn <- glm(data = route_diversity,
              formula = Diversity ~ Route.Type,
              family = Gamma(link = "inverse"))

#View model diagnostics
par(mfrow = c(2, 2))
plot(H_burn)

#View model output
summary(H_burn)

#Model species richness
S_burn <- glm(data = route_diversity,
              formula = Richness ~ Route.Type,
              family = poisson)

#View model diagnostics
par(mfrow = c(2, 2))
plot(S_burn)

#View model output
summary(S_burn)

#Model #2: years since fire ----------------------------------------------------------------
#I'm just going to look at Shannon diversity since the two seem to follow the exact same pattern
H_m_fy <- route_diversity %>% 
              glm(formula = Diversity ~ Years.Since.Fire,
                  family = Gamma(link = "inverse"))
#View model diagnostics
par(mfrow = c(2, 2))
plot(H_m_fy)

#View model output
summary(H_m_fy)

#model 3: Burn interactions ----------------------------------------------
H_m_fire <- route_diversity %>% 
  drop_na(Years.Since.Fire, Burn.Sevarity) %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ 1,
                                Route.Type == "R" ~ 0)) %>% 
  glm(formula = Diversity ~ Years.Since.Fire + 
                            Years.Since.Fire * Burn.Sevarity,
              family = Gamma(link = "inverse"))

#View model diagnostics
par(mfrow = c(2, 2))
plot(H_m_fire)

#View model output
summary(H_m_fire)


#Model #4: Shrub cover ---------------------------------------------------------
H_m_shrb <- route_diversity %>% 
  glm(formula = Diversity ~ Shrub.Cover,
      family = Gamma(link = "inverse"))

#View model diagnostics
par(mfrow = c(2, 2))
plot(H_m_shrb)

#View model output
summary(H_m_shrb)

#Model observer id -------------------------------------------------------------
H_m_obs <- route_diversity %>% 
  glm(formula = Diversity ~ Observer.ID,
      family = Gamma(link = "inverse"))
#View model diagnostics
par(mfrow = c(2, 2))
plot(H_m_obs)
#View model output
summary(H_m_obs)

#A model that I think might be good ----------------------------------------------------
H_m01 <- route_diversity %>% 
  glmer(formula = Diversity ~ 
          # Covariates
          Route.Type + 
          Shrub.Cover + 
          # TRI + 
          # Elevation + 
          # Annual.Cover + 
          # Road.Distance + 
          (1|Observer.ID) + (1|Route.ID), #Random Effects
          family = Gamma(link = "inverse"))
#View model diagnostics
par(mfrow = c(2, 2))
plot(H_m01)

#View model output
summary(H_m01)
