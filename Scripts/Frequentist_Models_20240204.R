#Frequentist models for practice and getting and idea of which variables woill be useful
#Will Harrod
#Created: 02/04/20024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)

#Add point count data
sobs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\sobs_data_20240113.csv")) %>% 
        dplyr::select(-X) #Remove the column that excel generated
#and view the data
glimpse(sobs)

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Select a species of interest (SOI)
soi <- "BRSP"

#Make a table of all possible route and visit combinations
possible_routes <- sobs %>% 
  expand(nesting(Route.ID, Year, Visit))
#View possib;e routes
glimpse(possible_routes)

#Make a table of how many of the species of interest were observed on each route
count_0inf <- sobs %>% 
  filter(Species == soi) %>%
  group_by(Route.ID, Year, Visit) %>% 
  reframe(Route.ID, Year, Visit, SOI.Count = n()) %>% 
  ungroup() %>% 
  distinct(Route.ID, Year, Visit, SOI.Count)
#Vieew soi count
glimpse(count_0inf)

#Join the two so I have the zero counts
sobs_count <- left_join(possible_routes, count_0inf, 
          by = c('Route.ID', 'Year', 'Visit')) %>% 
  mutate(SOI.Count = case_when(is.na(SOI.Count) ~ 0,
                               TRUE ~ SOI.Count)) 

#View the full count
glimpse(sobs_count)

#Combine with covariates 
sobs_count <- sobs_count %>% 
  left_join(covs, by = 'Route.ID') 

#Transform some columns to factors 
sobs_count <- sobs_count %>% 
  mutate_at(c('Route.Type', 'Aspect', 'Fire.Name'), as.factor)
  
#View the counts with covariates
glimpse(sobs_count)
print(sobs_count, n = 240)
hist(sobs_count$SOI.Count)

#Plot comparisons between SOI count and some covariates ------------------
#Scatterplot
sobs_count %>% 
  ggplot(aes(x =Shrub.Cover.RAP, y = SOI.Count)) +
  geom_point() +
  geom_smooth()

 #Boxplot
sobs_count %>% 
  ggplot(aes(x = Route.Type , y = SOI.Count)) +
  geom_boxplot()

#Modeling -------------------------------------------------------------
#First a naive model ----
model1_no_fire <- glm(data = sobs_count,
                     family = poisson(link = 'log'),
                     formula = SOI.Count ~ Route.Type + Sagebrush.Cover +
                               Annual.Cover + Shrub.Cover + 
                               Elevation + TRI + Aspect)
#View the model
summary(model1_no_fire)

#Plot the model
par(mfrow = c(2, 2))
plot(model1_no_fire)
par(mfrow = c(1, 1))

#More specific model ----
model2_most_relv <- glm(data = sobs_count,
                     family = poisson(link = 'log'),
                     formula = SOI.Count ~ Route.Type + Shrub.Cover.RAP + TRI)
#View the model
summary(model2_most_relv)

#Plot the model
par(mfrow = c(2, 2))
plot(model2_most_relv)
par(mfrow = c(1, 1))

#02/05/2024 Try the same thing with the output from distance -----------------------

#Just a bit of data prep ------------------------------
#add BRSP output from distance
brsp_est <- read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\soi_dens_est_20240205.csv") %>% 
  tibble() %>% 
  dplyr::select(Route.ID, SOI.Per.km2) %>% 
  rename(BRSP.Estimate = 'SOI.Per.km2') %>% 
  mutate(BRSP.Estimate = as.integer(BRSP.Estimate)) #only integers so I can model using a poisson process
#...and view
glimpse(brsp_est)

#add covariates
covs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Join covariates to estimates
brsp_est <- brsp_est %>% 
  left_join(covs, by = 'Route.ID') %>% 
  mutate(Aspect = factor(Aspect)) %>% 
  mutate(Route.Type = factor(Route.Type, levels = c("R", "B")))

#...and view
glimpse(brsp_est)

#Plot comparisons between BRSP estimates and some covariates ------------------
#Scatterplot
brsp_est %>% 
  ggplot(aes(x = TRI, y = BRSP.Estimate)) +
  geom_point() +
  geom_smooth()

brsp_est %>% 
  ggplot(aes(x = Route.Type , y = BRSP.Estimate)) +
  geom_boxplot()

#Now model-------------------------------------------------------
#First a naive model ----
model1_naive <- glm(data = brsp_est,
                      family = poisson(link = 'log'),
                      formula = BRSP.Estimate ~ Route.Type + Sagebrush.Cover +
                        Annual.Cover + Shrub.Cover.RCMAP + Road.Distance +
                        Elevation + TRI)
#View the model
summary(model1_naive)

#Plot the model
par(mfrow = c(2, 2))
plot(model1_naive )
par(mfrow = c(1, 1))

#Transform from a multiplicitive to an additive scale
output_exp <- exp(model1_naive)
class(output_exp)

#More specific model ----
model2_most_relv <- glm(data = sobs_count,
                        family = 'poisson',
                        formula = SOI.Count ~ Route.Type + Shrub.Cover.RCMAP + TRI)
#View the model
summary(model2_most_relv)

#Plot the model
par(mfrow = c(2, 2))
plot(model2_most_relv)
par(mfrow = c(1, 1))