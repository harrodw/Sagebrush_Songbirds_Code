#Frequentist models for practice and getting and idea of which variables woill be useful
#Will Harrod
#Created: 02/04/20024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)

#Remove objects
rm(list = ls())

#Add point count data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_count_covs.csv")) %>% 
        dplyr::select(-X) #Remove the column that excel generated
#and view the data
glimpse(sobs)

#Select a species of interest (SOI)
soi <- "WEME"

#Make a table of all possible route and visit combinations
possible_points <- sobs %>% 
  expand(nesting(Full.Point.ID, Year, Visit, Shrub.Cover, Shrub.Height,
                 Sagebrush.Cover, Annual.Cover, Fire.Distance, Aspect,
                 Bare.Ground.Cover, Burn.Sevarity, TRI, Elevation,
                 Road.Distance))
#View possib;e routes
glimpse(possible_points)

#Make a table of how many of the species of interest were observed on each route
count_0inf <- sobs %>% 
  filter(Species == soi) %>%
  group_by(Full.Point.ID, Year, Visit) %>% 
  reframe(Full.Point.ID, Year, Visit, SOI.Count = n()) %>% 
  distinct()
#Vieew soi count
glimpse(count_0inf)

#Join the two so I have the zero counts
sobs_count <- left_join(possible_points, count_0inf, 
          by = c('Full.Point.ID', 'Year', 'Visit')) %>% 
  mutate(SOI.Count = case_when(is.na(SOI.Count) ~ 0,
                               TRUE ~ SOI.Count)) 

#View the full count
glimpse(sobs_count)

#View the counts with covariates
glimpse(sobs_count)
print(sobs_count, n = 240)
hist(sobs_count$SOI.Count)

#test model to unserstand how a Bayesian framework can be used to model my data -----
#Make an object of how many ponts we had
#define the number 
points <- seq(from = 1, to = length(unique(sobs_count$Full.Point.ID)), by = 1)

#define storage objects
P_ak <- 0.8
P_dk <- 0.25
lambda <- 8
n_k <- rep(NA, length(points))

#simulate the process of coubnting birds from some population
for(i in 1:length(points)){
  N_k <- rneg(n = 1, lambda = lambda)
  n_k[i] <- rbinom(n = 1, size = N_k, p = P_ak * P_dk)
}
#compare this to what I actually saw
par(mfrow =c(1, 2))
hist(n_k)
hist(sobs_count$SOI.Count)
par(mfrow=c(1,1))

#Plot comparisons between SOI count and some covariates ------------------
#Scatterplot
sobs_count %>% 
  ggplot(aes(x = Bare.Ground.Cover, y = SOI.Count)) +
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
print(brsp_est, n = Inf)

#add covariates
covs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Join covariates to estimates
brsp_est <- brsp_est %>% 
  left_join(covs, by = 'Route.ID') %>% 
  mutate(Aspect = factor(Aspect)) %>% 
  mutate(Route.Type = factor(Route.Type, levels = c("R", "B"))) %>% 
#...and view
glimpse(brsp_est)

#Plot comparisons between BRSP estimates and some covariates ------------------
#Scatterplot
brsp_est %>%
  filter(!Route.ID %in% c('ID-C28')) %>% 
  ggplot(aes(x = Shrub.Cover.RCMAP, y = BRSP.Estimate, 
             fill = Route.Type)) +
  geom_point() +
  geom_smooth()

  #Boxplot for catagotical variables  
brsp_est %>% 
  ggplot(aes(x = Route.Type , y = BRSP.Estimate)) +
  geom_boxplot()

#I'm going to try removing ID-C-28 since it's such an outlier
brsp_est <- brsp_est %>% 
  filter(!Route.ID %in% c('ID-C28'))
#...and view
glimpse(brsp_est)

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
model2_most_relv <- glm(data = brsp_est,
                        family = 'poisson',
                        formula = BRSP.Estimate ~ Annual.Cover + 
                          Shrub.Cover.RCMAP + TRI +
                          shru])
#View the model
summary(model2_most_relv)

#Plot the model
par(mfrow = c(2, 2))
plot(model2_most_relv)
par(mfrow = c(1, 1))

#Transform outputs to an additive scale
model_out <- data.frame(Variable = c('intercept', 'shrub_offset', 
                                  'annual_offset', 'tri_offset'),
                        Value = c(exp(2.840468), exp(0.071835),
                                  exp(0.052171), exp(-0.103298)))
#...and view
model_out

#Notes 02/06/2024 this model is really bad. Both the detection function piece ----
#and the covariate piece do not fit well and could use a lot of work.
#this is for proof of concept 