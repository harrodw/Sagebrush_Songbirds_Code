#Practicing Bayesian modeling using my songbird data

#Clear Environments
rm(list = ls())

#add packages
library(jagsUI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)

#Prep ------------------------------------------------------------------------------------
#Add in Data
sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_count_covs.csv") %>% 
  dplyr::select(-X)
#view the data
glimpse(sobs) 

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/point_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#...and view
print(covs, n = Inf)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
soi <- "BRSP"
#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species  == soi) %>% 
  group_by(Full.Point.ID, Route.ID, Year, Visit, Date, Species) %>% 
  reframe(Full.Point.ID, Route.ID, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  tidyr::expand(nesting(Full.Point.ID, Route.ID, Year, 
                        Wind.Start, Visit, Date, Observer.ID))

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Full.Point.ID', 'Route.ID', 
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))
#...and view
glimpse(sobs_count)

#Join to route covariates
brsp_count <- sobs_count %>% 
  left_join(covs, by = c("Route.ID", "Full.Point.ID"))
#...and view
glimpse(brsp_count)

#Change necessary variables to scales and factors
brsp_count <- brsp_count %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  mutate(Burn.Sevarity = case_when(is.na(Burn.Sevarity) ~ 0,
                                   TRUE ~ Burn.Sevarity)) %>% 
  mutate_at(c("Aspect", "Burn.Sevarity", 
              "Fire.Name", "Observer.ID", "Route.ID"), factor) %>% 
  mutate(Burned = factor(Route.Type, levels = c("R", "B"))) %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown',
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown")))

#Fill in years since fire
for(i in 1:nrow(brsp_count)) {
  if(is.na(brsp_count$Years.Since.Fire[i])){
    brsp_count$Years.Since.Fire[i] <- rnorm(1, 100, 30)
  }
}

#...and view
glimpse(brsp_count)

#Create all observations of a single species
brsp <- sobs %>% 
  filter(Species == soi)
#... and view
glimpse(brsp)

#Plot comparisons --------------------------------------------------------------------

#Individual counts vs shrub cover
brsp_count %>% 
  ggplot(aes(x = Sagebrush.Cover, y = Count)) +
  geom_point() +
  geom_smooth()

#Build Models -------------------------------------------------------------------------------------------------------------

#Bayes model 1: Number of BRSP at each point as a function of fire characteristics ----

#The model
sink("bayes_files\\brsp_count")
cat("
  model{
    #Fixed effect Priors ----
    beta_burn ~ dnorm(0, 0.001) #Offset for burned or not
    beta_fire_y ~ dnorm(0, 0.001) #offset for years since fire
    beta_burn_sev ~ dnorm(0, 0.001) #offset for fire sevarity
    beta_elev ~ dnorm(0, 0.001) #offset for elevation
    
    
    sigma_route ~ dunif(0, 50) #between route variation
    sigma2_route <- pow(sigma_route, 2) 
    tau_route <- pow(sigma_route, -2)
    pd ~ dunif(0, 1) #probability of detection
    
    #Random intercept priors ---
    for(n in 1:n_routes){
      beta0[route[n]] ~ dnorm(0, tau_route)
    } #end random effects loop
    
    #State process Likelihood -----
    for(k in 1:K){ 
      lin_comb[k] <- beta0[route_visits[k]] +
                     beta_burn * burn_yn[k] + 
                     beta_fire_y * fire_y[k] * burn_yn[k] + 
                     beta_burn_sev * burn_sev[k] * burn_yn[k] +
                     beta_elev * elevation[k]
      #linear combination of predictors
      lambda[k] <- exp(lin_comb[k]) #log link
      n_brsp[k] ~ dpois(lambda[k]) 
    } 
    
    #Observation process Likelihood -----
     for(k in 1:K){
       obs_brsp[k] ~ dbin(pd, n_brsp[k])
     }
     
  }# end model
", fill = TRUE)
sink()

#Initial values
brsp_inits <- function(){list(
  beta_burn = rnorm(1, 0, 0.001),
  beta_fire_y = rnorm(1, 0, 0.001), 
  beta_burn_sev = rnorm(1, 0, 0.001), 
  beta_elev = rnorm(1, 0, 0.001), 
  beta_burn_elev = rnorm(1, 0, 0.001), 
  sigma_route = runif(1, 0, 50),
  pd = runif(1, 0.01, 0.99)
)}

#Bundle data
brsp_data <- list(obs_brsp = brsp_count$Count,
                  burn_yn = as.numeric(brsp_count$Burned) - 1,
                  fire_y = brsp_count$Years.Since.Fire,
                  burn_sev = as.numeric(brsp_count$Burn.Sevarity),
                  elevation = brsp_count$Elevation,
                  route = unique(as.numeric(brsp_count$Route.ID)),
                  route_visits = as.numeric(brsp_count$Route.ID),
                  n_routes = length(unique(brsp_count$Route.ID)),
                  K = nrow(brsp_count))

#parameters to save
brsp_params <- c("beta_0", "beta_burn", "beta_fire_y", "beta_burn_sev", 
                 "beta_elev", "beta_burn_elev", "pd",
                 "sigma2_route")

#MCMC settings
ni <- 30000
nt <- 5
nb <- 10000
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_count",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2)

#View model traceplots
par(mfrow = c(5, 5))
traceplot(brsp_fit)

#Bayes model 3: Number of BRSP at each point as a function of current site characteristics 
#with variation at the observation level---
#The model
sink("bayes_files\\brsp_count")
cat("
  model{
    #Fixed effect Priors ----
    
    #Abondance covariate priors
    beta_shb_cvr ~ dnorm(0, 0.001) #slope offset for shrub cover
    beta_shb_hgh ~ dnorm(0, 0.001) #slope offset for shrub height
    beta_sage ~ dnorm(0, 0.001) #slope offset for an interaction between sagebrush cover and shrub cover
    beta_annual ~ dnorm(0, 0.001) #slope offset for annual forb cover
    beta_tri ~ dnorm(0, 0.001) #slope offset for topographic ruggedness
    beta_elev ~ dnorm(0, 0.001) #offset for elevation
    
    #Site level random effect priors 
    sigma_route ~ dunif(0, 50) #between route variation
    sigma2_route <- pow(sigma_route, 2) 
    tau_route <- pow(sigma_route, -2)
    
    #Random intercept priors ---
    for(n in 1:n_routes){
      beta0_route[uqi_routes[n]] ~ dnorm(0, tau_route)
    } 
    
    #detection level random effect priors 
    pd ~ dunif(0, 1)

    #State process Likelihood -----
    for(k in 1:K){ 
      #linear combination of predictors
      lin_comb_abond[k] <- beta0_route[routes[k]] + 
                           beta_shb_cvr * shrub_cvr[k] + 
                           beta_shb_hgh * shrub_hgh[k] +
                           beta_sage * shrub_cvr[k] * sage_cvr[k] +
                           beta_annual * annual_cvr[k] +
                           beta_tri * tri[k] +
                           beta_elev * elevation[k]
      lambda[k] <- exp(lin_comb_abond[k])          #log link
      n_brsp[k] ~ dpois(lambda[k]) 
    } 
    
    #Observation process Likelihood -----
     for(k in 1:K){
       obs_brsp[k] ~ dbin(pd, n_brsp[k])
     }
     
  }# end model
", fill = TRUE)
sink()

#Initial values
brsp_inits <- function(){list(
  beta_shb_cvr = rnorm(1, 0, 0.001),
  beta_shb_hgh = rnorm(1, 0, 0.001), 
  beta_sage = rnorm(1, 0, 0.001), 
  beta_annual = rnorm(1, 0, 0.001), 
  beta_tri = rnorm(1, 0, 0.001),
  beta_elev = rnorm(1, 0, 0.001),
  sigma_route = runif(1, 0, 50),
  pd = runif(1, 0, 1),
)}

#Bundle data
brsp_data <- list(obs_brsp = brsp_count$Count,
                  shrub_cvr = brsp_count$Shrub.Cover - mean(brsp_count$Shrub.Cover),
                  shrub_hgh = brsp_count$Shrub.Height - mean( brsp_count$Shrub.Height),
                  sage_cvr = brsp_count$Sagebrush.Cover - mean(brsp_count$Sagebrush.Cover),
                  annual_cvr = brsp_count$Annual.Cover - mean(brsp_count$Annual.Cover),
                  tri = brsp_count$TRI - mean(brsp_count$Annual.Cover),
                  elevation = brsp_count$Elevation - mean(brsp_count$Elevation),
                  uqi_routes = unique(as.numeric(brsp_count$Route.ID)),
                  routes = as.numeric(brsp_count$Route.ID),
                  n_routes = length(unique(brsp_count$Route.ID)),
                  K = nrow(brsp_count))

#parameters to save
brsp_params <- c("beta_shb_cvr", "beta_shb_hgh", "beta_sage", 
                 "beta_annual", "beta_tri", "beta_elev", 
                 "sigma2_route", "beta0_route", "pd")

#MCMC settings
ni <- 30000
nt <- 3
nb <- 10000
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_count",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2)


#Bayes model 3: Number of BRSP at each point as a function of current site characteristics 
#with variation at the observation level---
#The model
sink("bayes_files\\brsp_count")
cat("
  model{
    #Fixed effect Priors ----
    
    #Abondance covariate priors
    beta_shb_cvr ~ dnorm(0, 0.001) #slope offset for shrub cover
    beta_sage ~ dnorm(0, 0.001) #slope offset for an interaction between sagebrush cover and shrub cover
    beta_tri ~ dnorm(0, 0.001) #slope offset for topographic ruggedness
    
    #Site level random effect priors 
    sigma_route ~ dunif(0, 50) #between route variation
    sigma2_route <- pow(sigma_route, 2) 
    tau_route <- pow(sigma_route, -2)
    
    #Random intercept priors ---
    for(n in 1:n_routes){
      beta0_route[uqi_routes[n]] ~ dnorm(0, tau_route)
    } 
    
    #detection level random effect priors 
    beta_wind ~ dnorm(0, 0.001)
    sigma_obs ~ dunif(0, 50) #between observer variation
    sigma2_obs <- pow(sigma_route, 2)
    tau_obs <- pow(sigma_route, -2)

    #Detection level random effects
    for(m in 1:observers){
        beta0_obs[uqi_obs[m]] ~ dnorm(0, tau_obs)
    }
    
    #State process Likelihood -----
    for(k in 1:K){ 
      #linear combination of predictors
      lin_comb_abond[k] <- beta0_route[routes[k]] + 
                           beta_shb_cvr * shrub_cvr[k] + 
                           beta_sage * shrub_cvr[k] * sage_cvr[k] +
                           beta_tri * tri[k]
      lambda[k] <- exp(lin_comb_abond[k])    #log link
      n_est[k] ~ dpois(lambda[k]) 
    } 
    
    #Total abondance in burned and unburned plots
    N_burn <- sum(n_est * burned)
    N_ref <- sum(n_est) - N_burn
    
    #Observation process Likelihood -----
     for(k in 1:K){
       lin_comb_pd[k] <- beta0_obs[obs[k]] + beta_wind * wind[k]
       pd[k] <- exp(lin_comb_pd[k]) / (1 + exp(lin_comb_pd[k])) #undo the logit link
       n_obs[k] ~ dbin(pd[k], n_est[k])
     }
     
  }# end model
", fill = TRUE)
sink()

#Initial values
brsp_inits <- function(){list(
  beta_shb_cvr = rnorm(1, 0, 0.001),
  beta_sage = rnorm(1, 0, 0.001), 
  beta_tri = rnorm(1, 0, 0.001),
  sigma_route = runif(1, 0, 50),
  sigma_obs = runif(1, 0, 50),
  beta_wind = rnorm(1, 0, 0.001),
  n_est = max(brsp_count$Count)
  #pd = runif(1, 0, 1)
)}

#Bundle data
brsp_data <- list(n_obs = brsp_count$Count,
                  shrub_cvr = brsp_count$Shrub.Cover,
                  sage_cvr = brsp_count$Sagebrush.Cover,
                  tri = brsp_count$TRI,
                  wind = as.numeric(brsp_count$Wind.Start),
                  burned = as.numeric(brsp_count$Burned) - 1,
                  obs = as.numeric(brsp_count$Observer.ID),
                  uqi_obs = unique(as.numeric(brsp_count$Observer.ID)),
                  observers = length(unique(brsp_count$Observer.ID)),
                  uqi_routes = unique(as.numeric(brsp_count$Route.ID)),
                  routes = as.numeric(brsp_count$Route.ID),
                  n_routes = length(unique(brsp_count$Route.ID)),
                  K = nrow(brsp_count))

#parameters to save
brsp_params <- c('n_est', "N_burn", "N_ref", "beta_shb_cvr", "beta_sage", 
                 "beta_annual", "beta_tri", "beta_elev", 'beta_wind', 
                 "sigma2_obs", "sigma2_route", "beta0_route", "beta0_obs")

#MCMC settings
ni <- 1000
nt <- 5
nb <- 100
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_count",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2)

#Model 4: Single parameter model for abundance with detections binned by distance
