#Practicing Bayesian modeling using my songbird data

#Clear Environments
rm(list = ls())

#add packages
library(jagsUI)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(forcats)

#Prep ------------------------------------------------------------------------------------
#Add in Data
sobs <- read.csv("Data//Outputs//sobs_data.csv") %>% 
  dplyr::select(-X)
#view the data
glimpse(sobs) 

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("Data//Outputs//point_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
soi <- "BRSP"

#define a truncation distance
trunc_dist <- 125

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species  == soi) %>%
  filter(Distance <= trunc_dist) %>% 
  group_by(Full.Point.ID, Route.ID, Year, Visit, Date, Species) %>% 
  reframe(Full.Point.ID, Route.ID, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  # filter(Year == "Y2") %>% 
  tidyr::expand(nesting(Full.Point.ID, Route.ID, Point.Time, Year, Sky.Start,
                        Wind.Start, Temp.Start, Visit, Date, Observer.ID))

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Full.Point.ID', 'Route.ID', 
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))
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
  mutate(Burned = factor(Route.Type, levels = c("R", "B"))) %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown',
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown', #change the wind data
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>%
  mutate(Sky.Start = factor(Sky.Start, levels = c("Clear", "Partly Cloudy", "Cloudy",
                                                  "Drizzle", "Fog or Smoke"))) %>%
  mutate_at(c("Aspect", "Burn.Sevarity", 
              "Fire.Name", "Observer.ID", "Route.ID"), factor) %>% 
  dplyr::select(-Species, -Route.Type, -Fire.Name, -Fire.Year) %>% 
  arrange(Year, Visit, Route.ID, Full.Point.ID)

# Fill in years since fire
for(i in 1:nrow(brsp_count)) {
  if(is.na(brsp_count$Years.Since.Fire[i])){
    brsp_count$Years.Since.Fire[i] <- floor(rnorm(1, 100, 20))
  }
}

#...and view
glimpse(brsp_count)

#Create all observations of a single species for the detection function
brsp <- sobs %>% 
  filter(Species == soi) %>% #only one species
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% # Same order as the others
  filter(Distance <= trunc_dist) %>% #Only close observations 
  mutate(Burned = factor(Route.Type, levels = c("R", "B"))) %>% 
  dplyr::select(Distance, Minute, How.Detected, 
         Route.ID, Full.Point.ID, Observer.ID, Year, Visit,
         Burned, Ord.Date, 
         # MAS, 
         Wind.Start) %>% 
  left_join(covs, by = c("Route.ID", "Full.Point.ID")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  dplyr::select(-Burn.Sevarity, -Perennial.Cover, - Fire.Distance,
         -Fire.Name, - Fire.Year, - Route.Type, - Aspect) %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))


#Fill in years since fire
for(i in 1:nrow(brsp)) {
  if(is.na(brsp$Years.Since.Fire[i])){
    brsp$Years.Since.Fire[i] <- as.integer(rnorm(1, 100, 20))
  }
}
  
#... and view
glimpse(brsp)

#Histogram of distances
brsp %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(col = "darkgray", fill = "lightblue")

#Histogram of Distance scaled by radius
brsp %>% 
  filter(Distance < trunc_dist) %>%
  mutate(Dist.Bin = round_any(Distance, 10, floor)) %>% 
  mutate(Dist.Bin = case_when(Dist.Bin == 0 ~ 1, TRUE ~ Dist.Bin)) %>% 
  group_by(Dist.Bin) %>% 
  reframe(Dist.Bin, Count = n(), Scaled.Count = n()/ Dist.Bin) %>%
  distinct() %>% 
  mutate(Scaled.Count = Scaled.Count / max(Scaled.Count)) %>% 
  ggplot(aes(x = Dist.Bin, y = Scaled.Count)) +
  geom_col(fill = "lightblue") +
  stat_smooth()

#I'll start with five 25m bins
bin_size <- 25

#Add this to the data 
brsp <- brsp %>% 
  mutate(Dist.Bin = round_any(Distance, bin_size, floor)) %>% 
  mutate(Dist.Bin = case_when(Dist.Bin == 0 ~ 1,
                              Dist.Bin == 125 ~ 100,
                              TRUE ~ Dist.Bin))%>% 
  mutate(Dist.Bin.Midpoint = case_when(Dist.Bin == 1 ~ 12,
                                   TRUE ~ Dist.Bin + floor(bin_size/2)))
#...and view
glimpse(brsp)

#Plot of detentions by time
brsp %>% 
  mutate(Time.Bin = as.factor(case_when(Minute %in% c(1, 2) ~ "1",
                              Minute %in% c(3, 4) ~ "2",
                              Minute %in% c(5, 6) ~ "3"))) %>% 
  group_by(Time.Bin) %>% 
  reframe(Time.Bin, Count = n()) %>% 
  distinct() %>% 
  ggplot(aes(x = Time.Bin, y = Count)) +
  geom_col()

#Looks good, I'm adding it to the data
brsp_obs <- brsp %>% 
  mutate(Time.Bin = as.factor(case_when(Minute %in% c(1, 2) ~ "1",
                                        Minute %in% c(3, 4) ~ "2",
                                        Minute %in% c(5, 6) ~ "3")))
#...and view
glimpse(brsp_obs)
glimpse(brsp_count)

#Prep the data for N-mixture model
nmix_dat <- brsp_count %>% 
  filter(Year == "Y3") %>%
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>%
  mutate(Wind.Start = as.numeric(Wind.Start)) %>% 
  dplyr::select(Full.Point.ID, Route.ID, Visit, Count, Shrub.Cover, Wind.Start, Burned) %>%  #Pull out only the variables of interest
  pivot_wider(names_from = Visit, values_from = c('Count', 'Shrub.Cover', 'Wind.Start', "Burned")) %>%  #Pivot the data so it can be turned into a matrix
  drop_na(Count_V1, Count_V2) %>% #Remove the sites that were only visited once
  mutate(Burned = factor(Burned_V1, levels = c("R", "B"))) %>% 
  mutate(Burned = as.numeric(Burned)) %>% 
  dplyr::select(-Shrub.Cover_V2, -Burned_V1, -Burned_V2) 

#...and view
glimpse(nmix_dat)

#Build Models ##############################################################################

#Bayes model 1: Number of BRSP at each point as a function of fire characteristics ----

#The model
sink("bayes_files\\brsp_count")
cat("
  model{
    #Fixed effect Priors ----
    beta_burn ~ dnorm(0, 0.001) #Offset for burned or not
    beta_fire_y ~ dnorm(0, 0.001) #offset for years since fire
    beta_burn_elev ~ dnorm(0, 0.001) #offset for fire sevarity
    beta_elev ~ dnorm(0, 0.001) #offset for elevation

    #random effect hyperparameters
    sigma_route ~ dunif(0, 50) #between route variation
    sigma2_route <- pow(sigma_route, 2)
    tau_route <- pow(sigma2_route, -2)
    pd ~ dunif(0, 1) #the model calculates the probability of detection 

    #Random intercept priors ---
    for(n in 1:n_routes){
      beta0[route[n]] ~ dnorm(0, tau_route)
    } #end random effects loop

    #State process Likelihood ------------------
    for(k in 1:K){
      lin_comb[k] <- beta0[route_visits[k]] + #Random intercept for route visit
                     beta_burn * burn_yn[k]  #Fixed effect of burned vs ref
                     + beta_fire_y * fire_y[k] * burn_yn[k]  #Fixed effect of time since fire
                     + beta_elev * elevation[k] #Fixed effect of elevation 
                     + beta_burn_elev * fire_y[k] * elevation[k] * burn_yn[k] #interaction between fire year and elevation
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
  beta_elev = rnorm(1, 0, 0.001),
  beta_burn_elev = rnorm(1, 0, 0.001),
  sigma_route = runif(1, 0, 50),
  pd = runif(1, 0.001, 0.999)
)}

#Bundle data
brsp_data <- list(obs_brsp = brsp_count$Count,
                  burn_yn = as.numeric(brsp_count$Burned) - 1,
                  fire_y = brsp_count$Years.Since.Fire,
                  elevation = brsp_count$Elevation,
                  route = unique(as.numeric(brsp_count$Route.ID)),
                  route_visits = as.numeric(brsp_count$Route.ID),
                  n_routes = length(unique  (brsp_count$Route.ID)),
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
    beta_shb_cvr ~ dnorm(0, 0.001) #offset for shrub cover
    beta_shb_hgh ~ dnorm(0, 0.001) #offset for shrub height
    beta_sage ~ dnorm(0, 0.001) #offset for an interaction between sagebrush cover and shrub cover
    beta_annual ~ dnorm(0, 0.001) #offset for annual forb cover
    beta_tri ~ dnorm(0, 0.001) #offset for topographic ruggedness
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
      lin_comb_abond[k] <- beta0_route[routes[k]] 
                           + beta_shb_cvr * shrub_cvr[k] 
                           + beta_shb_hgh * shrub_hgh[k] 
                           + beta_sage * shrub_cvr[k] * sage_cvr[k] 
                           + beta_annual * annual_cvr[k] 
                           + beta_tri * tri[k] 
                           + beta_elev * elevation[k]
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
  pd = runif(1, 0, 1)
)}

#Bundle data
brsp_data <- list(obs_brsp = brsp_count$Count,
                  shrub_cvr = brsp_count$Shrub.Cover - mean(brsp_count$Shrub.Cover),
                  shrub_hgh = brsp_count$Shrub.Height - mean(brsp_count$Shrub.Height),
                  sage_cvr = brsp_count$Sagebrush.Cover - mean(brsp_count$Sagebrush.Cover),
                  annual_cvr = brsp_count$Annual.Cover - mean(brsp_count$Annual.Cover),
                  tri = brsp_count$TRI - mean(brsp_count$TRI),
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

########################################################################################################
#Model 4: ZIP N-Mixture ----------------------------------------------------------------------------

#View the new data
head(nmix_dat)
glimpse(nmix_dat)

#The model
sink("bayes_files\\brsp_count")
cat("
  model{
    #Fixed effect Priors ----
    beta_burn ~ dnorm(0, 0.001) #Offset for burned or not
    random effect hyperparameters
    sigma_route ~ dunif(0, 50)
    sigma2_route <- pow(sigma_route, 2)
    tau_route <- pow(sigma_route, -2)
    beta_p_wind ~ dnorm(0, 0.001) 
    beta0_a ~ dnorm(0, 0.001)
    beta0_p ~ dnorm(0, 0.001)
    omega ~ dunif(0, 1)
     
    #Random intercept priors ---
    for(n in 1:n_routes){
      beta0_route[n] ~ dnorm(0, tau_route)
    }
    
    #State process Likelihood -----
    for(k in 1:n_points){ 
      
      z[k] ~ dbern(omega) #Zero inflation Bernoulli trial
      
      lin_comb[k] <- beta0_route[routes[k]]  + beta_burn * burned[k] #linear combination of predictors
      lambda[k] <- exp(lin_comb[k]) #log link
      lambda_eff[k] <- z[k] * lambda[k] + 0.1 #Zero inflate lambda
      n_brsp[k] ~ dpois(lambda_eff[k]) 
     
    
    #Observation process Likelihood -----
     for(j in 1:n_visits){
       obs_brsp[k, j] ~ dbin(p[k, j], n_brsp[k])
       p[k, j] <- exp(lp[k, j]) /(1+ exp(lp[k, j]))
       lp[k, j] <- beta0_p + beta_p_wind * wind[k, j] 
       
       # # Assess model fit using Chi-squared discrepancy
       # # Compute fit statistic E for observed data
       # eval[k,j] <- p[k,j]*n_brsp[k]
       # E[k,j] <- pow((obs_brsp[k,j] - eval[k,j]),2)/(eval[k,j] + 0.5)
       # 
       # # Generate replicate data and compute fit stats for them
       # y.new[k,j] ~ dbin(p[k,j], n_brsp[k])
       # E.new[k,j] <-pow((y.new[k,j]- eval[k,j]),2)/
       # (eval[k,j] + 0.5)
       
     } #j
    } #k
    
    # Derived quantities
    
    totalN <- sum(n_brsp[])
    # fit <- sum(E[,])
    # fit.new <- sum(E.new[,])
    
  }# end model
", fill = TRUE)
sink()

#Initial values
brsp_inits <- function(){list(
  beta_burn = rnorm(1, 0, 0.1),
  beta_p_wind = rnorm(1, 0, 0.1),
  beta0_p = rnorm(1, 0, 0.1),
  beta0_a = rnorm(1, 0, 0.1),
  omega = runif(1, 0, 1),
  n_brsp = apply(as.matrix(nmix_dat[, 2:3]), 1, max) 
)}

#Bundle data
brsp_data <- list(obs_brsp = as.matrix(nmix_dat[, 3:4]),
                  burned = as.numeric(nmix_dat$Burned) - 1,
                  wind = as.matrix(nmix_dat[, 6:7]),
                  routes = as.numeric(as.factor(nmix_dat$Route.ID)),
                  n_visits = 2,
                  n_routes = length(unique(nmix_dat$Route.ID)),
                  n_points = nrow(nmix_dat))

dat_mtrx <- as.numeric(as.factor(as.matrix(nmix_dat[, 5:6])))

#parameters to save
brsp_params <- c("beta_0", "beta_burn", "beta_p_wind", "n_brsp", "fit")

#MCMC settings
ni <- 5000
nt <- 3
nb <- 1000
nc <- 3

#Send the data to BUGS
brsp_fit_0inf <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_count",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model traceplots
par(mfrow = c(5, 5))
# traceplot(brsp_fit_0inf)

print(brsp_fit_0inf$summary)

#View model output
print(brsp_fit, digits = 2)

#Plot evaluation off fit
plot(brsp_fit_0inf$sims, )

# Evaluation of fit
plot(brsp_fit_0inf$sims.list$fit, brsp_fit_0inf$sims.list$fit.new, main="", xlab=
       "Discrepancy actual data",ylab="Discrepancy replicate data",
     frame.plot=FALSE)
abline(0, 1,lwd=2,col="black")

########################################################################################################
#Model 5: Random effects N-Mixture ----------------------------------------------------------------------------

#View the new data
head(nmix_dat)
glimpse(nmix_dat)

# Random Effects ###############################################################

#The model
sink("brsp_count_random.txt")
cat(
"
  model{
    #Fixed effect Priors ----
    beta_shrub ~ dnorm(0, 0.001) #Offset for shrub cover
    beta_p_wind ~ dnorm(0, 0.001) #offset for average wind speed
    
    #Random effect hyperparameters
    tau.lam <- pow(sd.lam, -2)
    sd.lam ~ dunif(0,3)
    tau.p <- pow(sd.p, -2)
    sd.p ~ dunif(0,3)

    # #Random intercept priors ---
    for(i in 1:n_routes){
      beta0[i] ~ dnorm(0, tau.lam) # Abundance noise
       for(j in 1:n_visits){  # Observation noise
       beta0_p[i,j] ~ dnorm(0, tau.p)
      }
    }
    
   #State process Likelihood -----
    for(i in 1:n_routes){ 
      lin_comb[i] <- beta0[i] + beta_shrub * shrub[i] 
      #linear combination of predictors
      lambda[i] <- exp(lin_comb[i]) #log link
      n_brsp[i] ~ dpois(lambda[i]) 
    
    #Observation process Likelihood -----
     for(j in 1:n_visits){
       obs_brsp[i, j] ~ dbin(p[i, j], n_brsp[i]) #actual likelihood
       p[i, j] <- exp(lp[i, j]) /(1+ exp(lp[i, j])) #logit link
       lp[i, j] <- beta0_p[i,j] + beta_p_wind * wind[i, j] #linear combonation  of predictors
       
       # Assess model fit using Chi-squared discrepancy
       # Compute fit statistic E for observed data
       eval[i,j] <- p[i,j]*n_brsp[i]
       #chi square test statistic
       E[i,j] <- pow((obs_brsp[i,j] - eval[i,j]),2)/(eval[i,j] + 0.5)
       
       # Generate replicate data and compute fit stats for them
       y.new[i,j]~dbin(p[i,j],n_brsp[i])
       #Chi sqare test statistic
       E.new[i,j]<-pow((y.new[i,j]- eval[i,j]),2)/(eval[i,j] + 0.5)
       
     } #j
    } #i
    
    # Derived quantities
    
    totalN <- sum(n_brsp[]) #Total population size
    fit <- sum(E[,]) #true data fit
    fit.new <- sum(E.new[,]) #simulated data fit
    
  }# end model
"
, fill = TRUE)
sink()

#Initial values
brsp_inits <- function(){list(
  beta_shrub = rnorm(1, 0, 0.001),
  beta_p_wind= rnorm(1, 0, 0.001),
  # beta0_p = rnorm(1, 0, 0.001), # We can add inits for all betas,
  # but right now, I'm just letting R guess
  n_brsp = apply(as.matrix(nmix_dat[, 2:3]), 1, max) + 1
)}

#Bundle data
brsp_data <- list(obs_brsp = as.matrix(nmix_dat[, 2:3]),
                  shrub = nmix_dat$Shrub.Cover_V1,
                  wind = as.matrix(nmix_dat[, 5:6]),
                  n_visits = 2,
                  n_routes = nrow(nmix_dat)
)

#parameters to save
brsp_params <- c("beta0", "beta_shrub", "sd.lam", "sd.p", "totalN", "beta_p_wind",
                 "fit", "fit.new")

#MCMC settings 
ni <- 10000
nt <- 5
nb <- 1000
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "brsp_count_random.txt",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2)

#View model traceplots
par(mfrow = c(5, 5))
traceplot(brsp_fit)

# Evaluation of fit
par(mfrow = c(1, 1))
plot(brsp_fit$sims.list$fit,brsp_fit$sims.list$fit.new,main="",xlab=
       "Discrepancy actual data",ylab="Discrepancy replicate data",
     frame.plot=FALSE)
abline(0, 1,lwd=2,col="black")

##############################################################################################################
#isolate just a single visit for counts
count_v1 <- brsp_count %>% 
  filter(Year == "Y3" & Visit == "V1") %>% 
  arrange(Visit.ID) %>% 
  mutate(Visit.ID = as.factor(Visit.ID)) %>% 
  mutate(Visit.ID.num = as.numeric(Visit.ID))
#...and view
glimpse(count_v1)

#Pull out just the visit ID
visit_ids <- count_v1 %>% 
  select(Visit.ID, Visit.ID.num)
#...and view
glimpse(visit_ids)

#only Y3 V1 observations
obs_v1 <- brsp_obs %>% 
  mutate(Dist.Bin = case_when(Dist.Bin == 1 ~ 1,
                              Dist.Bin == 25 ~ 2,
                              Dist.Bin == 50 ~ 3,
                              Dist.Bin == 75 ~ 4,
                              Dist.Bin == 100 ~ 5)) %>% 
  filter(Year == "Y3" & Visit == "V1") %>% 
  left_join(visit_ids, by = "Visit.ID")
  
  #...and view
glimpse(obs_v1)

#Model 6: Single parameter model for abundance with detentions binned by distance with one visit ---------------------
sink("bayes_files\\brsp_model_distance")
cat(
"
  model{
    #Priors #################
    #Detection level priors
    sigma_observer ~ dunif(0, 500)   #between observer variation in detection
    sigma2_observer <- pow(sigma_observer, 2)
    tau_observer <- pow(sigma_observer, -2)
    beta_pd_wind ~ dnorm(0, 0.001) #offset in detection probability for minutes after sunrise               
   
    #Random intercepts for observer
    for(o in 1: n_observers){
      alpha_observer[o] ~ dnorm(0, tau_observer)
    }
    
    #Abundance level priors ----------
    sigma_route ~ dunif(0, 50) #Between route variation in abundance
    sigma2_route <- pow(sigma_route, 2)
    tau_route <- pow(sigma_route, -2)
    beta_lambda_burn ~ dnorm(0, 0.001) #difference in abundance between burned and ref routes
    
    #Random intercept for route ID
    for(j in 1: n_routes){
      alpha_route[j] ~ dnorm(0, tau_route) 
    }
    
    #Likelihood ##############################################################
    
    ##DETECTION PROBABILITY ------------------------------
    #Linear combination of covariates
    for(k in 1:n_surveys){
      #Log linear combination of predictors for detection probability
      ln_sigma[k] <- alpha_observer[observers[k]] + beta_pd_wind * wind[k]
      #Log link
      sigma[k] <- exp(ln_sigma[k])

      #Distance sampling detection probability estimation -----------------------
      # Using summation technique - Pr(p of x)=exp(-x^2/2*sigma^2)*f(x)
      for(b in 1:n_bins){
        # half-normal detection function - first half of eq.,
        ln_g[b, k] <- (-bin_midpoints[b]*bin_midpoints[b]) / (2*sigma[k]*sigma[k])
        #Log link
        g[b, k] <- exp(ln_g[b, k])
        # this is f(x), the scaled radial density function
        f[b, k]<-  (2*bin_midpoints[b]*delta) / (max_dist*max_dist)

        #this is the product Pr(detect)*Pr(distribution)
        pi_pd[b, k]<- g[b, k]*f[b, k]
        #standardizing based on overall capture probability - conditional formulation
        pi_pd_c[b, k] <- pi_pd[b, k]/pd[k]
      } #b

      #probability of detection is the sum of all rectangular areas
      pd[k] <- sum(pi_pd[,k])
    }#k

    ######## Observation-level model ----------------------------------
    for(i in 1:n_observations){
      #single trial with categorical distribution linking distance class to survey point
      dist_class[i] ~ dcat(pi_pd_c[,surveys_obs[i]])
    } #i

    ################# Abundance level model  -----------------------------
    for(k in 1:n_surveys){
      # Abundance
      #Linear combination of abundance predictors
      lin_comb_lambda <- alpha_route[surveys_count[k]] + beta_lambda_burn * burned[k]
      #Log link
      lambda <- exp(lin_comb_lambda)
      #Poisson process of birds being distributed across the landscape
      N[k] ~ dpois(lambda)
      
      #Detection process likelihood
      n[k] ~ dbin(pd[k], N[k])
    } #k
  } #end model
"
    , fill = TRUE)


#Initial values
brsp_inits <- function(){list(
  sigma_route = runif(1, 0, 50),
  sigma_observer = runif(1, 0, 50),
  beta_lambda_burn = rnorm(1, 0, 0.001),
  beta_pd_wind = rnorm(1, 0, 0.001)
)}

#Parameters to save
brsp_params <- c("pd", "beta_lambda_burn", "beta_pd_wind", "N")

#Bundle data
brsp_data <- list(
  #Parameters
  n = count_v1$Count,
  routes = as.numeric(as.factor(count_v1$Route.ID)),
  surveys_count = count_v1$Visit.ID.num,
  surveys_obs = obs_v1$Visit.ID.num,
  observers = as.numeric(as.factor(count_v1$Observer.ID)),
  burned = as.numeric(count_v1$Burned) -1,
  dist_class = obs_v1$Dist.Bin,
  bin_midpoints = sort(unique(obs_v1$Dist.Bin.Midpoint)),
  wind = as.numeric(count_v1$Wind.Start),
  delta = 25,
  max_dist = 125,
  
  #Length Objects
  n_surveys = length(count_v1$Visit.ID),               #k
  n_routes = length(unique(count_v1$Route.ID)),        #n
  n_observations = nrow(obs_v1),                       #i
  n_observers = length(unique(count_v1$Observer.ID)),  #o
  n_bins = length(unique(obs_v1$Dist.Bin))             #b
)

#MCMC settings
ni <- 1000
nt <- 5
nb <- 100
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_model_distance",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2)

