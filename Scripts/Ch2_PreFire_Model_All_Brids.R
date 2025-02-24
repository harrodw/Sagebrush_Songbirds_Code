#----------------------------------------------------------------
# Will Harrod
# Hierarchical distance sampling for sagebrush songbird point count data
# September 2024
# 
# Code based on open source code from: 
# Amundson et al. 2014
# Kery et al. 2024, 
# Van Lanen et al. 2023, 
#---------------------------------------------------------------

# Add packages
library(nimble)
library(tidyverse)
library(MCMCvis)

#clear environments
rm(list = ls())
# dev.off()

################################################################################
# 1.0) Data Prep  ################################################################
################################################################################

# 1.1) Read in data ############################################################

# List of Species 
all_species <- c("BRSP", 
                 "SATH", 
                 "GTTO",
                 "VESP", 
                 "WEME", 
                 "HOLA")

# Loop over all species 
# for(s in 1:length(all_species)){

# Pick a species to model
# model_species <- all_species[s]
model_species <- all_species[1]

# Add in count data from local drive
# Two grids (ID-C11 and ID-C22) were missing their Y1V1 survey
# Counts/weather were estimated based on the mode for other visits to those grids
sobs_counts_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Grid_Counts.csv")) %>%
  tibble() %>%
  select(-X)
#view the counts
glimpse(sobs_counts_temp)

# Add in the observation data from the local drive
sobs_observations_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Observations.csv")) %>% 
  select(-X)
# View the observations
glimpse(sobs_observations_temp)

# Add pre fire covariates from the local drive
# Aspect == -1 means that the area is flat other classes start at 1=NE clockwise
# Fire Distance == 1000000 mean that the area is outside of a fire
# Burn sevarity == 0 means that the area did not burn
# Burn sevarity == -1 means that no data was available 
# Fire year == 1800 means there are no recorded fires in the area
covs <- tibble(read.csv("Data/Outputs/pre_fire_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# # or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/pre_fire_covs.csv) %>%
#   dplyr::select(-X) %>%
#   tibble()

# 1.2) Prepare the count level data ################################################################

# Change necessary variables to scales and factors
sobs_counts_temp2 <- sobs_counts_temp %>%
  # Add covariates
  left_join(covs, by = c("Grid.ID", "Grid.Type")) %>% 
  # Sort the data
  arrange(Visit.ID, Grid.ID) %>% 
  mutate(
         # Time since Fire
         Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID", "Year"), factor) %>% 
  # Switch grid type to a binary numeric
  mutate(Burned = case_when(Grid.Type == "R" ~ 0,
                            Grid.Type == "B" ~ 1)) %>% 
  # Remove columns that are no longer needed
  dplyr::select(-Grid.Type) 


#...and view
glimpse(sobs_counts_temp2)

# Isolate the grids with pre fire data
fire_covs <- sobs_counts_temp2 %>% 
  # filter(Burned == 1) %>% 
  select(Grid.ID, Burned, Years.Since.Fire, Shrub.Cover.PF, PFG.Cover.PF,
         ln.AFG.Cover.PF, ln.Tree.Cover.PF, BG.Cover.PF) %>% 
  distinct(Grid.ID, Burned, Years.Since.Fire, Shrub.Cover.PF, PFG.Cover.PF,
           ln.AFG.Cover.PF, ln.Tree.Cover.PF, BG.Cover.PF) %>% 
  arrange(Grid.ID)
# View
glimpse(fire_covs)
print(fire_covs, n = Inf)

# Mean values for each pre-fire covariate on burn grids
mean_fyear <- mean(fire_covs$Years.Since.Fire)
mean_shrub_cvr <- mean(fire_covs$Shrub.Cover.PF)
mean_pfg_cvr <- mean(fire_covs$PFG.Cover.PF)
mean_afg_cvr <- mean(fire_covs$ln.AFG.Cover.PF)
mean_tree_cvr <- mean(fire_covs$ln.Tree.Cover.PF)
mean_bg_cvr <- mean(fire_covs$BG.Cover.PF)

# Standard deviation values for each covariate on burn grids
sd_fyear <- sd(fire_covs$Years.Since.Fire)
sd_shrub_cvr <- sd(fire_covs$Shrub.Cover.PF)
sd_pfg_cvr <- sd(fire_covs$PFG.Cover.PF)
sd_afg_cvr <- sd(fire_covs$ln.AFG.Cover.PF)
sd_tree_cvr <- sd(fire_covs$ln.Tree.Cover.PF)
sd_bg_cvr <- sd(fire_covs$BG.Cover.PF)

# Scale the other covariates
sobs_counts <- sobs_counts_temp2 %>%
  mutate(
    # Manually scale pre-fire covariates
    Years.Since.Fire =(Years.Since.Fire - mean_fyear) / sd_fyear,
    Shrub.Cover.PF = (Shrub.Cover.PF - mean_shrub_cvr) / sd_shrub_cvr,
    PFG.Cover.PF = (PFG.Cover.PF - mean_pfg_cvr) / sd_pfg_cvr,
    ln.AFG.Cover.PF = (ln.AFG.Cover.PF - mean_afg_cvr) / sd_afg_cvr,
    ln.Tree.Cover.PF = (ln.Tree.Cover.PF - mean_tree_cvr) / sd_tree_cvr,
    BG.Cover.PF = (BG.Cover.PF - mean_bg_cvr) / sd_bg_cvr, 
    # Automatically scale others
    Elevation.scl = scale(Elevation)[,1],
    Mean.MAS = scale(Mean.MAS)[,1],   
    Ord.Date = scale(Ord.Date)[,1]    
  )
#...and view
glimpse(sobs_counts)

# 1.3) Prepare the observation level data ################################################################

# Make sure the same numeric values for factors are shaped between the two datasets
sobs_counts <- sobs_counts %>% 
  arrange(Year, Visit.ID, Grid.ID) %>% 
  mutate(Grid.ID.num = as.numeric(Grid.ID),
         Year.num = as.numeric(Year),
         Visit.ID.num = as.numeric(Visit.ID),
         Observer.ID.num = as.numeric(Observer.ID)) 

# View who each observer is
sobs_counts %>% 
  distinct(Observer.ID, Observer.ID.num, Observer.Experience) %>% 
  arrange(Observer.ID.num) %>% 
  print(n = Inf)

# Pull out the covariates that need to be shared across counts and observations
point_ids <- sobs_counts %>% 
  mutate_at(c("Grid.ID", "Year", "Visit.ID"), as.character) %>% 
  dplyr::select(Grid.ID, Grid.ID.num, Year, Year.num, Visit.ID, Visit.ID.num, Observer.ID.num)
#...and view
glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
sobs_observations <- sobs_observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Year", "Visit.ID"))

# View Counts
glimpse(sobs_counts)

# View observations
glimpse(sobs_observations)

# Plot counts against a covaariate
# sobs_counts %>%
#   filter(Burned == 1) %>%
#   ggplot(aes(x = AFG.Cover.PF, y = Count)) +
#   # geom_boxplot()
#   geom_smooth(method = "lm", col = "aquamarine4", fill = "aquamarine3") +
#   geom_jitter(col = "aquamarine4") +
#   # geom_histogram(col = "aquamarine4", fill = "aquamarine3") +
#   theme_bw()

# 1.4) prepare objects for NIMBLE ################################################################

# Define a truncation distance (km)
trunc_dist <- 0.125

# Matrix dimentions
nrows <- length(unique(sobs_counts$Grid.ID.num))  # Number of survey grids
ncols <- length(unique(sobs_counts$Visit.ID.num)) # Times each grid was visited

# Storage Matrices 
count_mat <- matrix(NA, nrow = nrows, ncol = ncols) # Storage matrix of observations by visit
year_mat <- matrix(NA, nrow = nrows, ncol = ncols)  # Storage matrix of years by visit
time_mat <- matrix(NA, nrow = nrows, ncol = ncols)  # Storage matrix of survey times by visit
date_mat <- matrix(NA, nrow = nrows, ncol = ncols)  # Storage matrix of survey dates by visit
area_mat <- matrix(NA, nrow = nrows, ncol = ncols)  # Storage matrix for the proportion of points visited during each survey
obsv_mat <- matrix(NA, nrow = nrows, ncol = ncols)  # Storage matrix for who conducted each survey
exp_mat <- matrix(NA, nrow = nrows, ncol = ncols)   # Storage matrix for observer experience
fyear_mat <- matrix(NA, nrow = nrows, ncol = ncols) # Storage matrix for years since fire during each survey

# Fill in the matrix
for(j in 1:nrow(count_mat)){
  # Filter for a specific grid
  count_visit <- sobs_counts %>% 
    arrange(Visit.ID.num) %>% 
    filter(Grid.ID.num == j)
  # Assign values to each row
  count_mat[j,] <- count_visit$Count
  year_mat[j,] <- count_visit$Year
  time_mat[j,] <- count_visit$Mean.MAS
  date_mat[j,] <- count_visit$Ord.Date
  # Area surveyd din km^2
  area_mat[j,] <- pi * count_visit$n.Points * trunc_dist^2  # Area surveyed in km^2
  obsv_mat[j,] <- count_visit$Observer.ID.num
  fyear_mat[j,] <- count_visit$Years.Since.Fire
}

# Loop sizes 
ngrids <- length(unique(sobs_counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(sobs_observations)                            # Number of individuals detected 
nobsv <- length(unique(sobs_counts$Observer.ID.num))       # Number of unique observers
nbins <- length(unique(sobs_observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(sobs_observations$Time.Interval))   # Number of time intervals
nvst <- length(unique(sobs_counts$Visit.ID))               # Number of visits in each year
nyears <- length(unique(sobs_counts$Year.num))             # Number of years we surveyed

# Observation Level data 
midpt <- sort(unique(sobs_observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
observers <- obsv_mat                                      # Random effect for observer associated with each survey
obs_visit <- sobs_observations$Visit.ID.num                # During which visit did each observation take place
obs_grid <- sobs_observations$Grid.ID.num                  # In which grid did each observation take place
dclass <- sobs_observations$Dist.Bin                       # Distance class of each observation
delta <- trunc_dist / nbins                                # Bin size

# Availability date
tint <- sobs_observations$Time.Interval                    # Time interval for each observation 
time <- time_mat                                           # Matrix of scaled time after sunrise
day <- date_mat                                            # Matrix of scaled dates

# Grid level data
area <- area_mat                                           # Proportion of points surveyed       
n_dct <- count_mat                                         # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                          # Matrix of year numbers
burned <- sobs_counts$Burned[1:ngrids]                     # Whether or not each grid burned
elevation <- sobs_counts$Elevation.scl[1:ngrids]           # Elevation on each grid
shrub_cvr <- sobs_counts$Shrub.Cover.scl[1:ngrids]         # Percent shrub cover on each grid
pern_cvr <- sobs_counts$Perennial.Cover.scl[1:ngrids]      # Percent perennial cover on each grid
fire_year <- fyear_mat                                     # How long since the most recent fire in each grid
shrub_cvr <- sobs_counts$Shrub.Cover.PF[1:ngrids]          # Percent shrub cover
pfg_cvr <- sobs_counts$PFG.Cover.PF[1:ngrids]              # Percent perennial forb and grass cover
afg_cvr <- sobs_counts$ln.AFG.Cover.PF[1:ngrids]           # Percent annual grass cover

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Model definition ################################################################
# Add random noise in the detection function intercepts
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sobs_model_code <- nimbleCode({
  # Priors for all parameters 
  # ------------------------------------------------------------------
  
  # Parameters in the availability component of the detection model
  gamma0 ~ dnorm(0, sd = 3)            # Mean availability
  gamma_date ~ dnorm(0, sd = 1.5)      # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 1.5)     # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 1.5)      # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 1.5)     # Effect of time of day on singing rate (quadratic)
  
  # Effect of observer experience (Can't be greater than 0)
  for(o in 1:nobsv){ # (nobsv = 17)
    alpha0_obsv[o] ~ T(dnorm(-2, sd = 3), -9, 0) # Prior mean centered on approximately sigma = exp(-2) = 0.125km
  } # End loop through observers
  
  # Parameters on the abundance component of the model

  # Fixed effects on abundance
  beta0 ~ dnorm(0, sd = 3)             # Abundance intercept
  beta_burn ~ dnorm(0, sd = 3)         # Effect of fire
  beta_elv ~  dnorm(0, sd = 1.5)       # Effect of elevation
  beta_fyear ~ dnorm(0, sd = 1.5)      # Effect of years since fire on burned grids
  beta_shrub ~ dnorm(0, sd = 1.5)      # Effect of shrub cover
  beta_pfg ~ dnorm(0, sd = 1.5)        # Effect of Perennial Cover
  beta_afg ~ dnorm(0, sd = 1.5)        # Effect of annual grass cover

  # Sd in yearly abundance hyperparameter
  sd_eps_year ~ dgamma(shape = 0.5, scale = 0.5) # Random effect on abundance hyperparameter for each year

  # Random noise among the other years
  for(y in 2:nyears){ # nyears = 3. Force the first year (2022) to be the intercept
    eps_year[y] ~ dnorm(0, sd = sd_eps_year)
  } # end loop over years
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood
  
  # Iterate over all survey grids
  for(j in 1:ngrids){
    
    # Zero-inflation component on abundance
    psi[j] ~ dunif(0.0001, 0.9999)                                    # Occupancy probability
    present[j] ~ dbern(psi[j])                                        # Number of grids where that individual can be present

    # Iterate over all of the visits to each survey grid 
    for(k in 1:nvst){ 
      
      ### Imperfect availability portion of the model ###
      
      # Construction of the cell probabilities for the nints time intervals
      for (t in 1:nints){
        pi_pa[j, k, t] <- phi[j, k] * (1 - phi[j, k])^(t - 1)  # Availability cell probability
        pi_pa_c[j, k, t] <- pi_pa[j, k, t] / p_a[j, k]         # Proportion of availability probability in each time interval class
      }
      # Rectangular integral approx. of integral that yields the Pr(available)
      p_a[j, k] <- sum(pi_pa[j, k, ])
      
      ### Imperfect detection portion of the model ###
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[j, k, b]) <- -midpt[b] * midpt[b] / (2 * sigma[j, k]^2) # Half-normal detection function
        f[j, k, b] <- ((2 * midpt[b]) / trunc_dist^2) * delta         # Prob density function out to max truncation distance 
        pi_pd[j, k, b] <- g[j, k, b] * f[j, k, b]                     # Detection cell probability
        pi_pd_c[j, k, b] <- pi_pd[j, k, b] / p_d[j, k]                # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the detection probability
      p_d[j, k] <- sum(pi_pd[j, k, ])
      
      ### Binomial portion of mixture ###
      
      # Number of individual birds detected (observations)
      n_dct[j, k] ~ dbin(p_d[j, k], n_avail[j, k]) 
      
      # Number of birds avaiable
      n_avail[j, k] ~ dbin(p_a[j, k], N_indv[j, k]) 
      
      # Poisson abundance portion of mixture
      N_indv[j, k] ~ dpois(lambda[j, k] * (present[j] + 0.0001) * area[j, k])   # ZIP true abundance at site j during visit k
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[j, k]) <- alpha0_obsv[observers[j, k]]                # Effect of each observer on detectability
      
      # Availability (phi) Log-linear model for availability
      logit(phi[j, k]) <- gamma0 +                                    # Intercept on availability 
                          gamma_date * day[j, k] +                    # Effect of scaled ordinal date 
                          gamma_date2 * day[j, k]^2 +                 # Effect of scaled ordinal date squared 
                          gamma_time * time[j, k] +                   # Effect of scaled time of day 
                          gamma_time2 * time[j, k]^2                  # Effect of scaled time of day squared 
      
      # Abundance (lambda) Log-linear model 
      log(lambda[j, k]) <- beta0  +                                   # Intercept on abundance
                           beta_burn * burned[j] +                    # Effect of fire 
                           beta_elv* elevation[j] +                   # Effect of elevation
                           beta_fyear * fire_year[j, k] * burned[j] + # Effect of years since fire on burned grids
                           beta_shrub * shrub_cvr[j] * burned[j] +    # Effect of shrub cover
                           beta_pfg *  pfg_cvr[j] * burned[j] +       # Effect of Perennial Cover
                           beta_afg * afg_cvr[j] * burned[j] +        # Effect of annual grass cover
                           eps_year[years[j, k]]                      # Random noise by year
                           
      # -------------------------------------------------------------------------------------------------------------------
      # Assess model fit: compute Bayesian p-value for using a test statisitcs
      
      # Chi square statisitc for the availability portion of the model
      e_pa[j, k] <- p_a[j, k] * N_indv[j, k]                                      # Expected value for availability binomial portion of the model
      n_avail_new[j, k] ~ dbin(p_a[j, k], N_indv[j, k])                           # Draw new available birds from the same binomial
      Chi_pa[j, k] <- (n_avail[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5)         # Compute availability chi squared statistic for observed data
      Chi_pa_new[j, k] <- (n_avail_new[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5) # Compute availability chi squared statistic for simulated data data
      
      # Chi square statisitc for the detection portion of the model
      e_pd[j, k] <- p_d[j, k] * n_avail[j, k]                                     # Expected value for detection binomial portion of the model
      n_dct_new[j, k] ~ dbin(p_d[j, k], n_avail[j, k])                            # Draw new detections from the same binomial
      Chi_pd[j, k] <- (n_dct[j, k] - e_pd[j, k])^2 / (e_pd[j, k] + 0.5)           # Compute detecability chi squared statistic for observed data
      Chi_pd_new[j, k] <- (n_dct_new[j, k] - e_pd[j, k])^2 / (e_pd[j, k] + 0.5)   # Compute detecability chi squared statistic for simulated data data
      
                           
      
    } # end loop through visits
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ]) # linking distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])   # linking time removal data
  } # end observation loop
  
  # --------------------------------------------------------------------------------------------
  # Combine fit statistics
  
  # Add up fit stats for availability across sites and years
  fit_pa <- sum(Chi_pa[,])
  fit_pa_new <- sum(Chi_pa_new[,])
  
  # Add up fit stats for detectability across sites and years
  fit_pd <- sum(Chi_pd[,])
  fit_pd_new <- sum(Chi_pd_new[,])
  
})

# 2.2) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
sobs_const <- list (
  # Misc. Constants
  area = area,             # Number of points surveyed per grid per visit
  delta = delta,           # Bin width
  trunc_dist = trunc_dist, # Truncation distance
  # For loop sizes
  ngrids = ngrids,         # Number of survey grids
  nind = nind,             # Number of individuals detected 
  nobsv = nobsv,           # Number of unique observers
  nvst = nvst,             # Number of times each grid was surveyed (6)
  nbins = nbins,           # Number of distance bins
  nints = nints,           # Number of time intervals
  nyears = nyears,          # Number of years we surveyed
  # Non-stochastic constants
  years = years,          # Year when each survey took place
  obs_visit  = obs_visit,  # Visit when each observation took place
  obs_grid  = obs_grid,    # Grid of each observation 
  observers = observers    # Effect of observer associated with each survey
)
# View Nimble constants 
str(sobs_const)

# Data to be fed into Nimble 
sobs_dat <- list(
  # Observation Level data
  dclass = dclass,        # Distance category for each observation
  midpt = midpt,          # Midpoints of distance bins
  # Abundance data
  tint = tint,            # Time interval for each observation
  time = time,            # Scaled mean time after sunrise 
  day = day,              # Scaled date
  #Point level data
  n_dct = n_dct,          # Number of detected individuals per site
  elevation = elevation,  # Elevation (m)
  fire_year = fire_year,  # How long since the most recent fire in each grid
  burned = burned,        # Whether or not each grid burned
  shrub_cvr = shrub_cvr,  # Percent shrub cover
  pfg_cvr = pfg_cvr,      # Percent perennial forb and grass cover
  afg_cvr = afg_cvr       # Percent annual grass cover
)
# View Nimble data 
str(sobs_dat)

# Object dimensions
sobs_dims <- list(
  g = c(ngrids, nvst, nbins),        # Cell prob x radial distance
  f = c(ngrids, nvst, nbins),        # Cell prob x radial distance
  sigma = c(ngrids, nvst),           # Linear combination on detectability
  pi_pd = c(ngrids, nvst, nbins),    # Detection probability in each cell
  pi_pd_c = c(ngrids, nvst, nbins),  # Proportion of total detection probability in each cell
  p_dct = c(ngrids, nvst),           # Detection probability 
  p_a = c(ngrids, nvst),             # Linear combination on avalibility  
  pi_pa =  c(ngrids, nvst, nints),   # Availability cell prob in each time interval
  pi_pa_c = c(ngrids, nvst, nints),  # Proportion of total availability probability in each cell
  phi = c(ngrids, nvst),             # Availability probability
  lambda = c(ngrids, nvst),          # Poisson random variable
  e_pa = c(ngrids , nvst),           # Expected value for availebility portion of the model
  e_pd = c(ngrids , nvst),           # Expected value for detection portion of the model
  Chi_pa = c(ngrids , nvst),         # Observed Chi square statistic for availability
  Chi_pa_new = c(ngrids, nvst),      # Simulated Chi square statistic for availability
  Chi_pd = c(ngrids , nvst),         # Observed Chi square statistic for detection 
  Chi_pd_new = c(ngrids, nvst)       # Simulated Chi square statistic for detection
)
# View the dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detectablility
  alpha0_obsv = runif(nobsv, -2, -0.1),  # Effect of each observer on detecability
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),             # Intercept on availability
  gamma_date = rnorm(1, 0, 0.1),         # Effect of date on availability
  gamma_time = rnorm(1, 0, 0.1),         # Effect of time of day on availability
  gamma_date2 = rnorm(1, 0, 0.1),        # Effect of date on availability (quadratic)
  gamma_time2 = rnorm(1, 0, 0.1),        # Effect of time of day on availability (quadratic)
  # Abundance 
  sd_eps_year = runif(1, 0, 0.5),        # Magnitude of random effect
  eps_year = rep(0, nyears),             # Random effect of year
  beta0 = rnorm(1, 0, 0.1),              # Intercept
  beta_burn = rnorm(1, 0, 0.1),          # Effect of Fire
  beta_elv = rnorm(1, 0, 0.1),           # Effect of elevation
  beta_fyear = rnorm(1, 0, 0.1),         # Effect of time since fire on burned grids
  beta_shrub = rnorm(1, 0, 0.1),         # Effect of pre fire shrub cover
  beta_pfg = rnorm(1, 0, 0.1),           # Effect of pre fire perennial forb and grass cover
  beta_afg = rnorm(1, 0, 0.1),           # Effect of pre fire annual grass cover
  # Presence 
  psi = runif(ngrids, 0.4, 0.6),          # Probability of each grid being occupied for zero inflation
  present = rbinom(ngrids, 1, 0.5),       # Binary presence absence for zero-inflation
  # Simulated counts
  n_avail = count_mat + 1,                # Number of available birds (helps to start each grid with an individual present)
  n_dct_new = count_mat,                  # Simulated detected birds 
  n_avail_new = count_mat + 1,            # Simulated available birds (helps to start each grid with an individual present)
  N_indv = count_mat + 1                  # "True" abundance (helps to start each grid with an individual present)
)  
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
                 "fit_pd",          # Fit statistic for observed data
                 "fit_pd_new",      # Fit statistic for simulated detection  data
                 "fit_pa",          # Fit statistic for first availability data
                 "fit_pa_new",      # Fit statistic for simulated availability data
                 "beta0",           # Intercept
                 "beta_burn",       # Effect of Fire
                 "beta_elv",        # Effect of elevation
                 "beta_fyear",      # Effect of time since fire on burned grids
                 "beta_shrub",      # Effect of pre fire shrub cover
                 "beta_pfg",        # Effect of pre fire perennial forb and grass cover
                 "beta_afg",        # Effect of pre fire annual grass cover
                 "sd_eps_year",     # Random noise on abundance by year
                 "gamma0",          # Intercept on availability
                 "gamma_date",      # Effect of date on singing rate
                 "gamma_date2",     # Quadratic effect of date on singing rate
                 "gamma_time",      # Effect of time of day on singing rate
                 "gamma_time2",     # Quadratic e of time of day on singing rate
                 "alpha0_obsv"      # Intercept for each observer on detection rate
                 )


# 2.3) Configure and Run the model ###########################################################

# Build the nimble model
sobs_model_vect <- nimbleModel(sobs_model_code, 
                               data = sobs_dat,
                               constants = sobs_const,
                               dimensions = sobs_dims,
                               inits = sobs_inits)

# Assign default samplers to nodes
sobs_mcmcConf <- configureMCMC(sobs_model_vect, 
                               monitors = sobs_params)

# Block all availability (gamma) nodes together
sobs_mcmcConf$removeSamplers("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2")

sobs_mcmcConf$addSampler(target = c("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2"),
                         type = 'RW_block')

# Block all detection (alpha) nodes together
sobs_mcmcConf$removeSamplers("alpha0_obsv[1]", "alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]", 
                             "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
                             "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]", 
                             "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", 
                             "alpha0_obsv[17]")

sobs_mcmcConf$addSampler(target = c("alpha0_obsv[1]", "alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]", 
                                    "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
                                    "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]", 
                                    "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", 
                                    "alpha0_obsv[17]"),
                         type = 'RW_block')

# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers("beta0", "beta_elv", "beta_burn", "beta_fyear", 
                             "beta_shrub","beta_pfg", "beta_afg",
                             "eps_year[2]", "eps_year[3]"
                             )

sobs_mcmcConf$addSampler(target = c("beta0", "beta_elv", "beta_burn", "beta_fyear", 
                                    "beta_shrub","beta_pfg", "beta_afg",
                                    "eps_year[2]", "eps_year[3]"
                                    ),
                         type = 'RW_block')

# Block all occupancy (psi) nodes together
sobs_mcmcConf$removeSamplers(
  "psi[1]", "psi[2]", "psi[3]", "psi[4]", "psi[5]", "psi[6]",
  "psi[7]", "psi[8]", "psi[9]", "psi[10]", "psi[11]", "psi[12]",
  "psi[13]", "psi[14]", "psi[15]", "psi[16]", "psi[17]", "psi[18]",
  "psi[19]", "psi[20]", "psi[21]", "psi[22]", "psi[23]", "psi[24]",
  "psi[25]", "psi[26]", "psi[27]", "psi[28]", "psi[29]", "psi[30]",
  "psi[31]", "psi[32]", "psi[33]", "psi[34]", "psi[35]", "psi[36]",
  "psi[37]", "psi[38]", "psi[39]", "psi[40]", "psi[41]", "psi[42]",
  "psi[43]", "psi[44]", "psi[45]", "psi[46]", "psi[47]", "psi[48]",
  "psi[49]", "psi[50]", "psi[51]", "psi[52]", "psi[53]", "psi[54]",
  "psi[55]", "psi[56]", "psi[57]", "psi[58]", "psi[59]", "psi[60]"
)
sobs_mcmcConf$addSampler(target = c(
  "psi[1]", "psi[2]", "psi[3]", "psi[4]", "psi[5]", "psi[6]",
  "psi[7]", "psi[8]", "psi[9]", "psi[10]", "psi[11]", "psi[12]",
  "psi[13]", "psi[14]", "psi[15]", "psi[16]", "psi[17]", "psi[18]",
  "psi[19]", "psi[20]", "psi[21]", "psi[22]", "psi[23]", "psi[24]",
  "psi[25]", "psi[26]", "psi[27]", "psi[28]", "psi[29]", "psi[30]",
  "psi[31]", "psi[32]", "psi[33]", "psi[34]", "psi[35]", "psi[36]",
  "psi[37]", "psi[38]", "psi[39]", "psi[40]", "psi[41]", "psi[42]",
  "psi[43]", "psi[44]", "psi[45]", "psi[46]", "psi[47]", "psi[48]",
  "psi[49]", "psi[50]", "psi[51]", "psi[52]", "psi[53]", "psi[54]",
  "psi[55]", "psi[56]", "psi[57]", "psi[58]", "psi[59]", "psi[60]"
), type = 'RW_block')

# View the blocks
sobs_mcmcConf$printSamplers() # print samplers being used 
sobs_mcmcConf$unsampledNodes  # look at unsampled nodes

# Build MCMC object: can customize thinning, monitors, etc.
sobs_modelMCMC <- buildMCMC(sobs_mcmcConf) # Update mcmc configuration based on graph

# Compile Nimble object and MCMC object to one C++ object (gets an ~3 fold speed up, even 
# if you do nothing else) spend time on getting this step right because this is the 
# easiest way to accelerate convergence. 
cModel <- compileNimble(sobs_model_vect) # compiled model
cMCMC <- compileNimble(sobs_modelMCMC, project = cModel, resetFunctions = T) # compiled mcmc

# MCMC settings for the real model. Pick one, comment out the rest 
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1           # Quick test to see if the model runs
# nc <- 3  ;  ni <- 150000  ;  nb <- 50000;  nt <- 10    # longer test where most parameters should
nc <- 4;  ni <- 500000;  nb <- 250000;  nt <- 25        # Run the model for real

# Quick check of how many samples we'll keep in the posterior
message(paste((ni - nb) / nt), " samples will be kept from the posterior")

# Run the sampler
start <- Sys.time()                                     # Start time for the sampler
start
PreFire_mcmc_out <- runMCMC(cMCMC,
                         niter = ni, 
                         nburnin = nb, 
                         thin = nt, 
                         nchains = nc,
                         samplesAsCodaMCMC = T,
                         summary = T)
difftime(Sys.time(), start)                             # End time for the sampler

# Save model output to local drive
saveRDS(PreFire_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     study_species, "_PreFire_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################


# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
PreFire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                          model_species, "_PreFire_model.rds"))

# Traceplots and density graphs 
MCMCtrace(object = PreFire_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_PreFire_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = PreFire_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = PreFire_mcmc_out$samples,
         excl = c("fit_pa", "fit_pa_new", "fit_pd", "fit_pd_new"),
         guide_lines = TRUE,
         params = sobs_params)

# } # End the loop over species

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# 4.1) Prepare data for model output ################################################

# Clear environments again
rm(list = ls())

# Add packages
library(tidyverse)
library(gridExtra)

# 4.2) Graph each species responce to fire ###############################################################

# List of Species to plot 
all_plot_species <- c(
  "BRSP",
  "SATH",
  "VESP",
  "WEME",
  "HOLA",
  "GTTO"
)

# Loop over all species 
for(s in 1:length(all_plot_species)) { # (Comment this out) ----
  
# Name the species to model again
plot_species <- all_plot_species[s]
# plot_species <- all_plot_species[1]
  
# Data frame for naming species
plot_species_df <- data.frame(Species.Code = plot_species) %>% 
  mutate(Species.Name = case_when(Species.Code == "SATH" ~ "Sage Thrasher",
                                    Species.Code == "BRSP" ~ "Brewer's Sparrow", 
                                    Species.Code == "SABS" ~ "Sagebrush Sparrow",
                                    Species.Code == "GTTO" ~ "Green-Tailed Towhee",
                                    Species.Code == "VESP" ~ "Vesper Sparrow",
                                    Species.Code == "WEME" ~ "Western Meadowlark",
                                    Species.Code == "HOLA" ~ "Horned Lark"))
species_name <- plot_species_df$Species.Name
# View
species_name
  
# Load the output back in
PreFire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//",
                                        plot_species, "_PreFire_model.rds"))
  
# View MCMC summary
PreFire_mcmc_out$summary$all.chains
  
# Extract effect sizes
beta0 <- PreFire_mcmc_out$summary$all.chains[18,]
beta_burned <- PreFire_mcmc_out$summary$all.chains[20,]
beta_elv <- PreFire_mcmc_out$summary$all.chains[21,]
beta_fyear <- PreFire_mcmc_out$summary$all.chains[22,] 
beta_shrub <- PreFire_mcmc_out$summary$all.chains[24,] 
beta_pfg <- PreFire_mcmc_out$summary$all.chains[23,]
beta_afg <- PreFire_mcmc_out$summary$all.chains[19,]
  
# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0,
                                 beta_burned,
                                 beta_elv,
                                 beta_fyear,
                                 beta_shrub,
                                 beta_pfg,
                                 beta_afg)) %>% 
    mutate(Parameter = c("beta0", "beta.burned", "beta.elvation", "beta.fyear", 
                         "beta.shrub", "beta.pfg", "beta.afg")) %>% 
    relocate(Parameter, .before = Mean) %>% 
    rename(CRI.lb = X95.CI_low,
           CRI.ub = X95.CI_upp)
  
# View output dataframe
head(beta_dat, n = Inf)
  
# Parameter estimate plot -------------------------------------------------------------------------
  
# Create the plot
params_plot <- beta_dat %>% 
# Rename Parameters
mutate(Parameter = case_when(Parameter == "beta0" ~ "Intercept",
                                 Parameter == "beta.burned" ~ "Burned",
                                 Parameter == "beta.elvation" ~ "Elevation (m)",
                                 Parameter == "beta.fyear" ~ "Years Since Fire",
                                 Parameter == "beta.shrub" ~ "Pre-Fire Shrub Cover",
                                 Parameter == "beta.pfg" ~ "Pre-Fire Perennial Cover",
                                 Parameter == "beta.afg" ~ "Pre-Fire Annual Cover",
                                 Parameter == "beta.tree" ~ "Pre-Fire Tree Cover",
                                 Parameter == "beta.bg" ~ "Pre-Fire Bare Ground Cover"),
        # Add a New column for whether or not the CRI crosses Zero
        Significant = factor(case_when(CRI.lb * CRI.ub <= 0 ~ "No",
                                          CRI.lb * CRI.ub > 0 ~ "Yes"), 
                                levels = c("No", "Yes"))) %>% 
    # Switch to a factor 
    mutate(Parameter = factor(Parameter, levels = c("Pre-Fire Bare Ground Cover",
                                                    "Pre-Fire Tree Cover",
                                                    "Pre-Fire Annual Cover",
                                                    "Pre-Fire Perennial Cover",
                                                    "Pre-Fire Shrub Cover",
                                                    "Years Since Fire",
                                                    "Elevation (m)",
                                                    "Burned",
                                                    "Intercept"
                                                    ))) %>% 
    # Open the plot
    ggplot(aes(y = Parameter)) +
    # Add points at the mean values for each parameters
    geom_point(aes(x = Mean, color = Significant), shape = 15, size = 4) +
    # Add whiskers for 95% Bayesian Credible intervals
    geom_linerange(aes(xmin = CRI.lb, xmax = CRI.ub, color = Significant), linewidth = 1.5) +
    # Add a vertical Line at zero
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    # Change the Labels
    labs(x = "Parameter Estimate", y = "", title = species_name) +
    # Simple theme
    theme_classic() +
    # Custom colors
    scale_color_manual(values = c("No" = "lightsteelblue4", 
                                  "Yes" = "navyblue")) +
    # Edit theme
    theme(plot.title = element_text(size = 22, hjust = 0.4),
          legend.position = "none",
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 16)) 
  
  
# View the plot
params_plot
  
# Save the plot as a png
ggsave(plot = params_plot,
       paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred",
              plot_species, "_params.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

# # save the plot as an RDS
saveRDS(object = params_plot,
        file =  paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred",
                       plot_species, "_params.rds"))
  
} # End plotting loop over all species (Comment this out) ----


# All sagebrush species plots together ---------------------------------------------------------------

# Add sagebrush obligate plots back in
sath_prefire_params <-readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred_",
                                     "SATH", "_params.rds"))
brsp_prefire_params <-readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred_",
                                     "BRSP", "_params.rds"))
gtto_prefire_params <-readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred_",
                                     "GTTO", "_params.rds"))

# Combine these plots
sage_sp_prefire_params <- grid.arrange(sath_prefire_params, brsp_prefire_params, gtto_prefire_params,
                                       nrow = 1, ncol = 3)

# View the combined plot
sage_sp_prefire_params

# Save the plot as a png
ggsave(plot = sage_sp_prefire_params,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\pre_fire_",
                         "sage_species", "_params.png"),
       width = 500,
       height = 150,
       units = "mm",
       dpi = 300)

# All grassland species plots together ---------------------------------------------------------------

# Add grassland associated species plots back in
vesp_prefire_params <-readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred_",
                                     "VESP", "_params.rds"))
weme_prefire_params <-readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred_",
                                     "WEME", "_params.rds"))
hola_prefire_params <-readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\PreFire_pred_",
                                     "HOLA", "_params.rds"))

# Combine these plots
grass_sp_prefire_params <- grid.arrange(vesp_prefire_params, weme_prefire_params, hola_prefire_params,
                                        nrow = 1, ncol = 3)

# View the combined plot
grass_sp_prefire_params

# Save the plot as a png
ggsave(plot = grass_sp_prefire_params,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\pre_fire_",
                         "grass_species", "_params.png"),
       width = 500,
       height = 150,
       units = "mm",
       dpi = 300)
