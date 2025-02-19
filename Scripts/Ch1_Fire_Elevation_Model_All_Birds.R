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

################################################################################
# 1.0) Data Prep  ##############################################################
################################################################################

# 1.1) Read in data ############################################################

# List of Species 
all_species <- c(
  "BRSP",
  "SATH",
  "VESP",
  "WEME",
  "HOLA",
  "GTTO"
)

# Loop over all species
for(s in 1:length(all_species)){ # (Comment this out) ----

# Pick a species to model
model_species <- all_species[s]
# model_species <- all_species[1]

# Add in count data from local drive
# Two grids (ID-C11 and ID-C22) were missing their Y1V1 survey these were imputed using the second visit
# Total Indv = number of all other species (Good proxy for noise)
sobs_counts_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Grid_Counts.csv")) %>%
  tibble() %>%
  select(-X)
# Or from github
# covs <- read.csv(paste0("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/",
#                         model_species, "_Grid_Counts.csv"))%>%
#   dplyr::select(-X) %>%
#   tibble()
#view the counts
glimpse(sobs_counts_temp)

# Add in the observation data from the local drive
sobs_observations_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Observations.csv")) %>% 
  select(-X)
# #  or from github
# covs <- read.csv(paste0("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/",
# model_species, "_Observations.csv"))%>%
#   dplyr::select(-X) %>%
#   tibble()
# View the observations
glimpse(sobs_observations_temp)

# Add covariates from the local drive
# Aspect only on areas with greater than 3 degree slopes
# Burn sevarity == 0 means that the area did not burn
# Burn sevarity == -1 means that no data was available 
# Fire year == 1800 means there are no recorded fires in the area
covs <- tibble(read.csv("Data/Outputs/grid_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# # or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/grid_covs.csv") %>%
#   dplyr::select(-X) %>%
#   tibble()
# View covariates
glimpse(covs)
  
# 1.2) Prepare the count level data ################################################################

# Change necessary variables to scales and factors
sobs_counts_temp2 <- sobs_counts_temp %>%
  # Add covariates
  left_join(covs, by = c("Grid.ID", "Grid.Type")) %>% 
  # Sort the data
  arrange(Visit.ID, Grid.ID) %>% 
  mutate(# Numeric burned vs unburned
         Burned = as.numeric(factor(Grid.Type, levels = c("R", "B"))) - 1,
         # Time since Fire
         Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>%
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID", "Observer.ID", "Year"), factor) %>% 
  mutate(ln.Years.Since.Fire = log(Years.Since.Fire)) %>% 
  # Remove columns that are no longer needed
  dplyr::select(-Grid.Type) %>% 
  # Make a binary column for whether or not a grid is high elevation
  mutate(High.Elevation = case_when(Elevation.125m < 1800 ~ 1,
                                    Elevation.125m >= 1800 ~ 2,
                                    TRUE ~ NA)) %>% 
  # Make a treatment column (elevation x burned)
  mutate(Treatment = case_when(Burned == 0 & High.Elevation == 1 ~ 1,
                               Burned == 0 & High.Elevation == 2 ~ 2,
                               Burned == 1 & High.Elevation == 1 ~ 3,
                               Burned == 1 & High.Elevation == 2 ~ 4,
                               TRUE ~ NA)) %>% 
  # Make a unique column for whether or not each grid was in each of the treatments
  mutate(High.Reference = case_when(Treatment == 2 ~ 1, TRUE ~ 0),
         Low.Burned = case_when(Treatment == 3 ~ 1, TRUE ~ 0),
         High.Burned = case_when(Treatment == 4 ~ 1, TRUE ~ 0)) 
  

#...and view
# glimpse(sobs_counts_temp2)

# Isolate the burned Grids as their own object so I can accurately scale them
fire_stats <- sobs_counts_temp2 %>% 
  filter(Burned == 1) %>% 
  distinct(Grid.ID, Years.Since.Fire, ln.Years.Since.Fire, rdnbr.125m) 
# View
# glimpse(fire_stats)

# Find the mean and standard deviation of the "real" burns
mean_fyear <- mean(fire_stats$Years.Since.Fire)
sd_fyear <- sd(fire_stats$Years.Since.Fire)
mean_ln_fyear <- mean(fire_stats$ln.Years.Since.Fire)
sd_ln_fyear <- sd(fire_stats$ln.Years.Since.Fire)
mean_rdnbr <- mean(fire_stats$rdnbr.125m)
sd_rdnbr <- sd(fire_stats$rdnbr.125m)

# Scale each covariate
sobs_counts <- sobs_counts_temp2 %>% 
  mutate(ln.Years.Since.Fire = log(Years.Since.Fire)) %>% 
  mutate(Elevation.scl = scale(Elevation.125m)[,1],
         Mean.MAS.scl = scale(Mean.MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1],
         Mean.Birds.scl = scale(Mean.Birds)[,1],
         Years.Since.Fire.scl = (Years.Since.Fire - mean_fyear) / sd_fyear,
         ln.Years.Since.Fire.scl = (ln.Years.Since.Fire - mean_ln_fyear) /sd_ln_fyear,
         rdnbr.scl = (rdnbr.125m - mean_rdnbr) / sd_rdnbr)
# View
# glimpse(sobs_counts)

# 1.3) Prepare the observation level data ################################################################

# Make sure the same numeric values for factors are shaped between the two datasets
sobs_counts <- sobs_counts %>% 
  arrange(Year, Visit.ID, Grid.ID) %>% 
  mutate(Grid.ID.num = as.numeric(Grid.ID),
         Year.num = as.numeric(Year),
         Visit.ID.num = as.numeric(Visit.ID),
         Observer.ID.num = as.numeric(Observer.ID))

# View who each observer is
# sobs_counts %>%
#   distinct(Observer.ID, Observer.ID.num, Observer.Experience) %>%
#   arrange(Observer.ID.num) %>%
#   print(n = Inf)

# Pull out the covariates that need to be shared across counts and observations
point_ids <- sobs_counts %>% 
  mutate_at(c("Grid.ID", "Year", "Visit.ID"), as.character) %>% 
  dplyr::select(Grid.ID, Grid.ID.num, Year, Year.num, Visit.ID, Visit.ID.num, Observer.ID.num)
#...and view
# glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
sobs_observations <- sobs_observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Year", "Visit.ID"))

# View Counts
glimpse(sobs_counts)

# View observations
glimpse(sobs_observations)


# 1.4) prepare objects for NIMBLE ################################################################

# Define a truncation distance (km)
trunc_dist <- 0.125

# Matrix dimentions
nrows <- length(unique(sobs_counts$Grid.ID.num))  # Number of survey grids
ncols <- length(unique(sobs_counts$Visit.ID.num)) # Times each grid was visited

# Build a storage matrix of observations by visit
count_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix of years by visit
year_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix of survey times by visit
time_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix of survey dates by visit
date_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for the proportion of points visited during each survey
area_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for who conducted each survey
obsv_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for observer experience 
exp_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for years since fire during each survey
fyear_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for how many individual birds were seen on each visit
mean_birds_mat <- matrix(NA, nrow = nrows, ncol = ncols)

# Fill in the matrix
for(y in 1:nrow(count_mat)){
  # Filter for a specific grid
  count_visit <- sobs_counts %>% 
    arrange(Visit.ID.num) %>% 
    filter(Grid.ID.num == y)
  # Assign values to each row
  count_mat[y,] <- count_visit$Count
  year_mat[y,] <- count_visit$Year
  time_mat[y,] <- count_visit$Mean.MAS.scl
  date_mat[y,] <- count_visit$Ord.Date.scl
  area_mat[y,] <- pi * count_visit$n.Points * trunc_dist^2  # Area surveyed in km^2
  obsv_mat[y,] <- count_visit$Observer.ID.num
  exp_mat[y,] <- count_visit$Observer.Experience 
  fyear_mat[y,] <- count_visit$Years.Since.Fire.scl
  mean_birds_mat[y,] <- count_visit$Mean.Birds.scl
}

# Size Objects  
ngrids <- length(unique(sobs_counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(sobs_observations)                            # Number of individuals detected 
nobsv <- length(unique(sobs_counts$Observer.ID.num))       # Number of unique observers
nexp <- length(unique(sobs_counts$Observer.Experience))    # Number of experience levels
nbins <- length(unique(sobs_observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(sobs_observations$Time.Interval))   # Number of time intervals
nvst <- length(unique(sobs_counts$Visit.ID))               # Number of visits in each year
nyears <- length(unique(sobs_counts$Year.num))             # Number of years we surveyed
ntrts <- length(unique(sobs_counts$Treatment))             # Number of treatments (Elevation x Burn)
nelv <- length(unique(sobs_counts$High.Elevation))         # Number of elevations (High or Low)

# Observation Level data 
midpt <- sort(unique(sobs_observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
observers <- obsv_mat                                      # Random effect for observer associated with each survey
obsv_exp <- exp_mat                                        # Whether or not it was each observer's first point count season
obs_visit <- sobs_observations$Visit.ID.num                # During which visit did each observation take place
obs_grid <- sobs_observations$Grid.ID.num                  # In which grid did each observation take place
dclass <- sobs_observations$Dist.Bin                       # Distance class of each observation
delta <- trunc_dist / nbins                                # Size of distance bins
mean_birds <- mean_birds_mat                                # Number of individual birds seen per survey

# Availability date
tint <- sobs_observations$Time.Interval                    # Time interval for each observation 
time <- time_mat                                           # Matrix of scaled time after sunrise
day <- date_mat                                            # Matrix of scaled dates

# Grid level data
area <- area_mat                                           # Proportion of points surveyed       
n_dct <- count_mat                                         # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                          # Matrix of year numbers
grids <- sobs_counts$Grid.ID.num[1:ngrids]                 # Grid where each survey took place
elevation <- sobs_counts$High.Elevation[1:ngrids]          # Whether each grid is high (>= 1900m) or low (< 1900m) elevation
burned <- sobs_counts$Burned[1:ngrids]                     # Whether or not each grid burned
fyear <- fyear_mat                                         # How long since the most recent fire in each grid
burn_sev <- sobs_counts$rdnbr.scl[1:ngrids]                # Burn severity from the most recent fire
trts <- sobs_counts$Treatment[1:ngrids]                    # Grid type (Elevation x burned)

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Model definition ################################################################
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sobs_model_code <- nimbleCode({
 
  # ------------------------------------------------------------------
  # Priors for all parameters 
  
  # Parameters in the availability component of the detection model ----
  # Fixed effects on avaiablility
  gamma0 ~ dnorm(0, sd = 3)           # Availability intercept
  gamma_date ~ dnorm(0, sd = 1.5)     # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 1.5)    # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 1.5)     # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 1.5)    # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model ----
  # Fixed effects on detecability
  
  # Effect of observer experience (Can't be greater than 0)
  for(o in 1:nobsv){ # (nobsv = 17)
    alpha0_obsv[o] ~ T(dnorm(-2, sd = 3), -9, 0) # Prior mean centered on approximately sigma = exp(-2) = 0.125km
  } # End loop through observers
  
  # Single fixed effects on detection prob
  # alpha_nbirds ~ dnorm(0, sd = 1.5)  # Effect of how many total birds were seen at the site
  
  # Parameters on the abundance component of the model ----
  
  # Intercept by treatment
  for(l in 1:ntrts){ # ntrts = 4
    # Allow the intercept too varry for each treatment group
    beta0_treatment[l] ~ dnorm(0, sd = 3)
  } # End loop over treatements
  
  # Effect of years since fire on burned grids varying with elevation
  for(e in 1:nelv){ # nelv = 2 (high and low elevation)
    beta_fyear[e] ~ dnorm(0, sd = 1.5)
  } # End loop over burned elevations
  
  # Single fixed effects
  beta_burnsev ~ dnorm(0, sd = 1.5)   # Effect of initial burn severity on burned grids
  
  # Abundance random effect hyper-parameters
  sd_eps_year ~ dgamma(shape = 0.5, scale = 0.5) # Random effect on abundance hyperparameter for each year 

  # Random effect for each year
  for(y in 1:nyears){ # nyears = 3
    eps_year[y] ~ dnorm(0, sd = sd_eps_year)
  } # end loop over years
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey grids
  for(j in 1:ngrids){
   
    # Zero-inflation component on abundance
    psi[j] ~ T(dbeta(shape1 = 1.3, shape2 = 1.3), 0.001, ) # Occupancy probability can't be exactly zero
    present[j] ~ dbern(psi[j])                             # Number of grids where that individual can be present
    
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
      
      # # Multiply availability with detection probability to yield total probability of capturing a bird
      # p_cap[j, k] <- p_d[j, k] * p_a[j, k]
      
      ### Binomial portion of mixture ###
      
      # Number of individual birds detected (observations)
      n_dct[j, k] ~ dbin(p_d[j, k], n_avail[j, k]) 
      
      # Number of birds avaiable
      n_avail[j, k] ~ dbin(p_a[j, k], N_indv[j, k]) 
      
      # Poisson abundance portion of mixture
      N_indv[j, k] ~ dpois(lambda[j, k] * (present[j] + 0.0001) * area[j, k])   # ZIP true abundance at site j
      
      # Availability (phi) Logit-linear model for availability
      logit(phi[j, k]) <- gamma0 +                       # Intercept on availability
                          gamma_date * day[j, k] +       # Effect of scaled ordinal date
                          gamma_time * time[j, k] +      # Effect of scaled time of day
                          gamma_date2 * day[j, k]^2 +    # Effect of scaled ordinal date squared
                          gamma_time2 * time[j, k]^2     # Effect of scaled time of day squared
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[j, k]) <- alpha0_obsv[observers[j, k]]   # Intercept for of observer experience on detectability
                          # alpha_nbirds * mean_birds[j, k] # Effect of how many total birds were seen on that grid
        
      # Abundance (lambda) Log-linear model 
      log(lambda[j, k]) <- beta0_treatment[trts[j]] +                           # Intercept for each grid type
                           beta_fyear[elevation[j]] * fyear[j, k] * burned[j] + # Effect of time since fire on each treatment
                           beta_burnsev * burn_sev[j] * burned[j] +             # Effect of initial burn severity on burned grids
                           eps_year[years[j, k]]                                # Unexplained noise on abundance by year

      # Assess model fit: compute Bayesian p-value for using a test statisitc
      e_val[j, k] <- p_cap[j, k] * N_indv[j, k]                        # Expected value for binomial portion of the model
      Chi[j, k] <- (n_dct[j, k] - e_val[j, k])^2 / (e_val[j, k] + 0.5) # Compute chi squared statistic for observed data
      
      # Generate replicate count data and compute same fit stats for them
      n_dct_new[j, k] ~ dbin(p_cap[j, k], N_indv[j, k])                        # Draw new detentions from the same binomial
      Chi_new[j, k] <- (n_dct_new[j, k] - e_val[j, k])^2 / (e_val[j, k] + 0.5) # Compute chi squared statistic for simulated data data
                               
    } # end loop through visits
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ])    # likelihood for distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])      # likelihood for time removal data
  } # end observation distance and time of detection loop
  
  # Add up fit stats across sites and years
  fit <- sum(Chi[,])
  fit_new <- sum(Chi_new[,])
  
  # Compute over dispersion parameter (c_hat)
  c_hat <- fit / fit_new
  
}) # end model statement

# 2.2) Constants, data, Initial values, parameters to save, and dimensions ##################################

# Constants to be fed into Nimble
sobs_const <- list (
  # Misc. Constants
  area = area,                 # Number of points surveyed per grid per visit
  delta = delta,               # Bin width
  trunc_dist = trunc_dist,     # Truncation distance
  
  # For loop sizes
  ngrids = ngrids,             # Number of survey grids
  ntrts = ntrts,               # Number of treatments
  nind = nind,                 # Number of individuals detected 
  nobsv = nobsv,               # Number of unique observers
  nvst = nvst,                 # Number of times each grid was surveyed (6)
  nbins = nbins,               # Number of distance bins
  nints = nints,               # Number of time intervals
  nelv = nelv,                 # Number of elevations (2)
  nyears = nyears,             # Number of years we surveyed (3)
  
  # Non-stochastic constants
  years = years,               # Year when each survey took place
  trts = trts,                 # Grid type
  obs_visit  = obs_visit,      # Visit when each observation took place
  obs_grid  = obs_grid,        # Grid of each observation 
  observers = observers,       # Effect of observer associated with each survey
  elevation = elevation        # High or low elevation
)

# View Nimble constants 
str(sobs_const)

# Data to be fed into Nimble 
sobs_dat <- list(
  # Detection level data
  dclass = dclass,             # Distance category for each observation
  midpt = midpt,               # Midpoints of distance bins
  mean_birds = mean_birds,     # Total number of birds seen during each survey
  # Availability level data
  tint = tint,                 # Time interval for each observation
  time = time,                 # Scaled mean time after sunrise
  day = day,                   # Scaled date
  # Abundance level
  n_dct = n_dct,               # Number of detected individuals per site
  fyear = fyear,               # How long since the most recent fire in each grid
  burn_sev = burn_sev,         # Burn severity from the most recent fire
  burned = burned              # Whether or not each grid burned
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
  p_d = c(ngrids, nvst),             # Detection probability 
  phi = c(ngrids, nvst),             # Linear combination on availability  
  pi_pa =  c(ngrids, nvst, nints),   # Availability cell prob in each time interval
  pi_pa_c = c(ngrids, nvst, nints),  # Proportion of total availability probability in each cell
  p_a = c(ngrids, nvst),             # Availability probability
  p_cap = c(ngrids, nvst),           # Combined probability of detecting an available bird
  lambda = c(ngrids, nvst),          # Poisson random variable
  Chi = c(ngrids , nvst),            # Observed Chi square statistic
  Chi_new = c(ngrids, nvst)          # Simulated Chi square statistic
)

# View dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detectablility
  alpha0_obsv = runif(nobsv, -2, -0.1),
  # alpha_nbirds = rnorm(1, 0, 0.1),
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),
  gamma_date = rnorm(1, 0, 0.1),
  gamma_time = rnorm(1, 0, 0.1),
  gamma_date2 = rnorm(1, 0, 0.1),
  gamma_time2 = rnorm(1, 0, 0.1),
  # Abundance 
  beta0_treatment = rnorm(ntrts, 0, 0.1),
  beta_fyear = rnorm(nelv, 0, 0.1),
  beta_burnsev = rnorm(1, 0, 0.1),
  sd_eps_year = runif(1, 0, 1),
  eps_year = rnorm(years, 0, 0.1),
  # Presence 
  psi = runif(ngrids, 0.4, 0.6),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated counts
  n_avail = count_mat + 1,
  n_dct_new = count_mat,
  N_indv = count_mat + 1 # Counts helps to start each grid with an individual present       
)  

# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
  "fit",             # Fit statistic for observed data
  "fit_new",         # Fit statisitc for simulated data
  "c_hat",           # Overdispersion parameter
  "beta0_treatment", # Unique intercept by treatment
  "beta_fyear",      # Effect of each year after a fire
  "beta_burnsev",    # Effect of RdNBR burn sevarity
  "gamma0",          # Intercept on availability
  "gamma_date",      # Effect of date on singing rate
  "gamma_date2",     # Quadratic effect of date on singing rate
  "gamma_time",      # Effect of time of day on singing rate
  "gamma_time2",     # Quadratic e of time of day on singning rate
  "alpha0_obsv"      # Intercept for each observer on detection rate
  # "alpha_nbirds"     # Effect of how many total birds were seen on detection probability
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
sobs_mcmcConf$removeSamplers(
  # Intercept, effect of date and effect of time
  "gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2")
sobs_mcmcConf$addSampler(target = c(
  # Intercept, effect of date and effect of time
  "gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2"),
                         type = 'RW_block')

# Block all detection (alpha) nodes together
sobs_mcmcConf$removeSamplers(
  # Effect of each observer
  "alpha0_obsv[1]","alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]",
  "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
  "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]",
  "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", "alpha0_obsv[17]"
  # Effect of seeing more birds on a survey
  # "alpha_nbirds"
  )
sobs_mcmcConf$addSampler(target = c(
  # Effect of each observer
  "alpha0_obsv[1]","alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]",
  "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
  "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]",
  "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", "alpha0_obsv[17]"
  # Effect of seeing more birds on a survey
  # "alpha_nbirds"
  ), type = 'RW_block')

# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers(
  # Intercept by grid type
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Effect of time since fire by elevation
  "beta_fyear[1]", "beta_fyear[2]",
  # Effect of burn severity
  "beta_burnsev",
  # Random noise by year
  "eps_year[1]", "eps_year[2]", "eps_year[3]"
  )
sobs_mcmcConf$addSampler(target = c(
  # Intercept by grid type
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Effect of time since fire by elevation
  "beta_fyear[1]", "beta_fyear[2]",
  # Effect of burn severity
  "beta_burnsev",
  # Random noise by year
  "eps_year[1]", "eps_year[2]", "eps_year[3]"
  ), type = 'RW_block')

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

# Block all presence (present) nodes together
sobs_mcmcConf$removeSamplers(
  "present[1]", "present[2]", "present[3]", "present[4]", "present[5]", "present[6]", 
  "present[7]", "present[8]", "present[9]", "present[10]", "present[11]", "present[12]",
  "present[13]", "present[14]", "present[15]", "present[16]", "present[17]", "present[18]",
  "present[19]", "present[20]", "present[21]", "present[22]", "present[23]", "present[24]", 
  "present[25]", "present[26]", "present[27]", "present[28]", "present[29]", "present[30]", 
  "present[31]", "present[32]", "present[33]", "present[34]", "present[35]", "present[36]", 
  "present[37]", "present[38]", "present[39]", "present[40]", "present[41]", "present[42]", 
  "present[43]", "present[44]", "present[45]", "present[46]", "present[47]", "present[48]", 
  "present[49]", "present[50]", "present[51]", "present[52]", "present[53]", "present[54]", 
  "present[55]", "present[56]", "present[57]", "present[58]", "present[59]", "present[60]"
)
sobs_mcmcConf$addSampler(target = c(
  "present[1]", "present[2]", "present[3]", "present[4]", "present[5]", "present[6]", 
  "present[7]", "present[8]", "present[9]", "present[10]", "present[11]", "present[12]",
  "present[13]", "present[14]", "present[15]", "present[16]", "present[17]", "present[18]",
  "present[19]", "present[20]", "present[21]", "present[22]", "present[23]", "present[24]", 
  "present[25]", "present[26]", "present[27]", "present[28]", "present[29]", "present[30]", 
  "present[31]", "present[32]", "present[33]", "present[34]", "present[35]", "present[36]", 
  "present[37]", "present[38]", "present[39]", "present[40]", "present[41]", "present[42]", 
  "present[43]", "present[44]", "present[45]", "present[46]", "present[47]", "present[48]", 
  "present[49]", "present[50]", "present[51]", "present[52]", "present[53]", "present[54]", 
  "present[55]", "present[56]", "present[57]", "present[58]", "present[59]", "present[60]"
), type = 'RW_block')


# Block all density (N_indv) Nodes together
sobs_mcmcConf$removeSamplers(
  "N_indv[1, 1]", "N_indv[1, 2]", "N_indv[1, 3]", "N_indv[1, 4]", "N_indv[1, 5]", "N_indv[1, 6]",
  "N_indv[2, 1]", "N_indv[2, 2]", "N_indv[2, 3]", "N_indv[2, 4]", "N_indv[2, 5]", "N_indv[2, 6]",
  "N_indv[3, 1]", "N_indv[3, 2]", "N_indv[3, 3]", "N_indv[3, 4]", "N_indv[3, 5]", "N_indv[3, 6]",
  "N_indv[4, 1]", "N_indv[4, 2]", "N_indv[4, 3]", "N_indv[4, 4]", "N_indv[4, 5]", "N_indv[4, 6]",
  "N_indv[5, 1]", "N_indv[5, 2]", "N_indv[5, 3]", "N_indv[5, 4]", "N_indv[5, 5]", "N_indv[5, 6]",
  "N_indv[6, 1]", "N_indv[6, 2]", "N_indv[6, 3]", "N_indv[6, 4]", "N_indv[6, 5]", "N_indv[6, 6]",
  "N_indv[7, 1]", "N_indv[7, 2]", "N_indv[7, 3]", "N_indv[7, 4]", "N_indv[7, 5]", "N_indv[7, 6]",
  "N_indv[8, 1]", "N_indv[8, 2]", "N_indv[8, 3]", "N_indv[8, 4]", "N_indv[8, 5]", "N_indv[8, 6]",
  "N_indv[9, 1]", "N_indv[9, 2]", "N_indv[9, 3]", "N_indv[9, 4]", "N_indv[9, 5]", "N_indv[9, 6]",
  "N_indv[10, 1]", "N_indv[10, 2]", "N_indv[10, 3]", "N_indv[10, 4]", "N_indv[10, 5]", "N_indv[10, 6]",
  "N_indv[11, 1]", "N_indv[11, 2]", "N_indv[11, 3]", "N_indv[11, 4]", "N_indv[11, 5]", "N_indv[11, 6]",
  "N_indv[12, 1]", "N_indv[12, 2]", "N_indv[12, 3]", "N_indv[12, 4]", "N_indv[12, 5]", "N_indv[12, 6]",
  "N_indv[13, 1]", "N_indv[13, 2]", "N_indv[13, 3]", "N_indv[13, 4]", "N_indv[13, 5]", "N_indv[13, 6]",
  "N_indv[14, 1]", "N_indv[14, 2]", "N_indv[14, 3]", "N_indv[14, 4]", "N_indv[14, 5]", "N_indv[14, 6]",
  "N_indv[15, 1]", "N_indv[15, 2]", "N_indv[15, 3]", "N_indv[15, 4]", "N_indv[15, 5]", "N_indv[15, 6]",
  "N_indv[16, 1]", "N_indv[16, 2]", "N_indv[16, 3]", "N_indv[16, 4]", "N_indv[16, 5]", "N_indv[16, 6]",
  "N_indv[17, 1]", "N_indv[17, 2]", "N_indv[17, 3]", "N_indv[17, 4]", "N_indv[17, 5]", "N_indv[17, 6]",
  "N_indv[18, 1]", "N_indv[18, 2]", "N_indv[18, 3]", "N_indv[18, 4]", "N_indv[18, 5]", "N_indv[18, 6]",
  "N_indv[19, 1]", "N_indv[19, 2]", "N_indv[19, 3]", "N_indv[19, 4]", "N_indv[19, 5]", "N_indv[19, 6]",
  "N_indv[20, 1]", "N_indv[20, 2]", "N_indv[20, 3]", "N_indv[20, 4]", "N_indv[20, 5]", "N_indv[20, 6]",
  "N_indv[21, 1]", "N_indv[21, 2]", "N_indv[21, 3]", "N_indv[21, 4]", "N_indv[21, 5]", "N_indv[21, 6]",
  "N_indv[22, 1]", "N_indv[22, 2]", "N_indv[22, 3]", "N_indv[22, 4]", "N_indv[22, 5]", "N_indv[22, 6]",
  "N_indv[23, 1]", "N_indv[23, 2]", "N_indv[23, 3]", "N_indv[23, 4]", "N_indv[23, 5]", "N_indv[23, 6]",
  "N_indv[24, 1]", "N_indv[24, 2]", "N_indv[24, 3]", "N_indv[24, 4]", "N_indv[24, 5]", "N_indv[24, 6]",
  "N_indv[25, 1]", "N_indv[25, 2]", "N_indv[25, 3]", "N_indv[25, 4]", "N_indv[25, 5]", "N_indv[25, 6]",
  "N_indv[26, 1]", "N_indv[26, 2]", "N_indv[26, 3]", "N_indv[26, 4]", "N_indv[26, 5]", "N_indv[26, 6]",
  "N_indv[27, 1]", "N_indv[27, 2]", "N_indv[27, 3]", "N_indv[27, 4]", "N_indv[27, 5]", "N_indv[27, 6]",
  "N_indv[28, 1]", "N_indv[28, 2]", "N_indv[28, 3]", "N_indv[28, 4]", "N_indv[28, 5]", "N_indv[28, 6]",
  "N_indv[29, 1]", "N_indv[29, 2]", "N_indv[29, 3]", "N_indv[29, 4]", "N_indv[29, 5]", "N_indv[29, 6]",
  "N_indv[30, 1]", "N_indv[30, 2]", "N_indv[30, 3]", "N_indv[30, 4]", "N_indv[30, 5]", "N_indv[30, 6]",
  "N_indv[31, 1]", "N_indv[31, 2]", "N_indv[31, 3]", "N_indv[31, 4]", "N_indv[31, 5]", "N_indv[31, 6]",
  "N_indv[32, 1]", "N_indv[32, 2]", "N_indv[32, 3]", "N_indv[32, 4]", "N_indv[32, 5]", "N_indv[32, 6]",
  "N_indv[33, 1]", "N_indv[33, 2]", "N_indv[33, 3]", "N_indv[33, 4]", "N_indv[33, 5]", "N_indv[33, 6]",
  "N_indv[34, 1]", "N_indv[34, 2]", "N_indv[34, 3]", "N_indv[34, 4]", "N_indv[34, 5]", "N_indv[34, 6]",
  "N_indv[35, 1]", "N_indv[35, 2]", "N_indv[35, 3]", "N_indv[35, 4]", "N_indv[35, 5]", "N_indv[35, 6]",
  "N_indv[36, 1]", "N_indv[36, 2]", "N_indv[36, 3]", "N_indv[36, 4]", "N_indv[36, 5]", "N_indv[36, 6]",
  "N_indv[37, 1]", "N_indv[37, 2]", "N_indv[37, 3]", "N_indv[37, 4]", "N_indv[37, 5]", "N_indv[37, 6]",
  "N_indv[38, 1]", "N_indv[38, 2]", "N_indv[38, 3]", "N_indv[38, 4]", "N_indv[38, 5]", "N_indv[38, 6]",
  "N_indv[39, 1]", "N_indv[39, 2]", "N_indv[39, 3]", "N_indv[39, 4]", "N_indv[39, 5]", "N_indv[39, 6]",
  "N_indv[40, 1]", "N_indv[40, 2]", "N_indv[40, 3]", "N_indv[40, 4]", "N_indv[40, 5]", "N_indv[40, 6]",
  "N_indv[41, 1]", "N_indv[41, 2]", "N_indv[41, 3]", "N_indv[41, 4]", "N_indv[41, 5]", "N_indv[41, 6]",
  "N_indv[42, 1]", "N_indv[42, 2]", "N_indv[42, 3]", "N_indv[42, 4]", "N_indv[42, 5]", "N_indv[42, 6]",
  "N_indv[43, 1]", "N_indv[43, 2]", "N_indv[43, 3]", "N_indv[43, 4]", "N_indv[43, 5]", "N_indv[43, 6]",
  "N_indv[44, 1]", "N_indv[44, 2]", "N_indv[44, 3]", "N_indv[44, 4]", "N_indv[44, 5]", "N_indv[44, 6]",
  "N_indv[45, 1]", "N_indv[45, 2]", "N_indv[45, 3]", "N_indv[45, 4]", "N_indv[45, 5]", "N_indv[45, 6]",
  "N_indv[46, 1]", "N_indv[46, 2]", "N_indv[46, 3]", "N_indv[46, 4]", "N_indv[46, 5]", "N_indv[46, 6]",
  "N_indv[47, 1]", "N_indv[47, 2]", "N_indv[47, 3]", "N_indv[47, 4]", "N_indv[47, 5]", "N_indv[47, 6]",
  "N_indv[48, 1]", "N_indv[48, 2]", "N_indv[48, 3]", "N_indv[48, 4]", "N_indv[48, 5]", "N_indv[48, 6]",
  "N_indv[49, 1]", "N_indv[49, 2]", "N_indv[49, 3]", "N_indv[49, 4]", "N_indv[49, 5]", "N_indv[49, 6]",
  "N_indv[50, 1]", "N_indv[50, 2]", "N_indv[50, 3]", "N_indv[50, 4]", "N_indv[50, 5]", "N_indv[50, 6]",
  "N_indv[51, 1]", "N_indv[51, 2]", "N_indv[51, 3]", "N_indv[51, 4]", "N_indv[51, 5]", "N_indv[51, 6]",
  "N_indv[52, 1]", "N_indv[52, 2]", "N_indv[52, 3]", "N_indv[52, 4]", "N_indv[52, 5]", "N_indv[52, 6]",
  "N_indv[53, 1]", "N_indv[53, 2]", "N_indv[53, 3]", "N_indv[53, 4]", "N_indv[53, 5]", "N_indv[53, 6]",
  "N_indv[54, 1]", "N_indv[54, 2]", "N_indv[54, 3]", "N_indv[54, 4]", "N_indv[54, 5]", "N_indv[54, 6]",
  "N_indv[55, 1]", "N_indv[55, 2]", "N_indv[55, 3]", "N_indv[55, 4]", "N_indv[55, 5]", "N_indv[55, 6]",
  "N_indv[56, 1]", "N_indv[56, 2]", "N_indv[56, 3]", "N_indv[56, 4]", "N_indv[56, 5]", "N_indv[56, 6]",
  "N_indv[57, 1]", "N_indv[57, 2]", "N_indv[57, 3]", "N_indv[57, 4]", "N_indv[57, 5]", "N_indv[57, 6]",
  "N_indv[58, 1]", "N_indv[58, 2]", "N_indv[58, 3]", "N_indv[58, 4]", "N_indv[58, 5]", "N_indv[58, 6]",
  "N_indv[59, 1]", "N_indv[59, 2]", "N_indv[59, 3]", "N_indv[59, 4]", "N_indv[59, 5]", "N_indv[59, 6]",
  "N_indv[60, 1]", "N_indv[60, 2]", "N_indv[60, 3]", "N_indv[60, 4]", "N_indv[60, 5]", "N_indv[60, 6]"
)
sobs_mcmcConf$addSampler(target = c(
  "N_indv[1, 1]", "N_indv[1, 2]", "N_indv[1, 3]", "N_indv[1, 4]", "N_indv[1, 5]", "N_indv[1, 6]",
  "N_indv[2, 1]", "N_indv[2, 2]", "N_indv[2, 3]", "N_indv[2, 4]", "N_indv[2, 5]", "N_indv[2, 6]",
  "N_indv[3, 1]", "N_indv[3, 2]", "N_indv[3, 3]", "N_indv[3, 4]", "N_indv[3, 5]", "N_indv[3, 6]",
  "N_indv[4, 1]", "N_indv[4, 2]", "N_indv[4, 3]", "N_indv[4, 4]", "N_indv[4, 5]", "N_indv[4, 6]",
  "N_indv[5, 1]", "N_indv[5, 2]", "N_indv[5, 3]", "N_indv[5, 4]", "N_indv[5, 5]", "N_indv[5, 6]",
  "N_indv[6, 1]", "N_indv[6, 2]", "N_indv[6, 3]", "N_indv[6, 4]", "N_indv[6, 5]", "N_indv[6, 6]",
  "N_indv[7, 1]", "N_indv[7, 2]", "N_indv[7, 3]", "N_indv[7, 4]", "N_indv[7, 5]", "N_indv[7, 6]",
  "N_indv[8, 1]", "N_indv[8, 2]", "N_indv[8, 3]", "N_indv[8, 4]", "N_indv[8, 5]", "N_indv[8, 6]",
  "N_indv[9, 1]", "N_indv[9, 2]", "N_indv[9, 3]", "N_indv[9, 4]", "N_indv[9, 5]", "N_indv[9, 6]",
  "N_indv[10, 1]", "N_indv[10, 2]", "N_indv[10, 3]", "N_indv[10, 4]", "N_indv[10, 5]", "N_indv[10, 6]",
  "N_indv[11, 1]", "N_indv[11, 2]", "N_indv[11, 3]", "N_indv[11, 4]", "N_indv[11, 5]", "N_indv[11, 6]",
  "N_indv[12, 1]", "N_indv[12, 2]", "N_indv[12, 3]", "N_indv[12, 4]", "N_indv[12, 5]", "N_indv[12, 6]",
  "N_indv[13, 1]", "N_indv[13, 2]", "N_indv[13, 3]", "N_indv[13, 4]", "N_indv[13, 5]", "N_indv[13, 6]",
  "N_indv[14, 1]", "N_indv[14, 2]", "N_indv[14, 3]", "N_indv[14, 4]", "N_indv[14, 5]", "N_indv[14, 6]",
  "N_indv[15, 1]", "N_indv[15, 2]", "N_indv[15, 3]", "N_indv[15, 4]", "N_indv[15, 5]", "N_indv[15, 6]",
  "N_indv[16, 1]", "N_indv[16, 2]", "N_indv[16, 3]", "N_indv[16, 4]", "N_indv[16, 5]", "N_indv[16, 6]",
  "N_indv[17, 1]", "N_indv[17, 2]", "N_indv[17, 3]", "N_indv[17, 4]", "N_indv[17, 5]", "N_indv[17, 6]",
  "N_indv[18, 1]", "N_indv[18, 2]", "N_indv[18, 3]", "N_indv[18, 4]", "N_indv[18, 5]", "N_indv[18, 6]",
  "N_indv[19, 1]", "N_indv[19, 2]", "N_indv[19, 3]", "N_indv[19, 4]", "N_indv[19, 5]", "N_indv[19, 6]",
  "N_indv[20, 1]", "N_indv[20, 2]", "N_indv[20, 3]", "N_indv[20, 4]", "N_indv[20, 5]", "N_indv[20, 6]",
  "N_indv[21, 1]", "N_indv[21, 2]", "N_indv[21, 3]", "N_indv[21, 4]", "N_indv[21, 5]", "N_indv[21, 6]",
  "N_indv[22, 1]", "N_indv[22, 2]", "N_indv[22, 3]", "N_indv[22, 4]", "N_indv[22, 5]", "N_indv[22, 6]",
  "N_indv[23, 1]", "N_indv[23, 2]", "N_indv[23, 3]", "N_indv[23, 4]", "N_indv[23, 5]", "N_indv[23, 6]",
  "N_indv[24, 1]", "N_indv[24, 2]", "N_indv[24, 3]", "N_indv[24, 4]", "N_indv[24, 5]", "N_indv[24, 6]",
  "N_indv[25, 1]", "N_indv[25, 2]", "N_indv[25, 3]", "N_indv[25, 4]", "N_indv[25, 5]", "N_indv[25, 6]",
  "N_indv[26, 1]", "N_indv[26, 2]", "N_indv[26, 3]", "N_indv[26, 4]", "N_indv[26, 5]", "N_indv[26, 6]",
  "N_indv[27, 1]", "N_indv[27, 2]", "N_indv[27, 3]", "N_indv[27, 4]", "N_indv[27, 5]", "N_indv[27, 6]",
  "N_indv[28, 1]", "N_indv[28, 2]", "N_indv[28, 3]", "N_indv[28, 4]", "N_indv[28, 5]", "N_indv[28, 6]",
  "N_indv[29, 1]", "N_indv[29, 2]", "N_indv[29, 3]", "N_indv[29, 4]", "N_indv[29, 5]", "N_indv[29, 6]",
  "N_indv[30, 1]", "N_indv[30, 2]", "N_indv[30, 3]", "N_indv[30, 4]", "N_indv[30, 5]", "N_indv[30, 6]",
  "N_indv[31, 1]", "N_indv[31, 2]", "N_indv[31, 3]", "N_indv[31, 4]", "N_indv[31, 5]", "N_indv[31, 6]",
  "N_indv[32, 1]", "N_indv[32, 2]", "N_indv[32, 3]", "N_indv[32, 4]", "N_indv[32, 5]", "N_indv[32, 6]",
  "N_indv[33, 1]", "N_indv[33, 2]", "N_indv[33, 3]", "N_indv[33, 4]", "N_indv[33, 5]", "N_indv[33, 6]",
  "N_indv[34, 1]", "N_indv[34, 2]", "N_indv[34, 3]", "N_indv[34, 4]", "N_indv[34, 5]", "N_indv[34, 6]",
  "N_indv[35, 1]", "N_indv[35, 2]", "N_indv[35, 3]", "N_indv[35, 4]", "N_indv[35, 5]", "N_indv[35, 6]",
  "N_indv[36, 1]", "N_indv[36, 2]", "N_indv[36, 3]", "N_indv[36, 4]", "N_indv[36, 5]", "N_indv[36, 6]",
  "N_indv[37, 1]", "N_indv[37, 2]", "N_indv[37, 3]", "N_indv[37, 4]", "N_indv[37, 5]", "N_indv[37, 6]",
  "N_indv[38, 1]", "N_indv[38, 2]", "N_indv[38, 3]", "N_indv[38, 4]", "N_indv[38, 5]", "N_indv[38, 6]",
  "N_indv[39, 1]", "N_indv[39, 2]", "N_indv[39, 3]", "N_indv[39, 4]", "N_indv[39, 5]", "N_indv[39, 6]",
  "N_indv[40, 1]", "N_indv[40, 2]", "N_indv[40, 3]", "N_indv[40, 4]", "N_indv[40, 5]", "N_indv[40, 6]",
  "N_indv[41, 1]", "N_indv[41, 2]", "N_indv[41, 3]", "N_indv[41, 4]", "N_indv[41, 5]", "N_indv[41, 6]",
  "N_indv[42, 1]", "N_indv[42, 2]", "N_indv[42, 3]", "N_indv[42, 4]", "N_indv[42, 5]", "N_indv[42, 6]",
  "N_indv[43, 1]", "N_indv[43, 2]", "N_indv[43, 3]", "N_indv[43, 4]", "N_indv[43, 5]", "N_indv[43, 6]",
  "N_indv[44, 1]", "N_indv[44, 2]", "N_indv[44, 3]", "N_indv[44, 4]", "N_indv[44, 5]", "N_indv[44, 6]",
  "N_indv[45, 1]", "N_indv[45, 2]", "N_indv[45, 3]", "N_indv[45, 4]", "N_indv[45, 5]", "N_indv[45, 6]",
  "N_indv[46, 1]", "N_indv[46, 2]", "N_indv[46, 3]", "N_indv[46, 4]", "N_indv[46, 5]", "N_indv[46, 6]",
  "N_indv[47, 1]", "N_indv[47, 2]", "N_indv[47, 3]", "N_indv[47, 4]", "N_indv[47, 5]", "N_indv[47, 6]",
  "N_indv[48, 1]", "N_indv[48, 2]", "N_indv[48, 3]", "N_indv[48, 4]", "N_indv[48, 5]", "N_indv[48, 6]",
  "N_indv[49, 1]", "N_indv[49, 2]", "N_indv[49, 3]", "N_indv[49, 4]", "N_indv[49, 5]", "N_indv[49, 6]",
  "N_indv[50, 1]", "N_indv[50, 2]", "N_indv[50, 3]", "N_indv[50, 4]", "N_indv[50, 5]", "N_indv[50, 6]",
  "N_indv[51, 1]", "N_indv[51, 2]", "N_indv[51, 3]", "N_indv[51, 4]", "N_indv[51, 5]", "N_indv[51, 6]",
  "N_indv[52, 1]", "N_indv[52, 2]", "N_indv[52, 3]", "N_indv[52, 4]", "N_indv[52, 5]", "N_indv[52, 6]",
  "N_indv[53, 1]", "N_indv[53, 2]", "N_indv[53, 3]", "N_indv[53, 4]", "N_indv[53, 5]", "N_indv[53, 6]",
  "N_indv[54, 1]", "N_indv[54, 2]", "N_indv[54, 3]", "N_indv[54, 4]", "N_indv[54, 5]", "N_indv[54, 6]",
  "N_indv[55, 1]", "N_indv[55, 2]", "N_indv[55, 3]", "N_indv[55, 4]", "N_indv[55, 5]", "N_indv[55, 6]",
  "N_indv[56, 1]", "N_indv[56, 2]", "N_indv[56, 3]", "N_indv[56, 4]", "N_indv[56, 5]", "N_indv[56, 6]",
  "N_indv[57, 1]", "N_indv[57, 2]", "N_indv[57, 3]", "N_indv[57, 4]", "N_indv[57, 5]", "N_indv[57, 6]",
  "N_indv[58, 1]", "N_indv[58, 2]", "N_indv[58, 3]", "N_indv[58, 4]", "N_indv[58, 5]", "N_indv[58, 6]",
  "N_indv[59, 1]", "N_indv[59, 2]", "N_indv[59, 3]", "N_indv[59, 4]", "N_indv[59, 5]", "N_indv[59, 6]",
  "N_indv[60, 1]", "N_indv[60, 2]", "N_indv[60, 3]", "N_indv[60, 4]", "N_indv[60, 5]", "N_indv[60, 6]"
), type = 'RW_block')

# Block all simulated data (n_dct_new) Nodes together
sobs_mcmcConf$removeSamplers(
  "n_dct_new[1, 1]", "n_dct_new[1, 2]", "n_dct_new[1, 3]", "n_dct_new[1, 4]", "n_dct_new[1, 5]", "n_dct_new[1, 6]",
  "n_dct_new[2, 1]", "n_dct_new[2, 2]", "n_dct_new[2, 3]", "n_dct_new[2, 4]", "n_dct_new[2, 5]", "n_dct_new[2, 6]",
  "n_dct_new[3, 1]", "n_dct_new[3, 2]", "n_dct_new[3, 3]", "n_dct_new[3, 4]", "n_dct_new[3, 5]", "n_dct_new[3, 6]",
  "n_dct_new[4, 1]", "n_dct_new[4, 2]", "n_dct_new[4, 3]", "n_dct_new[4, 4]", "n_dct_new[4, 5]", "n_dct_new[4, 6]",
  "n_dct_new[5, 1]", "n_dct_new[5, 2]", "n_dct_new[5, 3]", "n_dct_new[5, 4]", "n_dct_new[5, 5]", "n_dct_new[5, 6]",
  "n_dct_new[6, 1]", "n_dct_new[6, 2]", "n_dct_new[6, 3]", "n_dct_new[6, 4]", "n_dct_new[6, 5]", "n_dct_new[6, 6]",
  "n_dct_new[7, 1]", "n_dct_new[7, 2]", "n_dct_new[7, 3]", "n_dct_new[7, 4]", "n_dct_new[7, 5]", "n_dct_new[7, 6]",
  "n_dct_new[8, 1]", "n_dct_new[8, 2]", "n_dct_new[8, 3]", "n_dct_new[8, 4]", "n_dct_new[8, 5]", "n_dct_new[8, 6]",
  "n_dct_new[9, 1]", "n_dct_new[9, 2]", "n_dct_new[9, 3]", "n_dct_new[9, 4]", "n_dct_new[9, 5]", "n_dct_new[9, 6]",
  "n_dct_new[10, 1]", "n_dct_new[10, 2]", "n_dct_new[10, 3]", "n_dct_new[10, 4]", "n_dct_new[10, 5]", "n_dct_new[10, 6]",
  "n_dct_new[11, 1]", "n_dct_new[11, 2]", "n_dct_new[11, 3]", "n_dct_new[11, 4]", "n_dct_new[11, 5]", "n_dct_new[11, 6]",
  "n_dct_new[12, 1]", "n_dct_new[12, 2]", "n_dct_new[12, 3]", "n_dct_new[12, 4]", "n_dct_new[12, 5]", "n_dct_new[12, 6]",
  "n_dct_new[13, 1]", "n_dct_new[13, 2]", "n_dct_new[13, 3]", "n_dct_new[13, 4]", "n_dct_new[13, 5]", "n_dct_new[13, 6]",
  "n_dct_new[14, 1]", "n_dct_new[14, 2]", "n_dct_new[14, 3]", "n_dct_new[14, 4]", "n_dct_new[14, 5]", "n_dct_new[14, 6]",
  "n_dct_new[15, 1]", "n_dct_new[15, 2]", "n_dct_new[15, 3]", "n_dct_new[15, 4]", "n_dct_new[15, 5]", "n_dct_new[15, 6]",
  "n_dct_new[16, 1]", "n_dct_new[16, 2]", "n_dct_new[16, 3]", "n_dct_new[16, 4]", "n_dct_new[16, 5]", "n_dct_new[16, 6]",
  "n_dct_new[17, 1]", "n_dct_new[17, 2]", "n_dct_new[17, 3]", "n_dct_new[17, 4]", "n_dct_new[17, 5]", "n_dct_new[17, 6]",
  "n_dct_new[18, 1]", "n_dct_new[18, 2]", "n_dct_new[18, 3]", "n_dct_new[18, 4]", "n_dct_new[18, 5]", "n_dct_new[18, 6]",
  "n_dct_new[19, 1]", "n_dct_new[19, 2]", "n_dct_new[19, 3]", "n_dct_new[19, 4]", "n_dct_new[19, 5]", "n_dct_new[19, 6]",
  "n_dct_new[20, 1]", "n_dct_new[20, 2]", "n_dct_new[20, 3]", "n_dct_new[20, 4]", "n_dct_new[20, 5]", "n_dct_new[20, 6]",
  "n_dct_new[21, 1]", "n_dct_new[21, 2]", "n_dct_new[21, 3]", "n_dct_new[21, 4]", "n_dct_new[21, 5]", "n_dct_new[21, 6]",
  "n_dct_new[22, 1]", "n_dct_new[22, 2]", "n_dct_new[22, 3]", "n_dct_new[22, 4]", "n_dct_new[22, 5]", "n_dct_new[22, 6]",
  "n_dct_new[23, 1]", "n_dct_new[23, 2]", "n_dct_new[23, 3]", "n_dct_new[23, 4]", "n_dct_new[23, 5]", "n_dct_new[23, 6]",
  "n_dct_new[24, 1]", "n_dct_new[24, 2]", "n_dct_new[24, 3]", "n_dct_new[24, 4]", "n_dct_new[24, 5]", "n_dct_new[24, 6]",
  "n_dct_new[25, 1]", "n_dct_new[25, 2]", "n_dct_new[25, 3]", "n_dct_new[25, 4]", "n_dct_new[25, 5]", "n_dct_new[25, 6]",
  "n_dct_new[26, 1]", "n_dct_new[26, 2]", "n_dct_new[26, 3]", "n_dct_new[26, 4]", "n_dct_new[26, 5]", "n_dct_new[26, 6]",
  "n_dct_new[27, 1]", "n_dct_new[27, 2]", "n_dct_new[27, 3]", "n_dct_new[27, 4]", "n_dct_new[27, 5]", "n_dct_new[27, 6]",
  "n_dct_new[28, 1]", "n_dct_new[28, 2]", "n_dct_new[28, 3]", "n_dct_new[28, 4]", "n_dct_new[28, 5]", "n_dct_new[28, 6]",
  "n_dct_new[29, 1]", "n_dct_new[29, 2]", "n_dct_new[29, 3]", "n_dct_new[29, 4]", "n_dct_new[29, 5]", "n_dct_new[29, 6]",
  "n_dct_new[30, 1]", "n_dct_new[30, 2]", "n_dct_new[30, 3]", "n_dct_new[30, 4]", "n_dct_new[30, 5]", "n_dct_new[30, 6]",
  "n_dct_new[31, 1]", "n_dct_new[31, 2]", "n_dct_new[31, 3]", "n_dct_new[31, 4]", "n_dct_new[31, 5]", "n_dct_new[31, 6]",
  "n_dct_new[32, 1]", "n_dct_new[32, 2]", "n_dct_new[32, 3]", "n_dct_new[32, 4]", "n_dct_new[32, 5]", "n_dct_new[32, 6]",
  "n_dct_new[33, 1]", "n_dct_new[33, 2]", "n_dct_new[33, 3]", "n_dct_new[33, 4]", "n_dct_new[33, 5]", "n_dct_new[33, 6]",
  "n_dct_new[34, 1]", "n_dct_new[34, 2]", "n_dct_new[34, 3]", "n_dct_new[34, 4]", "n_dct_new[34, 5]", "n_dct_new[34, 6]",
  "n_dct_new[35, 1]", "n_dct_new[35, 2]", "n_dct_new[35, 3]", "n_dct_new[35, 4]", "n_dct_new[35, 5]", "n_dct_new[35, 6]",
  "n_dct_new[36, 1]", "n_dct_new[36, 2]", "n_dct_new[36, 3]", "n_dct_new[36, 4]", "n_dct_new[36, 5]", "n_dct_new[36, 6]",
  "n_dct_new[37, 1]", "n_dct_new[37, 2]", "n_dct_new[37, 3]", "n_dct_new[37, 4]", "n_dct_new[37, 5]", "n_dct_new[37, 6]",
  "n_dct_new[38, 1]", "n_dct_new[38, 2]", "n_dct_new[38, 3]", "n_dct_new[38, 4]", "n_dct_new[38, 5]", "n_dct_new[38, 6]",
  "n_dct_new[39, 1]", "n_dct_new[39, 2]", "n_dct_new[39, 3]", "n_dct_new[39, 4]", "n_dct_new[39, 5]", "n_dct_new[39, 6]",
  "n_dct_new[40, 1]", "n_dct_new[40, 2]", "n_dct_new[40, 3]", "n_dct_new[40, 4]", "n_dct_new[40, 5]", "n_dct_new[40, 6]",
  "n_dct_new[41, 1]", "n_dct_new[41, 2]", "n_dct_new[41, 3]", "n_dct_new[41, 4]", "n_dct_new[41, 5]", "n_dct_new[41, 6]",
  "n_dct_new[42, 1]", "n_dct_new[42, 2]", "n_dct_new[42, 3]", "n_dct_new[42, 4]", "n_dct_new[42, 5]", "n_dct_new[42, 6]",
  "n_dct_new[43, 1]", "n_dct_new[43, 2]", "n_dct_new[43, 3]", "n_dct_new[43, 4]", "n_dct_new[43, 5]", "n_dct_new[43, 6]",
  "n_dct_new[44, 1]", "n_dct_new[44, 2]", "n_dct_new[44, 3]", "n_dct_new[44, 4]", "n_dct_new[44, 5]", "n_dct_new[44, 6]",
  "n_dct_new[45, 1]", "n_dct_new[45, 2]", "n_dct_new[45, 3]", "n_dct_new[45, 4]", "n_dct_new[45, 5]", "n_dct_new[45, 6]",
  "n_dct_new[46, 1]", "n_dct_new[46, 2]", "n_dct_new[46, 3]", "n_dct_new[46, 4]", "n_dct_new[46, 5]", "n_dct_new[46, 6]",
  "n_dct_new[47, 1]", "n_dct_new[47, 2]", "n_dct_new[47, 3]", "n_dct_new[47, 4]", "n_dct_new[47, 5]", "n_dct_new[47, 6]",
  "n_dct_new[48, 1]", "n_dct_new[48, 2]", "n_dct_new[48, 3]", "n_dct_new[48, 4]", "n_dct_new[48, 5]", "n_dct_new[48, 6]",
  "n_dct_new[49, 1]", "n_dct_new[49, 2]", "n_dct_new[49, 3]", "n_dct_new[49, 4]", "n_dct_new[49, 5]", "n_dct_new[49, 6]",
  "n_dct_new[50, 1]", "n_dct_new[50, 2]", "n_dct_new[50, 3]", "n_dct_new[50, 4]", "n_dct_new[50, 5]", "n_dct_new[50, 6]",
  "n_dct_new[51, 1]", "n_dct_new[51, 2]", "n_dct_new[51, 3]", "n_dct_new[51, 4]", "n_dct_new[51, 5]", "n_dct_new[51, 6]",
  "n_dct_new[52, 1]", "n_dct_new[52, 2]", "n_dct_new[52, 3]", "n_dct_new[52, 4]", "n_dct_new[52, 5]", "n_dct_new[52, 6]",
  "n_dct_new[53, 1]", "n_dct_new[53, 2]", "n_dct_new[53, 3]", "n_dct_new[53, 4]", "n_dct_new[53, 5]", "n_dct_new[53, 6]",
  "n_dct_new[54, 1]", "n_dct_new[54, 2]", "n_dct_new[54, 3]", "n_dct_new[54, 4]", "n_dct_new[54, 5]", "n_dct_new[54, 6]",
  "n_dct_new[55, 1]", "n_dct_new[55, 2]", "n_dct_new[55, 3]", "n_dct_new[55, 4]", "n_dct_new[55, 5]", "n_dct_new[55, 6]",
  "n_dct_new[56, 1]", "n_dct_new[56, 2]", "n_dct_new[56, 3]", "n_dct_new[56, 4]", "n_dct_new[56, 5]", "n_dct_new[56, 6]",
  "n_dct_new[57, 1]", "n_dct_new[57, 2]", "n_dct_new[57, 3]", "n_dct_new[57, 4]", "n_dct_new[57, 5]", "n_dct_new[57, 6]",
  "n_dct_new[58, 1]", "n_dct_new[58, 2]", "n_dct_new[58, 3]", "n_dct_new[58, 4]", "n_dct_new[58, 5]", "n_dct_new[58, 6]",
  "n_dct_new[59, 1]", "n_dct_new[59, 2]", "n_dct_new[59, 3]", "n_dct_new[59, 4]", "n_dct_new[59, 5]", "n_dct_new[59, 6]",
  "n_dct_new[60, 1]", "n_dct_new[60, 2]", "n_dct_new[60, 3]", "n_dct_new[60, 4]", "n_dct_new[60, 5]", "n_dct_new[60, 6]"
)
sobs_mcmcConf$addSampler(target = c(
  "n_dct_new[1, 1]", "n_dct_new[1, 2]", "n_dct_new[1, 3]", "n_dct_new[1, 4]", "n_dct_new[1, 5]", "n_dct_new[1, 6]",
  "n_dct_new[2, 1]", "n_dct_new[2, 2]", "n_dct_new[2, 3]", "n_dct_new[2, 4]", "n_dct_new[2, 5]", "n_dct_new[2, 6]",
  "n_dct_new[3, 1]", "n_dct_new[3, 2]", "n_dct_new[3, 3]", "n_dct_new[3, 4]", "n_dct_new[3, 5]", "n_dct_new[3, 6]",
  "n_dct_new[4, 1]", "n_dct_new[4, 2]", "n_dct_new[4, 3]", "n_dct_new[4, 4]", "n_dct_new[4, 5]", "n_dct_new[4, 6]",
  "n_dct_new[5, 1]", "n_dct_new[5, 2]", "n_dct_new[5, 3]", "n_dct_new[5, 4]", "n_dct_new[5, 5]", "n_dct_new[5, 6]",
  "n_dct_new[6, 1]", "n_dct_new[6, 2]", "n_dct_new[6, 3]", "n_dct_new[6, 4]", "n_dct_new[6, 5]", "n_dct_new[6, 6]",
  "n_dct_new[7, 1]", "n_dct_new[7, 2]", "n_dct_new[7, 3]", "n_dct_new[7, 4]", "n_dct_new[7, 5]", "n_dct_new[7, 6]",
  "n_dct_new[8, 1]", "n_dct_new[8, 2]", "n_dct_new[8, 3]", "n_dct_new[8, 4]", "n_dct_new[8, 5]", "n_dct_new[8, 6]",
  "n_dct_new[9, 1]", "n_dct_new[9, 2]", "n_dct_new[9, 3]", "n_dct_new[9, 4]", "n_dct_new[9, 5]", "n_dct_new[9, 6]",
  "n_dct_new[10, 1]", "n_dct_new[10, 2]", "n_dct_new[10, 3]", "n_dct_new[10, 4]", "n_dct_new[10, 5]", "n_dct_new[10, 6]",
  "n_dct_new[11, 1]", "n_dct_new[11, 2]", "n_dct_new[11, 3]", "n_dct_new[11, 4]", "n_dct_new[11, 5]", "n_dct_new[11, 6]",
  "n_dct_new[12, 1]", "n_dct_new[12, 2]", "n_dct_new[12, 3]", "n_dct_new[12, 4]", "n_dct_new[12, 5]", "n_dct_new[12, 6]",
  "n_dct_new[13, 1]", "n_dct_new[13, 2]", "n_dct_new[13, 3]", "n_dct_new[13, 4]", "n_dct_new[13, 5]", "n_dct_new[13, 6]",
  "n_dct_new[14, 1]", "n_dct_new[14, 2]", "n_dct_new[14, 3]", "n_dct_new[14, 4]", "n_dct_new[14, 5]", "n_dct_new[14, 6]",
  "n_dct_new[15, 1]", "n_dct_new[15, 2]", "n_dct_new[15, 3]", "n_dct_new[15, 4]", "n_dct_new[15, 5]", "n_dct_new[15, 6]",
  "n_dct_new[16, 1]", "n_dct_new[16, 2]", "n_dct_new[16, 3]", "n_dct_new[16, 4]", "n_dct_new[16, 5]", "n_dct_new[16, 6]",
  "n_dct_new[17, 1]", "n_dct_new[17, 2]", "n_dct_new[17, 3]", "n_dct_new[17, 4]", "n_dct_new[17, 5]", "n_dct_new[17, 6]",
  "n_dct_new[18, 1]", "n_dct_new[18, 2]", "n_dct_new[18, 3]", "n_dct_new[18, 4]", "n_dct_new[18, 5]", "n_dct_new[18, 6]",
  "n_dct_new[19, 1]", "n_dct_new[19, 2]", "n_dct_new[19, 3]", "n_dct_new[19, 4]", "n_dct_new[19, 5]", "n_dct_new[19, 6]",
  "n_dct_new[20, 1]", "n_dct_new[20, 2]", "n_dct_new[20, 3]", "n_dct_new[20, 4]", "n_dct_new[20, 5]", "n_dct_new[20, 6]",
  "n_dct_new[21, 1]", "n_dct_new[21, 2]", "n_dct_new[21, 3]", "n_dct_new[21, 4]", "n_dct_new[21, 5]", "n_dct_new[21, 6]",
  "n_dct_new[22, 1]", "n_dct_new[22, 2]", "n_dct_new[22, 3]", "n_dct_new[22, 4]", "n_dct_new[22, 5]", "n_dct_new[22, 6]",
  "n_dct_new[23, 1]", "n_dct_new[23, 2]", "n_dct_new[23, 3]", "n_dct_new[23, 4]", "n_dct_new[23, 5]", "n_dct_new[23, 6]",
  "n_dct_new[24, 1]", "n_dct_new[24, 2]", "n_dct_new[24, 3]", "n_dct_new[24, 4]", "n_dct_new[24, 5]", "n_dct_new[24, 6]",
  "n_dct_new[25, 1]", "n_dct_new[25, 2]", "n_dct_new[25, 3]", "n_dct_new[25, 4]", "n_dct_new[25, 5]", "n_dct_new[25, 6]",
  "n_dct_new[26, 1]", "n_dct_new[26, 2]", "n_dct_new[26, 3]", "n_dct_new[26, 4]", "n_dct_new[26, 5]", "n_dct_new[26, 6]",
  "n_dct_new[27, 1]", "n_dct_new[27, 2]", "n_dct_new[27, 3]", "n_dct_new[27, 4]", "n_dct_new[27, 5]", "n_dct_new[27, 6]",
  "n_dct_new[28, 1]", "n_dct_new[28, 2]", "n_dct_new[28, 3]", "n_dct_new[28, 4]", "n_dct_new[28, 5]", "n_dct_new[28, 6]",
  "n_dct_new[29, 1]", "n_dct_new[29, 2]", "n_dct_new[29, 3]", "n_dct_new[29, 4]", "n_dct_new[29, 5]", "n_dct_new[29, 6]",
  "n_dct_new[30, 1]", "n_dct_new[30, 2]", "n_dct_new[30, 3]", "n_dct_new[30, 4]", "n_dct_new[30, 5]", "n_dct_new[30, 6]",
  "n_dct_new[31, 1]", "n_dct_new[31, 2]", "n_dct_new[31, 3]", "n_dct_new[31, 4]", "n_dct_new[31, 5]", "n_dct_new[31, 6]",
  "n_dct_new[32, 1]", "n_dct_new[32, 2]", "n_dct_new[32, 3]", "n_dct_new[32, 4]", "n_dct_new[32, 5]", "n_dct_new[32, 6]",
  "n_dct_new[33, 1]", "n_dct_new[33, 2]", "n_dct_new[33, 3]", "n_dct_new[33, 4]", "n_dct_new[33, 5]", "n_dct_new[33, 6]",
  "n_dct_new[34, 1]", "n_dct_new[34, 2]", "n_dct_new[34, 3]", "n_dct_new[34, 4]", "n_dct_new[34, 5]", "n_dct_new[34, 6]",
  "n_dct_new[35, 1]", "n_dct_new[35, 2]", "n_dct_new[35, 3]", "n_dct_new[35, 4]", "n_dct_new[35, 5]", "n_dct_new[35, 6]",
  "n_dct_new[36, 1]", "n_dct_new[36, 2]", "n_dct_new[36, 3]", "n_dct_new[36, 4]", "n_dct_new[36, 5]", "n_dct_new[36, 6]",
  "n_dct_new[37, 1]", "n_dct_new[37, 2]", "n_dct_new[37, 3]", "n_dct_new[37, 4]", "n_dct_new[37, 5]", "n_dct_new[37, 6]",
  "n_dct_new[38, 1]", "n_dct_new[38, 2]", "n_dct_new[38, 3]", "n_dct_new[38, 4]", "n_dct_new[38, 5]", "n_dct_new[38, 6]",
  "n_dct_new[39, 1]", "n_dct_new[39, 2]", "n_dct_new[39, 3]", "n_dct_new[39, 4]", "n_dct_new[39, 5]", "n_dct_new[39, 6]",
  "n_dct_new[40, 1]", "n_dct_new[40, 2]", "n_dct_new[40, 3]", "n_dct_new[40, 4]", "n_dct_new[40, 5]", "n_dct_new[40, 6]",
  "n_dct_new[41, 1]", "n_dct_new[41, 2]", "n_dct_new[41, 3]", "n_dct_new[41, 4]", "n_dct_new[41, 5]", "n_dct_new[41, 6]",
  "n_dct_new[42, 1]", "n_dct_new[42, 2]", "n_dct_new[42, 3]", "n_dct_new[42, 4]", "n_dct_new[42, 5]", "n_dct_new[42, 6]",
  "n_dct_new[43, 1]", "n_dct_new[43, 2]", "n_dct_new[43, 3]", "n_dct_new[43, 4]", "n_dct_new[43, 5]", "n_dct_new[43, 6]",
  "n_dct_new[44, 1]", "n_dct_new[44, 2]", "n_dct_new[44, 3]", "n_dct_new[44, 4]", "n_dct_new[44, 5]", "n_dct_new[44, 6]",
  "n_dct_new[45, 1]", "n_dct_new[45, 2]", "n_dct_new[45, 3]", "n_dct_new[45, 4]", "n_dct_new[45, 5]", "n_dct_new[45, 6]",
  "n_dct_new[46, 1]", "n_dct_new[46, 2]", "n_dct_new[46, 3]", "n_dct_new[46, 4]", "n_dct_new[46, 5]", "n_dct_new[46, 6]",
  "n_dct_new[47, 1]", "n_dct_new[47, 2]", "n_dct_new[47, 3]", "n_dct_new[47, 4]", "n_dct_new[47, 5]", "n_dct_new[47, 6]",
  "n_dct_new[48, 1]", "n_dct_new[48, 2]", "n_dct_new[48, 3]", "n_dct_new[48, 4]", "n_dct_new[48, 5]", "n_dct_new[48, 6]",
  "n_dct_new[49, 1]", "n_dct_new[49, 2]", "n_dct_new[49, 3]", "n_dct_new[49, 4]", "n_dct_new[49, 5]", "n_dct_new[49, 6]",
  "n_dct_new[50, 1]", "n_dct_new[50, 2]", "n_dct_new[50, 3]", "n_dct_new[50, 4]", "n_dct_new[50, 5]", "n_dct_new[50, 6]",
  "n_dct_new[51, 1]", "n_dct_new[51, 2]", "n_dct_new[51, 3]", "n_dct_new[51, 4]", "n_dct_new[51, 5]", "n_dct_new[51, 6]",
  "n_dct_new[52, 1]", "n_dct_new[52, 2]", "n_dct_new[52, 3]", "n_dct_new[52, 4]", "n_dct_new[52, 5]", "n_dct_new[52, 6]",
  "n_dct_new[53, 1]", "n_dct_new[53, 2]", "n_dct_new[53, 3]", "n_dct_new[53, 4]", "n_dct_new[53, 5]", "n_dct_new[53, 6]",
  "n_dct_new[54, 1]", "n_dct_new[54, 2]", "n_dct_new[54, 3]", "n_dct_new[54, 4]", "n_dct_new[54, 5]", "n_dct_new[54, 6]",
  "n_dct_new[55, 1]", "n_dct_new[55, 2]", "n_dct_new[55, 3]", "n_dct_new[55, 4]", "n_dct_new[55, 5]", "n_dct_new[55, 6]",
  "n_dct_new[56, 1]", "n_dct_new[56, 2]", "n_dct_new[56, 3]", "n_dct_new[56, 4]", "n_dct_new[56, 5]", "n_dct_new[56, 6]",
  "n_dct_new[57, 1]", "n_dct_new[57, 2]", "n_dct_new[57, 3]", "n_dct_new[57, 4]", "n_dct_new[57, 5]", "n_dct_new[57, 6]",
  "n_dct_new[58, 1]", "n_dct_new[58, 2]", "n_dct_new[58, 3]", "n_dct_new[58, 4]", "n_dct_new[58, 5]", "n_dct_new[58, 6]",
  "n_dct_new[59, 1]", "n_dct_new[59, 2]", "n_dct_new[59, 3]", "n_dct_new[59, 4]", "n_dct_new[59, 5]", "n_dct_new[59, 6]",
  "n_dct_new[60, 1]", "n_dct_new[60, 2]", "n_dct_new[60, 3]", "n_dct_new[60, 4]", "n_dct_new[60, 5]", "n_dct_new[60, 6]"
),
  type = 'RW_block')

# Block all available birds nodes (n_avail) Nodes together
sobs_mcmcConf$removeSamplers(
  "n_avail[1, 1]", "n_avail[1, 2]", "n_avail[1, 3]", "n_avail[1, 4]", "n_avail[1, 5]", "n_avail[1, 6]",
  "n_avail[2, 1]", "n_avail[2, 2]", "n_avail[2, 3]", "n_avail[2, 4]", "n_avail[2, 5]", "n_avail[2, 6]",
  "n_avail[3, 1]", "n_avail[3, 2]", "n_avail[3, 3]", "n_avail[3, 4]", "n_avail[3, 5]", "n_avail[3, 6]",
  "n_avail[4, 1]", "n_avail[4, 2]", "n_avail[4, 3]", "n_avail[4, 4]", "n_avail[4, 5]", "n_avail[4, 6]",
  "n_avail[5, 1]", "n_avail[5, 2]", "n_avail[5, 3]", "n_avail[5, 4]", "n_avail[5, 5]", "n_avail[5, 6]",
  "n_avail[6, 1]", "n_avail[6, 2]", "n_avail[6, 3]", "n_avail[6, 4]", "n_avail[6, 5]", "n_avail[6, 6]",
  "n_avail[7, 1]", "n_avail[7, 2]", "n_avail[7, 3]", "n_avail[7, 4]", "n_avail[7, 5]", "n_avail[7, 6]",
  "n_avail[8, 1]", "n_avail[8, 2]", "n_avail[8, 3]", "n_avail[8, 4]", "n_avail[8, 5]", "n_avail[8, 6]",
  "n_avail[9, 1]", "n_avail[9, 2]", "n_avail[9, 3]", "n_avail[9, 4]", "n_avail[9, 5]", "n_avail[9, 6]",
  "n_avail[10, 1]", "n_avail[10, 2]", "n_avail[10, 3]", "n_avail[10, 4]", "n_avail[10, 5]", "n_avail[10, 6]",
  "n_avail[11, 1]", "n_avail[11, 2]", "n_avail[11, 3]", "n_avail[11, 4]", "n_avail[11, 5]", "n_avail[11, 6]",
  "n_avail[12, 1]", "n_avail[12, 2]", "n_avail[12, 3]", "n_avail[12, 4]", "n_avail[12, 5]", "n_avail[12, 6]",
  "n_avail[13, 1]", "n_avail[13, 2]", "n_avail[13, 3]", "n_avail[13, 4]", "n_avail[13, 5]", "n_avail[13, 6]",
  "n_avail[14, 1]", "n_avail[14, 2]", "n_avail[14, 3]", "n_avail[14, 4]", "n_avail[14, 5]", "n_avail[14, 6]",
  "n_avail[15, 1]", "n_avail[15, 2]", "n_avail[15, 3]", "n_avail[15, 4]", "n_avail[15, 5]", "n_avail[15, 6]",
  "n_avail[16, 1]", "n_avail[16, 2]", "n_avail[16, 3]", "n_avail[16, 4]", "n_avail[16, 5]", "n_avail[16, 6]",
  "n_avail[17, 1]", "n_avail[17, 2]", "n_avail[17, 3]", "n_avail[17, 4]", "n_avail[17, 5]", "n_avail[17, 6]",
  "n_avail[18, 1]", "n_avail[18, 2]", "n_avail[18, 3]", "n_avail[18, 4]", "n_avail[18, 5]", "n_avail[18, 6]",
  "n_avail[19, 1]", "n_avail[19, 2]", "n_avail[19, 3]", "n_avail[19, 4]", "n_avail[19, 5]", "n_avail[19, 6]",
  "n_avail[20, 1]", "n_avail[20, 2]", "n_avail[20, 3]", "n_avail[20, 4]", "n_avail[20, 5]", "n_avail[20, 6]",
  "n_avail[21, 1]", "n_avail[21, 2]", "n_avail[21, 3]", "n_avail[21, 4]", "n_avail[21, 5]", "n_avail[21, 6]",
  "n_avail[22, 1]", "n_avail[22, 2]", "n_avail[22, 3]", "n_avail[22, 4]", "n_avail[22, 5]", "n_avail[22, 6]",
  "n_avail[23, 1]", "n_avail[23, 2]", "n_avail[23, 3]", "n_avail[23, 4]", "n_avail[23, 5]", "n_avail[23, 6]",
  "n_avail[24, 1]", "n_avail[24, 2]", "n_avail[24, 3]", "n_avail[24, 4]", "n_avail[24, 5]", "n_avail[24, 6]",
  "n_avail[25, 1]", "n_avail[25, 2]", "n_avail[25, 3]", "n_avail[25, 4]", "n_avail[25, 5]", "n_avail[25, 6]",
  "n_avail[26, 1]", "n_avail[26, 2]", "n_avail[26, 3]", "n_avail[26, 4]", "n_avail[26, 5]", "n_avail[26, 6]",
  "n_avail[27, 1]", "n_avail[27, 2]", "n_avail[27, 3]", "n_avail[27, 4]", "n_avail[27, 5]", "n_avail[27, 6]",
  "n_avail[28, 1]", "n_avail[28, 2]", "n_avail[28, 3]", "n_avail[28, 4]", "n_avail[28, 5]", "n_avail[28, 6]",
  "n_avail[29, 1]", "n_avail[29, 2]", "n_avail[29, 3]", "n_avail[29, 4]", "n_avail[29, 5]", "n_avail[29, 6]",
  "n_avail[30, 1]", "n_avail[30, 2]", "n_avail[30, 3]", "n_avail[30, 4]", "n_avail[30, 5]", "n_avail[30, 6]",
  "n_avail[31, 1]", "n_avail[31, 2]", "n_avail[31, 3]", "n_avail[31, 4]", "n_avail[31, 5]", "n_avail[31, 6]",
  "n_avail[32, 1]", "n_avail[32, 2]", "n_avail[32, 3]", "n_avail[32, 4]", "n_avail[32, 5]", "n_avail[32, 6]",
  "n_avail[33, 1]", "n_avail[33, 2]", "n_avail[33, 3]", "n_avail[33, 4]", "n_avail[33, 5]", "n_avail[33, 6]",
  "n_avail[34, 1]", "n_avail[34, 2]", "n_avail[34, 3]", "n_avail[34, 4]", "n_avail[34, 5]", "n_avail[34, 6]",
  "n_avail[35, 1]", "n_avail[35, 2]", "n_avail[35, 3]", "n_avail[35, 4]", "n_avail[35, 5]", "n_avail[35, 6]",
  "n_avail[36, 1]", "n_avail[36, 2]", "n_avail[36, 3]", "n_avail[36, 4]", "n_avail[36, 5]", "n_avail[36, 6]",
  "n_avail[37, 1]", "n_avail[37, 2]", "n_avail[37, 3]", "n_avail[37, 4]", "n_avail[37, 5]", "n_avail[37, 6]",
  "n_avail[38, 1]", "n_avail[38, 2]", "n_avail[38, 3]", "n_avail[38, 4]", "n_avail[38, 5]", "n_avail[38, 6]",
  "n_avail[39, 1]", "n_avail[39, 2]", "n_avail[39, 3]", "n_avail[39, 4]", "n_avail[39, 5]", "n_avail[39, 6]",
  "n_avail[40, 1]", "n_avail[40, 2]", "n_avail[40, 3]", "n_avail[40, 4]", "n_avail[40, 5]", "n_avail[40, 6]",
  "n_avail[41, 1]", "n_avail[41, 2]", "n_avail[41, 3]", "n_avail[41, 4]", "n_avail[41, 5]", "n_avail[41, 6]",
  "n_avail[42, 1]", "n_avail[42, 2]", "n_avail[42, 3]", "n_avail[42, 4]", "n_avail[42, 5]", "n_avail[42, 6]",
  "n_avail[43, 1]", "n_avail[43, 2]", "n_avail[43, 3]", "n_avail[43, 4]", "n_avail[43, 5]", "n_avail[43, 6]",
  "n_avail[44, 1]", "n_avail[44, 2]", "n_avail[44, 3]", "n_avail[44, 4]", "n_avail[44, 5]", "n_avail[44, 6]",
  "n_avail[45, 1]", "n_avail[45, 2]", "n_avail[45, 3]", "n_avail[45, 4]", "n_avail[45, 5]", "n_avail[45, 6]",
  "n_avail[46, 1]", "n_avail[46, 2]", "n_avail[46, 3]", "n_avail[46, 4]", "n_avail[46, 5]", "n_avail[46, 6]",
  "n_avail[47, 1]", "n_avail[47, 2]", "n_avail[47, 3]", "n_avail[47, 4]", "n_avail[47, 5]", "n_avail[47, 6]",
  "n_avail[48, 1]", "n_avail[48, 2]", "n_avail[48, 3]", "n_avail[48, 4]", "n_avail[48, 5]", "n_avail[48, 6]",
  "n_avail[49, 1]", "n_avail[49, 2]", "n_avail[49, 3]", "n_avail[49, 4]", "n_avail[49, 5]", "n_avail[49, 6]",
  "n_avail[50, 1]", "n_avail[50, 2]", "n_avail[50, 3]", "n_avail[50, 4]", "n_avail[50, 5]", "n_avail[50, 6]",
  "n_avail[51, 1]", "n_avail[51, 2]", "n_avail[51, 3]", "n_avail[51, 4]", "n_avail[51, 5]", "n_avail[51, 6]",
  "n_avail[52, 1]", "n_avail[52, 2]", "n_avail[52, 3]", "n_avail[52, 4]", "n_avail[52, 5]", "n_avail[52, 6]",
  "n_avail[53, 1]", "n_avail[53, 2]", "n_avail[53, 3]", "n_avail[53, 4]", "n_avail[53, 5]", "n_avail[53, 6]",
  "n_avail[54, 1]", "n_avail[54, 2]", "n_avail[54, 3]", "n_avail[54, 4]", "n_avail[54, 5]", "n_avail[54, 6]",
  "n_avail[55, 1]", "n_avail[55, 2]", "n_avail[55, 3]", "n_avail[55, 4]", "n_avail[55, 5]", "n_avail[55, 6]",
  "n_avail[56, 1]", "n_avail[56, 2]", "n_avail[56, 3]", "n_avail[56, 4]", "n_avail[56, 5]", "n_avail[56, 6]",
  "n_avail[57, 1]", "n_avail[57, 2]", "n_avail[57, 3]", "n_avail[57, 4]", "n_avail[57, 5]", "n_avail[57, 6]",
  "n_avail[58, 1]", "n_avail[58, 2]", "n_avail[58, 3]", "n_avail[58, 4]", "n_avail[58, 5]", "n_avail[58, 6]",
  "n_avail[59, 1]", "n_avail[59, 2]", "n_avail[59, 3]", "n_avail[59, 4]", "n_avail[59, 5]", "n_avail[59, 6]",
  "n_avail[60, 1]", "n_avail[60, 2]", "n_avail[60, 3]", "n_avail[60, 4]", "n_avail[60, 5]", "n_avail[60, 6]"
)
sobs_mcmcConf$addSampler(target = c(
  "n_avail[1, 1]", "n_avail[1, 2]", "n_avail[1, 3]", "n_avail[1, 4]", "n_avail[1, 5]", "n_avail[1, 6]",
  "n_avail[2, 1]", "n_avail[2, 2]", "n_avail[2, 3]", "n_avail[2, 4]", "n_avail[2, 5]", "n_avail[2, 6]",
  "n_avail[3, 1]", "n_avail[3, 2]", "n_avail[3, 3]", "n_avail[3, 4]", "n_avail[3, 5]", "n_avail[3, 6]",
  "n_avail[4, 1]", "n_avail[4, 2]", "n_avail[4, 3]", "n_avail[4, 4]", "n_avail[4, 5]", "n_avail[4, 6]",
  "n_avail[5, 1]", "n_avail[5, 2]", "n_avail[5, 3]", "n_avail[5, 4]", "n_avail[5, 5]", "n_avail[5, 6]",
  "n_avail[6, 1]", "n_avail[6, 2]", "n_avail[6, 3]", "n_avail[6, 4]", "n_avail[6, 5]", "n_avail[6, 6]",
  "n_avail[7, 1]", "n_avail[7, 2]", "n_avail[7, 3]", "n_avail[7, 4]", "n_avail[7, 5]", "n_avail[7, 6]",
  "n_avail[8, 1]", "n_avail[8, 2]", "n_avail[8, 3]", "n_avail[8, 4]", "n_avail[8, 5]", "n_avail[8, 6]",
  "n_avail[9, 1]", "n_avail[9, 2]", "n_avail[9, 3]", "n_avail[9, 4]", "n_avail[9, 5]", "n_avail[9, 6]",
  "n_avail[10, 1]", "n_avail[10, 2]", "n_avail[10, 3]", "n_avail[10, 4]", "n_avail[10, 5]", "n_avail[10, 6]",
  "n_avail[11, 1]", "n_avail[11, 2]", "n_avail[11, 3]", "n_avail[11, 4]", "n_avail[11, 5]", "n_avail[11, 6]",
  "n_avail[12, 1]", "n_avail[12, 2]", "n_avail[12, 3]", "n_avail[12, 4]", "n_avail[12, 5]", "n_avail[12, 6]",
  "n_avail[13, 1]", "n_avail[13, 2]", "n_avail[13, 3]", "n_avail[13, 4]", "n_avail[13, 5]", "n_avail[13, 6]",
  "n_avail[14, 1]", "n_avail[14, 2]", "n_avail[14, 3]", "n_avail[14, 4]", "n_avail[14, 5]", "n_avail[14, 6]",
  "n_avail[15, 1]", "n_avail[15, 2]", "n_avail[15, 3]", "n_avail[15, 4]", "n_avail[15, 5]", "n_avail[15, 6]",
  "n_avail[16, 1]", "n_avail[16, 2]", "n_avail[16, 3]", "n_avail[16, 4]", "n_avail[16, 5]", "n_avail[16, 6]",
  "n_avail[17, 1]", "n_avail[17, 2]", "n_avail[17, 3]", "n_avail[17, 4]", "n_avail[17, 5]", "n_avail[17, 6]",
  "n_avail[18, 1]", "n_avail[18, 2]", "n_avail[18, 3]", "n_avail[18, 4]", "n_avail[18, 5]", "n_avail[18, 6]",
  "n_avail[19, 1]", "n_avail[19, 2]", "n_avail[19, 3]", "n_avail[19, 4]", "n_avail[19, 5]", "n_avail[19, 6]",
  "n_avail[20, 1]", "n_avail[20, 2]", "n_avail[20, 3]", "n_avail[20, 4]", "n_avail[20, 5]", "n_avail[20, 6]",
  "n_avail[21, 1]", "n_avail[21, 2]", "n_avail[21, 3]", "n_avail[21, 4]", "n_avail[21, 5]", "n_avail[21, 6]",
  "n_avail[22, 1]", "n_avail[22, 2]", "n_avail[22, 3]", "n_avail[22, 4]", "n_avail[22, 5]", "n_avail[22, 6]",
  "n_avail[23, 1]", "n_avail[23, 2]", "n_avail[23, 3]", "n_avail[23, 4]", "n_avail[23, 5]", "n_avail[23, 6]",
  "n_avail[24, 1]", "n_avail[24, 2]", "n_avail[24, 3]", "n_avail[24, 4]", "n_avail[24, 5]", "n_avail[24, 6]",
  "n_avail[25, 1]", "n_avail[25, 2]", "n_avail[25, 3]", "n_avail[25, 4]", "n_avail[25, 5]", "n_avail[25, 6]",
  "n_avail[26, 1]", "n_avail[26, 2]", "n_avail[26, 3]", "n_avail[26, 4]", "n_avail[26, 5]", "n_avail[26, 6]",
  "n_avail[27, 1]", "n_avail[27, 2]", "n_avail[27, 3]", "n_avail[27, 4]", "n_avail[27, 5]", "n_avail[27, 6]",
  "n_avail[28, 1]", "n_avail[28, 2]", "n_avail[28, 3]", "n_avail[28, 4]", "n_avail[28, 5]", "n_avail[28, 6]",
  "n_avail[29, 1]", "n_avail[29, 2]", "n_avail[29, 3]", "n_avail[29, 4]", "n_avail[29, 5]", "n_avail[29, 6]",
  "n_avail[30, 1]", "n_avail[30, 2]", "n_avail[30, 3]", "n_avail[30, 4]", "n_avail[30, 5]", "n_avail[30, 6]",
  "n_avail[31, 1]", "n_avail[31, 2]", "n_avail[31, 3]", "n_avail[31, 4]", "n_avail[31, 5]", "n_avail[31, 6]",
  "n_avail[32, 1]", "n_avail[32, 2]", "n_avail[32, 3]", "n_avail[32, 4]", "n_avail[32, 5]", "n_avail[32, 6]",
  "n_avail[33, 1]", "n_avail[33, 2]", "n_avail[33, 3]", "n_avail[33, 4]", "n_avail[33, 5]", "n_avail[33, 6]",
  "n_avail[34, 1]", "n_avail[34, 2]", "n_avail[34, 3]", "n_avail[34, 4]", "n_avail[34, 5]", "n_avail[34, 6]",
  "n_avail[35, 1]", "n_avail[35, 2]", "n_avail[35, 3]", "n_avail[35, 4]", "n_avail[35, 5]", "n_avail[35, 6]",
  "n_avail[36, 1]", "n_avail[36, 2]", "n_avail[36, 3]", "n_avail[36, 4]", "n_avail[36, 5]", "n_avail[36, 6]",
  "n_avail[37, 1]", "n_avail[37, 2]", "n_avail[37, 3]", "n_avail[37, 4]", "n_avail[37, 5]", "n_avail[37, 6]",
  "n_avail[38, 1]", "n_avail[38, 2]", "n_avail[38, 3]", "n_avail[38, 4]", "n_avail[38, 5]", "n_avail[38, 6]",
  "n_avail[39, 1]", "n_avail[39, 2]", "n_avail[39, 3]", "n_avail[39, 4]", "n_avail[39, 5]", "n_avail[39, 6]",
  "n_avail[40, 1]", "n_avail[40, 2]", "n_avail[40, 3]", "n_avail[40, 4]", "n_avail[40, 5]", "n_avail[40, 6]",
  "n_avail[41, 1]", "n_avail[41, 2]", "n_avail[41, 3]", "n_avail[41, 4]", "n_avail[41, 5]", "n_avail[41, 6]",
  "n_avail[42, 1]", "n_avail[42, 2]", "n_avail[42, 3]", "n_avail[42, 4]", "n_avail[42, 5]", "n_avail[42, 6]",
  "n_avail[43, 1]", "n_avail[43, 2]", "n_avail[43, 3]", "n_avail[43, 4]", "n_avail[43, 5]", "n_avail[43, 6]",
  "n_avail[44, 1]", "n_avail[44, 2]", "n_avail[44, 3]", "n_avail[44, 4]", "n_avail[44, 5]", "n_avail[44, 6]",
  "n_avail[45, 1]", "n_avail[45, 2]", "n_avail[45, 3]", "n_avail[45, 4]", "n_avail[45, 5]", "n_avail[45, 6]",
  "n_avail[46, 1]", "n_avail[46, 2]", "n_avail[46, 3]", "n_avail[46, 4]", "n_avail[46, 5]", "n_avail[46, 6]",
  "n_avail[47, 1]", "n_avail[47, 2]", "n_avail[47, 3]", "n_avail[47, 4]", "n_avail[47, 5]", "n_avail[47, 6]",
  "n_avail[48, 1]", "n_avail[48, 2]", "n_avail[48, 3]", "n_avail[48, 4]", "n_avail[48, 5]", "n_avail[48, 6]",
  "n_avail[49, 1]", "n_avail[49, 2]", "n_avail[49, 3]", "n_avail[49, 4]", "n_avail[49, 5]", "n_avail[49, 6]",
  "n_avail[50, 1]", "n_avail[50, 2]", "n_avail[50, 3]", "n_avail[50, 4]", "n_avail[50, 5]", "n_avail[50, 6]",
  "n_avail[51, 1]", "n_avail[51, 2]", "n_avail[51, 3]", "n_avail[51, 4]", "n_avail[51, 5]", "n_avail[51, 6]",
  "n_avail[52, 1]", "n_avail[52, 2]", "n_avail[52, 3]", "n_avail[52, 4]", "n_avail[52, 5]", "n_avail[52, 6]",
  "n_avail[53, 1]", "n_avail[53, 2]", "n_avail[53, 3]", "n_avail[53, 4]", "n_avail[53, 5]", "n_avail[53, 6]",
  "n_avail[54, 1]", "n_avail[54, 2]", "n_avail[54, 3]", "n_avail[54, 4]", "n_avail[54, 5]", "n_avail[54, 6]",
  "n_avail[55, 1]", "n_avail[55, 2]", "n_avail[55, 3]", "n_avail[55, 4]", "n_avail[55, 5]", "n_avail[55, 6]",
  "n_avail[56, 1]", "n_avail[56, 2]", "n_avail[56, 3]", "n_avail[56, 4]", "n_avail[56, 5]", "n_avail[56, 6]",
  "n_avail[57, 1]", "n_avail[57, 2]", "n_avail[57, 3]", "n_avail[57, 4]", "n_avail[57, 5]", "n_avail[57, 6]",
  "n_avail[58, 1]", "n_avail[58, 2]", "n_avail[58, 3]", "n_avail[58, 4]", "n_avail[58, 5]", "n_avail[58, 6]",
  "n_avail[59, 1]", "n_avail[59, 2]", "n_avail[59, 3]", "n_avail[59, 4]", "n_avail[59, 5]", "n_avail[59, 6]",
  "n_avail[60, 1]", "n_avail[60, 2]", "n_avail[60, 3]", "n_avail[60, 4]", "n_avail[60, 5]", "n_avail[60, 6]"
),
type = 'RW_block')


# View the blocks
sobs_mcmcConf$printSamplers()     # Print samplers being used 
sobs_mcmcConf$unsampledNodes      # Look at unsampled nodes
sobs_mcmcConf$getUnsampledNodes() # Names of unsampled nodes

# Build MCMC object: can customize thinning, monitors, etc.
sobs_modelMCMC <- buildMCMC(sobs_mcmcConf) # Update mcmc configuration based on graph

# Compile Nimble object and MCMC object to one C++ object (gets an ~3 fold speed up, even 
# if you do nothing else) spend time on getting this step right because this is the 
# easiest way to accelerate convergence. 
cModel <- compileNimble(sobs_model_vect) # compiled model
cMCMC <- compileNimble(sobs_modelMCMC, project = cModel, resetFunctions = T) # compiled mcmc

# MCMC settings for the real model. Pick one, comment out the rest 
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1        # Quick test to see if the model runs
# nc <- 3  ;  ni <- 150000  ;  nb <- 50000;  nt <- 10  # longer test where most parameters should
nc <- 4;  ni <- 500000;  nb <- 250000;  nt <- 25    # Run the model for real

# Quick check of how many samples we'll keep in the posterior
message(paste((ni - nb) / nt), " samples will be kept from the posterior")

# Run the sampler
start <- Sys.time() %>%  print()          # Start time for the sampler
fire_mcmc_out<- runMCMC(cMCMC,
                  niter = ni, 
                  nburnin = nb, 
                  thin = nt, 
                  nchains = nc,
                  samplesAsCodaMCMC = T,
                  summary = T)
difftime(Sys.time(), start)               # End time for the sampler

# Save model output to local drive
saveRDS(fire_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", model_species, "_fire_elevation_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

# Load the output back in
fire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", model_species, "_fire_elevation_model.rds"))
  
 # Traceplots and density graphs 
MCMCtrace(object = fire_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_fire_elevation_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = fire_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = fire_mcmc_out$samples,
         guide_lines = TRUE,
         params = sobs_params)

} # End modeling loop (Comment this out) ----

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# 4.1) Prepare and view model output ################################################

# Loop over all species 
for(s in 1:length(all_species)) { # (Comment this out) ----

# Name the species to model again
plot_species <- all_species[s]
# plot_species <- all_species[2]

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
fire_mcmc_out<- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//",
                                       plot_species, "_fire_elevation_model.rds"))

# View MCMC summary
fire_mcmc_out$summary$all.chains

# 4.2) Extract coefficient values ###############################################################

# Extract effect sizes

# Treatment intercepts
beta0_ref_low <- fire_mcmc_out$summary$all.chains[18,]
beta0_ref_high <- fire_mcmc_out$summary$all.chains[19,]
beta0_burn_low <- fire_mcmc_out$summary$all.chains[20,]
beta0_burn_high <- fire_mcmc_out$summary$all.chains[21,]
# Fire Year
beta_fyear_low <- fire_mcmc_out$summary$all.chains[23,]
beta_fyear_high <- fire_mcmc_out$summary$all.chains[24,]
# Burn Sevarity
beta_burnsev <- fire_mcmc_out$summary$all.chains[22,]

# View Betas
bind_rows(beta0_ref_low,
          beta0_ref_high,
          beta0_burn_low,
          beta0_burn_high,
          beta_fyear_low,
          beta_fyear_high,
          beta_burnsev)

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0_ref_low,
                                 beta0_ref_high,
                                 beta0_burn_low,
                                 beta0_burn_high,
                                 beta_fyear_low,
                                 beta_fyear_high,
                                 beta_burnsev
                                 )) %>% 
  mutate(Parameter = c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high",
                       "beta.fyear.low", "beta.fyear.high", "beta.burnsev"
                       )) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CRI.lb = X95.CI_low,
         CRI.ub = X95.CI_upp)

# View output dataframe
head(beta_dat, n = Inf)

# 4.3) Predicted values ##########################################################################

# Pick a number of samples
n <- 1000

# Rearrange the columns that I need
beta_dat_long <- beta_dat %>%
  pivot_longer(cols = c(Mean, CRI.lb, CRI.ub),
               names_to = "Statistic",
               values_to = "Value") %>% 
  mutate(ColumnName = paste(Parameter, Statistic, sep = ".")) %>%
  select(ColumnName, Value)

# Then pivot wider to a shape that works for plotting
beta_dat_pred <- beta_dat_long %>%
  pivot_wider(names_from = ColumnName,
              values_from = Value) %>%
  # Repeat each sample n times
  slice(rep(1:n(), each = n)) %>% 
  # Add in the predictive values (2 sd's above or bellow for 95% of the data) 
  mutate(Pred = seq(from = -2, to = 2, length.out = n))

# View the new data 
glimpse(beta_dat_pred)

# 4.3) Plot each predicted value ###########################################

# Parameter estimates -----------------------------------------------------------------------------------------------

# Create the plot
params_plot <- beta_dat %>% 
  # Rename Parameters
  mutate(Parameter = case_when(Parameter == "beta0.ref.low" ~ "Reference Below 1800m",
                               Parameter == "beta0.ref.high" ~ "Reference Above 1800m",
                               Parameter == "beta0.burn.low" ~ "Burned Below 1800m",
                               Parameter == "beta0.burn.high" ~ "Burned Above 1800m",
                               Parameter == "beta.fyear.low" ~ "Years Since Fire  Below 1800m",
                               Parameter == "beta.fyear.high" ~ "Years Since Fire Above 1800m",
                               Parameter == "beta.burnsev" ~ "RdNBR Burn Sevarity"),
         # Add a New column for whether or not the CRI crosses Zero
         Significant = factor(case_when(CRI.lb * CRI.ub <= 0 ~ "No",
                                        CRI.lb * CRI.ub > 0 ~ "Yes"), 
                              levels = c("No", "Yes"))) %>% 
  # Switch to a factor 
  mutate(Parameter = factor(Parameter, levels = c("Reference Below 1800m", "Reference Above 1800m",
                                                  "Burned Below 1800m","Burned Above 1800m",
                                                  "Years Since Fire  Below 1800m", "Years Since Fire Above 1800m",
                                                  "RdNBR Burn Sevarity"))) %>% 
  # Open the plot
  ggplot(aes(y = Parameter)) +
  # Add points at the mean values for each parameters
  geom_point(aes(x = Mean, color = Significant), shape = 15, size = 4) +
  # Add whiskers for 95% Bayesian Credible intervals
  geom_linerange(aes(xmin = CRI.lb, xmax = CRI.ub, color = Significant), linewidth = 1.5) +
  # Add a vertical Line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  # Zoom Out
  # coord_cartesian(xlim = c(-1.5, 4.5)) +
  # Change the Labels
  labs(x = "Parameter Estimate", y = "") +
  # Simple theme
  theme_classic() +
  # Custom colors
  
  scale_color_manual(values = c("No" = "lightsteelblue4", 
                                "Yes" = "navyblue")) +
  # Edit theme
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 16)) 


# View the plot
params_plot

# Save the plot
ggsave(plot = params_plot,
       paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
                                       plot_species, "_params.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)


# Burn verses Reference at different Elevations  --------------------------------------------------------------------

# Create the plot
treatment_pred_plot <- beta_dat %>%
  # Only relevant levels
  filter(Parameter %in% c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high")) %>% 
  # Reorder 'Parameter' factor levels to match the desired color order
   mutate(Parameter = factor(Parameter, 
                            levels = c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Parameter, y = exp(Mean), color = Parameter), 
               width = 0.45, size = 0.8) +
  geom_errorbar(aes(x = Parameter, ymin = exp(CRI.lb), ymax = exp(CRI.ub), color = Parameter), 
                width = 0.2, linewidth = 1.4) +
  # Customize scales and labels
  scale_color_manual(values = c("beta0.ref.low" = "mediumseagreen",
                                "beta0.ref.high" = "darkslategray4",
                                "beta0.burn.low" = "red3",
                                "beta0.burn.high" = "orange2"),
                     labels = c(
                                "Low Elevation Reference",
                                "High Elevation Reference",
                                "Low Elevation Burn",
                                "High Elevation Burn"
                                ),
                     name = "") +
  labs(x = "Grid Type", 
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA)) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

# Display the plot
treatment_pred_plot

# Save the plot
ggsave(plot = treatment_pred_plot,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
                         plot_species, "_treatment.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

# Years since fire plot -------------------------------------------------------

# Find the minimum time since fire
min_fyear <- sobs_counts %>% 
  filter(Years.Since.Fire < 30) %>%
  select(Years.Since.Fire) %>% 
  arrange(Years.Since.Fire) %>% 
  slice_head(n = 1)

# Find the maximum time since fire
max_fyear <- sobs_counts %>% 
  filter(Years.Since.Fire < 30) %>%
  select(Years.Since.Fire) %>% 
  arrange(-Years.Since.Fire) %>% 
  slice_head(n = 1)

# Plot predicted responce to time since fire
fyear_pred_plot <- beta_dat_pred %>% 
  mutate(Pred.Naive = Pred * sd_fyear + mean_fyear) %>% 
  # Only show up to where I have data so I am not making forcasts
  filter(Pred.Naive >= min_fyear$Years.Since.Fire[1] & 
         Pred.Naive <= max_fyear$Years.Since.Fire[1]) %>%
  ggplot() +
  # Low elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.low.Mean), 
                color = "Low Elevation Reference"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.low.CRI.lb), 
                  ymax = exp(beta0.ref.low.CRI.ub), 
                  fill = "Low Elevation Reference"), alpha = 0.2) +
  # High elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.high.Mean), 
                color = "High Elevation Reference"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.high.CRI.lb), 
                  ymax = exp(beta0.ref.high.CRI.ub), 
                  fill = "High Elevation Reference"), alpha = 0.2) +
  # Low elevation time since fire
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.low.Mean + beta.fyear.low.Mean * Pred), 
                color = "Low Elevation Burn"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.low.CRI.lb + beta.fyear.low.CRI.lb * Pred), 
                  ymax = exp(beta0.burn.low.CRI.ub + beta.fyear.low.CRI.ub * Pred), 
                  fill = "Low Elevation Burn"), alpha = 0.2) +
  # High elevation time since fire
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.high.Mean + beta.fyear.high.Mean * Pred), 
                color = "High Elevation Burn"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.high.CRI.lb + beta.fyear.high.CRI.lb * Pred), 
                  ymax = exp(beta0.burn.high.CRI.ub + beta.fyear.high.CRI.ub * Pred), 
                  fill = "High Elevation Burn"), alpha = 0.2) +
  # Customize scales and labels
  scale_color_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                                "High Elevation Reference" = "darkslategray4",
                                "Low Elevation Burn" = "red3",
                                "High Elevation Burn" = "orange2"),
                     name = "") +
  scale_fill_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                               "High Elevation Reference" = "darkslategray4",
                               "Low Elevation Burn" = "red3",
                               "High Elevation Burn" = "orange2"),
                    name = "") +
  labs(x = "Years Since Fire", 
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA)) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

# Display the plot
fyear_pred_plot

# Save the plot
ggsave(plot = fyear_pred_plot,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
                         plot_species, "_fyear.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

# Burn Severity plot -------------------------------------------------------

# View predictive data
glimpse(beta_dat_pred)

# Find the minimum burn severity
min_rdnbr <- covs %>%
  filter(Grid.Type == "B") %>% 
  select(Grid.ID, rdnbr.125m) %>% 
  arrange(rdnbr.125m) %>% 
  slice_head(n = 1)

# Plot predicted responce to Burn Sevarity
rdnbr_pred_plot <- beta_dat_pred %>% 
  mutate(Pred.Naive = Pred * sd_rdnbr + mean_rdnbr) %>% 
  filter(Pred.Naive >= min_rdnbr$rdnbr.125m[1]) %>%
  ggplot() +
  # Low elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.low.Mean), 
                color = "Low Elevation Reference"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.low.CRI.lb), 
                  ymax = exp(beta0.ref.low.CRI.ub), 
                  fill = "Low Elevation Reference"), alpha = 0.2) +
  # High elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.high.Mean), 
                color = "High Elevation Reference"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.high.CRI.lb), 
                  ymax = exp(beta0.ref.high.CRI.ub), 
                  fill = "High Elevation Reference"), alpha = 0.2) +
  # Low elevation burn severity
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.low.Mean + beta.burnsev.Mean * Pred), 
                color = "Low Elevation Burn"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.low.CRI.lb + beta.burnsev.CRI.lb * Pred), 
                  ymax = exp(beta0.burn.low.CRI.ub + beta.burnsev.CRI.ub * Pred 
                  ), 
                  fill = "Low Elevation Burn"), alpha = 0.2) +
  # High elevation burn severity
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.high.Mean + beta.burnsev.Mean * Pred), 
                color = "High Elevation Burn"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.high.CRI.lb + beta.burnsev.CRI.lb * Pred), 
                  ymax = exp(beta0.burn.high.CRI.ub + beta.burnsev.CRI.ub * Pred), 
                  fill = "High Elevation Burn"), alpha = 0.2) +
  # Customize scales and labels
  scale_color_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                                "High Elevation Reference" = "darkslategray4",
                                "Low Elevation Burn" = "red3",
                                "High Elevation Burn" = "orange2"), name = "") +
  scale_fill_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                               "High Elevation Reference" = "darkslategray4",
                               "Low Elevation Burn" = "red3",
                               "High Elevation Burn" = "orange2"), name = "") +
  labs(x = "RdNBR Burn Severity", 
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA)) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

# Display the plot
rdnbr_pred_plot

# Save the plot
ggsave(plot = rdnbr_pred_plot,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
                         plot_species, "_burnsev.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

} # End plotting loop over all species (Comment this out) ----

