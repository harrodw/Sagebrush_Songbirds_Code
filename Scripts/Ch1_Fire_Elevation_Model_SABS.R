#----------------------------------------------------------------
# Will Harrod
# Hierarchical distance sampling for sagebrush songbird point count data
# Simplified script for sagerbush sparrow to acount for few detections
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

# Pick a species to model
model_species <- "SABS"

# Add in count data from local drive
# Two grids (ID-C11 and ID-C22) were missing their Y1V1 survey
# Counts/weather were estimated based on the mode for other visits to those grids
sabs_counts_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Grid_Counts.csv")) %>%
  tibble() %>%
  select(-X)
#view the counts
glimpse(sabs_counts_temp)

# Add in the observation data from the local drive
sabs_observations_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Observations.csv")) %>% 
  select(-X)
# View the observations
glimpse(sabs_observations_temp)

# Add covariates from the local drive
# Aspect == -1 means that the area is flat other classes start at 1=NE clockwise
# Fire Distance == 1000000 mean that the area is outside of a fire
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
sabs_counts_temp2 <- sabs_counts_temp %>%
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
glimpse(sabs_counts_temp2)

# Scale the other covariates
sabs_counts <- sabs_counts_temp2 %>% 
  mutate(ln.Years.Since.Fire = log(Years.Since.Fire)) %>% 
  mutate(Elevation.scl = scale(Elevation.125m)[,1],
         Mean.MAS.scl = scale(Mean.MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1])

# 1.3) Prepare the observation level data ################################################################

# Make sure the same numeric values for factors are shaped between the two datasets
sabs_counts <- sabs_counts %>% 
  arrange(Year, Visit.ID, Grid.ID) %>% 
  mutate(Grid.ID.num = as.numeric(Grid.ID),
         Year.num = as.numeric(Year),
         Visit.ID.num = as.numeric(Visit.ID),
         Observer.ID.num = as.numeric(Observer.ID)) 

# View who each observer is
sabs_counts %>% 
  distinct(Observer.ID, Observer.ID.num, Observer.Experience) %>% 
  arrange(Observer.ID.num) %>% 
  print(n = Inf)

# Pull out the covariates that need to be shared across counts and observations
point_ids <- sabs_counts %>% 
  mutate_at(c("Grid.ID", "Year", "Visit.ID"), as.character) %>% 
  dplyr::select(Grid.ID, Grid.ID.num, Year, Year.num, Visit.ID, Visit.ID.num, Observer.ID.num)
#...and view
glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
sabs_observations <- sabs_observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Year", "Visit.ID"))

# View Counts
glimpse(sabs_counts)

# View observations
glimpse(sabs_observations)

# 1.4) Plot preliminary correlations in the data ##############################################################

# Plot a specific treatment and bird abundance
sabs_counts %>%
  ggplot() +
  geom_boxplot(aes(x = factor(Treatment), y = Count, color = factor(Treatment))) +
  theme_bw()

# Plot burn sevarity and bird abundance
sabs_counts %>%
  filter(Burned == 1) %>%
  ggplot() +
  geom_smooth(aes(x = rdnbr.125m, y = Count,
                  # color = factor(Treatment)
                  ), method = "glm", method.args = list(family = "quasipoisson")) +
  geom_jitter(aes(x = rdnbr.125m, y = Count,
                  # color = factor(Treatment)
              )) +
  theme_bw()

# # What are these low elevation recovered burn grids with high counts?
# sabs_counts %>% 
#   # filter(Burned == 1) %>% 
#   select(Grid.ID, Treatment, Years.Since.Fire, Count, Elevation.125m) %>% 
#   arrange(Years.Since.Fire) %>% 
#   print(n = Inf)
# 
# Plot detection frequency by distance for each observer to insurre that the data is appropriate for distance sampling
sabs_observations %>%
  group_by(Grid.ID, Observer.ID, Observer.Experience, Dist.Bin, Dist.Bin.Midpoint) %>%
  reframe(Grid.ID, Observer.ID, Observer.Experience, Dist.Bin, , Dist.Bin.Midpoint, bin.count = n()) %>%
  distinct() %>%
  ggplot(aes(x = Dist.Bin, y = bin.count
             )) +
  geom_col(fill = "lightblue") +
  theme_bw() +
  facet_wrap(~Observer.ID)

# How many observations did each observer have?
sabs_observations %>%
  count(Observer.ID)

# 1.4) prepare objects for NIMBLE ################################################################

# Define a truncation distance (km)
trunc_dist <- 0.125

# Matrix dimentions
nrows <- length(unique(sabs_counts$Grid.ID.num))  # Number of survey grids
ncols <- length(unique(sabs_counts$Visit.ID.num)) # Times each grid was visited

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

# Fill in the matrix
for(y in 1:nrow(count_mat)){
  # Filter for a specific grid
  count_visit <- sabs_counts %>% 
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
}

# Size Objects  
ngrids <- length(unique(sabs_counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(sabs_observations)                            # Number of individuals detected 
# nobsv <- length(unique(sabs_counts$Observer.ID.num))       # Number of unique observers
nexp <- length(unique(sabs_counts$Observer.Experience))    # Number of experience levels
nbins <- length(unique(sabs_observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(sabs_observations$Time.Interval))   # Number of time intervals
nvst <- length(unique(sabs_counts$Visit.ID))               # Number of visits in each year
nyears <- length(unique(sabs_counts$Year.num))             # Number of years we surveyed
nelv <- length(unique(sabs_counts$High.Elevation))         # Number of elevations (High or Low)

# Observation Level data 
midpt <- sort(unique(sabs_observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
# observers <- obsv_mat                                      # Random effect for observer associated with each survey
# obsv_exp <- exp_mat                                        # Whether or not it was each observer's first point count season
obs_visit <- sabs_observations$Visit.ID.num                # During which visit did each observation take place
obs_grid <- sabs_observations$Grid.ID.num                  # In which grid did each observation take place
dclass <- sabs_observations$Dist.Bin                       # Distance class of each observation
delta <- trunc_dist / nbins                                # Size of distance bins

# Availability date
tint <- sabs_observations$Time.Interval               # Time interval for each observation 
time <- time_mat                                      # Matrix of scaled time after sunrise
day <- date_mat                                       # Matrix of scaled dates

# Grid level data
area <- area_mat                                      # Proportion of points surveyed       
n_dct <- count_mat                                    # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                     # Matrix of year numbers
grids <- sabs_counts$Grid.ID.num[1:ngrids]            # Grid where each survey took place
elevation <- sabs_counts$High.Elevation[1:ngrids]     # Whether each grid is high (>= 1900m) or low (< 1900m) elevation
# burned <- sabs_counts$Burned[1:ngrids]                # Whether or not each grid burned
high_ref <- sabs_counts$High.Reference[1:ngrids]      # Whether or not each grid was high elevation and reference
low_burn <- sabs_counts$Low.Burned[1:ngrids]          # Whether or not each grid was low elevation and burned
high_burn <- sabs_counts$High.Burned[1:ngrids]        # Whether or not each grid was high elevation and burned

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Model definition ################################################################
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sabs_model_code <- nimbleCode({
  # Priors for all parameters 
  # ------------------------------------------------------------------
  
  # Parameters in the availability component of the detection model
  gamma0 ~ dnorm(0, sd = 3)              # Availability intercept
  gamma_date ~ dnorm(0, sd = 1.5)        # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 1.5)       # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 1.5)        # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 1.5)       # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model
  # for(o in 1:nobsv){
  #   sigma_obsv[o] ~ dunif(0, 1)          # Shape parameter on the natural scale (0 - 1km)
  #   alpha0_obsv[o] <- log(sigma_obsv[o]) # Effect of observer experience on detecability on the log scale 
  # } # End loop through observers
  alpha0 ~ T(dnorm(0, 3), -9, 0) # Intercept on detecability
  
  # Parameters on the abundance component of the model
  # Random effect hyper-parameters
  mean_lambda ~ T(dnorm(0, sd = 100), 0, ) # Mean abundance hyperparameter (natural scale)
  mean_beta0 <- log(mean_lambda)           # Mean abundance hyperparameter (log scale) 
  sd_beta0 ~ T(dt(0, 1, 2), 0, 2)          # Sd in yearly abundance hyperparameter 
    
  # Random intercept on abundance for each year
  for(t in 1:nyears){ # Loop through year
    beta0_year[t] ~ dnorm(mean_beta0, sd = sd_beta0)
    } # End loop through years
  
  # Fixed effects
  beta_high_ref ~ dnorm(0, sd = 1.5)  # Effect of being in a high elevation reference
  beta_low_burn ~ dnorm(0, sd = 1.5)  # Effect of being in a low elevation burn
  beta_high_burn ~ dnorm(0, sd = 1.5) # Effect of being in a high elevation burn  

  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey grids
  for(s in 1:ngrids){
    
    # Zero-inflation component on abundance
    psi[s] ~ T(dbeta(shape1 = 1.3, shape2 = 1.3), 0.001, ) # Occupancy probability can't be exactly zero
    present[s] ~ dbern(psi[s])                             # Number of grids where that individual can be present
    
    # Iterate over all of the visits to each survey grid 
    for(y in 1:nvst){ 
      
      ### Imperfect availability portion of the model ###
      
      # Construction of the cell probabilities for the nints time intervals
      for (j in 1:nints){
        pi_pa[s, y, j] <- avail[s, y] * (1 - avail[s, y])^(j - 1)  # Availability cell probability
        pi_pa_c[s, y, j] <- pi_pa[s, y, j] / p_a[s, y]             # Proportion of availability probability in each time interval class
      }
      
      # Rectangular integral approx. of integral that yields the Pr(available)
      p_a[s, y] <- sum(pi_pa[s, y, ])
      
      ### Imperfect detection portion of the model ###
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, y, b]) <- -(midpt[b]^2) / (2 * sigma[s, y]^2) # Half-normal detection function
        f[s, y, b] <- (2 * midpt[b]) / trunc_dist^2  * delta   # Prob density function out to max truncation distance 
        pi_pd[s, y, b] <- g[s, y, b] * f[s, y, b]              # Detection cell probability
        pi_pd_c[s, y, b] <- f[s, y, b] / p_d[s, y]             # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_d[s, y] <- sum(pi_pd[s, y, ])
      
      # Multiply availability with detection probability to yield total probability of capturing a bird
      p_cap[s, y] <- p_d[s, y] * p_a[s, y]
      
      ### Binomial portion of mixture ###
      
      # Poisson abundance portion of mixture
      N_indv[s, y] ~ dpois(lambda[s, y] * (present[s] + 0.0001) * area[s, y])   # ZIP true abundance at site s in year y
      
      # Number of individual birds captured (observations)
      n_dct[s, y] ~ dbin(p_cap[s, y], N_indv[s, y])         
        
      # Availability (avail) Logit-linear model for availability
      logit(avail[s, y]) <- gamma0 +                        # Intercept on availability
                            gamma_date * day[s, y] +        # Effect of scaled ordinal date
                            gamma_time * time[s, y] +       # Effect of scaled time of day
                            gamma_date2 * day[s, y]^2 +     # Effect of scaled ordinal date squared
                            gamma_time2 * time[s, y]^2      # Effect of scaled time of day squared
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s, y]) <- alpha0                            # Single intercept on detectability

      # Abundance (lambda) Log-linear model 
      log(lambda[s, y]) <- beta0_year[years[s, y]] +        # Random intercept by year
                           beta_high_ref * high_ref[s] +    # Effect of being in a high elevation reference
                           beta_low_burn * low_burn[s] +    # Effect of being in a low elevation burn
                           beta_high_burn * high_burn[s]    # Effect of being in a high elevation burn  
      
      # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
      e_val[s, y] <- p_cap[s, y] * N_indv[s, y]             # Expected value for binomial portion of the model
      FT[s, y] <- (sqrt(n_dct[s, y]) - sqrt(e_val[s, y]))^2 # Compute fit statistic for observed data
      
      # Generate replicate count data and compute same fit stats for them
      n_dct_new[s, y] ~ dbin(p_cap[s, y], N_indv[s, y])              
      FT_new[s, y] <- (sqrt(n_dct_new[s, y]) - sqrt(e_val[s, y]))^2 
                               
    } # end loop through visits
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ])    # likelihood for distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])      # likelihood for time removal data
  } # end observation distance and time of detection loop
  
  # Add up fit stats across sites and years
  fit <- sum(FT[,]) 
  fit_new <- sum(FT_new[,]) 
  
  # c-hat value, should converge to ~1
  c_hat <- fit / fit_new
  
}) # end model statement

# 2.2) Constants, data, Initial values, parameters to save, and dimensions ##################################

# Constants to be fed into Nimble
sabs_const <- list (
  # Misc. Constants
  area = area,                 # Number of points surveyed per grid per visit
  delta = delta,               # Bin width
  trunc_dist = trunc_dist,     # Truncation distance
  
  # For loop sizes
  ngrids = ngrids,             # Number of survey grids
  nind = nind,                 # Number of individuals detected 
  # nobsv = nobsv,               # Number of unique observers
  nvst = nvst,                 # Number of times each grid was surveyed (6)
  nbins = nbins,               # Number of distance bins
  nints = nints,               # Number of time intervals
  nelv = nelv,                 # Number of elevations (2)
  nyears = nyears,             # Number of years we surveyed (3)
  
  # Non-stochastic constants
  years = years,               # Year when each survey took place
  obs_visit  = obs_visit,      # Visit when each observation took place
  obs_grid  = obs_grid,        # Grid of each observation 
  # observers = observers,       # Effect of observer associated with each survey
  elevation = elevation        # High or low elevation
)

# View Nimble constants 
str(sabs_const)

# Data to be fed into Nimble 
sabs_dat <- list(
  # Detection level data
  dclass = dclass,         # Distance category for each observation
  midpt = midpt,           # Midpoints of distance bins
  # Availability level data
  tint = tint,             # Time interval for each observation
  time = time,             # Scaled mean time after sunrise
  day = day,               # Scaled date
  # Abundance level
  n_dct = n_dct,           # Number of detected individuals per site
  high_ref = high_ref,     # Whether or not each grid was a high elevation reference
  low_burn = low_burn,     # hether or not each grid was a high elevation burn
  high_burn = high_burn    # Whether or not each grid was a high elevation burn
  # burned = burned          # Whether or not each grid burned
)
# View Nimble data 
str(sabs_dat)

# Object dimensions
sabs_dims <- list(
  g = c(ngrids, nvst, nbins),        # Cell prob x radial distance
  f = c(ngrids, nvst, nbins),        # Cell prob x radial distance
  sigma = c(ngrids, nvst),           # Linear combination on detectability
  pi_pd = c(ngrids, nvst, nbins),    # Detection probability in each cell
  pi_pd_c = c(ngrids, nvst, nbins),  # Proportion of total detection probability in each cell
  p_d = c(ngrids, nvst),             # Detection probability 
  avail = c(ngrids, nvst),           # Linear combination on avalibility  
  pi_pa =  c(ngrids, nvst, nints),   # Availability cell prob in each time interval
  pi_pa_c = c(ngrids, nvst, nints),  # Proportion of total availability probability in each cell
  p_a= c(ngrids, nvst),              # Availability probability
  p_cap = c(ngrids, nvst),           # Combined probability of detecting an available bird
  lambda = c(ngrids, nvst),          # Poisson random variable
  FT = c(ngrids , nvst),             # Observed data Freeman Tukey Discrepancy
  FT_new = c(ngrids, nvst)           # Simulated data Freeman Tukey Discrepancy
)

# View dimensions
str(sabs_dims)

# Initial Values
sabs_inits <- list(
  # Detectablility
  alpha0 = runif(1, 0.4, 0.6),
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),
  gamma_date = rnorm(1, 0, 0.1),
  gamma_time = rnorm(1, 0, 0.1),
  gamma_date2 = rnorm(1, 0, 0.1),
  gamma_time2 = rnorm(1, 0, 0.1),
  # Abundance 
  mean_lambda = runif(1, 0, 1),
  sd_beta0 = runif(1, 0, 0.1),
  beta0_year = runif(nyears, 0.1, 1),
  beta_high_ref = rnorm(1, 0, 0.1),
  beta_low_burn = rnorm(1, 0, 0.1),
  beta_high_burn = rnorm(1, 0, 0.1),
  # Presence 
  psi = runif(ngrids, 0, 1),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated counts
  n_dct_new = count_mat,
  N_indv = count_mat + 1 # Counts helps to start each grid with an individual present       
)  

# View the initial values
str(sabs_inits)

# Params to save
sabs_params <- c(
                 "fit",            # Fit statistic for observed data
                 "fit_new",        # Fit statisitc for simulated data
                 "c_hat",          # Measure of model fit, should converge to ~1
                 "mean_beta0",
                 "sd_beta0",
                 "beta0_year",
                 "beta_high_ref",
                 "beta_low_burn",
                 "beta_high_burn",
                 "gamma0",
                 "gamma_date",
                 "gamma_date2",
                 "gamma_time",
                 "gamma_time2",
                 "alpha0"
                 )

# 2.3) Configure and Run the model ###########################################################

# Build the nimble model
sabs_model_vect <- nimbleModel(sabs_model_code, 
                               data = sabs_dat,
                               constants = sabs_const,
                               dimensions = sabs_dims,
                               inits = sabs_inits)

# MCMC settings for default nimble model setup
# nc_test <- 1  ;  ni_test <- 1000  ;  nb_test <- 500  ;  nt_test <- 10          

# From Kezia and Liz:
# Next, run an MCMC with NIMBLE defaults; this can be a slow step.
# the just-do-it way to run an MCMC. this will take all steps to set up
# and run an MCMC using NIMBLE’s default configuration. Handles everything from 
# model building to compilation to running the MCMC
# sabs_model_out_test <- nimbleMCMC(code = sabs_model_code,
#                                          constants = sabs_const,
#                                          data = sabs_dat,
#                                          dimensions = sabs_dims,
#                                          inits = sabs_inits,
#                                          monitors = sabs_params,
#                                          nchains = nc_test,
#                                          niter = ni_test, 
#                                          nburnin = nb_test, 
#                                          thin = nt_test, 
#                                          samplesAsCodaMCMC = T,
#                                          summary = T)

# Assign default samplers to nodes
sabs_mcmcConf <- configureMCMC(sabs_model_vect, 
                               monitors = sabs_params)

# Block all availability (gamma) nodes together
sabs_mcmcConf$removeSamplers("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2")

sabs_mcmcConf$addSampler(target = c("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2"),
                         type = 'RW_block')


# Block all abundance (beta) nodes together
sabs_mcmcConf$removeSamplers(
  "beta0_year[1]", "beta0_year[2]", "beta0_year[3]",
  "beta_high_ref", "beta_low_burn", "beta_high_burn"
  )

sabs_mcmcConf$addSampler(target = c(
  "beta0_year[1]", "beta0_year[2]", "beta0_year[3]",
  "beta_high_ref", "beta_low_burn", "beta_high_burn"
  ), type = 'RW_block')



# View the blocks
sabs_mcmcConf$printSamplers() # print samplers being used 
sabs_mcmcConf$unsampledNodes  # look at unsampled nodes
sabs_mcmcConf$getUnsampledNodes() # Names of unsampled nodes

# Build MCMC object: can customize thinning, monitors, etc.
sabs_modelMCMC <- buildMCMC(sabs_mcmcConf) # Update mcmc configuration based on graph

# Compile Nimble object and MCMC object to one C++ object (gets an ~3 fold speed up, even 
# if you do nothing else) spend time on getting this step right because this is the 
# easiest way to accelerate convergence. 
cModel <- compileNimble(sabs_model_vect) # compiled model
cMCMC <- compileNimble(sabs_modelMCMC, project = cModel, resetFunctions = T) # compiled mcmc

# MCMC settings for the real model. Pick one, comment out the rest 
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1        # Quick test to see if the model runs
# nc <- 3  ;  ni <- 150000  ;  nb <- 50000;  nt <- 10   # longer test where most parameters should
nc <- 4;  ni <- 500000;  nb <- 250000;  nt <- 25        # Run the model for real

# Quick check of how many samples we'll keep in the posterior
message(paste((ni - nb) / nt), " samples will be kept from the posterior")

# Run the sampler
start <- Sys.time()                                     # Start time for the sampler
start
sabs_mcmc_out<- runMCMC(cMCMC,
                  niter = ni, 
                  nburnin = nb, 
                  thin = nt, 
                  nchains = nc,
                  samplesAsCodaMCMC = T,
                  summary = T)
difftime(Sys.time(), start)                             # End time for the sampler

# Save model output to local drive
saveRDS(sabs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", model_species, "_fire_elevation_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
sabs_mcmc_out<- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", model_species, "_fire_elevation_model.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = sabs_mcmc_out$samples,
          params = sabs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_fire_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = sabs_mcmc_out$samples, 
            params = sabs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = sabs_mcmc_out$samples,
         guide_lines = TRUE,
         params = sabs_params)

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# 4.1) Prepare and view model output ################################################

# Species code and name
plot_species <- "SABS"
species_name <- "Sagebrush Sparrow"

# Load the output back in
sabs_mcmc_out<- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//",
                                       plot_species, "_fire_elevation_model.rds"))

# View MCMC summary
sabs_mcmc_out$summary$all.chains

# 4.2) Extract coefficient values ###############################################################

# Extract effect sizes

# Mean across years 
beta0_year1 <- sabs_mcmc_out$summary$all.chains[2,] 
beta0_year2 <- sabs_mcmc_out$summary$all.chains[3,]
beta0_year3 <- sabs_mcmc_out$summary$all.chains[4,]
year_mean_intercept <- colMeans(bind_rows(beta0_year1, beta0_year2, beta0_year3))

# Treatment intercepts
beta0_ref_low <- year_mean_intercept # Low reference gets the overall mean
# sabs_mcmc_out$summary$all.chains[33,]   
beta0_ref_high <- year_mean_intercept + sabs_mcmc_out$summary$all.chains[6,]
beta0_burn_low <- year_mean_intercept + sabs_mcmc_out$summary$all.chains[7,]
beta0_burn_high <- year_mean_intercept + sabs_mcmc_out$summary$all.chains[5,]

# View Betas
bind_rows(beta0_ref_low,
          beta0_ref_high,
          beta0_burn_low,
          beta0_burn_high)

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0_ref_low,
                                 beta0_ref_high,
                                 beta0_burn_low,
                                 beta0_burn_high
                                 )) %>% 
  mutate(Parameter = c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high"
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
                               Parameter == "beta.burnsev" ~ "RdNBR Burn Sevarity",
                               Parameter == "beta.south" ~ "Proportion South-Facing Slopes"),
         # Add a New column for whether or not the CRI crosses Zero
         Significant = factor(case_when(CRI.lb * CRI.ub <= 0 ~ 0,
                                        CRI.lb * CRI.ub > 0 ~ 1))) %>% 
  # Switch to a factor 
  mutate(Parameter = factor(Parameter, levels = c("Reference Below 1800m", "Reference Above 1800m",
                                                  "Burned Below 1800m","Burned Above 1800m",
                                                  "Years Since Fire  Below 1800m", "Years Since Fire Above 1800m",
                                                  "RdNBR Burn Sevarity", "Proportion South-Facing Slopes"))) %>% 
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
  scale_color_manual(values = c("lightsteelblue4", "navyblue")) +
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
