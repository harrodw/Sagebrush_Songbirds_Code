#----------------------------------------------------------------
# Will Harrod
# Hierarchical distance sampling for sagebrush sparrow point count data
# Simplified script for sagebrush sparrow to account for few detections
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
  mutate(High.Elevation = case_when(Elevation < 1800 ~ 1,
                                    Elevation >= 1800 ~ 2,
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
  mutate(Elevation.scl = scale(Elevation)[,1],
         Mean.Birds.scl = scale(Mean.Birds)[,1],
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

sabs_counts %>% 
  select(Grid.ID, Visit.ID, Count) %>% 
  arrange(-Count) %>% 
  print(n = Inf)

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
# Build a storage matrix for how many individual birds were seen on each visit
mean_birds_mat <- matrix(NA, nrow = nrows, ncol = ncols)

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
  mean_birds_mat[y,] <- count_visit$Mean.Birds.scl
}

# Size Objects  
ngrids <- length(unique(sabs_counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(sabs_observations)                            # Number of individuals detected 
nexp <- length(unique(sabs_counts$Observer.Experience))    # Number of experience levels
nbins <- length(unique(sabs_observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(sabs_observations$Time.Interval))   # Number of time intervals
nvst <- length(unique(sabs_counts$Visit.ID))               # Number of visits in each year
nyears <- length(unique(sabs_counts$Year.num))             # Number of years we surveyed
ntrts <- length(unique(sabs_counts$Treatment))             # Number of treatments (Elevation x Burn)

# Observation Level data 
midpt <- sort(unique(sabs_observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
# observers <- obsv_mat                                      # Random effect for observer associated with each survey
# obsv_exp <- exp_mat                                        # Whether or not it was each observer's first point count season
obs_visit <- sabs_observations$Visit.ID.num                # During which visit did each observation take place
obs_grid <- sabs_observations$Grid.ID.num                  # In which grid did each observation take place
dclass <- sabs_observations$Dist.Bin                       # Distance class of each observation
delta <- trunc_dist / nbins                                # Size of distance bins
mean_birds <- mean_birds_mat                                # Number of individual birds seen per survey

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
trts <- sabs_counts$Treatment[1:ngrids]               # Grid type (Elevation x burned)

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
  alpha0 ~ T(dnorm(mean = log(0.125), sd = 3), -9, 0) # Prior mean centered on approximately sigma = exp(-2) = 0.125km
  
  # Parameters on the abundance component of the model
  
  # Intercept by treatment
  for(l in 1:ntrts){ # ntrts = 4
    # Allow the intercept to vary for each treatment group
    beta0_treatment[l] ~ dnorm(0, sd = 3)
  } # End loop over treatments
  
  # Abundance random effect hyper-parameters
  sd_eps_year ~ dgamma(shape = 0.5, scale = 0.5)   # Random effect on abundance hyperparameter for each year
  
  # Random noise between years
  for(y in 2:nyears){ # nyears = 3
    # Force the first year (2022) to be the intercept
    eps_year[y] ~ dnorm(0, sd = sd_eps_year)
  } # end loop over years
  

  
  # # Occupancy probability by grid type (Burned x Elevation) for zero inflation
  # for(l in 1:ntrts){
  #   psi_trt[l] ~ dbeta(shape1 = 1.3, shape2 = 1.3)
  # } # end loop over treatments

  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey grids
  for(j in 1:ngrids){
    
    # Iterate over all of the visits to each survey grid 
    for(k in 1:nvst){ 
      
      # # Whether or not individuals are present at each visit to each site (Zero-Inflation)
      # present[j, k] ~ dbern(psi_trt[trts[j]])      
      
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
      N_indv[j, k] ~ dpois(lambda[j, k] *  area[j, k])   # ZIP true abundance at site s in year y
      # (present[j, k] + 0.0001) *  
      
      # Availability (avail) Logit-linear model for availability
      logit(phi[j, k]) <- gamma0 +                        # Intercept on availability
                          gamma_date * day[j, k] +        # Effect of scaled ordinal date
                          gamma_time * time[j, k] +       # Effect of scaled time of day
                          gamma_date2 * day[j, k]^2 +     # Effect of scaled ordinal date squared
                          gamma_time2 * time[j, k]^2      # Effect of scaled time of day squared
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[j, k]) <- alpha0                          # Single intercept on detectability

      # Abundance (lambda) Log-linear model 
      log(lambda[j, k]) <- beta0_treatment[trts[j]] +     # Intercept for each grid type
                           eps_year[years[j, k]]          # Unexplained noise on abundance by year
      
      # -------------------------------------------------------------------------------------------------------------------
      # Assess model fit: compute Bayesian p-value for using a test statisitcs
      
      # # Chi square statisitc for the availability portion of the model
      # e_pa[j, k] <- p_a[j, k] * N_indv[j, k]                                      # Expected value for availability binomial portion of the model
      # n_avail_new[j, k] ~ dbin(p_a[j, k], N_indv[j, k])                           # Draw new available birds from the same binomial
      # Chi_pa[j, k] <- (n_avail[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5)         # Compute availability chi squared statistic for observed data
      # Chi_pa_new[j, k] <- (n_avail_new[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5) # Compute availability chi squared statistic for simulated data data
      # 
      # # Chi square statisitc for the detection portion of the model
      # e_pd[j, k] <- p_d[j, k] * n_avail[j, k]                                     # Expected value for detection binomial portion of the model
      # n_dct_new[j, k] ~ dbin(p_d[j, k], n_avail[j, k])                            # Draw new detections from the same binomial
      # Chi_pd[j, k] <- (n_dct[j, k] - e_pd[j, k])^2 / (e_pd[j, k] + 0.5)           # Compute detecability chi squared statistic for observed data
      # Chi_pd_new[j, k] <- (n_dct_new[j, k] - e_pd[j, k])^2 / (e_pd[j, k] + 0.5)   # Compute detecability chi squared statistic for simulated data data
      
    } # end loop through visits
  } # end loop through survey grids
  
  # --------------------------------------------------------------------------------------------------------
  # Availability and detecability likelihood
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ])    # likelihood for distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])      # likelihood for time removal data
  } # end observation distance and time of detection loop
  
  # --------------------------------------------------------------------------------------------
  # Combine fit statistics
  
  # # Add up fit stats for availability across sites and years
  # fit_pa <- sum(Chi_pa[,])
  # fit_pa_new <- sum(Chi_pa_new[,])
  # 
  # # Add up fit stats for detectability across sites and years
  # fit_pd <- sum(Chi_pd[,])
  # fit_pd_new <- sum(Chi_pd_new[,])
  
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
  nvst = nvst,                 # Number of times each grid was surveyed (6)
  nbins = nbins,               # Number of distance bins
  nints = nints,               # Number of time intervals
  ntrts = ntrts,               # Number of treatments
  nyears = nyears,             # Number of years we surveyed (3)
  # Non-stochastic constants
  years = years,               # Year when each survey took place
  trts = trts,                 # Grid type
  obs_visit  = obs_visit,      # Visit when each observation took place
  obs_grid  = obs_grid         # Grid of each observation 
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
  # Simulated counts
  n_avail = count_mat,     # Number of available birds 
  n_dct_new = count_mat,   # Simulated detected birds 
  n_avail_new = count_mat, # Simulated available birds 
  N_indv = count_mat + 1   # "True" abundance (helps to start each grid with an individual present)
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
  phi = c(ngrids, nvst),             # Linear combination on availability  
  pi_pa =  c(ngrids, nvst, nints),   # Availability cell prob in each time interval
  pi_pa_c = c(ngrids, nvst, nints),  # Proportion of total availability probability in each cell
  p_a = c(ngrids, nvst),             # Availability probability
  lambda = c(ngrids, nvst)           # Poisson random variable
  # e_pa = c(ngrids , nvst),           # Expected value for availebility portion of the model
  # e_pd = c(ngrids , nvst),           # Expected value for detection portion of the model
  # Chi_pa = c(ngrids , nvst),         # Observed Chi square statistic for availability
  # Chi_pa_new = c(ngrids, nvst),      # Simulated Chi square statistic for availability
  # Chi_pd = c(ngrids , nvst),         # Observed Chi square statistic for detection 
  # Chi_pd_new = c(ngrids, nvst)       # Simulated Chi square statistic for detection
)

# View dimensions
str(sabs_dims)

# Initial Values
sabs_inits <- list(
  # Detectablility
  alpha0 = runif(1, -3, -1),
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),
  gamma_date = rnorm(1, 0, 0.1),
  gamma_time = rnorm(1, 0, 0.1),
  gamma_date2 = rnorm(1, 0, 0.1),
  gamma_time2 = rnorm(1, 0, 0.1),
  # Abundance 
  beta0_treatment = rnorm(ntrts, 0, 0.1),
  sd_eps_year = runif(1, 0, 1),               # Magnitude of random noise by year (only positive)
  eps_year = c(0, rnorm(nyears-1, 0, 0.1)),   # Random noise on abundance by year
  # Presence 
  # Simulated counts
  # n_dct_new = count_mat,
  N_indv = count_mat + 1 # Counts helps to start each grid with an individual present       
)  

# View the initial values
str(sabs_inits)

# Params to save
sabs_params <- c(
                 # "fit_pd",          # Fit statistic for observed data
                 # "fit_pd_new",      # Fit statisitc for simulated detection  data
                 # "fit_pa",          # Fit statistic for first availability data
                 # "fit_pa_new",      # Fit statisitc for simulated avaiability data
                 "beta0_treatment", # Mean abundance by grid type
                 "sd_eps_year",     # Random noise on abundance by year
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

# Assign default samplers to nodes
sabs_mcmcConf <- configureMCMC(sabs_model_vect, 
                               monitors = sabs_params)

# Block all availability (gamma) nodes together
sabs_mcmcConf$removeSamplers(
  # Intercept
  "gamma0", 
  # effect of date of year
  "gamma_date", "gamma_time", 
  # Effect of time of day
  "gamma_date2", "gamma_time2"
  )

sabs_mcmcConf$addSampler(target = c(  
  # Intercept
  "gamma0", 
  # effect of date of year
  "gamma_date", "gamma_time", 
  # Effect of time of day
  "gamma_date2", "gamma_time2"
  ), type = 'RW_block')

# Block all abundance (beta) nodes together
sabs_mcmcConf$removeSamplers(
  # Intercept by Treatment
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Random noise by year
  "eps_year[2]", "eps_year[3]"
  )

sabs_mcmcConf$addSampler(target = c(
  # Intercept by Treatment
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Random noise by year
  "eps_year[2]", "eps_year[3]"
  ), type = 'RW_block')

# # Block all occupancy (psi) nodes together
# sabs_mcmcConf$removeSamplers(
#   "psi_trt[1]", "psi_trt[2]", "psi_trt[3]", "psi_trt[4]"
# )
# sabs_mcmcConf$addSampler(target = c(
#   "psi_trt[1]", "psi_trt[2]", "psi_trt[3]", "psi_trt[4]"
# ), type = 'RW_block')

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
saveRDS(sabs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//SABS_fire_elevation_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
sabs_mcmc_out<- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//SABS_fire_elevation_model.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = sabs_mcmc_out$samples,
          params = sabs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//",
          filename = "SABS_fire_model_traceplot",
          type = 'both')

# View MCMC summary
MCMCsummary(object = sabs_mcmc_out$samples, 
            params = sabs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = sabs_mcmc_out$samples,
         excl = c("fit_pa", "fit_pa_new", "fit_pd", "fit_pd_new"),
         guide_lines = TRUE,
         params = sabs_params)

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# 4.1) Prepare and view model output ################################################

# Add packages
library(tidyverse)
library(gridExtra)
library(extrafont)

#Load fonts
font_import()
loadfonts(device = "win")

# Species code and name
plot_species <- "SABS"
species_name <- "Sagebrush Sparrow"

# Load the output back in
sabs_mcmc_out<- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//SABS_fire_elevation_model.rds"))

# View MCMC summary
sabs_mcmc_out$summary$all.chains

# 4.2) Extract coefficient values ###############################################################

# Treatment abundance intercepts
beta0_ref_low <- sabs_mcmc_out$summary$all.chains[2,] 
beta0_ref_high <- sabs_mcmc_out$summary$all.chains[3,]
beta0_burn_low <- sabs_mcmc_out$summary$all.chains[4,]
beta0_burn_high <- sabs_mcmc_out$summary$all.chains[5,]

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

# Burn verses Reference at different Elevations  --------------------------------------------------------------------

# Create the plot
treatment_pred_plot <- beta_dat %>%
  # Only relevant levels
  filter(Parameter %in% c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high")) %>% 
  # Reorder 'Parameter' factor levels to match the desired color order
  mutate(Parameter = factor(Parameter, 
                            levels = c("beta0.ref.low", "beta0.burn.low", "beta0.ref.high", "beta0.burn.high"))) %>% 
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
                                "Low Elevation Burn",
                                "High Elevation Reference",
                                "High Elevation Burn"
                                ),
                     name = "") +
  labs(x = "Grid Type", 
       y = paste0(species_name, "s per km", "\u00B2"),
       title = "Sagebrush Sparrow") +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA)) +
  theme(
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", family = "Times New Roman"),
    legend.position = "none",
    legend.title = element_text(size = 18, family = "Times New Roman"),
    legend.key.size = unit(1, "cm")) 

# Create a plot to use the legend 
legend_plot <- beta_dat %>%
  # Only relevant levels
  filter(Parameter %in% c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high")) %>% 
  # Reorder 'Parameter' factor levels to match the desired color order
  mutate(Parameter = factor(Parameter, 
                            levels = c("beta0.ref.low", "beta0.burn.low", "beta0.ref.high", "beta0.burn.high"))) %>% 
  ggplot() +
  geom_errorbar(aes(x = Parameter, ymin = exp(CRI.lb), ymax = exp(CRI.ub), color = Parameter),
                width = 5, linewidth = 8, alpha = 0.8) +
  # Customize scales and labels
  scale_color_manual(values = c("beta0.ref.low" = "mediumseagreen",
                                "beta0.ref.high" = "darkslategray4",
                                "beta0.burn.low" = "red3",
                                "beta0.burn.high" = "orange2"),
                     labels = c(
                       "Low Elevation Reference",
                       "Low Elevation Burn",
                       "High Elevation Reference",
                       "High Elevation Burn"), name = "") +
  theme_classic() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 18, family = "Times New Roman"),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.key.size = unit(1, "cm")) +
  # 2x2 legend
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))

# extract the legend
legend <- ggpubr::get_legend(legend_plot)

# Add the legend
full_fire_elv_pred_plot_legend <- grid.arrange(treatment_pred_plot, 
                                               legend,
                                               nrow = 2, 
                                               heights = c(0.6, 0.1))

# Save the plot as a png
ggsave(plot = full_fire_elv_pred_plot_legend,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_SABS_treatment.png"),
       width = 160,
       height = 175,
       units = "mm",
       dpi = 300)

# save the plot as an RDS
saveRDS(object = full_fire_elv_pred_plot_legend, 
        file = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_SABS_treatment.rds"))

