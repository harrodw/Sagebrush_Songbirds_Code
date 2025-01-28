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
# 1.0) Data Prep  ################################################################
################################################################################

# 1.1) Read in data ################################################################

# Add in count data from local drive
# Two grids (ID-C11 and ID-C22) were missing their Y1V1 survey
# Counts/weather were estimated based on the mode for other visits to those grids
counts_temp <- read.csv(paste0("Data/Outputs/BRSP_Grid_Counts.csv")) %>%
  tibble() %>%
  select(-X)
#view the counts
glimpse(counts_temp)

# Add in the observation data from the local drive
observations_temp <- read.csv(paste0("Data/Outputs/BRSP_Observations.csv"))

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
counts_temp2 <- counts_temp %>%
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
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID", "Year"), factor) %>% 
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
                              TRUE ~ NA))

#...and view
glimpse(counts_temp2)

# Isolate the burned Grids as their own object so I can accurately scale them
fire_stats <- counts_temp2 %>% 
  filter(Burned == 1) %>% 
  dplyr::select(Grid.ID, Years.Since.Fire, ln.Years.Since.Fire, rdnbr.125m) 

# Find the mean and standard deviation of the "real" burns
mean_fyear <- mean(fire_stats$Years.Since.Fire)
sd_fyear <- sd(fire_stats$Years.Since.Fire)
mean_ln_fyear <- mean(fire_stats$ln.Years.Since.Fire)
sd_ln_fyear <- sd(fire_stats$ln.Years.Since.Fire)
mean_rdnbr <- mean(fire_stats$rdnbr.125m)
sd_rdnbr <- sd(fire_stats$rdnbr.125m)

# Scale the other covariates
counts <- counts_temp2 %>% 
  mutate(ln.Years.Since.Fire = log(Years.Since.Fire)) %>% 
  mutate(Elevation.scl = scale(Elevation.125m)[,1],
         Mean.MAS.scl = scale(Mean.MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1],
         Years.Since.Fire.scl = (Years.Since.Fire - mean_fyear) / sd_fyear,
         ln.Years.Since.Fire.scl = (ln.Years.Since.Fire - mean_ln_fyear) /sd_ln_fyear,
         rdnbr.scl = (rdnbr.125m - mean_rdnbr) / sd_rdnbr)

# 1.3) Prepare the observation level data ################################################################

# Make sure the same numeric values for factors are shaped between the two datasets
counts <- counts %>% 
  arrange(Year, Visit.ID,  Grid.ID) %>% 
  mutate(Grid.ID.num = as.numeric(Grid.ID),
         Year.num = as.numeric(Year),
         Visit.ID.num = as.numeric(Visit.ID),
         Observer.ID.num = as.numeric(Observer.ID)) 

# View who each observer is
counts %>% 
  distinct(Observer.ID, Observer.ID.num) %>% 
  arrange(Observer.ID.num)

# Pull out the covariates that need to be shared across counts and observations
point_ids <- counts %>% 
  mutate_at(c("Grid.ID", "Year", "Visit.ID"), as.character) %>% 
  dplyr::select(Grid.ID, Grid.ID.num, Year, Year.num, Visit.ID, Visit.ID.num, Observer.ID.num)
#...and view
glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
observations <- observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Year", "Visit.ID"))

# View Counts
glimpse(counts)

# View Observations
glimpse(observations)

# 1.4) Plot preliminary correlations in the data ##############################################################

# # Plot a specific covariate against bird abundance
counts %>%
  # filter(Burned == 1) %>%
  ggplot() +
  # geom_line(aes(y = mean(counts$Count[counts$Burned == 0]), lwd = 0.3)) +
  geom_boxplot(aes(x = factor(Treatment), y = Count, color = factor(Treatment))) +
  # geom_smooth(aes(x = Years.Since.Fire, y = Count, color = factor(Treatment)), method = "glm", method.args = list(family = "quasipoisson")) +
  # geom_jitter(aes(x = Years.Since.Fire, y = Count, color = factor(Treatment))) +
  theme_bw()

# What are these low elevation recovered burn grids with high counts?
counts %>% 
  # filter(Burned == 1) %>% 
  select(Grid.ID, Treatment, Years.Since.Fire, Count, Elevation.125m) %>% 
  arrange(Years.Since.Fire) %>% 
  print(n = Inf)

 # # Plot detection frequency by distance for each observer to insurre that the data is appropriate for distance sampling
# observations %>%
#   group_by(Grid.ID, Observer.ID, Dist.Bin, Dist.Bin.Midpoint) %>%
#   reframe(Grid.ID, Observer.ID, Dist.Bin, , Dist.Bin.Midpoint, bin.count = n()) %>%
#   distinct() %>%
#   # group_by(Observer.ID, Grid.ID) %>%
#   # reframe(Grid.ID, Observer.ID, Dist.Bin, Dist.Bin.Midpoint, bin.count, total.count = sum(bin.count)) %>%
#   # mutate(scl.bin.count = bin.count / (pi*(2000*Dist.Bin.Midpoint)^2)) %>%
#   ggplot(aes(x = Dist.Bin, y = bin.count
#              # scl.bin.count / total.count
#              )) +
#   geom_col(fill = "lightblue") +
#   theme_bw() +
#   facet_wrap(~ Observer.ID)

# 1.4) prepare objects for NIMBLE ################################################################

# Define a truncation distance (km)
trunc_dist <- 0.125

# Matrix dimentions
nrows <- length(unique(counts$Grid.ID.num))  # Number of survey grids
ncols <- length(unique(counts$Visit.ID.num)) # Times each grid was visited

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
# Build a storage matrix for years since fire during each survey
fyear_mat <- matrix(NA, nrow = nrows, ncol = ncols)

# Fill in the matrix
for(y in 1:nrow(count_mat)){
  # Filter for a specific grid
  count_visit <- counts %>% 
    arrange(Visit.ID.num) %>% 
    filter(Grid.ID.num == y)
  # Assign values to each row
  count_mat[y,] <- count_visit$Count
  year_mat[y,] <- count_visit$Year
  time_mat[y,] <- count_visit$Mean.MAS.scl
  date_mat[y,] <- count_visit$Ord.Date.scl
  # Area surveyed in km^2
  area_mat[y,] <- pi * count_visit$n.Points * trunc_dist^2 
  obsv_mat[y,] <- count_visit$Observer.ID.num
  fyear_mat[y,] <- count_visit$Years.Since.Fire.scl
}

# View the matrices
count_mat  # Number of individuals recorded at each visit
year_mat   # Year during which each visit took place
time_mat   # Scaled mean time of day for each visit
date_mat   # Scaled date for each visit
area_mat # Number of points visited during each survey 
obsv_mat   # Who conducted each survey
fyear_mat  # Years since fire during each survey

# Size Objects  
ngrids <- length(unique(counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(observations)                            # Number of individuals detected 
nobsv <- length(unique(counts$Observer.ID.num))       # Number of unique observers
nbins <- length(unique(observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(observations$Time.Interval))   # Number of time intervals
nvst <- length(unique(counts$Visit.ID))               # Number of visits in each year
nyears <- length(unique(counts$Year.num))             # Number of years we surveyed
ntrts <- length(unique(counts$Treatment))             # Number of treatments (Elevation x Burn)
nelv <- length(unique(counts$High.Elevation))         # Number of elevations (High or Low)

# Observation Level data 
midpt <- sort(unique(observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
observers <- obsv_mat                                 # Random effect for observer associated with each survey
obs_visit <- observations$Visit.ID.num                # During which visit did each observation take place
obs_grid <- observations$Grid.ID.num                  # In which grid did each observation take place
dclass <- observations$Dist.Bin                       # Distance class of each observation
delta <- bin_size <- trunc_dist / nbins               # Size of distance bins

# Availability date
tint <- observations$Time.Interval                    # Time interval for each observation 
time <- time_mat                                      # Matrix of scaled time after sunrise
day <- date_mat                                       # Matrix of scaled dates

# Grid level data
area <- area_mat                                      # Proportion of points surveyed       
n_dct <- count_mat                                    # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                     # Matrix of year numbers
elevation <- counts$High.Elevation[1:ngrids]          # Whether each grid is high (>= 1900m) or low (< 1900m) elevation
treatment <- counts$Treatment[1:ngrids]               # Treatment of each survey grid (1 = low-ref, 2 = high-ref, 3 = low-burn, 4 = high-burn)
burned <- counts$Burned[1:ngrids]                     # Whether or not each grid burned
fyear <- fyear_mat                                    # How long since the most recent fire in each grid
burn_sev <- counts$rdnbr.scl[1:ngrids]                # Burn severity from the most recent fire

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Model definition ################################################################
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sobs_model_code <- nimbleCode({
  # Priors for all parameters 
  # ------------------------------------------------------------------
  
  # Parameters in the availability component of the detection model
  gamma0 ~ dnorm(0, sd = 3)            # phi intercept on log scale 
  gamma_date ~ dnorm(0, sd = 1.5)        # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 1.5)       # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 1.5)        # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 1.5)       # Effect of time of day on singing rate (quadratic)

  # Parameters in the detection portion of the model
  # alpha0 ~ dnorm(0, sd = 3)            # Intercept on detectability
  for(o in 1:nobsv){
    alpha0_obsv[o] ~ dnorm(0, sd = 1.5)   # Effect of each observer on detecability 
  }

  # Parameters on the abundance component of the model
  # Hyperparameters
  mean_beta0 ~ dnorm(0, sd = 3)  # Mean abundance hyperparameter
  sd_beta0 ~ dunif(0, 5)         # Sd in yearly abundance hyperparameter
  # Random intercept on abundance
  for(s in 1:ngrids){
    beta0_grid[s] ~ dnorm(mean_beta0, sd = sd_beta0)
  }
  
  # Fixed effects for the abundance component of the model
  
  # Intercept offsets for each treatment type
  # (1 = low-ref, 2 = high-ref, 3 = low-burn, 4 = high-burn)
  for(d in 1:ntrts){ # ntrts = 4
    beta_treatment[d] ~ dnorm(0, sd = 3)
  }
  # Unique slopes for fire year and burn sevarity
  for(e in 1:nelv){ # nelv = 2
    beta_fyear[e] ~  dnorm(0, sd = 1.5)      # Effect of years since fire on burned grids
    beta_burnsev[e] ~ dnorm(0, sd = 1.5)     # Effect of initial burn severity on burned grids
  }

  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey grids
  for(s in 1:ngrids){
    
    # Zero-inflation component on abundance
    psi[s] ~ dunif(0.0001, 0.9999)                              # Occupancy probability
    present[s] ~ dbern(psi[s])                                  # Number of grids where that individual can be present

    # Iterate over all of the visits to each survey grid 
    for(y in 1:nvst){ 
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, y, b]) <- -(midpt[b]^2) / (2 * sigma[s, y]^2) # Half-normal detection function
        f[s, y, b] <- (2 * midpt[b]) / trunc_dist^2  * delta   # Prob density function out to max truncation distance 
        pi_pd[s, y, b] <- g[s, y, b] * f[s, y, b]              # Detection cell probability
        pi_pd_c[s, y, b] <- f[s, y, b] / p_dct[s, y]           # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_dct[s, y] <- sum(pi_pd[s, y, ])
      
      # Construction of the cell probabilities for the nints time intervals
      for (k in 1:nints){
        pi_pa[s, y, k] <- p_a[s, y] * (1 - p_a[s, y])^(k - 1)  # Availability cell probability
        pi_pa_c[s, y, k] <- pi_pa[s, y, k] / p_avail[s, y]     # Proportion of availability probability in each time interval class
      }

      # Rectangular integral approx. of integral that yields the Pr(available)
      p_avail[s, y] <- sum(pi_pa[s, y, ])

      # Multiply availability with capture probability to yield total marginal probability
      p_cap[s, y] <- p_dct[s, y] * p_avail[s, y]
      
      ### Binomial portion of mixture 
      n_dct[s, y] ~ dbin(p_cap[s, y], N_indv[s, y])         # Number of individual birds captured (observations)
      
      # Poisson abundance portion of mixture
      N_indv[s, y] ~ dpois(lambda[s, y] * (present[s] + 0.0001) * area[s, y]) # ZIP true abundance at site s in year y
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s, y]) <- alpha0_obsv[observers[s, y]]                 # Effect of each observer on detectability
                          # alpha0 +
      
      # Availability (p_a) Logit-linear model for availability
      logit(p_a[s, y]) <- gamma0 +                                    # Intercept on availability
                          gamma_date * day[s, y] +                    # Effect of scaled ordinal date
                          gamma_time * time[s, y] +                   # Effect of scaled time of day
                          gamma_date2 * day[s, y]^2 +                 # Effect of scaled ordinal date squared
                          gamma_time2 * time[s, y]^2                  # Effect of scaled time of day squared

      # Abundance (lambda) Log-linear model 
      log(lambda[s, y]) <- beta_treatment[treatment[s]] +                         # Effect of each treatment
                           beta_fyear[elevation[s]] * fyear[s, y] * burned[s] +    # Effect of time since fire on each treatment
                           beta_burnsev[elevation[s]] * burn_sev[s] * burned[s]    # Effect of initial burn severity on burned grids  
      
    } # end loop through visits
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ])    # likelihood for distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])      # likelihood for time removal data
  } # end observation loop
  
})

# 2.2) Constants, data, Initial values, parameters to save, and dimensions ##################################

# Constants to be fed into Nimble
sobs_const <- list (
  # Misc. Constants
  area = area,                 # Number of points surveyed per grid per visit
  delta = delta,               # Bin width
  trunc_dist = trunc_dist,     # Truncation distance
  
  # For loop sizes
  ngrids = ngrids,             # Number of survey grids
  nind = nind,                 # Number of individuals detected 
  nobsv = nobsv,               # Number of unique observers
  nvst = nvst,                 # Number of times each grid was surveyed (6)
  nbins = nbins,               # Number of distance bins
  nints = nints,               # Number of time intervals
  # nyears = nyears,             # Number of years we surveyed (3)
  ntrts = ntrts,               # Number of treatments (4)
  nelv = nelv,                 # Number of elevations (2)
  
  # Non-stochastic constants
  # years = years,               # Year when each survey took place
  obs_visit  = obs_visit,      # Visit when each observation took place
  obs_grid  = obs_grid,        # Grid of each observation 
  observers = observers,       # Effect of observer associated with each survey
  elevation = elevation,       # High or low elevation
  treatment = treatment        # Treatment of each survey grid (1 = low-ref, 2 = high-ref, 3 = low-burn, 4 = high-burn)
)

# View Nimble constants 
str(sobs_const)

# Data to be fed into Nimble 
sobs_dat <- list(
  # Detection level data
  dclass = dclass,        # Distance category for each observation
  midpt = midpt,          # Midpoints of distance bins
  
  # Availability level data
  tint = tint,            # Time interval for each observation
  time = time,            # Scaled mean time after sunrise
  day = day,              # Scaled date
  
  # Abundance level
  n_dct = n_dct,          # Number of detected individuals per site
  fyear = fyear,          # How long since the most recent fire in each grid
  burn_sev = burn_sev,    # Burn severity from the most recent fire
  burned = burned         # Whether or not each grid burned
)
# View Nimble data 
str(sobs_dat)

# set seed
# set.seed(01151999)

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
  p_avail = c(ngrids, nvst),         # Availability probability
  p_cap = c(ngrids, nvst),           # Combined probability of detecting an available bird
  lambda = c(ngrids, nvst)           # Poisson random variable
)

# View dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detectablility
  # alpha0 = rnorm(1, 0, 0.1),
  alpha0_obsv = rnorm(nobsv, 0, 0.1),
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),
  gamma_date = rnorm(1, 0, 0.1),
  gamma_time = rnorm(1, 0, 0.1),
  gamma_date2 = rnorm(1, 0, 0.1),
  gamma_time2 = rnorm(1, 0, 0.1),
  # Abundance 
  mean_beta0 = runif(1, 0, 1),
  sd_beta0 = runif(1, 0, 0.1),
  beta0_grid = runif(ngrids, 0, 1),
  beta_treatment = rnorm(ntrts, 0, 0.1),
  beta_fyear = rnorm(nelv, 0, 0.1),
  beta_burnsev = rnorm(nelv, 0, 0.1),
  # Presence 
  psi = runif(ngrids, 0, 1),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated data
  N_indv = count_mat + 1              # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
                 # "beta0_grid",
                 "beta_treatment",
                 "beta_fyear",
                 "beta_burnsev",
                 "gamma0",
                 "gamma_date",
                 "gamma_date2",
                 "gamma_time",
                 "gamma_time2",
                 "alpha0_obsv"
                 )

# 2.3) Configure and Run the model ###########################################################

# Build the nimble model
sobs_model_vect <- nimbleModel(sobs_model_code, 
                               data = sobs_dat,
                               constants = sobs_const,
                               dimensions = sobs_dims,
                               inits = sobs_inits)

# MCMC settings for default nimble model setup
# nc_test <- 1  ;  ni_test <- 1000  ;  nb_test <- 500  ;  nt_test <- 10          

# From Kezia and Liz:
# Next, run an MCMC with NIMBLE defaults; this can be a slow step.
# the just-do-it way to run an MCMC. this will take all steps to set up
# and run an MCMC using NIMBLE’s default configuration. Handles everything from 
# model building to compilation to running the MCMC
# sobs_model_out_test <- nimbleMCMC(code = sobs_model_code,
#                                          constants = sobs_const,
#                                          data = sobs_dat,
#                                          dimensions = sobs_dims,
#                                          inits = sobs_inits,
#                                          monitors = sobs_params,
#                                          nchains = nc_test,
#                                          niter = ni_test, 
#                                          nburnin = nb_test, 
#                                          thin = nt_test, 
#                                          samplesAsCodaMCMC = T,
#                                          summary = T)

# Assign default samplers to nodes
sobs_mcmcConf <- configureMCMC(sobs_model_vect, 
                               monitors = sobs_params)

# Block all availability (gamma) nodes together
sobs_mcmcConf$removeSamplers("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2")

sobs_mcmcConf$addSampler(target = c("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2"),
                         type = 'RW_block')

# Block all detection (alpha) nodes together
sobs_mcmcConf$removeSamplers(
                             # "alpha0",
                             "alpha0_obsv[1]", "alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]", 
                             "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
                             "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]", 
                             "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", 
                             "alpha0_obsv[17]")

sobs_mcmcConf$addSampler(target = c(
                                    # "alpha0", 
                                    "alpha0_obsv[1]", "alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]", 
                                    "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
                                    "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]", 
                                    "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", 
                                    "alpha0_obsv[17]"),
                         type = 'RW_block')


# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers(
  # Random effects
  "beta0_grid[1]", "beta0_grid[2]", "beta0_grid[3]", "beta0_grid[4]", "beta0_grid[5]", "beta0_grid[6]", 
  "beta0_grid[7]", "beta0_grid[8]", "beta0_grid[9]", "beta0_grid[10]", "beta0_grid[11]", "beta0_grid[12]", 
  "beta0_grid[13]", "beta0_grid[14]", "beta0_grid[15]", "beta0_grid[16]", "beta0_grid[17]", "beta0_grid[18]", 
  "beta0_grid[19]", "beta0_grid[20]", "beta0_grid[21]", "beta0_grid[22]", "beta0_grid[23]", "beta0_grid[24]", 
  "beta0_grid[25]", "beta0_grid[26]", "beta0_grid[27]", "beta0_grid[28]", "beta0_grid[29]", "beta0_grid[30]", 
  "beta0_grid[31]", "beta0_grid[32]", "beta0_grid[33]", "beta0_grid[34]", "beta0_grid[35]", "beta0_grid[36]", 
  "beta0_grid[37]", "beta0_grid[38]", "beta0_grid[39]", "beta0_grid[40]", "beta0_grid[41]", "beta0_grid[42]", 
  "beta0_grid[43]", "beta0_grid[44]", "beta0_grid[45]", "beta0_grid[46]", "beta0_grid[47]", "beta0_grid[48]", 
  "beta0_grid[49]", "beta0_grid[50]", "beta0_grid[51]", "beta0_grid[52]", "beta0_grid[53]", "beta0_grid[54]", 
  "beta0_grid[55]", "beta0_grid[56]", "beta0_grid[57]", "beta0_grid[58]", "beta0_grid[59]", "beta0_grid[60]",
  # Fixed Effects
  "beta_treatment[1]", "beta_treatment[2]", "beta_treatment[3]", "beta_treatment[4]",
  "beta_fyear[1]", "beta_fyear[2]",
  "beta_burnsev[1]", "beta_burnsev[2]"
                             )

sobs_mcmcConf$addSampler(target = c(
  # Random effects
  "beta0_grid[1]", "beta0_grid[2]", "beta0_grid[3]", "beta0_grid[4]", "beta0_grid[5]", "beta0_grid[6]", 
  "beta0_grid[7]", "beta0_grid[8]", "beta0_grid[9]", "beta0_grid[10]", "beta0_grid[11]", "beta0_grid[12]", 
  "beta0_grid[13]", "beta0_grid[14]", "beta0_grid[15]", "beta0_grid[16]", "beta0_grid[17]", "beta0_grid[18]", 
  "beta0_grid[19]", "beta0_grid[20]", "beta0_grid[21]", "beta0_grid[22]", "beta0_grid[23]", "beta0_grid[24]", 
  "beta0_grid[25]", "beta0_grid[26]", "beta0_grid[27]", "beta0_grid[28]", "beta0_grid[29]", "beta0_grid[30]", 
  "beta0_grid[31]", "beta0_grid[32]", "beta0_grid[33]", "beta0_grid[34]", "beta0_grid[35]", "beta0_grid[36]", 
  "beta0_grid[37]", "beta0_grid[38]", "beta0_grid[39]", "beta0_grid[40]", "beta0_grid[41]", "beta0_grid[42]", 
  "beta0_grid[43]", "beta0_grid[44]", "beta0_grid[45]", "beta0_grid[46]", "beta0_grid[47]", "beta0_grid[48]", 
  "beta0_grid[49]", "beta0_grid[50]", "beta0_grid[51]", "beta0_grid[52]", "beta0_grid[53]", "beta0_grid[54]", 
  "beta0_grid[55]", "beta0_grid[56]", "beta0_grid[57]", "beta0_grid[58]", "beta0_grid[59]", "beta0_grid[60]",
  # Fixed Effects
  "beta_treatment[1]", "beta_treatment[2]", "beta_treatment[3]", "beta_treatment[4]",
  "beta_fyear[1]", "beta_fyear[2]",
  "beta_burnsev[1]", "beta_burnsev[2]"
                                    ),
                         type = 'RW_block')

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
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1        # Quick test to see if the model runs
# nc <- 3  ;  ni <- 150000  ;  nb <- 50000;  nt <- 10   # longer test where most parameters should
nc <- 4;  ni <- 500000;  nb <- 250000;  nt <- 25        # Run the model for real

# Quick check of how many samples we'll keep in the posterior
message(paste((ni - nb) / nt), " samples will be kept from the posterior")

# Run the sampler
start <- Sys.time()                                     # Start time for the sampler
start
sobs_mcmc_out <- runMCMC(cMCMC,
                  niter = ni, 
                  nburnin = nb, 
                  thin = nt, 
                  nchains = nc,
                  samplesAsCodaMCMC = T,
                  summary = T)
difftime(Sys.time(), start)                             # End time for the sampler

# Save model output to local drive
saveRDS(sobs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//VESP_fire_elevation_model.rds"))


################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//BRSP_fire_model.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = sobs_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = "BRSP_fire_model_traceplot",
          type = 'both')

# View MCMC summary
MCMCsummary(object = sobs_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# Params to plot
sobs_params_plot <- c(
                 "beta_treatment",
                 "beta_fyear",
                 "beta_burnsev"
                 )

# View MCMC plot
MCMCplot(object = sobs_mcmc_out$samples,
         guide_lines = TRUE,
         labels = ,
         params = sobs_params_plot)

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//BRSP_fire_model_linear.rds"))

# View MCMC summary
sobs_mcmc_out$summary$all.chains

# 4.1) Extract coefficient values ###############################################################

# Extract effect sizes
# Treatment intercepts
beta0_ref_low <- sobs_mcmc_out$summary$all.chains[18,]
beta0_ref_high <- sobs_mcmc_out$summary$all.chains[19,]
beta0_burn_low <- sobs_mcmc_out$summary$all.chains[20,]
beta0_burn_high <- sobs_mcmc_out$summary$all.chains[21,]
# Fire Year
beta_fyear_low <- sobs_mcmc_out$summary$all.chains[24,]
beta_fyear_high <- sobs_mcmc_out$summary$all.chains[25,]
# Burn Sevarity
beta_burnsev_low <- sobs_mcmc_out$summary$all.chains[22,]
beta_burnsev_high <- sobs_mcmc_out$summary$all.chains[23,]

# View Betas
bind_rows(beta0_ref_low,
          beta0_ref_high,
          beta0_burn_low,
          beta0_burn_high,
          beta_burnsev_low,
          beta_burnsev_high,
          beta_fyear_low,
          beta_fyear_high
          )

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0_ref_low,
                                 beta0_ref_high,
                                 beta0_burn_low,
                                 beta0_burn_high,
                                 beta_burnsev_low,
                                 beta_burnsev_high,
                                 beta_fyear_low,
                                 beta_fyear_high
                                 )) %>% 
  mutate(Parameter = c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high",
                       "beta.burnsev.low", "beta.burnsev.high",
                       "beta.fyear.low", "beta.fyear.high"
                       )) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CI.lb = X95.CI_low,
         CI.ub = X95.CI_upp)

# View output dataframe
head(beta_dat, n = Inf)

# 4.2) Predicted values ##########################################################################

# Pick a number of samples
n <- 1000

# Rearrange the columns that I need
beta_dat_long <- beta_dat %>%
  pivot_longer(cols = c(Mean, CI.lb, CI.ub),
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

# Reorder 'Parameter' factor levels to match the desired color order
beta_dat <- beta_dat %>%
  mutate(Parameter = factor(Parameter, 
                            levels = c("beta0.ref.low", 
                                       "beta0.ref.high", 
                                       "beta0.burn.low", 
                                       "beta0.burn.high")))

# Create the plot
treatment_pred_plot <- beta_dat %>% 
  filter(Parameter %in% c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high")) %>% 
  ggplot() +
  geom_boxplot(aes(x = Parameter, y = exp(Mean), color = Parameter), 
               width = 0.45, size = 0.8) +
  geom_errorbar(aes(x = Parameter, ymin = exp(CI.lb), ymax = exp(CI.ub), color = Parameter), 
                width = 0.2, size = 1.4) +
  # Customize scales and labels
  scale_color_manual(values = c("beta0.ref.low" = "mediumseagreen",
                                "beta0.ref.high" = "darkslategray4",
                                "beta0.burn.low" = "red3",
                                "beta0.burn.high" = "orange2"),
                     labels = c("High Elevation Burn",
                                "Low Elevation Burn",
                                "High Elevation Reference",
                                "Low Elevation Reference"),
                     name = "Treatment") +
  labs(x = "Treatment", 
       y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  ylim(0, 100) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
  )

# Display the plot
treatment_pred_plot


# Burn Sevarity plot -------------------------------------------------------
# View predictive data
glimpse(beta_dat_pred)

# Mean and sd for burn sevarity are found earlier in the script 

# Plot predicted responce to Burn Sevarity
rdnbr_pred_plot <- beta_dat_pred %>% 
  mutate(Pred.Naive = Pred * sd_rdnbr + mean_rdnbr) %>% 
  filter(Pred.Naive > 0) %>%
  ggplot() +
  # Low elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.low.Mean), 
                color = "Low Elevation Reference"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.low.CI.lb), 
                  ymax = exp(beta0.ref.low.CI.ub), 
                  fill = "Low Elevation Reference"), alpha = 0.2) +
  # High elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.high.Mean), 
                color = "High Elevation Reference"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.high.CI.lb), 
                  ymax = exp(beta0.ref.high.CI.ub), 
                  fill = "High Elevation Reference"), alpha = 0.2) +
  # Low elevation burn severity
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.low.Mean + beta.burnsev.low.Mean * Pred 
                                        # + beta.burnsev2.low.Mean * Pred^2
                                        ), 
                color = "Low Elevation Burn"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.low.CI.lb + beta.burnsev.low.CI.lb * Pred 
                                             # + beta.burnsev2.low.CI.lb * Pred^2
                                             ), 
                  ymax = exp(beta0.burn.low.CI.ub + beta.burnsev.low.CI.ub * Pred 
                             # + beta.burnsev2.low.CI.ub * Pred^2
                             ), 
                  fill = "Low Elevation Burn"), alpha = 0.2) +
  # High elevation burn severity
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.high.Mean + beta.burnsev.high.Mean * Pred
                                        # + beta.burnsev2.high.Mean * Pred^2
                                        ), 
                color = "High Elevation Burn"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.high.CI.lb + beta.burnsev.high.CI.lb * Pred 
                                             # + beta.burnsev2.high.CI.lb * Pred^2
                                             ), 
                  ymax = exp(beta0.burn.high.CI.ub + beta.burnsev.high.CI.ub * Pred 
                             # + beta.burnsev2.high.CI.ub * Pred^2
                             ), 
                  fill = "High Elevation Burn"), alpha = 0.2) +
  # Customize scales and labels
  scale_color_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                                "High Elevation Reference" = "darkslategray4",
                                "Low Elevation Burn" = "red3",
                                "High Elevation Burn" = "orange2"),
                     name = "Treatment") +
  scale_fill_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                               "High Elevation Reference" = "darkslategray4",
                               "Low Elevation Burn" = "red3",
                               "High Elevation Burn" = "orange2"),
                    name = "Treatment") +
  labs(x = "rDNBR Burn Severity", 
       y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  ylim(0, 100) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
  )

# Display the plot
rdnbr_pred_plot

# Years since fire plot -------------------------------------------------------

# Plot predicted responce to time since fire
fyear_pred_plot <- beta_dat_pred %>% 
  mutate(Pred.Naive = Pred * sd_fyear + mean_fyear) %>% 
  # Only show up to where I have data
  filter(Pred.Naive > 0 & Pred.Naive <= 25) %>%
  ggplot() +
  # Low elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.low.Mean), 
                color = "Low Elevation Reference"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.low.CI.lb), 
                  ymax = exp(beta0.ref.low.CI.ub), 
                  fill = "Low Elevation Reference"), alpha = 0.2) +
  # High elevation reference
  geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.high.Mean), 
                color = "High Elevation Reference"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.high.CI.lb), 
                  ymax = exp(beta0.ref.high.CI.ub), 
                  fill = "High Elevation Reference"), alpha = 0.2) +
  # Low elevation time since fire
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.low.Mean + beta.fyear.low.Mean * Pred 
                                        # + beta.fyear2.low.Mean * Pred^2
                                        ), 
                color = "Low Elevation Burn"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.low.CI.lb + beta.fyear.low.CI.lb * Pred 
                                               # + beta.fyear2.low.CI.lb * Pred^2
                                             ), 
                  ymax = exp(beta0.burn.low.CI.ub + beta.fyear.low.CI.ub * Pred 
                             # + beta.fyear2.low.CI.ub * Pred^2
                             ), 
                  fill = "Low Elevation Burn"), alpha = 0.2) +
  # High elevation time since fire
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.high.Mean + beta.fyear.high.Mean * Pred 
                                        # + beta.fyear2.high.Mean * Pred^2
                                        ), 
                color = "High Elevation Burn"), size = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.high.CI.lb + beta.fyear.high.CI.lb * Pred 
                                             # + beta.fyear2.high.CI.lb * Pred^2
                                             ), 
                  ymax = exp(beta0.burn.high.CI.ub + beta.fyear.high.CI.ub * Pred 
                             # + beta.fyear2.high.CI.ub * Pred^2
                             ), 
                  fill = "High Elevation Burn"), alpha = 0.2) +
  # Customize scales and labels
  scale_color_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                                "High Elevation Reference" = "darkslategray4",
                                "Low Elevation Burn" = "red3",
                                "High Elevation Burn" = "orange2"),
                     name = "Treatment") +
  scale_fill_manual(values = c("Low Elevation Reference" = "mediumseagreen",
                               "High Elevation Reference" = "darkslategray4",
                               "Low Elevation Burn" = "red3",
                               "High Elevation Burn" = "orange2"),
                    name = "Treatment") +
  labs(x = "Years Since Fire", 
       y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  theme_classic() +
  ylim(0, 100) +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
  )

# Display the plot
fyear_pred_plot

