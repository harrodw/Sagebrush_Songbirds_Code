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
all_species <- c("BRSP", "SATH", "SABS", "GTTO", "VESP", "WEME", "HOLA")

# Loop over all species 
for(k in 1:length(all_species)){

# Pick a species to model
model_species <- all_species[k]
# model_species <- "SATH"

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
  dplyr::select(-Grid.Type) %>% 
  # Log-transform Annual cover and Tree cover
  mutate(ln.AFG.Cover.PF = case_when(Burned == 1 ~ log(AFG.Cover.PF), 
                                  TRUE ~ AFG.Cover.PF),
         ln.Tree.Cover.PF = case_when(Burned == 1 ~ log(Tree.Cover.PF), 
                                   TRUE ~  Tree.Cover.PF))

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

# Build a storage matrix of observations by visit
count_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix of years by visit
year_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix of survey times by visit
time_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix of survey dates by visit
date_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for the proportion of points visited during each survey
area <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for who conducted each survey
obsv_mat <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for years since fire during each survey
fyear_mat <- matrix(NA, nrow = nrows, ncol = ncols)

# Fill in the matrix
for(y in 1:nrow(count_mat)){
  # Filter for a specific grid
  count_visit <- sobs_counts %>% 
    arrange(Visit.ID.num) %>% 
    filter(Grid.ID.num == y)
  # Assign values to each row
  count_mat[y,] <- count_visit$Count
  year_mat[y,] <- count_visit$Year
  time_mat[y,] <- count_visit$Mean.MAS
  date_mat[y,] <- count_visit$Ord.Date
  # Area surveyd din km^2
  area[y,] <- pi * count_visit$n.Points * trunc_dist^2 
  obsv_mat[y,] <- count_visit$Observer.ID.num
  fyear_mat[y,] <- count_visit$Years.Since.Fire
}

# View the matrices
count_mat  # Number of individuals recorded at each visit
year_mat   # Year during which each visit took place
time_mat   # Scaled mean time of day for each visit
date_mat   # Scaled date for each visit
area       # Number of points visited during each survey 
obsv_mat   # Who conducted each survey

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
n_dct <- count_mat                                         # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                          # Matrix of year numbers
burned <- sobs_counts$Burned[1:ngrids]                     # Whether or not each grid burned
fire_year <- fyear_mat                                     # How long since the most recent fire in each grid
shrub_cvr <- sobs_counts$Shrub.Cover.PF[1:ngrids]          # Percent shrub cover
pfg_cvr <- sobs_counts$PFG.Cover.PF[1:ngrids]              # Percent perennial forb and grass cover
afg_cvr <- sobs_counts$ln.AFG.Cover.PF[1:ngrids]           # Percent annual grass cover
tree_cvr <- sobs_counts$ln.Tree.Cover.PF[1:ngrids]         # Percent tree cover 
bg_cvr <- sobs_counts$BG.Cover.PF[1:ngrids]                # Percent perennial bare ground cover

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
  gamma0 ~ dnorm(0, sd = 3)         # Mean availability
  gamma_date ~ dnorm(0, sd = 1.5)     # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 1.5)    # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 1.5)     # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 1.5)    # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model
  for(o in 1:nobsv){
    alpha0_obsv[o] ~ dnorm(0, sd = 3)   # Effect of each observer on detecability 
  }

  # Parameters on the abundance component of the model
  mean_beta0 ~ dnorm(0, sd = 3)     # Mean abundance hyperparameter
  sd_beta0 ~ T(dt(0, 1, 2), 0, 3)   # Sd in yearly abundance hyperparameter 
  
  # Random intercept on abundance
  for(t in 1:nyears){
    beta0_year[t] ~ dnorm(mean_beta0, sd_beta0)
  }
  # Fixed effects on abundance
  cur_shrub ~  dnorm(0, sd = 1.5)   # Effect of current shrub cover on reference grids
  beta_burn ~ dnorm(0, sd = 3)      # Effect of fire
  beta_elv ~  dnorm(0, sd = 1.5)    # Effect of elevation
  beta_fyear ~ dnorm(0, sd = 1.5)   # Effect of years since fire on burned grids
  beta_shrub ~ dnorm(0, sd = 1.5)   # Effect of shrub cover
  beta_pfg ~ dnorm(0, sd = 1.5)     # Effect of Perennial Cover
  beta_afg ~ dnorm(0, sd = 1.5)     # Effect of annual grass cover
  beta_tree ~ dnorm(0, sd = 1.5)    # Effect of tree cover
  beta_bg ~ dnorm(0, sd = 1.5)      # Effect of bare ground cover
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood
  
  # Iterate over all survey grids
  for(s in 1:ngrids){
    
    # Zero-inflation component on abundance
    psi[s] ~ dunif(0.0001, 0.9999)                                    # Occupancy probability
    present[s] ~ dbern(psi[s])                                        # Number of grids where that individual can be present

    # Iterate over all of the visits to each survey grid 
    for(y in 1:nvst){ 
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, y, b]) <- -(midpt[b]^2) / (2 * sigma[s, y]^2)        # Half-normal detection function
        f[s, y, b] <- (2 * midpt[b] * delta) / trunc_dist^2           # Prob density function out to max truncation distance 
        pi_pd[s, y, b] <- g[s, y, b] * f[s, y, b]                     # Detection cell probability
        pi_pd_c[s, y, b] <- f[s, y, b] / p_dct[s, y]                  # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_dct[s, y] <- sum(pi_pd[s, y, ])
      
      # Construction of the cell probabilities for the nints time intervals
      for (k in 1:nints){
        pi_pa[s, y, k] <- p_a[s, y] * (1 - p_a[s, y])^(k - 1)         # Availability cell probability
        pi_pa_c[s, y, k] <- pi_pa[s, y, k] / phi[s, y]                # Proportion of availability probability in each time interval class
      }   
      
      # Rectangular integral approx. of integral that yields the Pr(available)
      phi[s, y] <- sum(pi_pa[s, y, ])
      
      # Multiply availability with capture probability to yield total marginal probability
      p_marg[s, y] <- p_dct[s, y] * phi[s, y]
      
      ### Binomial portion of mixture 
      n_dct[s, y] ~ dbin(p_marg[s, y], N_indv[s, y])                  # Number of individual birds captured (observations)
      
      # Poisson abundance portion of mixture
      N_indv[s, y] ~ dpois(lambda[s, y] * (present[s] + 0.0001) * area[s, y]) # ZIP true abundance at site s in year y
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s, y]) <- alpha0_obsv[observers[s, y]]                # Effect of each observer on detectability
      
      # Availability (p_a) Log-linear model for availability
      logit(p_a[s, y]) <- gamma0 +                                    # Intercept on availability 
                          gamma_date * day[s, y] +                    # Effect of scaled ordinal date 
                          gamma_date2 * day[s, y]^2 +                 # Effect of scaled ordinal date squared 
                          gamma_time * time[s, y] +                   # Effect of scaled time of day 
                          gamma_time2 * time[s, y]^2                  # Effect of scaled time of day squared 
      
      # Abundance (lambda) Log-linear model 
      log(lambda[s, y]) <- beta0_year[years[s, y]] +                         # Intercept on abundance
                           beta_elevation * elevation[s]                     # Effect of elevation
                           beta_cur_shrub[s] * cur_shrub[s] * reference[s] + # Effect of this years shrub cover on reference grids
                           beta_burn * burned[s] +                           # Effect of fire 
                           beta_fyear * fire_year[s, y] * burned[s] +        # Effect of years since fire on burned grids
                           beta_shrub * shrub_cvr[s] * burned[s] +           # Effect of shrub cover
                           beta_pfg *  pfg_cvr[s] * burned[s] +              # Effect of Perennial Cover
                           beta_afg * afg_cvr[s] * burned[s] +               # Effect of annual grass
                           beta_tree * tree_cvr[s] * burned[s] +             # Effect of tree cover
                           beta_bg * bg_cvr[s] * burned[s]                   # Effect of bare ground cover
      
    } # end loop through visits
    
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ]) # linking distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])   # linking time removal data
  } # end observation loop
  
})

# 2.2) Cpoints_mat# 2.2) Constants, data, Initial values, and dimensions #############################################

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
  afg_cvr = afg_cvr,      # Percent annual grass cover
  tree_cvr = tree_cvr,    # Percent tree cover
  bg_cvr = bg_cvr         # Percent bare ground cover
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
  lambda = c(ngrids, nvst)           # Poisson random variable
)
# View the dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detecability 
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
  beta0_year = runif(nyears, 0, 1),    # Random effects seem to be happier when I start them all at positive values
  beta_elv = rnorm(1, 0, 0.1),
  beta_burn = rnorm(1, 0, 0.1),
  beta_fyear = rnorm(1, 0, 0.1),
  beta_shrub = rnorm(1, 0, 0.1),
  beta_pfg = rnorm(1, 0, 0.1),
  beta_afg = rnorm(1, 0, 0.1),
  beta_tree = rnorm(1, 0, 0.1),
  beta_bg = rnorm(1, 0, 0.1),
  # Presence 
  psi = runif(ngrids, 0.4, 0.6),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated data
  N_indv = count_mat + 1              # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c("beta0_year",
                 "beta_elev",
                 "beta_burn",
                 "beta_fyear",
                 "beta_shrub",
                 "beta_pfg",
                 "beta_afg",
                 "beta_tree",
                 "beta_bg",
                 "gamma0", 
                 "gamma_date", 
                 "gamma_date2", 
                 "gamma_time", 
                 "gamma_time2", 
                 "alpha0_obsv")


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
sobs_mcmcConf$removeSamplers("beta0_year[1]", "beta0_year[2]", "beta0_year[3]",
                             "beta_elv",
                             "beta_burn", 
                             "beta_fyear", 
                             "beta_shrub", 
                             "beta_pfg", 
                             "beta_afg", 
                             "beta_tree", 
                             "beta_bg")

sobs_mcmcConf$addSampler(target = c("beta0_year[1]", "beta0_year[2]", "beta0_year[3]",
                                    "beta_elv",
                                    "beta_burn", 
                                    "beta_fyear",  
                                    "beta_shrub", 
                                    "beta_pfg", 
                                    "beta_afg", 
                                    "beta_tree", 
                                    "beta_bg"),
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
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1           # Quick test to see if the model runs
# nc <- 3  ;  ni <- 150000  ;  nb <- 50000;  nt <- 10    # longer test where most parameters should
nc <- 4;  ni <- 500000;  nb <- 250000;  nt <- 25        # Run the model for real

# Quick check of how many samples we'll keep in the posterior
message(paste((ni - nb) / nt), " samples will be kept from the posterior")

# Run the sampler
start <- Sys.time()                                     # Start time for the sampler
start
prefire_mcmc_out <- runMCMC(cMCMC,
                         niter = ni, 
                         nburnin = nb, 
                         thin = nt, 
                         nchains = nc,
                         samplesAsCodaMCMC = T,
                         summary = T)
difftime(Sys.time(), start)                             # End time for the sampler

# Save model output to local drive
saveRDS(prefire_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     study_species, "_PreFire_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################


# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
prefire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                          plot_species, "_PreFire_model_out.rds"))

# Traceplots and density graphs 
MCMCtrace(object = prefire_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_fire_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = prefire_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = prefire_mcmc_out$samples,
         guide_lines = TRUE,
         params = sobs_params)

} # End the loop over species 

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# 4.1) Prepare and view model output ################################################

# Pick a species to plot
plot_species <- "SATH"

# Data frame for naming species
plot_species_df <- data.frame(Species.Code = plot_species) %>% 
  mutate(Species.Name = case_when(Species.Code == "SATH" ~ "Sage Thrasher",
                                  Species.Code == "BRSP" ~ "Brewer's Sparrow", 
                                  Species.Code == "SABS" ~ "Sagebrush Sparrow",
                                  Species.Code == "GTTO" ~ "Green-Tailed Towhee",
                                  Species.Code == "VESP" ~ "Vesper Sparrow",
                                  Species.Code == "WEME" ~ "Western Meadowlark",
                                  Species.Code == "HOLA" ~ "Horned Lark"))
species_name <- plot_species_df$Species.Name[1]
# View
species_name

# Load the output back in
prefire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       plot_species, "_PreFire_model_out.rds"))

# View MCMC summary
prefire_mcmc_out$summary$all.chains

# Parameters for the MCMC plot
sobs_params_plot <- c(
  "beta0_year",
  "beta_burn",
  "beta_fyear",
  "beta_shrub", 
  "beta_pfg", 
  "beta_afg", 
  "beta_tree"
)

# View MCMC plot
MCMCplot(object = prefire_mcmc_out$samples,
         guide_lines = TRUE,
         labels = c(
                    "2022 Intercept",
                    "2023 Intercept",
                    "2024 Intercept",
                    "Fire",
                    "Years Since Fire",
                    "Prefire Shrub Cover",
                    "Prefire Perennial Cover",
                    "Prefire Annual Cover",
                    "Prefire Tree Cover"),
         main = species_name,
         col = "black",
         ref_ovl = TRUE,
         params = sobs_params_plot) 

# Save the plot
ggsave(plot = treatment_pred_plot,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
                         plot_species, "_treatment.png"),
       width = 700,
       height = 500,
       units = "mm",
       dpi = 1000)

# 4.2) Extract coefficient values ###############################################################


# Extract Beta0 values
beta0_year1 <- prefire_mcmc_out$summary$all.chains[18,]
beta0_year2 <- prefire_mcmc_out$summary$all.chains[19,]
beta0_year3 <- prefire_mcmc_out$summary$all.chains[20,]
# View intercepts
bind_rows(beta0_year1, beta0_year2, beta0_year3)

# Extract effect sizes
beta_burn <- prefire_mcmc_out$summary$all.chains[22,]
beta_fyear <- prefire_mcmc_out$summary$all.chains[23,]
beta_shrub <- prefire_mcmc_out$summary$all.chains[25,]
beta_pfg <- prefire_mcmc_out$summary$all.chains[24,]
beta_afg <- prefire_mcmc_out$summary$all.chains[21,]
beta_tree <- prefire_mcmc_out$summary$all.chains[26,]

# View Betas
bind_rows(beta_burn, beta_fyear,beta_shrub, beta_pfg, beta_afg, beta_tree)

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0_year1, beta0_year2, beta0_year3,
                                 beta_burn,
                                 beta_fyear,
                                 beta_shrub, 
                                 beta_pfg,
                                 beta_afg, 
                                 beta_tree)) %>% 
  mutate(Parameter = c("Beta0.Year1", "Beta0.Year2", "Beta0.Year3",
                       "Beta.Burn", 
                       "Beta.fYear",
                       "Beta.Shrub", 
                       "Beta.PFG", 
                       "Beta.AFG", 
                       "Beta.Tree")) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CI.lb = X95.CI_low,
         CI.ub = X95.CI_upp)

# View output dataframe
glimpse(beta_dat)
head(beta_dat, n = Inf)

# 4.3) Predicted values ##########################################################################

# Pick a number of samples for posterior predictions
n <- 1000

# Rearrange the columns that I need
beta_dat_long <- beta_dat %>%
  pivot_longer(cols = c(Mean, CI.lb, CI.ub),
               names_to = "Statistic",
               values_to = "Value") %>% 
  mutate(ColumnName = paste(Parameter, Statistic, sep = ".")) %>%
  select(ColumnName, Value)

# Then pivot to the desired format

# Then pivot to the desired format
beta_dat_pred <- beta_dat_long %>%
  pivot_wider(names_from = ColumnName,
              values_from = Value) %>%
  # Average the intercept across all years
  mutate(Beta0.Mean = rowMeans(select(., matches("Beta0.Year[0-9]+\\.Mean")), na.rm = TRUE),
         Beta0.lb = rowMeans(select(., matches("Beta0.Year[0-9]+\\.CI.lb")), na.rm = TRUE),
         Beta0.ub = rowMeans(select(., matches("Beta0.Year[0-9]+\\.CI.ub")), na.rm = TRUE)) %>% 
  # Repeat each sample n times
  slice(rep(1:n(), each = n)) %>% 
  # Add in the predictive values 
  mutate(Pred = seq(from = -2, to = 2, length.out = n))
  
# View the new data 
glimpse(beta_dat_pred)

# 4.3) Plot each predicted value ###########################################

# Shrub cover plot --------------------------------------------------------------------

# Plot shrub cover 
shrub_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the reference grid mean and CI
    Beta0.Pred.Trend =  exp(Beta0.Mean),
    Beta0.Pred.lb = exp(Beta0.lb),
    Beta0.Pred.ub =  exp(Beta0.ub),
    # Calculate the predicted response to shrub cover 
    Shrub.Pred.Trend =  exp(Beta0.Mean + Beta.Burn.Mean + Beta.Shrub.Mean * Pred),
    Shrub.Pred.lb = exp(Beta0.lb + Beta.Burn.CI.lb + Beta.Shrub.CI.lb * Pred),
    Shrub.Pred.ub =  exp(Beta0.ub + Beta.Burn.CI.ub + Beta.Shrub.CI.ub * Pred),
    # Transform shrub cover back to the native scale
    Pred.Naive = (Pred * sd_shrub_cvr) + mean_shrub_cvr
  ) %>% 
  filter(Pred.Naive >= 0) %>%
  ggplot() +
  # Mean and CI for counts on reference grids
  geom_line(aes(x = Pred.Naive, y = Beta0.Pred.Trend), col = "blue") +
  geom_ribbon(aes(x = Pred.Naive, ymin = Beta0.Pred.lb, ymax = Beta0.Pred.ub),
              alpha = 0.2, fill = "blue") +
  # Effect of fire and pre--fire shrub cover
  geom_line(aes(x = Pred.Naive, y = Shrub.Pred.Trend), color = "red", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = Shrub.Pred.lb , ymax = Shrub.Pred.ub), 
              alpha = 0.2, fill = "red") +  
  labs(title = "", x = "Percent Pre-Fire Shrub Cover", y = "Predicted Abundance (birds/km^2)") +
  ylim(0, 90) +
  theme_classic()

# Display the plot
shrub_pred_plot

# Perennial forb and grass cover plot --------------------------------------------------------------------

# Plot shrub cover 
pfg_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the reference grid mean and CI
    Beta0.Pred.Trend =  exp(Beta0.Mean),
    Beta0.Pred.lb = exp(Beta0.lb),
    Beta0.Pred.ub =  exp(Beta0.ub),
    # Calculate the predicted response to PFG cover 
    PFG.Pred.Trend =  exp(Beta0.Mean + Beta.Burn.Mean + Beta.PFG.Mean * Pred),
    PFG.Pred.lb = exp(Beta0.lb + Beta.Burn.CI.lb + Beta.PFG.CI.lb * Pred),
    PFG.Pred.ub =  exp(Beta0.ub + Beta.Burn.CI.ub + Beta.PFG.CI.ub * Pred),
    # Transform PFG cover back to the native scale
    Pred.Naive = (Pred * sd_pfg_cvr) + mean_pfg_cvr
  ) %>% 
  filter(Pred.Naive >= 0) %>%
  ggplot() +
  # Mean and CI for counts on reference grids
  geom_line(aes(x = Pred.Naive, y = Beta0.Pred.Trend), col = "blue") +
  geom_ribbon(aes(x = Pred.Naive, ymin = Beta0.Pred.lb, ymax = Beta0.Pred.ub),
              alpha = 0.2, fill = "blue") +
  # Effect of fire and pre--fire perennial cover
  geom_line(aes(x = Pred.Naive, y = PFG.Pred.Trend), color = "red", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = PFG.Pred.lb , ymax = PFG.Pred.ub), 
              alpha = 0.2, fill = "red") +  
  labs(title = "", x = "Percent Pre-Fire Perennial Cover", y = "Predicted Abundance (birds/km^2)") +
  ylim(0, 90) +
  theme_classic()

# Display the plot
pfg_pred_plot

# Annual grass cover plot --------------------------------------------------------------------

# Plot shrub cover 
afg_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the reference grid mean and CI
    Beta0.Pred.Trend =  exp(Beta0.Mean),
    Beta0.Pred.lb = exp(Beta0.lb),
    Beta0.Pred.ub =  exp(Beta0.ub),
    # Calculate the predicted response to AFG cover 
    AFG.Pred.Trend =  exp(Beta0.Mean + Beta.Burn.Mean + Beta.AFG.Mean * Pred),
    AFG.Pred.lb = exp(Beta0.lb + Beta.Burn.CI.lb + Beta.AFG.CI.lb * Pred),
    AFG.Pred.ub =  exp(Beta0.ub + Beta.Burn.CI.ub + Beta.AFG.CI.ub * Pred),
    # Transform AFG cover back to the native scale
    Pred.Naive = exp((Pred * sd_afg_cvr) + mean_afg_cvr)
  ) %>% 
  filter(Pred.Naive >= 0) %>%
  ggplot() +
  # Mean and CI for counts on reference grids
  geom_line(aes(x = Pred.Naive, y = Beta0.Pred.Trend), col = "blue") +
  geom_ribbon(aes(x = Pred.Naive, ymin = Beta0.Pred.lb, ymax = Beta0.Pred.ub),
              alpha = 0.2, fill = "blue") +
  # Effect of fire and pre--fire perennial cover
  geom_line(aes(x = Pred.Naive, y = AFG.Pred.Trend), color = "red", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = AFG.Pred.lb , ymax = AFG.Pred.ub), 
              alpha = 0.2, fill = "red") +  
  labs(title = "", x = "Percent Pre-Fire Annual Grass Cover", y = "Predicted Abundance (birds/km^2)") +
  ylim(0, 90) +
  theme_classic()

# Display the plot
afg_pred_plot

# Tree cover plot --------------------------------------------------------------------

# Plot shrub cover 
tree_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the reference grid mean and CI
    Beta0.Pred.Trend =  exp(Beta0.Mean),
    Beta0.Pred.lb = exp(Beta0.lb),
    Beta0.Pred.ub =  exp(Beta0.ub),
    # Calculate the predicted response to Tree cover 
    Tree.Pred.Trend =  exp(Beta0.Mean + Beta.Burn.Mean + Beta.Tree.Mean * Pred),
    Tree.Pred.lb = exp(Beta0.lb + Beta.Burn.CI.lb + Beta.Tree.CI.lb * Pred),
    Tree.Pred.ub =  exp(Beta0.ub + Beta.Burn.CI.ub + Beta.Tree.CI.ub * Pred),
    # Transform Tree cover back to the native scale
    Pred.Naive = exp((Pred * sd_tree_cvr) + mean_tree_cvr)
  ) %>% 
  filter(Pred.Naive >= 0) %>%
  ggplot() +
  # Mean and CI for counts on reference grids
  geom_line(aes(x = Pred.Naive, y = Beta0.Pred.Trend), col = "blue") +
  geom_ribbon(aes(x = Pred.Naive, ymin = Beta0.Pred.lb, ymax = Beta0.Pred.ub),
              alpha = 0.2, fill = "blue") +
  # Effect of fire and pre--fire perennial cover
  geom_line(aes(x = Pred.Naive, y = Tree.Pred.Trend), color = "red", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = Tree.Pred.lb , ymax = Tree.Pred.ub), 
              alpha = 0.2, fill = "red") +  
  labs(title = "", x = "Percent Pre-Fire Tree Cover", y = "Predicted Abundance (birds/km^2)") +
  ylim(0, 90) +
  theme_classic()

# Display the plot
tree_pred_plot
