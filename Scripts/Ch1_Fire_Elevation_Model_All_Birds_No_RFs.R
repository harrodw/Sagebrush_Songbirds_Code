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
  # "SABS",
  # "GTTO")
  "VESP",
  "WEME",
  "HOLA")

# Loop over all species
for(k in 1:length(all_species)){ # (Comment this out) ----

# Pick a species to model
model_species <- all_species[k]
# model_species <- all_species[3]

# Add in count data from local drive
# Two grids (ID-C11 and ID-C22) were missing their Y1V1 survey these were imputed using the second visit
sobs_counts_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Grid_Counts.csv")) %>%
  tibble() %>%
  select(-X)
# #  or from github
# covs <- read.csv(paste0("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/",
#                         model_species, "_Grid_Counts.csv"))%>%
#   dplyr::select(-X) %>%
#   tibble()
#view the counts
# glimpse(sobs_counts_temp)

# Add in the observation data from the local drive
sobs_observations_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Observations.csv")) %>% 
  select(-X)
# #  or from github
# covs <- read.csv(paste0("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/",
# model_species, "_Observations.csv"))%>%
#   dplyr::select(-X) %>%
#   tibble()
# View the observations
# glimpse(sobs_observations_temp)

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
# glimpse(covs)

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

# Isolate cover covariates on reference grids 
ref_stats <- sobs_counts_temp2 %>% 
  filter(Burned == 0) %>% 
  distinct(Grid.ID, Shrub.Cover.125m, Perennial.Cover.125m)
# View
# glimpse(ref_stats)

# Find the reference covariate mean and sd 
mean_shrub <- mean(ref_stats$Shrub.Cover.125m)
sd_shrub <- sd(ref_stats$Shrub.Cover.125m)
mean_pern <- mean(ref_stats$Perennial.Cover.125m)
sd_pern <- sd(ref_stats$Perennial.Cover.125m)

# Scale each covariate
sobs_counts <- sobs_counts_temp2 %>% 
  mutate(ln.Years.Since.Fire = log(Years.Since.Fire)) %>% 
  mutate(Elevation.scl = scale(Elevation.125m)[,1],
         Shrub.Cover.scl = (Shrub.Cover.125m - mean_shrub) - sd_shrub,
         Perennial.Cover.scl = (Perennial.Cover.125m - mean_pern) - sd_pern,
         Mean.MAS.scl = scale(Mean.MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1],
         Years.Since.Fire.scl = (Years.Since.Fire - mean_fyear) / sd_fyear,
         ln.Years.Since.Fire.scl = (ln.Years.Since.Fire - mean_ln_fyear) /sd_ln_fyear,
         rdnbr.scl = (rdnbr.125m - mean_rdnbr) / sd_rdnbr)

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
# glimpse(sobs_counts)

# View observations
# glimpse(sobs_observations)

# 1.4) Plot preliminary correlations in the data ##############################################################
# 
# # Plot a specific treatment and bird abundance
# sobs_counts %>%
#   ggplot() +
#   geom_boxplot(aes(x = factor(Treatment), y = Count, color = factor(Treatment))) +
#   theme_bw()
# 
# # Plot burn sevarity and bird abundance
# sobs_counts %>%
#   filter(Burned == 1) %>%
#   ggplot() +
#   geom_smooth(aes(x = rdnbr.125m, y = Count, 
#                   # color = factor(Treatment)
#                   ), method = "glm", method.args = list(family = "quasipoisson")) +
#   geom_jitter(aes(x = rdnbr.125m, y = Count, 
#                   # color = factor(Treatment)
#               )) +
#   theme_bw()
# 
# # Plot time since fire and bird abundance
# sobs_counts %>%
#   filter(Burned == 1) %>%
#   ggplot() +
#   geom_smooth(aes(x = Years.Since.Fire, y = Count, 
#                   color = factor(Treatment)),
#               method = "glm", method.args = list(family = "quasipoisson")) +
#   geom_jitter(aes(x = Years.Since.Fire, y = Count, 
#                   color = factor(Treatment))) +
#   theme_bw()
# 
# # What are these low elevation recovered burn grids with high counts?
# sobs_counts %>% 
#   # filter(Burned == 1) %>% 
#   select(Grid.ID, Treatment, Years.Since.Fire, Count, Elevation.125m) %>% 
#   arrange(Years.Since.Fire) %>% 
#   print(n = Inf)
# 
# # Plot detection frequency by distance for each observer to insurre that the data is appropriate for distance sampling
# sobs_observations %>%
#   group_by(Grid.ID, Observer.ID, Observer.Experience, Dist.Bin, Dist.Bin.Midpoint) %>%
#   reframe(Grid.ID, Observer.ID, Observer.Experience, Dist.Bin, , Dist.Bin.Midpoint, bin.count = n()) %>%
#   distinct() %>%
#   ggplot(aes(x = Dist.Bin, y = bin.count
#              )) +
#   geom_col(fill = "lightblue") +
#   theme_bw() +
#   facet_wrap(~Observer.ID)
# 
# # How many observations did each observer have?
# sobs_observations %>% 
#   count(Observer.ID) 

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
shrub_cvr <- sobs_counts$Shrub.Cover.scl[1:ngrids]         # Percent shrub cover on each grid
pern_cvr <- sobs_counts$Perennial.Cover.scl[1:ngrids]      # Percent perennial cover on each grid
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
  
  # Parameters on the abundance component of the model ----
  # Fixed effects on abundance ----
  
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
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_d[j, k] <- sum(pi_pd[j, k, ])
      
      # Multiply availability with detection probability to yield total probability of capturing a bird
      p_cap[j, k] <- p_d[j, k] * p_a[j, k]
      
      ### Binomial portion of mixture ###
      
      # Poisson abundance portion of mixture
      N_indv[j, k] ~ dpois(lambda[j, k] * (present[j] + 0.0001) * area[j, k])   # ZIP true abundance at site j
      
      # Number of individual birds captured (observations)
      n_dct[j, k] ~ dbin(p_cap[j, k], N_indv[j, k])         
        
      # Availability (phi) Logit-linear model for availability
      logit(phi[j, k]) <- gamma0 +                    # Intercept on availability
                          gamma_date * day[j, k] +     # Effect of scaled ordinal date
                          gamma_time * time[j, k] +    # Effect of scaled time of day
                          gamma_date2 * day[j, k]^2 +  # Effect of scaled ordinal date squared
                          gamma_time2 * time[j, k]^2   # Effect of scaled time of day squared
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[j, k]) <- alpha0_obsv[observers[j, k]] # Intercept for of observer experience on detectability

      # Abundance (lambda) Log-linear model 
      log(lambda[j, k]) <- beta0_treatment[trts[j]] +                           # Intercept for each grid type
                           beta_fyear[elevation[j]] * fyear[j, k] * burned[j] + # Effect of time since fire on each treatment
                           beta_burnsev * burn_sev[j] * burned[j]               # Effect of initial burn severity on burned grids
                           

      # Assess model fit: compute Bayesian p-value for using a test statisitc
      e_val[j, k] <- p_cap[j, k] * N_indv[j, k]                       # Expected value for binomial portion of the model
      Chi[j, k] <- (n_dct[j, k] - e_val[j, k])^2 / (e_val[j, k] +0.5) # Compute chi squared statistic for observed data
      
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
  
  # Non-stochastic constants
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
  # Presence 
  psi = runif(ngrids, 0, 1),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated counts
  n_dct_new = count_mat,
  N_indv = count_mat + 1 # Counts helps to start each grid with an individual present       
)  

# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
                 "fit",            # Fit statistic for observed data
                 "fit_new",        # Fit statisitc for simulated data
                 "beta0_treatment",# Unique intercept by treatment
                 "beta_fyear",     # Effect of each year after a fire
                 "beta_burnsev",   # Effect of RdNBR burn sevarity
                 "gamma0",         # Intercept on availability
                 "gamma_date",     # Effect of date on singing rate
                 "gamma_date2",    # Quadratic effect of date on singing rate
                 "gamma_time",     # Effect of time of day on singing rate
                 "gamma_time2",    # Quadratic e of time of day on singning rate
                 "alpha0_obsv"     # Intercept for each observer on detection rate
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
  "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]",
  "alpha0_obsv[17]")
sobs_mcmcConf$addSampler(target = c(
  # Effect of each observer
  "alpha0_obsv[1]","alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]",
  "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
  "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]",
  "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]",
  "alpha0_obsv[17]"
  ), type = 'RW_block')

# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers(
  # Intercept by grid type
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Effect of time since fire by elevation
  "beta_fyear[1]", "beta_fyear[2]",
  # Effect of burn severity
  "beta_burnsev"
  )
sobs_mcmcConf$addSampler(target = c(
  # Intercept by grid type
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Effect of time since fire by elevation
  "beta_fyear[1]", "beta_fyear[2]",
  # Effect of burn severity
  "beta_burnsev"
  ), type = 'RW_block')

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
saveRDS(fire_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", model_species, "_fire_elevation_model_no_rf.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

# Load the output back in
fire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", model_species, "_fire_elevation_model_no_rf.rds"))
  
 # Traceplots and density graphs 
MCMCtrace(object = fire_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_fire_elevation_model_no_rf_traceplot"),
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
for(k in 1:length(all_species)) { # (Comment this out) ----

# Name the species to model again
plot_species <- all_species[k]
# plot_species <- all_species[1]

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
                                       plot_species, "_fire_elevation_model_no_rf.rds"))

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
         Significant = factor(case_when(CRI.lb * CRI.ub <= 0 ~ 0,
                                        CRI.lb * CRI.ub > 0 ~ 1))) %>% 
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
       paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_no_rf_no_rf_pred_",
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
  geom_boxplot(aes(x = Parameter, y = exp(Mean), color = Parameter, fill = Parameter), 
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
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_no_rf_no_rf_pred_",
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
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_no_rf_no_rf_pred_",
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
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_no_rf_pred_",
                         plot_species, "_burnsev.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

} # End plotting loop over all species (Comment this out) ----

