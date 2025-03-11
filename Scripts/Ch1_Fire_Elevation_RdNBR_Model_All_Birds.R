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
  "SATH",
  "BRSP",
  "VESP",
  "WEME",
  "HOLA"
)

# Loop over all species
for(s in 1:length(all_species)){ # (Comment this out) ----

# Pick a species to model
model_species <- all_species[s]
# model_species <- all_species[2]

# Add in count data from local drive
# Two grids (ID-C11 and ID-C22) were missing their Y1V1 survey these were imputed using the second visit
# Total Indv = number of all other species (Good proxy for noise)
sobs_counts_temp <- read.csv(paste0("Data/Outputs/", model_species, "_Grid_Counts.csv")) %>%
  tibble() %>%
  select(-X)
# Or from github
# sobs_counts_temp <- read.csv(paste0("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/",
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
  mutate(High.Elevation = case_when(Elevation < 1800 ~ 1,
                                    Elevation >= 1800 ~ 2,
                                    TRUE ~ NA),
         # Binaary column for sagebrush type / R&R (1 = Wyoming or Basin, 2 = Mountain)
         Sage.Type = case_when(Sage.Type %in% c(1, 3) ~ 1,
                               Sage.Type == 2 ~ 2,
                               TRUE ~ Sage.Type)) %>% 
  # Make a treatment column (elevation x burned)
  mutate(Elv.Treatment = case_when(Burned == 0 & High.Elevation == 1 ~ 1,
                               Burned == 0 & High.Elevation == 2 ~ 2,
                               Burned == 1 & High.Elevation == 1 ~ 3,
                               Burned == 1 & High.Elevation == 2 ~ 4,
                               TRUE ~ NA),
         # Same thing but instead based on sagebrush type
         Sage.Treatment = case_when(Sage.Type == 1 & Burned == 0 ~ 1,
                                    Sage.Type == 2 & Burned == 0 ~ 2,
                                    Sage.Type == 1 & Burned == 1 ~ 3,
                                    Sage.Type == 2 & Burned == 1 ~ 4,
                                    TRUE ~ NA))
  

#...and view
glimpse(sobs_counts_temp2)

# Isolate the burned Grids as their own object so I can accurately scale them
fire_stats <- sobs_counts_temp2 %>% 
  filter(Burned == 1) %>% 
  distinct(Grid.ID, Years.Since.Fire, ln.Years.Since.Fire, rdnbr) 
# View
# glimpse(fire_stats)

# Find the mean and standard deviation of the "real" burns
mean_fyear <- mean(fire_stats$Years.Since.Fire)
sd_fyear <- sd(fire_stats$Years.Since.Fire)
mean_rdnbr <- mean(fire_stats$rdnbr)
sd_rdnbr <- sd(fire_stats$rdnbr)

# Scale each covariate
sobs_counts <- sobs_counts_temp2 %>% 
  mutate(Mean.MAS.scl = scale(Mean.MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1],
         Years.Since.Fire.scl = (Years.Since.Fire - mean_fyear) / sd_fyear,
         rdnbr.scl = (rdnbr - mean_rdnbr) / sd_rdnbr)
# View
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

# Plot species counts against a covariate
# sobs_counts %>%
#   filter(Burned == 1) %>%
# ggplot(aes(x = Years.Since.Fire, y = Count, color = factor(Elv.Treatment))) +
# geom_jitter() +
#   geom_smooth(
#               method = "glm",
#               method.args = list(family = "quasipoisson"),
#               se = TRUE)

# Plots by treatment group
# sobs_counts %>%
#   ggplot() +
#   geom_boxplot(aes(x = factor(Elv.Treatment), y = Count))


# 1.4) prepare objects for NIMBLE ################################################################

# Define a truncation distance (km)
trunc_dist <- 0.125

# Matrix dimentions
nrows <- length(unique(sobs_counts$Grid.ID.num))    # Number of survey grids
ncols <- length(unique(sobs_counts$Visit.ID.num))   # Times each grid was visited

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
  time_mat[j,] <- count_visit$Mean.MAS.scl
  date_mat[j,] <- count_visit$Ord.Date.scl
  area_mat[j,] <- pi * count_visit$n.Points * trunc_dist^2  # Area surveyed in km^2
  obsv_mat[j,] <- count_visit$Observer.ID.num
  exp_mat[j,] <- count_visit$Observer.Experience 
  fyear_mat[j,] <- count_visit$Years.Since.Fire.scl
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
ntrts <- length(unique(sobs_counts$Elv.Treatment))         # Number of treatments (Elevation x Burn)
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
elevation <- sobs_counts$High.Elevation[1:ngrids]          # Whether each grid is high (>= 1900m) or low (< 1900m) elevation
burned <- sobs_counts$Burned[1:ngrids]                     # Whether or not each grid burned
fyear <- fyear_mat                                         # How long since the most recent fire in each grid
rdnbr <- sobs_counts$rdnbr.scl[1:ngrids]                   # Burn severity from the most recent fire
trts <- sobs_counts$Elv.Treatment[1:ngrids]               # Grid type (Elevation x burned)

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Model definition ################################################################

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
  for(o in 1:nobsv){ # nobsv = 17
    alpha0_obsv[o] ~ T(dnorm(-2, sd = 3), -9, 0) # Prior mean centered on approximately sigma = exp(-2) = 0.125km
  } # End loop through observers
  
  # Parameters on the abundance component of the model ----
  
  # Intercept by treatment
  for(l in 1:ntrts){ # ntrts = 4
    # Allow the intercept too vary for each treatment group
    beta0_treatment[l] ~ dnorm(0, sd = 3)
  } # End loop over treatments
  
  # Effect of years since fire on burned grids varying with elevation
  for(e in 1:nelv){ # nelv = 2 (high and low elevation)
    beta_fyear[e] ~ dnorm(0, sd = 1.5)
  } # End loop over burned elevations
  
  # Effect of burn severity
  beta_rdnbr ~ dnorm(0, sd = 1.5)
  
  # Abundance random effect hyper-parameters
  sd_eps_year ~ dgamma(shape = 0.5, scale = 0.5)   # Random effect on abundance hyperparameter for each year
  sd_omega_grid ~ dgamma(shape = 0.5, scale = 0.5) # Random effect on abundance hyperparameter for each grid

  # Random noise between years
  for(y in 2:nyears){ # nyears = 3
    # Force the first year (2022) to be the intercept
    eps_year[y] ~ dnorm(0, sd = sd_eps_year)
  } # end loop over years
  
  # Random noise between sites
  for(j in 2:ngrids){ # ngrids = 60
    # Force the first grid to be the intercept
    omega_grid[j] ~ dnorm(0, sd = sd_omega_grid)
  } # end loop over years
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey grids
  for(j in 1:ngrids){ # ngrids = 60
    
    # Iterate over all of the visits to each survey grid 
    for(k in 1:nvst){ # nvst = 6
      
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
      N_indv[j, k] ~ dpois(lambda[j, k] * area[j, k])    # ZIP true abundance at site j during visit k
      
      # Availability (phi) Logit-linear model for availability
      logit(phi[j, k]) <- gamma0 +                       # Intercept on availability
                          gamma_date * day[j, k] +       # Effect of scaled ordinal date
                          gamma_time * time[j, k] +      # Effect of scaled time of day
                          gamma_date2 * day[j, k]^2 +    # Effect of scaled ordinal date squared
                          gamma_time2 * time[j, k]^2     # Effect of scaled time of day squared
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[j, k]) <- alpha0_obsv[observers[j, k]]   # Intercept for of observer experience on detectability
        
      # Abundance (lambda) Log-linear model 
      log(lambda[j, k]) <- beta0_treatment[trts[j]] +                           # Intercept for each grid type
                           beta_fyear[elevation[j]] * fyear[j, k] * burned[j] + # Effect of time since fire on each treatment
                           beta_rdnbr * rdnbr[j] * burned[j] +                  # Effect of burn severity 
                           eps_year[years[j, k]] +                              # Unexplained noise on abundance by year
                           omega_grid[j]                                        # Unexplained noise on abundance by grid

      # -------------------------------------------------------------------------------------------------------------------
      # Assess model fit: compute Bayesian p-value for using a test statisitcs
      
      # Chi square statisitc for the availability portion of the model
      e_pa[j, k] <- p_a[j, k] * N_indv[j, k]                                      # Expected value for availability binomial portion of the model
      n_avail_new[j, k] ~ dbin(p_a[j, k], N_indv[j, k])                           # Draw new available birds from the same binomial
      Chi_pa[j, k] <- (n_avail[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5)         # Compute availability chi squared statistic for observed data
      Chi_pa_new[j, k] <- (n_avail_new[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5) # Compute availability chi squared statistic for simulated data data
      
      # Chi square statisitc for the detection portion of the model
      e_pd[j, k] <- p_d[j, k] * n_avail[j, k]                                     # Expected value for detection binomial portion of the model
      n_dct_new[j, k] ~ dbin(p_d[j, k], n_avail[j, k])                            # Draw new detentions from the same binomial
      Chi_pd[j, k] <- (n_dct[j, k] - e_pd[j, k])^2 / (e_pd[j, k] + 0.5)           # Compute detecability chi squared statistic for observed data
      Chi_pd_new[j, k] <- (n_dct_new[j, k] - e_pd[j, k])^2 / (e_pd[j, k] + 0.5)   # Compute detecability chi squared statistic for simulated data data
      
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

  # Calculate the change in avian abundance following fire 
  fire_eff_low <- beta0_treatment[3] - beta0_treatment[1] # Mean change in population following a fire at low elevation
  fire_eff_hgh <- beta0_treatment[4] - beta0_treatment[2] # Mean change in population following a fire at high elevation
  
  # Combine fit statistics
  
  # Add up fit stats for availability across sites and years
  fit_pa <- sum(Chi_pa[,])
  fit_pa_new <- sum(Chi_pa_new[,])
  
  # Add up fit stats for detectability across sites and years
  fit_pd <- sum(Chi_pd[,])
  fit_pd_new <- sum(Chi_pd_new[,])
  
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
  # Availability level data
  tint = tint,                 # Time interval for each observation
  time = time,                 # Scaled mean time after sunrise
  day = day,                   # Scaled date
  # Abundance level
  n_dct = n_dct,               # Number of detected individuals per site
  fyear = fyear,               # How long since the most recent fire in each grid
  rdnbr = rdnbr,               # rdnbr burn severity
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
  lambda = c(ngrids, nvst),          # Poisson random variable
  e_pa = c(ngrids , nvst),           # Expected value for availebility portion of the model
  e_pd = c(ngrids , nvst),           # Expected value for detection portion of the model
  Chi_pa = c(ngrids , nvst),         # Observed Chi square statistic for availability
  Chi_pa_new = c(ngrids, nvst),      # Simulated Chi square statistic for availability
  Chi_pd = c(ngrids , nvst),         # Observed Chi square statistic for detection 
  Chi_pd_new = c(ngrids, nvst)       # Simulated Chi square statistic for detection
)

# View dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detectablility
  alpha0_obsv = runif(nobsv, -2, -0.1),       # Effect of each observer on detecability
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),                  # Intercept on availability
  gamma_date = rnorm(1, 0, 0.1),              # Effect of date on availability
  gamma_time = rnorm(1, 0, 0.1),              # Effect of time of day on availability
  gamma_date2 = rnorm(1, 0, 0.1),             # Effect of date on availability (quadratic)
  gamma_time2 = rnorm(1, 0, 0.1),             # Effect of time of day on availability (quadratic)
  # Abundance 
  beta0_treatment = rnorm(ntrts, 0, 0.1),     # Intercept by grid type
  beta_fyear = rnorm(nelv, 0, 0.1),           # Effect of time since fire by elevation
  beta_rdnbr = rnorm(1, 0, 0.1),              # Effect of burn severity
  sd_eps_year = runif(1, 0, 1),               # Magnitude of random noise by year (only positive)
  sd_omega_grid = runif(1, 0, 1),             # Magnitude of random noise by year (only positive)
  eps_year = c(0, rnorm(nyears-1, 0, 0.1)),   # Random noise on abundance by year
  omega_grid = c(0, rnorm(ngrids-1, 0, 0.1)), # Random noise on abundance by grid
  # Simulated counts
  n_avail = count_mat + 1,                    # Number of available birds (helps to start each grid with an individual present)
  n_dct_new = count_mat,                      # Simulated detected birds 
  n_avail_new = count_mat + 1,                # Simulated available birds (helps to start each grid with an individual present)
  N_indv = count_mat + 1                      # "True" abundance (helps to start each grid with an individual present)
)
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
  "fit_pd",          # Fit statistic for observed data
  "fit_pd_new",      # Fit statistic for simulated detection  data
  "fit_pa",          # Fit statistic for first availability data
  "fit_pa_new",      # Fit statistic for simulated availability data
  "beta0_treatment", # Unique intercept by treatment
  "beta_fyear",      # Effect of each year after a fire
  "beta_rdnbr",      # Effect of burn severity 
  "sd_eps_year",     # Random noise on abundance by year
  "sd_omega_grid",   # Random noise on abundance by grids
  "gamma0",          # Intercept on availability
  "gamma_date",      # Effect of date on singing rate
  "gamma_date2",     # Quadratic effect of date on singing rate
  "gamma_time",      # Effect of time of day on singing rate
  "gamma_time2",     # Quadratic effect of time of day on singing rate
  "alpha0_obsv",     # Intercept for each observer on detection rate
  "fire_eff_low",    # Mean change in population following a low elevation fire
  "fire_eff_hgh"     # Mean change in population following a high elevation fire
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
  )
sobs_mcmcConf$addSampler(target = c(
  # Effect of each observer
  "alpha0_obsv[1]","alpha0_obsv[2]", "alpha0_obsv[3]", "alpha0_obsv[4]",
  "alpha0_obsv[5]", "alpha0_obsv[6]", "alpha0_obsv[7]", "alpha0_obsv[8]",
  "alpha0_obsv[9]", "alpha0_obsv[10]", "alpha0_obsv[11]", "alpha0_obsv[12]",
  "alpha0_obsv[13]", "alpha0_obsv[14]", "alpha0_obsv[15]", "alpha0_obsv[16]", "alpha0_obsv[17]"
  ), type = 'RW_block')

# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers(
  # Intercept by grid type
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Effect of time since fire by elevation
  "beta_fyear[1]", "beta_fyear[2]",
  # Effect of burn severity
  "beta_rdnbr",
  # Random noise by year
  "eps_year[2]", "eps_year[3]",
  #Random noise by grid
  "omega_grid[2]", "omega_grid[3]", "omega_grid[4]", "omega_grid[5]", "omega_grid[6]", 
  "omega_grid[7]", "omega_grid[8]", "omega_grid[9]", "omega_grid[10]", "omega_grid[11]", "omega_grid[12]", 
  "omega_grid[13]", "omega_grid[14]", "omega_grid[15]", "omega_grid[16]", "omega_grid[17]", "omega_grid[18]", 
  "omega_grid[19]", "omega_grid[20]", "omega_grid[21]", "omega_grid[22]", "omega_grid[23]", "omega_grid[24]", 
  "omega_grid[25]", "omega_grid[26]", "omega_grid[27]", "omega_grid[28]", "omega_grid[29]", "omega_grid[30]", 
  "omega_grid[31]", "omega_grid[32]", "omega_grid[33]", "omega_grid[34]", "omega_grid[35]", "omega_grid[36]", 
  "omega_grid[37]", "omega_grid[38]", "omega_grid[39]", "omega_grid[40]", "omega_grid[41]", "omega_grid[42]", 
  "omega_grid[43]", "omega_grid[44]", "omega_grid[45]", "omega_grid[46]", "omega_grid[47]", "omega_grid[48]", 
  "omega_grid[49]", "omega_grid[50]", "omega_grid[51]", "omega_grid[52]", "omega_grid[53]", "omega_grid[54]", 
  "omega_grid[55]", "omega_grid[56]", "omega_grid[57]", "omega_grid[58]", "omega_grid[59]", "omega_grid[60]"
  )
sobs_mcmcConf$addSampler(target = c(
  # Intercept by grid type
  "beta0_treatment[1]", "beta0_treatment[2]", "beta0_treatment[3]", "beta0_treatment[4]",
  # Effect of time since fire by elevation
  "beta_fyear[1]", "beta_fyear[2]",
  # Effect of burn severity
  "beta_rdnbr",
  # Random noise by year
  "eps_year[2]", "eps_year[3]",
  #Random noise by grid
  "omega_grid[2]", "omega_grid[3]", "omega_grid[4]", "omega_grid[5]", "omega_grid[6]", 
  "omega_grid[7]", "omega_grid[8]", "omega_grid[9]", "omega_grid[10]", "omega_grid[11]", "omega_grid[12]", 
  "omega_grid[13]", "omega_grid[14]", "omega_grid[15]", "omega_grid[16]", "omega_grid[17]", "omega_grid[18]", 
  "omega_grid[19]", "omega_grid[20]", "omega_grid[21]", "omega_grid[22]", "omega_grid[23]", "omega_grid[24]", 
  "omega_grid[25]", "omega_grid[26]", "omega_grid[27]", "omega_grid[28]", "omega_grid[29]", "omega_grid[30]", 
  "omega_grid[31]", "omega_grid[32]", "omega_grid[33]", "omega_grid[34]", "omega_grid[35]", "omega_grid[36]", 
  "omega_grid[37]", "omega_grid[38]", "omega_grid[39]", "omega_grid[40]", "omega_grid[41]", "omega_grid[42]", 
  "omega_grid[43]", "omega_grid[44]", "omega_grid[45]", "omega_grid[46]", "omega_grid[47]", "omega_grid[48]", 
  "omega_grid[49]", "omega_grid[50]", "omega_grid[51]", "omega_grid[52]", "omega_grid[53]", "omega_grid[54]", 
  "omega_grid[55]", "omega_grid[56]", "omega_grid[57]", "omega_grid[58]", "omega_grid[59]", "omega_grid[60]"
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
# nc <- 3  ;  ni <- 150000  ;  nb <- 50000;  nt <- 10  # longer test where some parameters should converge
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
saveRDS(fire_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     model_species, "_fire_elevation_rdnbr_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

# Load the output back in
fire_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       model_species, "_fire_elevation_rdnbr_model.rds"))
  
 # Traceplots and density graphs 
MCMCtrace(object = fire_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_fire_elevation_rdnbr_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = fire_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = fire_mcmc_out$samples,
         excl = c("fit_pa", "fit_pa_new", "fit_pd", "fit_pd_new"),
         guide_lines = TRUE,
         params = sobs_params)

} # End modeling loop (Comment this out) ----

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# 4.1) Prepare data for model output ################################################

# Clear environments again
rm(list = ls())

# Add packages again
library(tidyverse)
library(MCMCvis)
library(gridExtra)
library(ggpubr)
library(grid)

# Add the counts back in (Just using them for the dates so it doesn't matter which species, I like Brewer's Sparrow)
sobs_counts_temp <- read.csv(paste0("Data/Outputs/BRSP_Grid_Counts.csv")) %>%
  tibble() %>% select(-X)

# Add in covariates
covs <- tibble(read.csv("Data/Outputs/grid_covs.csv")) %>%
  dplyr::select(-X) %>% tibble()

# Add some of the data back in so we have the values to unscale parameters

# Change necessary variables to scales and factors
fire_stats <- sobs_counts_temp %>%
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
  # Isolate the burned Grids as their own object so I can accurately scale them
  filter(Burned == 1) %>% 
  distinct(Grid.ID, Years.Since.Fire, rdnbr) 

# View
glimpse(fire_stats)

# Find the mean and standard deviation of the "real" burns
mean_fyear <- mean(fire_stats$Years.Since.Fire)
sd_fyear <- sd(fire_stats$Years.Since.Fire)
mean_rdnbr <- mean(fire_stats$rdnbr)
sd_rdnbr <- sd(fire_stats$rdnbr)

# 4.2) Graph each species response to fire ###############################################################

# List of Species to plot 
all_plot_species <- c(
  "SATH",
  "BRSP",
  "VESP",
  "WEME",
  "HOLA"
)

# Loop over all species 
# for(s in 1:length(all_plot_species)) { # (Comment this out) ----

# Name the species to model again
# plot_species <- all_plot_species[s]
plot_species <- all_plot_species[2]

# Data frame for naming species
plot_species_df <- data.frame(Species.Code = plot_species) %>% 
  mutate(Species.Name = case_when(Species.Code == "SATH" ~ "Sage Thrasher",
                                  Species.Code == "BRSP" ~ "Brewer's Sparrow", 
                                  Species.Code == "SABS" ~ "Sagebrush Sparrow",
                                  Species.Code == "VESP" ~ "Vesper Sparrow",
                                  Species.Code == "WEME" ~ "Western Meadowlark",
                                  Species.Code == "HOLA" ~ "Horned Lark"))
species_name <- plot_species_df$Species.Name
# View
species_name

# Load the output back in
fire_mcmc_out<- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//",
                                       plot_species, "_fire_elevation_rdnbr_model.rds"))

# View MCMC summary
fire_mcmc_out$summary$all.chains

# Extract model fit
fit <- MCMCchains(fire_mcmc_out$samples, params = "fit_pd")
fit_new <- MCMCchains(fire_mcmc_out$samples, params = "fit_pd_new")

# Treatment intercepts
beta0_ref_low <- fire_mcmc_out$summary$all.chains[18,]
beta0_ref_high <- fire_mcmc_out$summary$all.chains[19,]
beta0_burn_low <- fire_mcmc_out$summary$all.chains[20,]
beta0_burn_high <- fire_mcmc_out$summary$all.chains[21,]
# Fire Year
beta_fyear_low <- fire_mcmc_out$summary$all.chains[22,]
beta_fyear_high <- fire_mcmc_out$summary$all.chains[23,]
# Burn severity
beta_rdnbr <- fire_mcmc_out$summary$all.chains[24,]

# View Betas
bind_rows(beta0_ref_low,
          beta0_ref_high,
          beta0_burn_low,
          beta0_burn_high,
          beta_fyear_low,
          beta_fyear_high,
          beta_rdnbr)

# Combine everything into a data frame
beta_dat <- data.frame(bind_rows(beta0_ref_low,
                                 beta0_ref_high,
                                 beta0_burn_low,
                                 beta0_burn_high,
                                 beta_fyear_low,
                                 beta_fyear_high,
                                 beta_rdnbr
                                 )) %>% 
  mutate(Parameter = c("beta0.ref.low", "beta0.ref.high", "beta0.burn.low", "beta0.burn.high",
                       "beta.fyear.low", "beta.fyear.high", "beta.rdnbr"
                       )) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CRI.lb = X95.CI_low,
         CRI.ub = X95.CI_upp)

# View output dataframe
head(beta_dat, n = Inf)

# Predicted values 

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

# Maximum  based on low ref
max_low_ref <- exp(beta0_ref_low[5])
# Maximum based on high ref
max_high_ref <- exp(beta0_ref_high[5])
# Maximum based on low burns
max_low_burn <- exp(beta0_burn_low[5])
# Maximum based on high burns
max_high_burn <- exp(beta0_burn_high[5])
# Maximum based on low time since fire
min_low_fyear <- exp(beta0_burn_low[5] + beta_fyear_low[5]*-2)
max_low_fyear <- exp(beta0_burn_low[5]+ beta_fyear_low[5]*2)
# Maximum based on high time since fire
min_high_fyear <- exp(beta0_burn_high[5] + beta_fyear_high[5]*-2)
max_high_fyear <- exp(beta0_burn_high[5]+ beta_fyear_high[5]*2)
# Maximum based on burn severity at low elevation
min_low_rdnbr <- exp(beta0_burn_low[5] + beta_rdnbr[5]*-2)
max_low_rdnbr <- exp(beta0_burn_low[5]+ beta_rdnbr[5]*2)
# Maximum based on burn severity at high elevation
min_high_rdnbr <- exp(beta0_burn_high[5] + beta_rdnbr[5]*-2)
max_high_rdnbr <- exp(beta0_burn_high[5]+ beta_rdnbr[5]*2)

# Maximum number of possible birds for that species based on the model
max_birds <- max(c(max_low_ref, max_high_ref,
                   max_low_burn, max_high_burn,
                   min_low_fyear, max_low_fyear,
                   min_high_fyear, max_high_fyear,
                   min_low_rdnbr, max_low_rdnbr,
                   min_high_rdnbr, max_high_rdnbr
                   ))
# Or manually (Comment this out)
max_birds <- 120

# See how many birds are possible
max_birds

# Burn verses Reference at different Elevations Plot-----------------------------------------------------------

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
                                "High Elevation Reference",
                                "Low Elevation Burn",
                                "High Elevation Burn"
                                ),
                     name = "") +
  labs(x = "Grid Type", 
       y = paste0(species_name, "s per km", "\u00B2"),
       title = "(A)") +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16, color = "#00000000"),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_text(size = 16)
  )

# Display the plot
treatment_pred_plot

# # Save the plot as a png
# ggsave(plot = treatment_pred_plot,
#        filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
#                          plot_species, "_treatment.png"),
#        width = 200,
#        height = 120,
#        units = "mm",
#        dpi = 300)

# # # save the plot as an RDS
# saveRDS(object = treatment_pred_plot,
#         file = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
#                       plot_species, "_treatment.rds"))

# Years since fire plot -------------------------------------------------------

# Find the minimum time since fire
min_fyear <- fire_stats %>% 
  filter(Years.Since.Fire < 30) %>%
  select(Years.Since.Fire) %>% 
  arrange(Years.Since.Fire) %>% 
  slice_head(n = 1)

# Find the maximum time since fire
max_fyear <- fire_stats %>% 
  filter(Years.Since.Fire < 30) %>%
  select(Years.Since.Fire) %>% 
  arrange(-Years.Since.Fire) %>% 
  slice_head(n = 1)

# Plot predicted responce to time since fire
fyear_pred_plot <- beta_dat_pred %>% 
  mutate(Pred.Naive = Pred * sd_fyear + mean_fyear) %>% 
  # Only show up to where I have data so I am not making forcasts
  filter(Pred.Naive >= min_fyear$Years.Since.Fire & 
         Pred.Naive <= max_fyear$Years.Since.Fire) %>%
  ggplot() +
  # # Low elevation reference
  # geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.low.Mean), 
  #               color = "Low Elevation Reference"), linewidth = 1) +
  # geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.low.CRI.lb), 
  #                 ymax = exp(beta0.ref.low.CRI.ub), 
  #                 fill = "Low Elevation Reference"), alpha = 0.2) +
  # # High elevation reference
  # geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.high.Mean), 
  #               color = "High Elevation Reference"), linewidth = 1) +
  # geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.high.CRI.lb), 
  #                 ymax = exp(beta0.ref.high.CRI.ub), 
  #                 fill = "High Elevation Reference"), alpha = 0.2) +
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
       y = paste0(species_name, "s per km^2"),
       title = "(B)") +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(
    axis.title.x = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.title = element_text(size = 16)
  )

# Display the plot as a png
fyear_pred_plot

# # Save the plot
# ggsave(plot = fyear_pred_plot,
#        filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
#                          plot_species, "_fyear.png"),
#        width = 200,
#        height = 120,
#        units = "mm",
#        dpi = 300)

# # save the plot as an RDS
# saveRDS(object = fyear_pred_plot,
#         file = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
#                       plot_species, "_fyear.rds"))


# burn severity plot -------------------------------------------------------

# Plot predicted responce to time since fire
rdnbr_pred_plot <- beta_dat_pred %>% 
  mutate(Pred.Naive = Pred * sd_rdnbr + mean_rdnbr) %>% 
  # Only show up to where I have data so I am not making forcasts
  filter(Pred.Naive >= 0 & 
         Pred.Naive <= max(fire_stats$rdnbr)) %>%
  ggplot() +
  # # Low elevation reference
  # geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.low.Mean), 
  #               color = "Low Elevation Reference"), linewidth = 1) +
  # geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.low.CRI.lb), 
  #                 ymax = exp(beta0.ref.low.CRI.ub), 
  #                 fill = "Low Elevation Reference"), alpha = 0.2) +
  # # High elevation reference
  # geom_line(aes(x = Pred.Naive, y = exp(beta0.ref.high.Mean), 
  #               color = "High Elevation Reference"), linewidth = 1) +
  # geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.ref.high.CRI.lb), 
  #                 ymax = exp(beta0.ref.high.CRI.ub), 
  #                 fill = "High Elevation Reference"), alpha = 0.2) +
  # Low elevation time since fire
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.low.Mean + beta.rdnbr.Mean * Pred), 
                color = "Low Elevation Burn"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.low.CRI.lb + beta.rdnbr.CRI.lb * Pred), 
                  ymax = exp(beta0.burn.low.CRI.ub + beta.rdnbr.CRI.ub * Pred), 
                  fill = "Low Elevation Burn"), alpha = 0.2) +
  # High elevation time since fire
  geom_line(aes(x = Pred.Naive, y = exp(beta0.burn.high.Mean + beta.rdnbr.Mean * Pred), 
                color = "High Elevation Burn"), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(beta0.burn.high.CRI.lb + beta.rdnbr.CRI.lb * Pred), 
                  ymax = exp(beta0.burn.high.CRI.ub + beta.rdnbr.CRI.ub * Pred), 
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
  labs(x = "RdNBR Burn Severity", 
       y = paste0(species_name, "s per km^2"),
       title = "(C)") +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(
    axis.title.x = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.title = element_text(size = 16)
  )

# Display the plot as a png
rdnbr_pred_plot

# # Save the plot
# ggsave(plot = rdnbr_pred_plot,
#        filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
#                          plot_species, "_rdnbr.png"),
#        width = 200,
#        height = 120,
#        units = "mm",
#        dpi = 300)

# # save the plot as an RDS
# saveRDS(object = rdnbr_pred_plot,
#         file = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_pred_",
#                       plot_species, "_rdnbr.rds"))

# Plot all parameters -------------------------------------------------------------------------

# Create a plot to use the legend 
legend_plot <- beta_dat %>%
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
                       "High Elevation Burn"),name = "") +
  theme_classic() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1, "cm")) +
  # 2x2 legend
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))

# extract the legend
legend <- ggpubr::get_legend(legend_plot)

# Combine all three plots 
full_fire_elv_pred_plot <- grid.arrange(treatment_pred_plot, 
                                        fyear_pred_plot,
                                        rdnbr_pred_plot,
                               nrow = 1, ncol = 3)

# Create a title
plot_title <- textGrob(species_name, 
                       gp = gpar(fontsize = 20, fontface = "bold"))


# Add the legend
full_fire_elv_pred_plot_legend <- grid.arrange(plot_title, 
                                               full_fire_elv_pred_plot, 
                                               legend,
                                               nrow = 3, 
                                               heights = c(0.1, 0.6, 0.1))

# Save the plot as a png
ggsave(plot = full_fire_elv_pred_plot_legend,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\fire_elv_rdnbr_pred_",
                         plot_species, ".png"),
       width = 300,
       height = 175,
       units = "mm",
       dpi = 300)

} # End plotting loop over all species (Comment this out) ----

