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

# 1.1) Read in data ################################################################
# Add in Data from local drive
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# #or from github
# sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_data.csv") %>%
#   dplyr::select(-X) %>%
#   tibble()

#view the data
glimpse(sobs)

# Add covariates from the local drive
# Aspect == -1 means that the area is flat other classes start at 1=NE clockwise
# Fire Distance == 1000000 mean that the area is outside of a fire
# Burn sevarity == 0 means that the area did not burn
# Burn sevarity == -1 means that no data was available 
# Fire year == 1800 means there are no recorded fires in the area
covs <- tibble(read.csv("Data/Outputs/point_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# # or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/grid_covs.csv") %>%
#   dplyr::select(-X) %>%
#   tibble()

# View covariates
glimpse(covs)

# 1.2) Prepare the count level data ################################################################

# Define single species ----
study_species <- "BRSP"

# Define a truncation distance (km)
trunc_dist <- 0.125

# Make a table of important species sightings by visit
counts_0inf <- sobs %>%
  # Switch from m to km
  mutate(Distance = Distance/1000) %>% 
  # Only observations closer than the truncation distance
  filter(Distance <= trunc_dist) %>% 
  # Only one species
  filter(Species  == study_species) %>% 
  # Unque ID for each visit
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>% 
  group_by(Visit.ID) %>% 
  # Number of individuals per visit
  reframe(Visit.ID, Count = n()) %>% 
  distinct()
#...and view
glimpse(counts_0inf)

# View all observers
all_obsv <- sort(unique(sobs$Observer.ID))
all_obsv

# Define the levels of observer experience based on who had conducted previous avain surveys
int_obsv <- c("Aidan", "Anna", "Austin", "Ben", "Devin", "Eliza", "Eoin", "Emily", "Holden", "Keramie", "Thomas")
exp_obsv <- c("Andrew", "Alex", "Ruger", "Rory", "Trey", "Thea", "Will")

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Grid.Type, Year, Ord.Date, MAS, Observer.ID, Year, Visit) %>% 
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
         # Each visit should be treated separately
         Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"),
         Burned = as.numeric(factor(Grid.Type, levels = c("R", "B"))) - 1) %>% 
  # Create a column for Observer experience
  mutate(Observer.Experience = case_when(Observer.ID %in% int_obsv ~ 1,
                                         Observer.ID %in% exp_obsv ~ 2,
                                         TRUE ~ NA)) %>% 
  group_by(Visit.ID) %>% 
  reframe(Visit.ID, Full.Point.ID, Burned, Grid.ID, Year, Observer.ID, Observer.Experience, MAS, Ord.Date) %>% 
  distinct()

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
counts_temp <-  visit_count %>% 
  left_join(counts_0inf, by = c("Visit.ID")) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))

#...and view
glimpse(counts_temp)
print(counts_temp, n = Inf)
sum(counts_temp$Count)      

# Change necessary variables to scales and factors
counts <- counts_temp %>%
  # Add covariates
  left_join(covs, by = c("Full.Point.ID")) %>% 
  # Sort the data
  arrange(Year, Visit.ID, Grid.ID, Full.Point.ID) %>% 
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID"), factor) %>% 
  mutate(Year = str_remove(Year, "Y")) %>% 
  mutate(Year = factor(Year, levels = c(1, 2, 3)),
         Grid.ID = factor(Grid.ID)) %>% 
  # Scale all covariates
  mutate(
    Shrub.Cover = scale(Shrub.Cover)[,1],
    Perennial.Cover = scale(Perennial.Cover)[,1],
    TRI = scale(TRI)[,1],
    Trees.Present = Trees.Present, 
    MAS = scale(MAS)[,1],   
    Ord.Date = scale(Ord.Date)[,1]
    # UTM.X = scale(UTM.X)[,1],
    # UTM.Y = scale(UTM.Y)[,1]
  ) %>% 
  # Create numeric versions of factors
  mutate(Visit.ID.num = as.numeric(Visit.ID),
         Grid.ID.num = as.numeric(Grid.ID),
         Year.num = as.numeric(Year),
         Observer.ID.num = as.numeric(Observer.ID)) %>% 
  # Reorder the columns and remove columns that are no longer needed
  dplyr::select(Visit.ID, Visit.ID.num, Grid.ID, Grid.ID.num, Year, Year.num, Observer.ID, Observer.ID.num,  
                Observer.Experience, Count, MAS, Ord.Date, Shrub.Cover, Perennial.Cover, TRI, Trees.Present, Burned) 
#...and view
glimpse(counts)

# 1.3) Prepare the observation level data ################################################################

# Define a bin size
bin_size <- 0.025

# Define the number of bins
nbins <- 5

# Define the bins
bins <- seq(from = bin_size, to = bin_size * nbins, by = bin_size)

# View the bins
bins

# Create an object with all observations of a single species for the detection function
observations_temp <- sobs %>% 
  # Switch from m to km
  mutate(Distance = Distance / 1000,
         Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>%  
  # Only one species
  filter(Species == study_species) %>%   
  # Same order as the counts
  arrange(Year, Visit, Grid.ID) %>%
  # Only close observations
  filter(Distance <= trunc_dist) %>%          
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Ben", TRUE ~ Observer.ID)) %>%
  left_join(covs, by = "Full.Point.ID") %>% 
  # Make year a factor
  mutate(Year = str_remove(Year, "Y")) %>% 
  mutate(Year = factor(Year, levels = c(1, 2, 3))) %>% 
  # Calculate Distance Bins
  mutate(Dist.Bin = case_when(Distance >= 0 & Distance <= bins[1] ~ bins[1],
                              Distance > bins[1] & Distance <= bins[2] ~ bins[2],
                              Distance > bins[2] & Distance <= bins[3] ~ bins[3],
                              Distance > bins[3] & Distance <= bins[4] ~ bins[4],
                              Distance > bins[4] & Distance <= bins[5] ~ bins[5],
                              TRUE ~ NA)) %>% 
  # Calculate Midpoints
  mutate(Dist.Bin.Midpoint = Dist.Bin - bin_size/2) %>% 
  # Change to a factor
  mutate(Dist.Bin = case_when(Dist.Bin == bins[1] ~ 1,
                              Dist.Bin == bins[2] ~ 2,
                              Dist.Bin == bins[3] ~ 3,
                              Dist.Bin == bins[4] ~ 4,
                              Dist.Bin == bins[5] ~ 5,
                              TRUE ~ NA)) %>% 
  # Asign each observation to a two minute time interval
  mutate(Time.Interval = case_when(Minute %in% c(1, 2) ~ 1,
                                   Minute %in% c(3, 4) ~ 2,
                                   Minute %in% c(5, 6) ~ 3,
                                   TRUE ~ NA)) %>% 
  # Create a column for Observer experience
  mutate(Observer.Experience = case_when(Observer.ID %in% int_obsv ~ 1,
                                         Observer.ID %in% exp_obsv ~ 2,
                                         TRUE ~ NA)) %>% 
  # Reorder the columns
  dplyr::select(Visit.ID, Full.Point.ID, Grid.ID, Observer.ID, Observer.Experience,
                Distance, Dist.Bin, Dist.Bin.Midpoint, Time.Interval, Year)

# View the whole object
glimpse(observations_temp)
sort(unique(observations_temp$Dist.Bin))
sort(unique(observations_temp$Dist.Bin.Midpoint))
sort(unique(observations_temp$Time.Interval))
sort(unique(observations_temp$Observer.Experience))

# Pull out the covariates that need to be shared across counts and observations
point_ids <- counts %>% 
  mutate_at(c("Visit.ID"), as.character) %>% 
  dplyr::select(Visit.ID, Visit.ID.num, Grid.ID.num, Observer.ID.num)
#...and view
glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
observations <- observations_temp %>% 
  left_join(point_ids, by = c("Visit.ID"))

# View Counts
glimpse(counts)

# View Observations
glimpse(observations)

# Plot counts against a covaariate
counts %>%
  # filter(Count > 0) %>%
  ggplot(aes(x = Perennial.Cover, y = Count)) +
  # geom_boxplot(col = "aquamarine4", fill = "aquamarine3") +
  geom_smooth(col = "aquamarine4", fill = "aquamarine3",
              method = "glm", method.args = list(family = "quasipoisson")) +
  geom_jitter(col = "aquamarine4", alpha = 0.14) +
  ylim(0, 10) +
  theme_bw()

# Plot detection frequency by distance for each observer to insure that the data is appropriate for distance sampling
observations %>%
  # filter(!Observer.ID %in% c("Rory", "Will")) %>% 
  group_by(Visit.ID, 
           # Observer.ID,
           Observer.Experience,
           Dist.Bin, Dist.Bin.Midpoint) %>%
  reframe(Visit.ID, 
          Observer.ID,
          Observer.Experience,
          Dist.Bin, Dist.Bin.Midpoint, bin.count = n()) %>%
  distinct() %>%
  ggplot(aes(x = Dist.Bin, y = bin.count)) +
  geom_col(fill = "lightblue") +
  theme_bw() +
  # facet_wrap(~ Observer.ID)
  facet_wrap(~ Observer.Experience)

# 1.4) prepare objects for NIMBLE ################################################################

# Loop sizes 
npoints <- length(unique(counts$Visit.ID.num))        # Number of points visited
nind <- nrow(observations)                            # Number of individuals detected 
# nobsv <- length(unique(counts$Observer.ID.num))       # Number of unique observers
nexp <- length(unique(counts$Observer.Experience))    # Number of observer experience levels
nbins <- length(unique(observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(observations$Time.Interval))   # Number of time intervals
nyears <- length(unique(counts$Year.num))             # Number of years we surveyed
ngrids <- length(unique(counts$Grid.ID.num))          # Number of survey grids

# Observation Level data 
midpt <- sort(unique(observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
# observers <- counts$Observer.ID.num                 # Random effect for observer associated with each survey
obsv_exp <- counts$Observer.Experience                # Observer experience  
obs_point <- observations$Visit.ID.num                # In which grid did each observation take place
dclass <- observations$Dist.Bin                       # Distance class of each observation
delta <- bin_size                                     # Bin width

# Availability date
tint <- observations$Time.Interval                    # Time interval for each observation 
time <- counts$MAS                                    # Matrix of scaled time after sunrise
date <- counts$Ord.Date                               # Matrix of scaled dates

# Grid level data
n_dct <- counts$Count                                 # Matrix of the number of detected individuals per grid per survey 
years <- counts$Year.num                              # Matrix of year numbers
shrub_cvr <- counts$Shrub.Cover                       # Percent shrub cover
pfg_cvr <- counts$Perennial.Cover                     # Percent perennial forb and grass cover
tri <- counts$TRI                                     # Topographic ruggedness
trees <- as.numeric(counts$Trees.Present)             # Whether or not trees are present in the grid 
grids <- counts$Grid.ID.num                           # On which grid did the count take place

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
  gamma0 ~ dnorm(0, sd = 3)         # Mean availability
  gamma_date ~ dnorm(0, sd = 2)     # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 2)    # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 2)     # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 2)    # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model
  alpha0 ~ dnorm(0, sd = 3)            # Intercept on detectability
  for(o in 1:nexp){
    alpha_obsv[o] ~ dnorm(0, sd = 2)   # Effect of observer experience on detecability
  }

  # Parameters on the abundance component of the model
  mean_beta0 ~ dnorm(0, sd = 3)  # Mean abundance hyperparameter
  sd_beta0 ~ dunif(0, 1)         # Sd in yearly abundance hyperparameter
  # Random intercept on abundance
  for(t in 1:ngrids){
    beta0_grid[t] ~ dnorm(mean_beta0, sd_beta0)
  }
  # Fixed effects on abundance 
  beta_shrub ~ dnorm(0, sd = 2)     # Effect of shrub cover
  beta_pfg ~ dnorm(0, sd = 2)       # Effect of Perennial Cover
  beta_pfg2 ~ dnorm(0, sd = 2)      # Effect of Perennial Cover (quadratic)
  beta_tri ~ dnorm(0, sd = 2)       # Effect of ruggedness
  beta_tri2 ~ dnorm(0, sd = 2)      # Effect of ruggedness (quadratic)
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood
  
  # Iterate over all points
  for(s in 1:npoints){
    
    # Zero-inflation component on abundance
    psi[s] ~ dunif(0.0001, 0.9999)                                    # Occupancy probability
    present[s] ~ dbern(psi[s])                                        # Number of grids where that individual can be present
      
    # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, b]) <- -(midpt[b]^2) / (2 * sigma[s]^2)  # Half-normal detection function
        f[s, b] <- (2 * midpt[b] * delta) / trunc_dist^2  # Prob density function out to max truncation distance 
        pi_pd[s, b] <- g[s, b] * f[s, b]                  # Detection cell probability
        pi_pd_c[s, b] <- f[s, b] / p_dct[s]               # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_dct[s] <- sum(pi_pd[s, ])
      
      # Construction of the cell probabilities for the nints time intervals
      for (k in 1:nints){
        pi_pa[s, k] <- phi[s] * (1 - phi[s])^(k - 1)      # Availability cell probability
        pi_pa_c[s, k] <- pi_pa[s, k] / p_avail[s]         # Proportion of availability probability in each time interval class
      }   
      
      # Rectangular integral approx. of integral that yields the Pr(available)
      p_avail[s] <- sum(pi_pa[s, ])
      
      # Multiply availability with capture probability to yield total marginal probability of capturing a bird
      p_cap[s] <- p_dct[s] * p_avail[s]
      
      # Binomial portion of mixture 
      n_dct[s] ~ dbin(p_cap[s], N_indv[s])  # Number of individual birds captured (observations)
      
      # Poisson abundance portion of mixture
      N_indv[s] ~ dpois(lambda[s])          # True abundance at point s
      # * (present[s] + 0.0001) # ZIP true abundance at point s
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s]) <- alpha0 +                    # Intercept on detecability
                       alpha_obsv[obsv_exp[s]]     # Effect of each observer on detectability
      
      
      # Availability (phi) Log-linear model for availability
      logit(phi[s]) <- gamma0 +                     # Intercept on availability 
                       gamma_date * date[s] +       # Effect of scaled ordinal date 
                       gamma_date2 * date[s]^2 +    # Effect of scaled ordinal date squared 
                       gamma_time * time[s] +       # Effect of scaled time of day 
                       gamma_time2 * time[s]^2      # Effect of scaled time of day squared 
      
      # Abundance (lambda) Log-linear model 
      log(lambda[s]) <- beta0_grid[grids[s]] +      # Intercept on abundance
                        beta_shrub * shrub_cvr[s] + # Effect of shrub cover
                        beta_pfg * pfg_cvr[s] +     # Effect of Perennial Cove
                        beta_pfg2 & pfg_cvr[s]^2 +  # Effect off perennial cover squared
                        beta_tri * tri[s] +         # Effect of ruggedness
                        beta_tri2 * tri[s]^2        # Effect of ruggedness squared

      # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
      # Compute fit statistic for observed data
      e_val[s] <- p_cap[s] * N_indv[s]             # Expected value for binomial portion of the model
      FT[s] <- (sqrt(n_dct[s]) - sqrt(e_val[s]))^2

      # Generate replicate count data and compute same fit stats for them
      n_dct_new[s] ~ dbin(p_cap[s], N_indv[s])
      FT_new[s] <- (sqrt(n_dct_new[s]) - sqrt(e_val[s]))^2
    
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_point[i], ])      # Linking distance class data
    tint[i] ~ dcat(pi_pa_c[obs_point[i], ])        # Linking time removal data
  } # end observation loop

  # Add up fit stats across sites and visits
  fit <- sum(FT[])
  fit_new <- sum(FT_new[])

  # c-hat value, should converge to ~1
  c_hat <- fit / fit_new

  # Compute Bayesian p-value for the model, should converge to ~0.5
  bpv <- step(fit_new - fit)
  
})

# 2.2) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
sobs_const <- list (
  # Misc. Constants
  delta = delta,           # Bin width
  trunc_dist = trunc_dist, # Truncation distance
  
  # For loop sizes
  npoints = npoints,       # Number of survey grids
  nind = nind,             # Number of individuals detected 
  # nobsv = nobsv,           # Number of unique observers
  nexp = nexp,             # Number of experience levels
  nbins = nbins,           # Number of distance bins
  nints = nints,           # Number of time intervals
  ngrids = ngrids,         # Number of survey grids (transects)
  # nyears = nyears,         # Number of years we surveyed
  
  # Non-stochastic constants
  # observers = observers,   # Effect of observer associated with each survey
  obsv_exp = obsv_exp,     # Observer experience associated with each point
  midpt = midpt,           # Midpoints of distance bins
  grids = grids,           # Grid where each point count took place  
  # years = years,           # Year when each survey took place
  obs_point  = obs_point   # Point associated with each observation 
  
)
# View Nimble constants 
str(sobs_const)

# Data to be fed into Nimble 
sobs_dat <- list(
  # Observation Level data
  dclass = dclass,         # Distance category for each observation
  tint = tint,             # Time interval for each observation
  
  # Availability data
  time = time,             # Scaled mean time after sunrise 
  date = date,             # Scaled date
  
  #Point level data
  n_dct = n_dct,           # Number of detected individuals per site
  shrub_cvr = shrub_cvr,   # Percent shrub cover
  pfg_cvr = pfg_cvr,       # Percent perennial forb and grass cover
  tri = tri                # Topographic ruggedness
)
# View Nimble data 
str(sobs_dat)

#Object dimensions
sobs_dims <- list(
  g = c(npoints, nbins),        # Cell prob x radial distance
  f = c(npoints, nbins),        # Cell prob x radial distance
  sigma = npoints,              # Linear combination on detectability
  pi_pd = c(npoints, nbins),    # Detection probability in each cell
  pi_pd_c = c(npoints, nbins),  # Proportion of total detection probability in each cell
  p_dct = npoints,              # Detection probability 
  phi = npoints,                # Linear combination on avalibility  
  pi_pa =  c(npoints, nints),   # Availability cell prob in each time interval
  pi_pa_c = c(npoints, nints),  # Proportion of total availability probability in each cell
  p_avail = npoints,            # Availability probability
  lambda = npoints,             # Poisson random variable
  e_val = npoints,              # Expected value for binomial portion of the model
  FT = npoints,                 # Freeman Tukey Discrepancy
  FT_new = npoints              # New Freeman Tukey Discrepancy
)
# View dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detecability 
  alpha0 = rnorm(1, 0, 0.1),
  alpha_obsv = rnorm(nexp, 0, 0.1),
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),
  gamma_date = rnorm(1, 0, 0.1), 
  gamma_time = rnorm(1, 0, 0.1),
  gamma_date2 = rnorm(1, 0, 0.1),  
  gamma_time2 = rnorm(1, 0, 0.1),
  # Abundance 
  mean_beta0 = runif(1, 0, 1),
  sd_beta0 = runif(1, 0, 0.1), 
  # beta0_year = runif(nyears, 0, 1)     # Random effects seem to be happier when I start them all at positive values
  beta0_grid = runif(ngrids, 0, 1),    # Random effects seem to be happier when I start them all at positive values
  beta_shrub = rnorm(1, 0, 0.1),
  beta_pfg = rnorm(1, 0, 0.1),
  beta_pfg2 = rnorm(1, 0, 0.1),
  beta_tri = rnorm(1, 0, 0.1),
  beta_tri2 = rnorm(1, 0, 0.1),
  # Presence 
  psi = runif(npoints, 0.4, 0.6),
  present = rbinom(npoints, 1, 0.5),
  # Simulated data
  n_dct_new = n_dct,              # Initialize the new capture data at the existing data values
  N_indv = n_dct + 1              # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
                 "mean_beta0",
                 "sd_beta0",
                 "beta_shrub",
                 "beta_pfg",
                 "beta_pfg2",
                 "beta_tri",
                 "beta_tri2",
                 "gamma0", 
                 "gamma_date",
                 "gamma_date2",
                 "gamma_time",
                 "gamma_time2",
                 "alpha0",
                 "alpha_obsv",
                 "fit",
                 "fit_new",
                 "c_hat",
                 "bpv"
                 )

# 2.3) Configure and Run the model ###########################################################

# Build the nimble model
sobs_model_vect <- nimbleModel(sobs_model_code, 
                               data = sobs_dat,
                               constants = sobs_const,
                               dimensions = sobs_dims,
                               inits = sobs_inits)

# # MCMC settings for default nimble model setup
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

# NOTE, these will always have a warning message. Just ignore it
# Block all availability (gamma) nodes together
sobs_mcmcConf$removeSamplers("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2")

sobs_mcmcConf$addSampler(target = c("gamma0", "gamma_date", "gamma_time", "gamma_date2", "gamma_time2"),
                         type = 'RW_block')

# Block all detection (alpha) nodes together
# sobs_mcmcConf$removeSamplers("alpha0", "alpha_obsv[1]", "alpha_obsv[2]", "alpha_obsv[3]", "alpha_obsv[4]", 
#                              "alpha_obsv[5]", "alpha_obsv[6]", "alpha_obsv[7]", "alpha_obsv[8]",
#                              "alpha_obsv[9]", "alpha_obsv[10]", "alpha_obsv[11]", "alpha_obsv[12]", 
#                              "alpha_obsv[13]", "alpha_obsv[14]", "alpha_obsv[15]", "alpha_obsv[16]", 
#                              "alpha_obsv[17]")
# 
# sobs_mcmcConf$addSampler(target = c("alpha0", "alpha_obsv[1]", "alpha_obsv[2]", "alpha_obsv[3]", "alpha_obsv[4]", 
#                                     "alpha_obsv[5]", "alpha_obsv[6]", "alpha_obsv[7]", "alpha_obsv[8]",
#                                     "alpha_obsv[9]", "alpha_obsv[10]", "alpha_obsv[11]", "alpha_obsv[12]", 
#                                     "alpha_obsv[13]", "alpha_obsv[14]", "alpha_obsv[15]", "alpha_obsv[16]", 
#                                     "alpha_obsv[17]"),
#                          type = 'RW_block')

# Block all detection (alpha) nodes together
sobs_mcmcConf$removeSamplers("alpha0", "alpha_obsv[1]", "alpha_obsv[2]")

sobs_mcmcConf$addSampler(target = c("alpha0", "alpha_obsv[1]", "alpha_obsv[2]"),
                         type = 'RW_block')

# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers(# Random effects
                             "beta0_grid[1]", "beta0_grid[2]", "beta0_grid[3]", "beta0_grid[4]", "beta0_grid[5]", 
                             "beta0_grid[6]", "beta0_grid[7]", "beta0_grid[8]", "beta0_grid[9]", "beta0_grid[10]", 
                             "beta0_grid[11]", "beta0_grid[12]", "beta0_grid[13]", "beta0_grid[14]", "beta0_grid[15]", 
                             "beta0_grid[16]", "beta0_grid[17]", "beta0_grid[18]", "beta0_grid[19]", "beta0_grid[20]", 
                             "beta0_grid[21]", "beta0_grid[22]", "beta0_grid[23]", "beta0_grid[24]", "beta0_grid[25]", 
                             "beta0_grid[26]", "beta0_grid[27]", "beta0_grid[28]", "beta0_grid[29]", "beta0_grid[30]", 
                             "beta0_grid[31]", "beta0_grid[32]", "beta0_grid[33]", "beta0_grid[34]", "beta0_grid[35]", 
                             "beta0_grid[36]", "beta0_grid[37]", "beta0_grid[38]", "beta0_grid[39]", "beta0_grid[40]", 
                             "beta0_grid[41]", "beta0_grid[42]", "beta0_grid[43]", "beta0_grid[44]", "beta0_grid[45]", 
                             "beta0_grid[46]", "beta0_grid[47]", "beta0_grid[48]", "beta0_grid[49]", "beta0_grid[50]", 
                             "beta0_grid[51]", "beta0_grid[52]", "beta0_grid[53]", "beta0_grid[54]", "beta0_grid[55]", 
                             "beta0_grid[56]", "beta0_grid[57]", "beta0_grid[58]", "beta0_grid[59]", "beta0_grid[60]",
                             # Fixed effects
                             "beta_shrub", "beta_pfg", "beta_pfg2", "beta_tri", "beta_tri2")

sobs_mcmcConf$addSampler(target = c(# Random effects
                                    "beta0_grid[1]", "beta0_grid[2]", "beta0_grid[3]", "beta0_grid[4]", "beta0_grid[5]", 
                                    "beta0_grid[6]", "beta0_grid[7]", "beta0_grid[8]", "beta0_grid[9]", "beta0_grid[10]", 
                                    "beta0_grid[11]", "beta0_grid[12]", "beta0_grid[13]", "beta0_grid[14]", "beta0_grid[15]", 
                                    "beta0_grid[16]", "beta0_grid[17]", "beta0_grid[18]", "beta0_grid[19]", "beta0_grid[20]", 
                                    "beta0_grid[21]", "beta0_grid[22]", "beta0_grid[23]", "beta0_grid[24]", "beta0_grid[25]", 
                                    "beta0_grid[26]", "beta0_grid[27]", "beta0_grid[28]", "beta0_grid[29]", "beta0_grid[30]", 
                                    "beta0_grid[31]", "beta0_grid[32]", "beta0_grid[33]", "beta0_grid[34]", "beta0_grid[35]", 
                                    "beta0_grid[36]", "beta0_grid[37]", "beta0_grid[38]", "beta0_grid[39]", "beta0_grid[40]", 
                                    "beta0_grid[41]", "beta0_grid[42]", "beta0_grid[43]", "beta0_grid[44]", "beta0_grid[45]", 
                                    "beta0_grid[46]", "beta0_grid[47]", "beta0_grid[48]", "beta0_grid[49]", "beta0_grid[50]", 
                                    "beta0_grid[51]", "beta0_grid[52]", "beta0_grid[53]", "beta0_grid[54]", "beta0_grid[55]", 
                                    "beta0_grid[56]", "beta0_grid[57]", "beta0_grid[58]", "beta0_grid[59]", "beta0_grid[60]",
                                    # Fixed effects
                                    "beta_shrub", "beta_pfg", "beta_pfg2", "beta_tri", "beta_tri2"),
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
nc <- 3  ;  ni <- 200000  ;  nb <- 150000;  nt <- 10    # longer test where most parameters should
# nc <- 3;  ni <- 500000;  nb <- 250000;  nt <- 25        # Run the model for real

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
saveRDS(sobs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     study_species, "_predictive_model_out.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output #######################################################

# Pick a species and scale
study_species <- "BRSP"

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       study_species, "_predictive_model_out.rds"))
  
# Traceplots and density graphs
MCMCtrace(object = sobs_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(study_species, "_predictive_model_out.rds"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = sobs_mcmc_out$samples,
            params = sobs_params,
            round = 2)

# Perameters for the MCMC plot
plot_params <- c(
  # "beta0",
  "beta_shrub", 
  "beta_pfg", 
  "beta_pfg2",
  "beta_tri",
  "beta_tri2"
  )


# View MCMC plot
MCMCplot(object = sobs_mcmc_out$samples,
         ref_ovl = TRUE,
         col = "black",
         params = plot_params)

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# View MCMC summary
sobs_mcmc_out$summary$all.chains

# 4.1) Extract coefficient values ###############################################################

# Extract Beta0 values
# beta0_year1 <- sobs_mcmc_out$summary$all.chains[4,1]
# beta0_year2 <- sobs_mcmc_out$summary$all.chains[5,1]
# beta0_year3 <- sobs_mcmc_out$summary$all.chains[6,1]
# beta0 <- mean(c(beta0_year1, beta0_year2, beta0_year3))
beta0 <- sobs_mcmc_out$summary$all.chains[20,1]
# View intercepts
# c(beta0_year1, beta0_year2, beta0_year3)
beta0

# Extract effect sizes
beta_shrub <- sobs_mcmc_out$summary$all.chains[8,1]
beta_pfg <- sobs_mcmc_out$summary$all.chains[7,1]
beta_pfg2 <- sobs_mcmc_out$summary$all.chains[,1]
beta_tri <- sobs_mcmc_out$summary$all.chains[10,1]
beta_tri2 <- sobs_mcmc_out$summary$all.chains[,1]
# View Betas
c(beta_shrub, beta_pfg, beta_pfg2, beta_tri, beta_tri2)

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0,
                                 beta_shrub, 
                                 beta_pfg,
                                 beta_pfg2,
                                 beta_tri, 
                                 beta_tri2)) %>% 
  mutate(Parameter = c("Beta0",
                       "Beta.Shrub", 
                       "Beta.PFG", 
                       "Beta.PFG2",
                       "Beta.TRI", 
                       "Beta.TRI2")) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CI.lb = X95.CI_low,
         CI.ub = X95.CI_upp)

# View output dataframe
glimpse(beta_dat)
head(beta_dat, n = Inf)

# Pick a number of samples
n <- 1000

# Rearrange the columns that I need
beta_dat_long <- beta_dat %>%
  pivot_longer(cols = c(Mean, CI.lb, CI.ub),
               names_to = "Statistic",
               values_to = "Value") %>% 
  mutate(ColumnName = paste(Parameter, Statistic, sep = ".")) %>%
  select(ColumnName, Value)

# Then pivot to the desired format
beta_dat_pred <- beta_dat_long %>%
  pivot_wider(names_from = ColumnName,
              values_from = Value) %>%
  # Repeat each sample n times
  slice(rep(1:n(), each = n)) %>% 
  # Add in the predictive values 
  mutate(Pred = seq(from = -3, to = 3, length.out = n))

# View the new data 
glimpse(beta_dat_pred)

# Plot a specific predictor
pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the predicted response to shrub cover for all years
    Shrub.Pred.Trend =  exp(Beta0.Mean + Beta.Shrub.Mean * Pred),
    Shrub.Pred.lb = exp(Beta0.lb + Beta.Shrub.CI.lb * Pred),
    Shrub.Pred.ub =  exp(Beta0.ub +  Beta.Shrub.CI.ub * Pred),
    # Transform shrub cover back to the native scale
    Pred.Naive = Pred * shrub_cvr_sd + shrub_cvr_mean
  ) %>% 
  ggplot() +
  geom_line(aes(x = Pred.Naive, y = Shrub.Pred.Trend), color = "blue", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = Shrub.Pred.lb, ymax = Shrub.Pred.ub), 
              alpha = 0.2, fill = "blue") +  
  labs(title = "", x = "Percent Shrub Cover", y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  ylim(0, 200) +
  xlim(0, 40) +
  theme_classic()

# Display the plot
pred_plot


# 4.2) Prepare rasters ##########################################################################

# Add spatial data packages
library(terra)
library(sf)
library(tmap)
library(viridisLite)

# Set a coordinate reference system (UTM Zone 12N)
target_crs <- "EPSG:32612"

# Path to rasters 
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add in raster layers
shrub_rast <- rast(paste0(ras_path, "shrub_cvr.tif"))
pfg_rast <- rast(paste0(ras_path, "pfg_cvr.tif"))
tri_rast <- rast(paste0(ras_path, "tri.tif"))

# Add in SF layers
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))
fires <- st_read(paste0(ras_path, "fire_perimeters_st_rg.shp"))
treatments <- st_read(paste0(ras_path, "treatments.shp"))

# Project sf layers
study_region_prj <- st_transform(study_region, target_crs)
treatments_prj <- st_transform(treatments, target_crs)

# Resample TRI to have the same cell size and extent as the other rasters
tri_rast_res <- resample(tri_rast, shrub_rast, method = "bilinear")

# Find the area around each point where birds were counted 
area <- pi * (trunc_dist * 1000)^2
area

# Take the square root of that area to find out resolution the rasters should use
resol <- sqrt(area)
resol

# Find out what multiplication factor we should to aggregate the rasters
agg_fact <- round(resol / res(shrub_rast)[1])
agg_fact

# Change the resolution to reflect that area 
shrub_rast_agg <- aggregate(shrub_rast, fact = agg_fact, fun = "mean")
pfg_rast_agg <- aggregate(pfg_rast, fact = agg_fact, fun = "mean")
tri_rast_agg <- aggregate(tri_rast_res, fact = agg_fact, fun = "mean")
tree_rast_agg <- aggregate(tree_rast, fact = agg_fact, fun = "sum")

# Reclassify the tree raster to be binary
tree_rast_agg[tree_rast_agg >= 1] <- 1

# Factors to scale covarates using the original model scales
shrub_mean <- mean(covs$Shrub.Cover)
shrub_sd <- sd(covs$Shrub.Cover)
pfg_mean <- mean(covs$Perennial.Cover)
pfg_sd <- sd(covs$Perennial.Cover)
tri_mean <- mean(covs$TRI)
tri_sd <- sd(covs$TRI)

# Function to scale raster
scale_rast <- function(rast, mu, sigma){
  scaled_rast <- (rast - mu) / sigma
  return(scaled_rast)
}

# Scale each raster
shrub_rast_scl <- scale_rast(rast = shrub_rast_agg, mu = shrub_mean, sigma = shrub_sd)
pfg_rast_scl <- scale_rast(rast = pfg_rast_agg, mu = pfg_mean, sigma = pfg_sd)
tri_rast_scl <- scale_rast(rast = tri_rast_agg, mu = tri_mean, sigma = tri_sd)

# Empty raster template for intercept value based on shrub cover raster
beta0_rast <- rast(extent = ext(shrub_rast_scl), 
                   resolution = res(shrub_rast_scl), 
                   crs = crs(shrub_rast_scl))

# Assign that raster the value of the model intercept
values(beta0_rast) <- beta0

# Create raster for each covariate multiplied by the value of the corresponding scaled raster
beta_shrub_rast <- beta_shrub * shrub_rast_scl
beta_pfg_rast <- beta_pfg * pfg_rast_scl
beta_pfg2_rast <- beta_pfg * pfg_rast_scl^2
beta_tri_rast <- beta_tri * tri_rast_scl
beta_tri2_rast <- beta_tri2 ** tri_rast_scl^2

# Combine all predictive layers
pred_rast_ln <- sum(c(beta0_rast, beta_shrub_rast, beta_pfg_rast, beta_pfg2_rast, beta_tri_rast, beta_tri2_rast))

# Exponentiate the predictive raster
pred_rast_lg <- exp(pred_rast_ln)

# Devide by the number of hectares in a cell
pred_rast_hect <- pred_rast_lg / (area * 0.0001)

# Clip to the study region
pred_rast <- mask(pred_rast_lg , study_region_prj)

# Make a pallete
palette <- viridis(5)

# Pick how to view the raster
tmap_mode("view")
# tmap_mode("plot")

# View the predictive raster
tm_shape(pred_rast) +
  tm_raster(
    title = "Predicted Abundance (birds/ha)",
    style = "fisher",                       
    palette = palette
  ) +
  tm_layout(main.title = "",
            main.title.size = 1.5,
            legend.outside = TRUE,                
            frame = FALSE) +
  tm_compass(type = "arrow",                       
             position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 10, 20),                
               position = c("right", "bottom"))

# Save the raster
writeRaster(pred_rast,
            paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Model_Files\\pred_rast_", 
            study_species, ".tif"),
            overwrite = TRUE)

