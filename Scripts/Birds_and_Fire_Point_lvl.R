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
study_species <- "HOLA"

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

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Year, Ord.Date, MAS, Observer.ID, Year, Visit) %>% 
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
          # Each visit should be treated separately
           Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>% 
  group_by(Visit.ID) %>% 
  reframe(Visit.ID, Full.Point.ID, Grid.ID,Year, Observer.ID,  MAS, Ord.Date) %>% 
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
counts_temp2 <- counts_temp %>%
  # Add covariates
  left_join(covs, by = c("Full.Point.ID")) %>% 
  # Sort the data
  arrange(Visit.ID, Full.Point.ID) %>% 
  # Calculate time since Fire
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>%
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID"), factor) %>% 
  mutate(Year = str_remove(Year, "Y")) %>% 
  mutate(Year = factor(Year, levels = c(1, 2, 3)))

#...and view
glimpse(counts_temp2)

# Isolate the burned Grids as their own object so I can accurately scale them
fire_stats <- counts_temp2 %>% 
  filter(Burned == 1) %>% 
  dplyr::select(Grid.ID, Years.Since.Fire, mean.rdnbr) 

# Find the mean and standard deviation of the "real" burns
mean_burnY <- mean(fire_stats$Years.Since.Fire)
sd_burnY <- sd(fire_stats$Years.Since.Fire)
mean_rdnbr <- mean(fire_stats$mean.rdnbr)
sd_rdnbr <- sd(fire_stats$mean.rdnbr)

# Scale the other covariates
counts <- counts_temp2 %>% 
  mutate(Elevation = scale(Elevation)[,1],
         MAS = scale(MAS)[,1],
         Ord.Date = scale(Ord.Date)[,1],
         Years.Since.Fire = (Years.Since.Fire - mean_burnY) / sd_burnY,
         mean.rdnbr = (mean.rdnbr - mean_rdnbr) / sd_rdnbr,
         X.scl = scale(UTM.X)[,1],
         Y.scl = scale(UTM.Y)[,1])
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
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID)) %>% 
  dplyr::select(Distance, Minute, Visit.ID, Full.Point.ID, Grid.ID, Observer.ID, Year, Visit.ID) %>% 
  left_join(covs, by = "Full.Point.ID") %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
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
                                   Minute %in% c(5, 6) ~ 3)) %>% 
  dplyr::select(Visit.ID, Full.Point.ID, Grid.ID, Observer.ID, Distance, Dist.Bin, Dist.Bin.Midpoint, Time.Interval, Year)

# View the whole object
glimpse(observations_temp)
sort(unique(observations_temp$Dist.Bin))
sort(unique(observations_temp$Dist.Bin.Midpoint))
sort(unique(observations_temp$Time.Interval))

# Make sure the same numeric values for factors are shaped between the two datasets
counts <- counts %>% 
  arrange(Year, Visit.ID,  Grid.ID) %>% 
  mutate(Grid.ID.num = as.numeric(Grid.ID),
         Year.num = as.numeric(Year),
         Visit.ID.num = as.numeric(Visit.ID),
         Observer.ID.num = as.numeric(Observer.ID)) 

# Pull out the covariates that need to be shared across counts and observations
point_ids <- counts %>% 
  mutate_at(c("Grid.ID", "Visit.ID"), as.character) %>% 
  dplyr::select(Grid.ID, Grid.ID.num, Visit.ID, Visit.ID.num, Observer.ID.num)
#...and view
glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
observations <- observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Visit.ID"))

# View Counts
glimpse(counts)

# View Observations
glimpse(observations)

# Plot a specific covariate against bird abundance
# counts %>%
#   ggplot(aes(x = Sage.Cover, y = Count)) +
#   # geom_boxplot()
#   geom_smooth(method = "lm", col = "aquamarine4", fill = "aquamarine3") +
#   geom_jitter(col = "aquamarine4") +
#   theme_bw()

# # Plot detection frequency by distance for each observer to insurre that the data is appropriate for distance sampling
# observations %>%
#   group_by(Grid.ID, Observer.ID, Dist.Bin, Dist.Bin.Midpoint) %>%
#   reframe(Grid.ID, Observer.ID, Dist.Bin, , Dist.Bin.Midpoint, bin.count = n()) %>%
#   distinct() %>%
#   group_by(Observer.ID, Grid.ID) %>%
#   reframe(Grid.ID, Observer.ID, Dist.Bin, Dist.Bin.Midpoint, bin.count, total.count = sum(bin.count)) %>%
#   mutate(scl.bin.count = bin.count / (pi*(2000*Dist.Bin.Midpoint)^2)) %>%
#   ggplot(aes(x = Dist.Bin, y = scl.bin.count / total.count)) +
#   geom_col(fill = "lightblue") +
#   theme_bw() +
#   facet_wrap(~ Observer.ID)

# 1.4) prepare objects for NIMBLE ################################################################

# Loop sizes 
npoints <- length(unique(counts$Visit.ID.num))        # Number of survey grids
nind <- nrow(observations)                            # Number of individuals detected 
nobsv <- length(unique(counts$Observer.ID.num))       # Number of unique observers
nbins <- length(unique(observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(observations$Time.Interval))   # Number of time intervals
nyears <- length(unique(counts$Year.num))             # Number of years we surveyed
ngrids <- length(unique(counts$Grid.ID.num))          # Unique survey grids

# Observation Level data 
midpt <- sort(unique(observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
observers <- observations$Observer.ID.num             # Random effect for observer associated with each survey
obs_point <- observations$Visit.ID.num                # In which grid did each observation take place
dclass <- observations$Dist.Bin                       # Distance class of each observation

# Availability date
tint <- observations$Time.Interval                    # Time interval for each observation 
time <- counts$MAS                                    # Matrix of scaled time after sunrise
day <- counts$Ord.Date                                # Matrix of scaled dates

# Point level data
n_dct <- counts$Count                                 # Matrix of the number of detected individuals per grid per survey 
years <- as.numeric(counts$Year)                      # Matrix of year numbers
grids <- counts$Grid.ID.num                           # Grid where each count took place
elevation <- counts$Elevation                         # Elevation (m) 
burned <- counts$Burned                               # Whether or not each grid burned
fire_year <- counts$Years.Since.Fire                  # How long since the most recent fire in each grid
burn_sev <- counts$mean.rdnbr                         # Burn severity from the most recent fire
X <- counts$X.scl                                     # X coordinate scaled UTM
Y <- counts$Y.scl                                     # Y coordinate scaled UTM

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
sobs_const <- list (
  # Misc. Constants
  delta = bin_size,            # Bin width
  trunc_dist = trunc_dist,     # Truncation distance
  
  # For loop sizes
  npoints = npoints,           # Number of survey grids
  nind = nind,                 # Number of individuals detected 
  nobsv = nobsv,               # Number of unique observers
  nbins = nbins,               # Number of distance bins
  nints = nints,               # Number of time intervals
  # ngrids = ngrids,             # Number of survey grids
  nyears = nyears,             # Number of years we surveyed (3)

  # Non-stochastic constants
  years = years,              # Year when each survey took place
  obs_point  = obs_point,      # Point where each observation took place
  observers = observers        # Effect of observer associated with each survey
)

# View Nimble constants 
str(sobs_const)

# Data to be fed into Nimble 
sobs_dat <- list(
  # Observation Level data 
  dclass = dclass,             # Distance category for each observation
  midpt = midpt,               # Midpoints of distance bins
  
  # Abundance data
  tint = tint,                 # Time interval for each observation
  time = time,                 # Scaled time after sunrise 
  day = day,                   # Scaled date
  
  #Point level data
  n_dct = n_dct,               # Number of detected individuals per site
  elevation = elevation,       # Scaled elevation
  # grids = grids,               # Grid where each survey took place
  burned = burned,             # Whether or not each grid burned
  fire_year = fire_year,       # How long since the most recent fire in each grid
  burn_sev = burn_sev,         # Burn severity from the most recent fire
  X = X,                       # X coordinate scaled UTM
  Y = Y                        # Y coordinate scaled UTM
)
# View Nimble data 
str(sobs_dat)

#set seed
# set.seed(01151999)

# Object dimensions
sobs_dims <- list(
  g = c(npoints, nbins),        # Cell prob x radial distance
  f = c(npoints, nbins),        # Cell prob x radial distance
  sigma = c(npoints),           # Linear combination on detectability
  pi_pd = c(npoints, nbins),    # Detection probability in each cell
  pi_pd_c = c(npoints, nbins),  # Proportion of total detection probability in each cell
  p_dct = c(npoints),           # Detection probability 
  p_a = c(npoints),             # Linear combination on avalibility  
  pi_pa = c(npoints, nints),    # Availability cell prob in each time interval
  pi_pa_c = c(npoints, nints),  # Proportion of total availability probability in each cell
  p_avail = c(npoints),         # Availability probability
  p_marg = c(npoints),          # Combined probability of detecting an available bird
  lambda = c(npoints)           # Poisson random variable
)

# Initial Values
sobs_inits <- list(
  # Detecability 
  alpha_obsv = rnorm(nobsv, 0, 0.01),
  # Availability 
  gamma0 = rnorm(1, 0.1, 0.01),
  gamma_date = rnorm(1, 0, 0.01), 
  gamma_time = rnorm(1, 0, 0.01),
  gamma_date2 = rnorm(1, 0, 0.01),
  gamma_time2 = rnorm(1, 0, 0.01),
  # Abundance 
  mean_beta0 = rnorm(1, 0, 0.01),
  sd_beta0 = runif(1, 0, 0.01),
  beta0_year = rnorm(nyears, 0, 0.01),
  beta_burn = rnorm(1, 0, 0.01),
  beta_elv = rnorm(1, 0, 0.01),
  beta_burn_elv = rnorm(1, 0, 0.01),
  beta_fyear = rnorm(1, 0, 0.01),
  beta_burnsev = rnorm(1, 0, 0.01),
  beta_elv2 = rnorm(1, 0, 0.1),
  beta_fyear2 = rnorm(1, 0, 0.01),
  beta_burn_elv2 = rnorm(1, 0, 0.01),
  beta_x = rnorm(1, 0, 0.01),
  beta_y = rnorm(1, 0, 0.01),
  # Presence 
  psi = runif(npoints, 0.4, 0.6),
  present = rbinom(npoints, 1, 0.5),
  # Simulated data
  N_indv = n_dct + 1              # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# 2.3) Model definition ################################################################
# Add random noise in the detection function intercepts
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sobs_model_code <- nimbleCode({
  # Priors for all parameters 
  # ------------------------------------------------------------------
  
  # Parameters in the availability component of the detection model
  gamma0 ~ dnorm(0, sd = 1)           # Mean availability
  gamma_date ~ dnorm(0, sd = 1)       # Effect of day of year on singing rate
  gamma_time ~ dnorm(0, sd = 1)       # Effect of time of day on singing rate
  gamma_date2 ~ dnorm(0, sd = 1)     # Effect of day of year on singing rate (quadratic)
  gamma_time2 ~ dnorm(0, sd = 1)     # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model
  for(o in 1:nobsv) {
    alpha_obsv[o] ~  dnorm(0, sd = 3) # Effect of each observer on detecability
  }
  
  # Parameters on the abundance component of the model
  
  # Prior hyperparameters
  mean_beta0 ~ dnorm(0, sd = 1)
  sd_beta0 ~ dunif(0, 1)
  # Intercept
  for(y in 1:nyears){
  beta0[y] ~ dnorm(mean_beta0 , sd = sd_beta0)           # Intercept on abundance on each year
  # beta0[y] ~ dnorm(mean_beta0, sd = sd_beta0) # Random interpect for each year
  }
  # Fixed effects
  beta_burn ~ dnorm(0, sd = 1)          # Effect of fire
  beta_elv ~ dnorm(0, sd = 1)           # Effect of elevation
  beta_burn_elv ~ dnorm(0, sd = 1)      # Interaction between fire and elevation
  beta_fyear ~ dnorm(0, sd = 1)         # Effect of years since fire on burned grids
  beta_burnsev ~ dnorm(0, sd = 1)       # Effect of initial burn severity on burned grids
  beta_fyear2 ~ dnorm(0, sd = 1)        # Effect of years since fire on burned grids squared
  beta_elv2 ~ dnorm(0, sd = 1)          # Effect of elevation on reference grids squared
  beta_burn_elv2 ~ dnorm(0, sd = 1)     # Interaction between fire and elevation squared
  beta_x ~ dnorm(0, sd = 1)             # Control for spatial autocorrelation in longitude
  beta_y ~ dnorm(0, sd = 1)             # Control for spatial autocorrelation in latitude
 
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey points
  for(s in 1:npoints){
    
    # Zero-inflation component on abundance
    psi[s] ~ dunif(0.0001, 0.9999)                            # Occupancy probability
    present[s] ~ dbern(psi[s])                                # Number of grids where that individual can be present
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, b]) <- -(midpt[b]^2) / (2 * sigma[s]^2)      # Half-normal detection function
        f[s, b] <- (2 * midpt[b] * delta) / trunc_dist^2      # Prob density function out to max truncation distance 
        pi_pd[s, b] <- g[s, b] * f[s, b]                      # Detection cell probability
        pi_pd_c[s, b] <- f[s, b] / p_dct[s]                   # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_dct[s] <- sum(pi_pd[s, ])
      
      # Construction of the cell probabilities for the nints time intervals
      for (k in 1:nints){
        pi_pa[s, k] <- p_a[s] * (1 - p_a[s])^(k - 1)          # Availability cell probability
        pi_pa_c[s, k] <- pi_pa[s, k] / p_avail[s]             # Proportion of availability probability in each time interval class
      }   
      
      # Rectangular integral approx. of integral that yields the Pr(available)
      p_avail[s] <- sum(pi_pa[s, ])
      
      # Multiply availability with capture probability to yield total marginal probability
      p_marg[s] <- p_dct[s] * p_avail[s]
      
      ### Binomial portion of mixture 
      n_dct[s] ~ dbin(p_marg[s], N_indv[s])                               # Number of individual birds captured (observations)
      
      # Poisson abundance portion of mixture
      N_indv[s] ~ dpois(lambda[s] * (present[s] + 0.0001))                # ZIP true abundance at site s in year y
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s]) <- alpha_obsv[observers[s]]                           # Effect of each observer on detectability
      
      # Availability (p_a) Log-linear model for availability
      logit(p_a[s]) <- gamma0 +                                           # Intercept on availability 
                       gamma_date * day[s] +                              # Effect of scaled ordinal date 
                       gamma_time * time[s] +                             # Effect of scaled time of day                  
                       gamma_date2 * day[s]^2 +                           # Effect of scaled ordinal date squared
                       gamma_time2 * time[s]^2                            # Effect of scaled time of day squared
      
      # Abundance (lambda) Log-linear model 
      log(lambda[s]) <- beta0_year[years[s]] +                            # Intercept on abundance
                        beta_burn * burned[s] +                           # Effect of fire
                        beta_elv *  elevation[s] +                        # Effect of elevation on reference grids 
                        beta_burn_elv * burned[s] * elevation[s] +        # Interaction between fire and elevation
                        beta_fyear * fire_year[s] * burned[s] +           # Effect of years since fire on burned grids
                        beta_burnsev * burn_sev[s] * burned[s] +          # Effect of initial burn severity on burned grids
                        beta_fyear2 * fire_year[s]^2 +                    # Effect of years since fire on burned grids squared
                        beta_elv2 *  elevation[s]^2 +                     # Effect of elevation squared
                        beta_burn_elv2 * burned[s] * elevation[s]^2 +     # Interaction between fire and elevation squared
                        beta_x * X[s] +                                   # Spatial autocorrelation in x
                        beta_y * Y[s]                                     # Spatial autocorrelation in y
    
  } # end loop through survey grids

  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_point[i], ])      # Linking distance class data
    tint[i] ~ dcat(pi_pa_c[obs_point[i], ])        # Linking time removal data
  } # end observation loop
  
  # Averages across grids and years
  mean_p_avail <- mean(p_avail[])                 # Average availability probability
  mean_p_dct <- mean(p_dct[])                     # Average detection probability
  mean_psi <- mean(psi[])                         # Average occupancy probability
})

# 2.4) Configure and Run the model ###########################################################

# Params to save
sobs_params <- c("beta0_year",
                 "beta_burn",
                 "beta_elv", 
                 "beta_elv2",
                 "beta_burn_elv",
                 "beta_burn_elv2",
                 "beta_fyear",
                 "beta_fyear2",
                 "beta_burnsev",
                 "beta_x",
                 "beta_y",
                 "gamma0", 
                 "gamma_date",
                 "gamma_time", 
                 "gamma_date2",
                 "gamma_time2",
                 "alpha_obsv",
                 "mean_p_avail",
                 "mean_p_dct",
                 "mean_psi")

# MCMC settings. Pick one, comment out the rest 
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1          # Quick test to see if the model runs
# nc <- 3  ;  ni <- 30000  ;  nb <- 20000;  nt <- 1     # longer test where most parameters should
nc <- 4;  ni <- 150000;  nb <- 100000;  nt <- 1        # Run the model for real

#Run the sampler
start <- Sys.time()                                   # Start time for the sampler
start                                                 # Print the sampler start time
sobs_mcmc_out <- nimbleMCMC(code = sobs_model_code,
                            data = sobs_dat,
                            constants = sobs_const,
                            dimensions = sobs_dims,
                            inits = sobs_inits,
                            monitors = sobs_params,
                            niter = ni,
                            nburnin = nb,
                            thin = nt,
                            nchains = nc,
                            # setSeed = 01151999,
                            samples = TRUE,
                            summary = TRUE)
difftime(Sys.time(), start)                           # End time for the sampler

# Save model output to local drive
saveRDS(sobs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     study_species, "_point_fire_model.rds"))


################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       study_species, "_point_fire_model.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = sobs_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(study_species, "_point_fire_model_traceplot"),
          type = 'both',
          iter = 10000)

# View MCMC summary
MCMCsummary(object = sobs_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = sobs_mcmc_out$samples,
         guide_lines = TRUE,
         params = sobs_params)

