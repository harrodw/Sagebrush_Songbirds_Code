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

# Define single species
study_species <- "BRSP"

# Define a truncation distance (km)
trunc_dist <- 0.125

# Function to find the mode
find_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]  # Find the most frequent value
}

# Make a table of important species sightings by visit
counts_0inf <- sobs %>%
  # Switch from m to km
  mutate(Distance = Distance/1000) %>% 
  # Only observations closer than the truncation distance
  filter(Distance <= trunc_dist) %>% 
  # Only one species
  filter(Species  == study_species) %>% 
  # Unque ID for each visit
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Year, Visit.ID, Ord.Date) %>% 
  # Number of individuals per visit
  reframe(Grid.ID, Year, Visit.ID, Ord.Date, Count = n()) %>% 
  distinct()
#...and view
glimpse(counts_0inf)

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Grid.Type, Year, Ord.Date, MAS, Observer.ID, Year, Visit) %>% 
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
          # Each visit should be treated separately
           Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Grid.Type, Year, Visit.ID, Ord.Date) %>% 
  reframe(Grid.ID, Visit.ID, 
          # Average minutes after sunrise on the survey
          Mean.MAS = mean(MAS),
          # Observer who submitted the most points
          Observer.ID = find_mode(Observer.ID),
          n.Points = n()) %>% 
  distinct()

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
counts_temp <-  visit_count %>% 
  left_join(counts_0inf, by = c("Grid.ID", "Year", "Visit.ID", "Ord.Date")) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))

#...and view
glimpse(counts_temp)
print(counts_temp, n = Inf)

# View the surveys that weren't done
counts_temp %>% 
  group_by(Grid.ID) %>% 
  filter(n() < 6)
# ID-C11, Y1-V1 and ID-C22, Y1-V1 weren't recorded

# For now I',m going to propagate the existing data to fill these in
new_dat <- tibble(Grid.ID = c("ID-C11", "ID-C22"),
                  Grid.Type = c("R", "R"),
                  Year = c("Y1", "Y1"),
                  Visit.ID = c("Y1-V1", "Y1-V1"),
                  # Saying Rory filled in the data
                  Observer.ID = c("Rory", "Rory"),
                  # Average ordianal date across visits
                  Ord.Date = c(floor(mean(counts_temp$Ord.Date[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$Ord.Date[which(counts_temp$Grid.ID == "ID-C22")]))),
                  # Average survey time across visits
                  Mean.MAS = c(mean(counts_temp$Mean.MAS[which(counts_temp$Grid.ID == "ID-C11")]),
                               mean(counts_temp$Mean.MAS[which(counts_temp$Grid.ID == "ID-C22")])),
                  # Average count across visits
                  Count = c(floor(mean(counts_temp$Count[which(counts_temp$Grid.ID == "ID-C11")])),
                            floor(mean(counts_temp$Count[which(counts_temp$Grid.ID == "ID-C22")]))),
                  # Average number of points surevyed
                  n.Points = c(floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C22")]))))

# Bind these to the exisitng data 
counts_temp2 <- bind_rows(counts_temp, new_dat)
#...and view
glimpse(counts_temp2)

# Change necessary variables to scales and factors
counts_temp3 <- counts_temp2 %>%
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
  # Remove columns that are no longer needed
  dplyr::select(-Grid.Type) 

#...and view
glimpse(counts_temp3)

# Isolate the burned Grids as their own object so I can accurately scale them
fire_stats <- counts_temp3 %>% 
  filter(Burned == 1) %>% 
  dplyr::select(Grid.ID, Years.Since.Fire, mean.rdnbr) 

# Find the mean and standard deviation of the "real" burns
mean_burnY <- mean(fire_stats$Years.Since.Fire)
sd_burnY <- sd(fire_stats$Years.Since.Fire)
mean_rdnbr <- mean(fire_stats$mean.rdnbr)
sd_rdnbr <- sd(fire_stats$mean.rdnbr)

# Scale the other covariates
counts <- counts_temp3 %>% 
  mutate(Elevation = scale(Elevation)[,1],
         Mean.MAS = scale(Mean.MAS)[,1],
         Ord.Date = scale(Ord.Date)[,1],
         Years.Since.Fire = (Years.Since.Fire - mean_burnY) / sd_burnY,
         mean.rdnbr = (mean.rdnbr - mean_rdnbr) / sd_rdnbr)

# 1.3) Prepare the observation level data ################################################################

# Define the number of bins
nbins <- 5

# Define a bin size
bin_size <- trunc_dist / nbins

# Define the bins
bins <- seq(from = bin_size, to = bin_size * nbins, by = bin_size)

# View the bins
bins

# Create an object with all observations of a single species for the detection function
observations_temp <- sobs %>% 
  # Switch from m to km
  mutate(Distance = Distance / 1000) %>%  
  # Only one species
  filter(Species == study_species) %>%   
  # Same order as the counts
  arrange(Year, Visit, Grid.ID) %>%
  # Only close observations
  filter(Distance <= trunc_dist) %>%          
  # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
  mutate(Observer.ID = case_when(Observer.ID == "Alex" ~ "Aidan", TRUE ~ Observer.ID),
         # A unique ID for each visit
         Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  dplyr::select(Distance, Minute, Grid.ID, Observer.ID, Year, Visit.ID) %>% 
  left_join(covs, by = "Grid.ID") %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
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
  dplyr::select(Grid.ID, Observer.ID, Distance, Dist.Bin, Dist.Bin.Midpoint, Time.Interval, Year, Visit.ID)

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

# # Plot a specific covariate against bird abundance
# counts %>%
#   ggplot(aes(x = Sage.Cover, y = Count)) +
#   # geom_boxplot()
#   geom_smooth(method = "lm", col = "aquamarine4", fill = "aquamarine3") +
#   geom_jitter(col = "aquamarine4") +
#   theme_bw() 
# 
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
points_mat <- matrix(NA, nrow = nrows, ncol = ncols)
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
  time_mat[y,] <- count_visit$Mean.MAS
  date_mat[y,] <- count_visit$Ord.Date
  # Area surveyed in km^2
  points_mat[y,] <- pi * count_visit$n.Points * trunc_dist^2 
  obsv_mat[y,] <- count_visit$Observer.ID.num
  fyear_mat[y,] <- count_visit$Years.Since.Fire
}

# View the matrices
count_mat  # Number of individuals recorded at each visit
year_mat   # Year during which each visit took place
time_mat   # Scaled mean time of day for each visit
date_mat   # Scaled date for each visit
points_mat # Number of points visited during each survey 
obsv_mat   # Who conducted each survey
fyear_mat  # Years since fire during each survey

# Loop sizes 
ngrids <- length(unique(counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(observations)                            # Number of individuals detected 
nobsv <- length(unique(counts$Observer.ID.num))       # Number of unique observers
nbins <- length(unique(observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(observations$Time.Interval))   # Number of time intervals
nvst <- length(unique(counts$Visit.ID))               # Number of visits in each year
nyears <- length(unique(counts$Year.num))             # Number of years we surveyed

# Observation Level data 
midpt <- sort(unique(observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
observers <- obsv_mat                                 # Random effect for observer associated with each survey
obs_visit <- observations$Visit.ID.num                # During which visit did each observation take place
obs_grid <- observations$Grid.ID.num                  # In which grid did each observation take place
dclass <- observations$Dist.Bin                       # Distance class of each observation
delta <- bin_size                                     # Size of distance bins

# Availability date
tint <- observations$Time.Interval                    # Time interval for each observation 
time <- time_mat                                      # Matrix of scaled time after sunrise
day <- date_mat                                       # Matrix of scaled dates

# Grid level data
area <- points_mat                                    # Proportion of points surveyed       
n_dct <- count_mat                                    # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                     # Matrix of year numbers
elevation <- counts$Elevation[1:ngrids]               # Elevation (m) 
burned <- counts$Burned[1:ngrids]                     # Whether or not each grid burned
fire_year <- fyear_mat                                # How long since the most recent fire in each grid
burn_sev <- counts$mean.rdnbr[1:ngrids]               # Burn severity from the most recent fire

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Constants, data, Initial values, and dimensions #############################################

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
  nyears = nyears,            # Number of years we surveyed (3)

  # Non-stochastic constants
  years = years,              # Year when each survey took place
  obs_visit  = obs_visit,      # Visit when each observation took place
  obs_grid  = obs_grid,        # Grid of each observation 
  observers = observers       # Effect of observer associated with each survey
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
  elevation = elevation,  # Scaled elevation
  fire_year = fire_year,  # How long since the most recent fire in each grid
  # burn_sev = bu rn_sev,   # Burn severity from the most recent fire
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
  p_marg = c(ngrids, nvst),          # Combined probability of detecting an available bird
  lambda = c(ngrids, nvst)           # Poisson random variable
)

# Initial Values
sobs_inits <- list(
  # Detecable 
  alpha0_obsv = runif(nobsv, 0, 1),
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),
  gamma_date = rnorm(1, 0, 0.1),
  gamma_time = rnorm(1, 0, 0.1),
  gamma_date2 = rnorm(1, 0, 0.1),
  gamma_time2 = rnorm(1, 0, 0.1),
  # Abundance 
  # mean_beta0 = runif(1, 0, 1),
  # sd_beta0 = runif(1, 0, 0.1),
  beta0_year = rnorm(nyears, 0, 0.1),
  # beta0 = rnorm(1, 0, 0.1),
  beta_burn = rnorm(1, 0, 0.1),
  beta_elv = rnorm(1, 0, 0.1),
  # beta_burn_elv = rnorm(1, 0, 0.1),
  beta_fyear = rnorm(1, 0, 0.1),
  # beta_burnsev = rnorm(1, 0, 0.1),
  # beta_elv2 = rnorm(1, 0, 0.1),
  # beta_fyear2 = rnorm(1, 0, 0.1),
  # beta_burn_elv2 = rnorm(1, 0, 0.1),
  # Presence 
  psi = runif(ngrids, 0, 1),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated data
  N_indv = count_mat + 1              # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# 2.3) Model definition ################################################################
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sobs_model_code <- nimbleCode({
  # Priors for all parameters 
  # ------------------------------------------------------------------
  
  # Parameters in the availability component of the detection model
  gamma0 ~ dnorm(0, 1)                 # phi intercept on log scale 
  gamma_date ~ dnorm(0, sd = 1)        # Effect of day of year on singing rate
  gamma_time ~ dnorm(0, sd = 1.7)      # Effect of time of day on singing rate
  gamma_date2 ~ dnorm(0, sd = 0.5)     # Effect of day of year on singing rate (quadratic)
  gamma_time2 ~ dnorm(0, sd = 0.5)     # Effect of time of day on singing rate (quadratic)

  # Parameters in the detection portion of the model
  for(o in 1:nobsv) {
    alpha0_obsv[o] ~ dnorm(0, sd = 3)     # Effect of each observer on detecability 
  }

  # Parameters on the abundance component of the model
  # mean_beta0 ~ dnorm(0, 5)       # Mean abundance hyperparameter
  # sd_beta0 ~ dunif(0, 5)         # Sd in yearly abundance hyperparameter
  # Random intercept on abundance
  for(t in 1:nyears){
    beta0_year[t] ~ dnorm(0, sd = 5)
    # beta0_year[t] ~ dnorm(mean_beta0, sd_beta0)
  }
  
  # Fixed effects for the abundance component of the model
  # beta0 ~ dnorm(0, sd = 5)             # Intercept on abundance
  beta_burn ~ dnorm(0, sd = 3)           # Effect of fire
  beta_elv ~ dnorm(0, sd = 1)            # Effect of elevation
  # beta_burn_elv ~ dnorm(0, sd = 1)     # Interaction between fire and elevation
  beta_fyear ~ dnorm(0, sd = 1)          # Effect of years since fire on burned grids
  # beta_burnsev ~ dnorm(0, sd = 1)        # Effect of initial burn severity on burned grids
  # beta_burn_elv2 ~ dnorm(0, sd = 0.5)  # Effect of fire and elevation squared
  # beta_fyear2 ~ dnorm(0, sd = 0.5)     # Effect of years since fire on burned grids squared
  # beta_elv2 ~ dnorm(0, sd = 0.5)       # Effect of elevation squared
  # beta_burn_elv2 ~ dnorm(0, sd = 0.5)  # Interaction between fire and elevation squared 
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood

  # Iterate over all survey grids
  for(s in 1:ngrids){
    
    # Zero-inflation component on abundance
    psi[s] ~ dunif(0.0001, 0.9999)                                     # Occupancy probability
    present[s] ~ dbern(psi[s])                                         # Number of grids where that individual can be present

    # Iterate over all of the visits to each survey grid 
    for(y in 1:nvst){ 
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, y, b]) <- -(midpt[b]^2) / (2 * sigma[s, y]^2)                # Half-normal detection function
        f[s, y, b] <- (2 * midpt[b] * delta) / trunc_dist^2                   # Prob density function out to max truncation distance 
        pi_pd[s, y, b] <- g[s, y, b] * f[s, y, b]                             # Detection cell probability
        pi_pd_c[s, y, b] <- f[s, y, b] / p_dct[s, y]                          # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_dct[s, y] <- sum(pi_pd[s, y, ])
      
      # Construction of the cell probabilities for the nints time intervals
      for (k in 1:nints){
        pi_pa[s, y, k] <- p_a[s, y] * (1 - p_a[s, y])^(k - 1)                 # Availability cell probability
        pi_pa_c[s, y, k] <- pi_pa[s, y, k] / p_avail[s, y]                    # Proportion of availability probability in each time interval class
      }

      # Rectangular integral approx. of integral that yields the Pr(available)
      p_avail[s, y] <- sum(pi_pa[s, y, ])

      # Multiply availability with capture probability to yield total marginal probability
      p_marg[s, y] <- p_dct[s, y] 
      # * p_avail[s, y]
      
      ### Binomial portion of mixture 
      n_dct[s, y] ~ dbin(p_marg[s, y], N_indv[s, y])                          # Number of individual birds captured (observations)
      
      # Poisson abundance portion of mixture
      N_indv[s, y] ~ dpois(lambda[s, y] * (present[s] + 0.0001) * area[s, y]) # ZIP true abundance at site s in year y
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s, y]) <- alpha0_obsv[observers[s, y]]                        # Effect of each observer on detectability
      
      # Availability (p_a) Log-linear model for availability
      logit(p_a[s, y]) <- gamma0 +                                            # Intercept on availability
                          gamma_date * day[s, y] +                            # Effect of scaled ordinal date
                          gamma_time * time[s, y] +                           # Effect of scaled time of day
                          gamma_date2 * day[s, y]^2 +                         # Effect of scaled ordinal date squared
                          gamma_time2 * time[s, y]^2                          # Effect of scaled time of day squared

      # Abundance (lambda) Log-linear model 
      log(lambda[s, y]) <- beta0_year[years[s, y]] +                          # Intercept on abundance 
                           beta_burn * burned[s] +                            # Effect of fire 
                           beta_elv * elevation[s] +                          # Effect of elevation on reference grids                            
                           # beta_burn_elv * burned[s] * elevation[s] +         # Interaction between fire and elevation
                           beta_fyear * fire_year[s, y] * burned[s]           # Effect of years since fire on burned grids
                           # beta_burnsev * burn_sev[s] * burned[s] +          # Effect of initial burn severity on burned grids
                           # beta_fyear2 * fire_year[s, y]^2 +                 # Effect of years since fire on burned grids squared
                           # beta_elv2 *  elevation[s]^2 +                     # Effect of elevation squared
                           # beta_burn_elv2 * burned[s] * elevation[s]^2       # Interaction between fire and elevation squared

    } # end loop through visits
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ])                    # likelihood for distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])                      # likelihood for time removal data
  } # end observation loop
  
  # Averages across grids and years
  mean_p_avail <- mean(p_avail[,])                                            # Average availability probability
  mean_p_dct <- mean(p_dct[,])                                                # Average detection probability
  mean_psi <- mean(psi[])                                                     # Average occupancy probability
})

# 2.4) Configure and Run the model ###########################################################

# Params to save
sobs_params <- c("beta0_year",
                 "beta_burn",
                 "beta_elv", 
                 # "beta_burn_elv",
                 "beta_fyear",
                 # "beta_burnsev",
                 # "beta_fyear2",
                 # "beta_elv2",
                 # "beta_burn_elv2",
                 "gamma0",
                 "gamma_date",
                 "gamma_time",
                 "gamma_date2",
                 "gamma_time2",
                 "alpha0_obsv",
                 "mean_p_avail",
                 "mean_p_dct",
                 "mean_psi")

# MCMC settings. Pick one, comment out the rest 
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1          # Quick test to see if the model runs
# nc <- 3  ;  ni <- 50000  ;  nb <- 25000;  nt <- 5    # longer test where most parameters should
nc <- 4;  ni <- 150000;  nb <- 100000;  nt <- 10        # Run the model for real

#Run the sampler
start <- Sys.time()                                     # Start time for the sampler
start                                                   # Print the sampler start time
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
difftime(Sys.time(), start)                             # End time for the sampler

# Save model output to local drive
saveRDS(sobs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     study_species, "_fire_model.rds"))


################################################################################
# 3) Model output and diagnostics ##############################################
###################5#############################################################

# Clear plots
# dev.off()

# 3.1) View model output

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       study_species, "_fire_model.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = sobs_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(study_species, "_fire_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = sobs_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = sobs_mcmc_out$samples,
         guide_lines = TRUE,
         params = sobs_params)

