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
# # or from github
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


# All possible scales
model_scales <- c("125m", "5km")

# Loop over both scales 
# for(m in 1:length(model_scales)){

# Pick a scale
# model_scale <- model_scales[m]
model_scale <- model_scales[1]

# Define relevant species
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
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Distance <= trunc_dist) %>% # only observations closer than the truncation distance
  filter(Species  == study_species) %>% # only one species
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Visit.ID) %>% 
  reframe(Grid.ID, Year, Visit.ID, Ord.Date, 
          Count = n()) %>% 
  distinct()
#...and view
glimpse(counts_0inf)

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Grid.Type, Year, Ord.Date, MAS, Wind.Start, Observer.ID, Year, Visit) %>% 
  mutate(
    # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
    Observer.ID = case_when(Observer.ID == "Alex" ~ "Ben", TRUE ~ Observer.ID),
    # Each visit should be treated separately
    Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Visit.ID) %>% 
  reframe(Grid.ID, Grid.Type, Year, Visit.ID, Ord.Date,
          # Average minutes after sunrise on the survey
          Mean.MAS = mean(MAS),
          # Most common wind catagory used on the survey
          Wind = find_mode(Wind.Start),
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
glimpse(counts_temp2)
# Define levels for wind factors
wind_lvl <- c("<1 mph", "1-3 mph", "4-7 mph", "8-12 mph", "13-18 mph", "Unknown")

# Change necessary variables to scales and factors
counts_temp3 <- counts_temp2 %>%
  # Add covariates
  left_join(covs, by = c("Grid.ID", "Grid.Type")) %>% 
  # Sort the data
  arrange(Visit.ID, Grid.ID) %>% 
         # Numeric burned vs unburned
  mutate(Burned = as.numeric(factor(Grid.Type, levels = c("R", "B"))) - 1,
         # Time since Fire
         Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  # Change the wind data to a factor with set levels
  mutate(Wind = case_when(is.na(Wind) ~ 'Unknown', 
                          TRUE ~ Wind)) %>% 
  mutate(Wind = factor(Wind, levels = wind_lvl)) %>%
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID", "Year"), factor) %>% 
  # Remove columns that are no longer needed
  dplyr::select(-Grid.Type) 

#...and view
glimpse(counts_temp3)

# This is where I can change the scale ----
# Scale the other covariates
counts <- counts_temp3 %>%
  mutate(
    Shrub.Cover = scale(!!sym(paste0("Shrub.Cover.", model_scale)))[,1],
    PFG.Cover = scale(!!sym(paste0("Perennial.Cover.", model_scale)))[,1],
    TRI = scale(!!sym(paste0("TRI.", model_scale)))[,1],
    Shrub.Patch.Size = scale(!!sym(paste0("ln.Shrub.Patch.Size.", model_scale)))[,1],
    Trees.Present = Trees.Present.125m, # Keep this at the smallest scale where it is relevant
    Mean.MAS = scale(Mean.MAS)[,1],   
    Ord.Date = scale(Ord.Date)[,1]    
  )
#...and view
glimpse(counts)


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
  dplyr::select(Grid.ID, Grid.ID.num, Year, Year.num, Visit.ID, Visit.ID.num, Observer.ID, Observer.ID.num)
#...and view
glimpse(point_ids)

# Link the factor levels from the count dataset to the observation dataset
observations <- observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Year", "Visit.ID"))

# View Counts
glimpse(counts)

# Plot counts against a covaariate
counts %>%
  # filter(Burned == 1) %>%
  ggplot(aes(x = Shrub.Cover, y = Count)) +
  # geom_boxplot()
  geom_smooth(method = "lm", col = "aquamarine4", fill = "aquamarine3") +
  geom_jitter(col = "aquamarine4") +
  theme_bw()


# View Observations
glimpse(observations)

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
area <- matrix(NA, nrow = nrows, ncol = ncols)
# Build a storage matrix for who conducted each survey
obsv_mat <- matrix(NA, nrow = nrows, ncol = ncols)

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
  # Area surveyd din km^2
  area[y,] <- pi * count_visit$n.Points * trunc_dist^2 
  obsv_mat[y,] <- count_visit$Observer.ID.num
}

# View the matrices
count_mat  # Number of individuals recorded at each visit
year_mat   # Year during which each visit took place
time_mat   # Scaled mean time of day for each visit
date_mat   # Scaled date for each visit
area # Number of points visited during each survey 
obsv_mat   # Who conducted each survey

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

# Availability date
tint <- observations$Time.Interval                    # Time interval for each observation 
time <- time_mat                                      # Matrix of scaled time after sunrise
day <- date_mat                                       # Matrix of scaled dates

# Grid level data
n_dct <- count_mat                                    # Matrix of the number of detected individuals per grid per survey 
years <- year_mat                                     # Matrix of year numbers
shrub_cvr <- counts$Shrub.Cover[1:ngrids]             # Percent shrub cover
pfg_cvr <- counts$PFG.Cover[1:ngrids]                 # Percent perennial forb and grass cover
tri <- counts$TRI[1:ngrids]                           # Topographic ruggedness
patch_size <- counts$Shrub.Patch.Size[1:ngrids]       # Average size of shrub patches on the log scale
trees <- counts$Trees.Present[1:ngrids]          # Whether or not trees are present in the grid 

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
  gamma_date ~ dnorm(0, sd = 2)     # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 2)    # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 2)     # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 2)    # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model
  alpha0 ~ dnorm(0, sd = 3)            # Intercept on detectability
  for(o in 1:nobsv){
    alpha_obsv[o] ~ dnorm(0, sd = 2)   # Effect of each observer on detecability 
  }

  # Parameters on the abundance component of the model
  mean_beta0 ~ dnorm(0, 3)       # Mean abundance hyperparameter
  sd_beta0 ~ dunif(0, 1)         # Sd in yearly abundance hyperparameter
  # Random intercept on abundance
  for(t in 1:nyears){
    beta0_year[t] ~ dnorm(mean_beta0, sd_beta0)
  }

  beta_shrub ~ dnorm(0, sd = 2)     # Effect of shrub cover
  # beta_shrub2 ~ dnorm(0, sd = 2)    # Effect of shrub cover squared
  beta_pfg ~ dnorm(0, sd = 2)       # Effect of Perennial Cover
  # beta_pfg2 ~ dnorm(0, sd = 2)      # Effect of perennial cover squared
  beta_tri ~ dnorm(0, sd = 2)       # Effect of ruggedness
  # beta_tri2 ~ dnorm(0, sd = 2)      # Effect of ruggedness squared
  beta_patch ~ dnorm(0, sd = 2)     # Effect of patch size
  beta_tree ~ dnorm(0, sd = 2)      # Effect of trees being present
  
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
      log(sigma[s, y]) <- alpha0 +                                    # Intercept on detecability
                          alpha_obsv[observers[s, y]]                 # Effect of each observer on detectability
      
      # Availability (p_a) Log-linear model for availability
      logit(p_a[s, y]) <- gamma0 +                                    # Intercept on availability 
                          gamma_date * day[s, y] +                    # Effect of scaled ordinal date 
                          gamma_date2 * day[s, y]^2 +                 # Effect of scaled ordinal date squared 
                          gamma_time * time[s, y] +                   # Effect of scaled time of day 
                          gamma_time2 * time[s, y]^2                  # Effect of scaled time of day squared 
      
      # Abundance (lambda) Log-linear model 
      log(lambda[s, y]) <- beta0_year[years[s, y]] +                  # Intercept on abundance
                           beta_shrub * shrub_cvr[s] +                # Effect of shrub cover
                           # beta_shrub2 * shrub_cvr[s]^2 +             # Effect of shrub cover squared
                           beta_pfg *  pfg_cvr[s] +                   # Effect of Perennial Cover
                           # beta_pfg2 * pfg_cvr[s]^2 +                 # Effect of perennial cover squared
                           beta_tri * tri[s] +                        # Effect of ruggedness
                           # beta_tri2 * tri[s]^2 +                     # Effect of ruggedness squared
                           beta_patch * patch_size[s] +               # Effect of shrub patch size
                           beta_tree * trees[s]                       # Effect of trees being present

      # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
      # Compute fit statistic for observed data
      e_val[s, y] <- p_marg[s, y] * N_indv[s, y]             # Expected value for binomial portion of the model
      FT[s, y] <- (sqrt(n_dct[s, y]) - sqrt(e_val[s, y]))^2 
      
      # Generate replicate count data and compute same fit stats for them
      n_dct_new[s, y] ~ dbin(p_marg[s, y], N_indv[s, y])              
      FT_new[s, y] <- (sqrt(n_dct_new[s, y]) - sqrt(e_val[s, y]))^2 
      
    } # end loop through visits
    
  } # end loop through survey grids
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ]) # linking distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])   # linking time removal data
  } # end observation loop
  
  
  # Averages across grids and years
  mean_psi <- mean(psi[])                                  # Average occupancy probability
  mean_phi <- mean(phi[,])                                 # Average availability probability
  mean_p_dct <- mean(p_dct[,])                             # Average detection probability
  
  # Add up fit stats across sites and years
  fit <- sum(FT[,]) 
  fit_new <- sum(FT_new[,]) 
  
  # c-hat value, should converge to ~1
  c_hat <- fit / fit_new
  
  # Compute Bayesian p-value for the model, should converge to ~0.5
  bpv <- step(fit_new - fit)
  
})

# 2.2) Cpoints_mat# 2.2) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
sobs_const <- list (
  # Misc. Constants
  area = area,       # Number of points surveyed per grid per visit
  delta = bin_size,        # Bin width
  trunc_dist = trunc_dist, # Truncation distance
  
  # For loop sizes
  ngrids = ngrids,         # Number of survey grids
  nind = nind,             # Number of individuals detected 
  nobsv = nobsv,           # Number of unique observers
  nvst = nvst,             # Number of times each grid was surveyed (6)
  nbins = nbins,           # Number of distance bins
  nints = nints,           # Number of time intervals
  nyears = nyears,          # Number of years we surveyd
  
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
  n_dct = n_dct,           # Number of detected individuals per site
  shrub_cvr = shrub_cvr,   # Percent shrub cover
  pfg_cvr = pfg_cvr,       # Percent perennial forb and grass cover
  tri = tri,               # Topographic ruggedness
  patch_size = patch_size, # Average size of shrub patches on the log scale
  trees = trees            # Whether or not trees are present in the grid 
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
  lambda = c(ngrids, nvst),          # Poisson random variable
  FT = c(ngrids , nvst),             # Freeman Tukey Discrepancy
  FT_new = c(ngrids, nvst)           # New Freeman Tukey Discrepancy
)
# View the dimensions
str(sobs_dims)

# Initial Values
sobs_inits <- list(
  # Detecability 
  alpha0 = rnorm(1, 0, 0.1),
  alpha_obsv = rnorm(nobsv, 0, 0.1),
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
  beta_shrub = rnorm(1, 0, 0.1),
  # beta_shrub2 = rnorm(1, 0, 0.1),
  beta_pfg = rnorm(1, 0, 0.1),
  # beta_pfg2 = rnorm(1, 0, 0.1),
  beta_tri = rnorm(1, 0, 0.1),
  # beta_tri2 = rnorm(1, 0, 0.1),
  beta_patch = rnorm(1, 0, 0.1),
  beta_tree = rnorm(1, 0, 0.1),
  # Presence 
  psi = runif(ngrids, 0.4, 0.6),
  present = rbinom(ngrids, 1, 0.5),
  # Simulated data
  n_dct_new = count_mat,              # Initialize the new capture data at the existing data values
  N_indv = count_mat + 1              # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c("beta0_year",
                 "beta_shrub",
                 # "beta_shrub2",
                 "beta_pfg",
                 # "beta_pfg2",
                 "beta_tri",
                 # "beta_tri2",
                 "beta_patch",
                 "beta_tree",
                 "gamma0", 
                 "gamma_date", 
                 "gamma_date2", 
                 "gamma_time", 
                 "gamma_time2", 
                 "alpha0",
                 "alpha_obsv",
                 "mean_psi",
                 "mean_p_dct",
                 "mean_phi",
                 "fit", 
                 "fit_new", 
                 "c_hat",
                 "bpv")


# 2.3) Configure and Run the model ###########################################################

# Build the nimble model
sobs_model_vect <- nimbleModel(sobs_model_code, 
                               data = sobs_dat,
                               constants = sobs_const,
                               dimensions = sobs_dims,
                               inits = sobs_inits)

# MCMC settings for default nimble model setup
nc_test <- 1  ;  ni_test <- 1000  ;  nb_test <- 500  ;  nt_test <- 10          

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
sobs_mcmcConf$removeSamplers("alpha0", "alpha_obsv[1]", "alpha_obsv[2]", "alpha_obsv[3]", "alpha_obsv[4]", 
                             "alpha_obsv[5]", "alpha_obsv[6]", "alpha_obsv[7]", "alpha_obsv[8]",
                             "alpha_obsv[9]", "alpha_obsv[10]", "alpha_obsv[11]", "alpha_obsv[12]", 
                             "alpha_obsv[13]", "alpha_obsv[14]", "alpha_obsv[15]", "alpha_obsv[16]", 
                             "alpha_obsv[17]")

sobs_mcmcConf$addSampler(target = c("alpha0", "alpha_obsv[1]", "alpha_obsv[2]", "alpha_obsv[3]", "alpha_obsv[4]", 
                                    "alpha_obsv[5]", "alpha_obsv[6]", "alpha_obsv[7]", "alpha_obsv[8]",
                                    "alpha_obsv[9]", "alpha_obsv[10]", "alpha_obsv[11]", "alpha_obsv[12]", 
                                    "alpha_obsv[13]", "alpha_obsv[14]", "alpha_obsv[15]", "alpha_obsv[16]", 
                                    "alpha_obsv[17]"),
                         type = 'RW_block')

# Block all abundance (beta) nodes together
sobs_mcmcConf$removeSamplers("beta0_year[1]", "beta0_year[2]", "beta0_year[3]",
                             "beta_shrub",
                             # "beta_shrub2",
                             "beta_pfg",
                             # "beta_pfg2",
                             "beta_tri",
                             # "beta_tri2",
                             "beta_patch", "beta_tree")

sobs_mcmcConf$addSampler(target = c("beta0_year[1]", "beta0_year[2]", "beta0_year[3]",
                                    "beta_shrub", 
                                    # "beta_shrub2",
                                    "beta_pfg", 
                                    # "beta_pfg2",
                                    "beta_tri", 
                                    # "beta_tri2",
                                    "beta_patch", "beta_tree"),
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
                                     study_species, "_", model_scale, "_landscape_model_out.rds"))
# } # End loop through scales


################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output #######################################################

# Pick a species and scale
study_species <- "SATH"
model_scale <- "125m"

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       study_species, "_", model_scale, "_landscape_model_out.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = sobs_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(study_species, "_", model_scale, "_landscape_model_traceplot"),
          type = 'both')

# View MCMC summary
# MCMCsummary(object = sobs_mcmc_out$samples, 
#             params = sobs_params,
#             round = 2)

# Perameters for the MCMC plot
plot_params <- c(
  # "beta0_year",
  "beta_shrub", "beta_pfg", "beta_tri", "beta_patch", "beta_tree")


# View MCMC plot
# MCMCplot(object = sobs_mcmc_out$samples,
#          ref_ovl = TRUE,
#          col = "black",
#          params = plot_params)

#####################################################################################
# 4) Posterior Inference ############################################################
#####################################################################################

# View MCMC summary
sobs_mcmc_out$summary$all.chains

# 4.1) Extract coefficient values ###############################################################

# Extract Beta0 values
beta0_year1 <- sobs_mcmc_out$summary$all.chains[19,]
beta0_year2 <- sobs_mcmc_out$summary$all.chains[20,]
beta0_year3 <- sobs_mcmc_out$summary$all.chains[21,]
# View intercepts
print(bind_rows(beta0_year1, beta0_year2, beta0_year3))

# Extract effect sizes
beta_shrub <- sobs_mcmc_out$summary$all.chains[24,]
# beta_shrub2 <- sobs_mcmc_out$summary$all.chains[26,]
beta_pfg <- sobs_mcmc_out$summary$all.chains[23,]
# beta_pfg2 <- sobs_mcmc_out$summary$all.chains[24,]
beta_tri <- sobs_mcmc_out$summary$all.chains[26,]
# beta_tri2 <- sobs_mcmc_out$summary$all.chains[29,]
beta_patch <- sobs_mcmc_out$summary$all.chains[22,]
beta_tree <- sobs_mcmc_out$summary$all.chains[25,]
# View Betas
bind_rows(beta_shrub, 
          # beta_shrub2, 
          beta_pfg, 
          # beta_pfg2,
          beta_tri, 
          # beta_tri2,
          beta_patch, beta_tree)

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta0_year1, beta0_year2, beta0_year3,
                                 beta_shrub, 
                                 # beta_shrub2, 
                                 beta_pfg,
                                 # beta_pfg2,
                                 beta_tri, 
                                 # beta_tri2, 
                                 beta_patch, beta_tree)) %>% 
  mutate(Parameter = c("Beta0.Year1", "Beta0.Year2", "Beta0.Year3",
                       "Beta.Shrub", 
                       # "Beta.Shrub2", 
                       "Beta.PFG", 
                       # "Beta.PFG2",
                       "Beta.TRI", 
                       # "Beta.TRI2", 
                       "Beta.Patch", "Beta.Tree")) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CI.lb = X95.CI_low,
         CI.ub = X95.CI_upp)

# View output dataframe
glimpse(beta_dat)
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
  mutate(Pred = seq(from = -3, to = 3, length.out = n))
  
# View the new data 
glimpse(beta_dat_pred)

# 4.3) Plot each predicted value ###########################################

# Shrub cover plot --------------------------------------------------------------------

# Find the mean and sd of shrub cover
# 125m scale
# shrub_cvr_mean <- mean(covs$Shrub.Cover.125m)
# shrub_cvr_sd = sd(covs$Shrub.Cover.125m)
# 1km scale
shrub_cvr_mean <- mean(covs$Shrub.Cover.1km)
shrub_cvr_sd = sd(covs$Shrub.Cover.1km)
# 5km scale
# shrub_cvr_mean <- mean(covs$Shrub.Cover.5km)
# shrub_cvr_sd = sd(covs$Shrub.Cover.5km)

# Plot shrub cover 
shrub_pred_plot <- beta_dat_pred %>% 
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
shrub_pred_plot

# Perennial forbs and grass plot -------------------------------------------------------

# Find the mean and sd of perennial forb and grass cover
# 125m scale
# PFG_cvr_mean <- mean(covs$Perennial.Cover.125m)
# PFG_cvr_sd = sd(covs$Perennial.Cover.125m)
# 1km scale
PFG_cvr_mean <- mean(covs$Perennial.Cover.1km)
PFG_cvr_sd = sd(covs$Perennial.Cover.1km)
# 5km scale
# PFG_cvr_mean <- mean(covs$Perennial.Cover.5km)
# PFG_cvr_sd = sd(covs$Perennial.Cover.5km)

# Plot perennial forb and grass cover
pfg_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the predicted response to PFG cover for all years
    PFG.Pred.Trend =  exp(Beta0.Mean + Beta.PFG.Mean * Pred),
    PFG.Pred.lb = exp(Beta0.lb + Beta.PFG.CI.lb * Pred),
    PFG.Pred.ub =  exp(Beta0.ub +  Beta.PFG.CI.ub * Pred),
    # Transform PFG cover back to the native scale
    Pred.Naive = Pred * PFG_cvr_sd + PFG_cvr_mean
  ) %>% 
  ggplot() +
  geom_line(aes(x = Pred.Naive, y = PFG.Pred.Trend), color = "blue", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = PFG.Pred.lb, ymax = PFG.Pred.ub), 
              alpha = 0.2, fill = "blue") +  
  labs(title = "", x = "Percent Perennial Forb and Grass Cover", y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  ylim(0, 115) +
  theme_classic()

# Display the plot
pfg_pred_plot

# Topographic ruggedness -------------------------------------------------------

# Find the mean and sd of TRI and grass cover
# 125m scaale
# tri_mean <- mean(covs$TRI.125m)
# tri_sd = sd(covs$TRI.125m)
# 1km scaale
# tri_mean <- mean(covs$TRI.1km)
# tri_sd = sd(covs$TRI.1km)
# 5km scaale
tri_mean <- mean(covs$TRI.5km)
tri_sd = sd(covs$TRI.5km)


# Plot topographic ruggedness 
tri_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the predicted response to TRI  for all years
    TRI.Pred.Trend =  exp(Beta0.Mean + Beta.TRI.Mean * Pred),
    TRI.Pred.lb = exp(Beta0.lb + Beta.TRI.CI.lb * Pred),
    TRI.Pred.ub =  exp(Beta0.ub +  Beta.TRI.CI.ub * Pred),
    # Transform TRI cover back to the native scale
    Pred.Naive = Pred * tri_sd + tri_mean
  ) %>% 
  # Can't have negative ruggedness
  filter(Pred.Naive > 0) %>% 
  ggplot() +
  geom_line(aes(x = Pred.Naive, y = TRI.Pred.Trend), color = "blue", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = TRI.Pred.lb, ymax = TRI.Pred.ub), 
              alpha = 0.2, fill = "blue") +  
  labs(title = "", x = "Topographic Ruggedness Index", y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  ylim(0, 75) +
  theme_classic()

# Display the plot
tri_pred_plot

# Shrub patches plot -------------------------------------------------------

# Find the mean and sd of Shrub patches
# 125m scale
# patches_mean <- mean(covs$ln.Shrub.Patch.Size.125m)
# patches_sd = sd(covs$ln.Shrub.Patch.Size.125m)
# 1km scale
# patches_mean <- mean(covs$ln.Shrub.Patch.Size.1km)
# patches_sd = sd(covs$ln.Shrub.Patch.Size.1km)
# 5km scale
patches_mean <- mean(covs$ln.Shrub.Patch.Size.5km)
patches_sd = sd(covs$ln.Shrub.Patch.Size.5km)

# Plot Shrub patches
patch_pred_plot <- beta_dat_pred %>% 
  mutate(
    # Calculate the predicted response to shrub patch size for all years
    Patch.Pred.Trend =  exp(Beta0.Mean + Beta.Patch.Mean * Pred),
    Patch.Pred.lb = exp(Beta0.lb + Beta.Patch.CI.lb * Pred),
    Patch.Pred.ub =  exp(Beta0.ub +  Beta.Patch.CI.ub * Pred),
    # Transform shrub patches back to the native scale (and undo log-transformation)
    Pred.Naive = exp(Pred * patches_sd + patches_mean)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Pred.Naive, y = Patch.Pred.Trend), color = "blue", lwd = 1) + 
  geom_ribbon(aes(x = Pred.Naive, ymin = Patch.Pred.lb, ymax = Patch.Pred.ub), 
              alpha = 0.2, fill = "blue") +  
  labs(title = "", x = "Average Size of Shrub patches (m^2)", y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  ylim(0, 65) +
  theme_classic()

# Display the plot
patch_pred_plot

# Trees Present plot -------------------------------------------------------

# Plot tree presence absence
tree_pred_plot <- beta_dat_pred %>% 
  slice_head(n = 2) %>% 
  select(Beta0.Mean, Beta0.lb, Beta0.ub, Beta.Tree.Mean, Beta.Tree.CI.lb, Beta.Tree.CI.ub) %>% 
  mutate(Pred = c(0, 1)) %>% 
  mutate(
    # Calculate the predicted response to trees for all years
    Tree.Pred.Trend =  exp(Beta0.Mean + Beta.Tree.Mean * Pred),
    Tree.Pred.lb = exp(Beta0.lb + Beta.Tree.CI.lb * Pred),
    Tree.Pred.ub =  exp(Beta0.ub +  Beta.Tree.CI.ub * Pred)
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(Pred), y = Tree.Pred.Trend), col = "blue") +
  geom_errorbar(aes(x = factor(Pred), ymin = Tree.Pred.lb, ymax = Tree.Pred.ub), 
                width = 0.2, color = "lightblue", size = 1) +
  scale_x_discrete(labels = c("0" = "Trees absent", "1" = "Trees present")) +
  labs(title = "", x = "", y = "Predicted Brewer's Sparrow Abundance (birds/km^2)") +
  theme_classic() +
  ylim(0, 65) +
  theme(axis.text = element_text(size = 12)) 

# Display the plot
tree_pred_plot
