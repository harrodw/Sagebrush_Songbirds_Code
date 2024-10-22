#----------------------------------------------------------------
# Will Harrod
# Hierarchical distance sampling for sagebrush songbird point count data
# September 2024
# 
# Model code Modefied from Kery et al. 2024: Integrated distance sampling models for simple point counts 
# Marc Kary, Nicolas Strebel, Tyler Hallman, Kenneth F. Kellner
# August 2024
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
# Define relevant species
soi <- "BRSP"

#define a truncation distance (km)
trunc_dist <- 0.125

# Custom function to find the mode
find_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]  # Find the most frequent value
}

# Make a table of important species sightings by visit
counts_0inf <- sobs %>%
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Distance <= trunc_dist) %>% # only observations closer than the truncation distance
  filter(Species  == soi) %>% # only one species
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  group_by(Grid.ID, Year, Visit.ID, Ord.Date) %>% 
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
  group_by(Grid.ID, Grid.Type, Year, Visit.ID, Ord.Date) %>% 
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
  left_join(counts_0inf, by = c("Grid.ID",
                                "Year", 
                                "Visit.ID", 
                                "Ord.Date")) %>% 
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
                               floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C22")]))),
)

# Bind these to the exisitng data 
counts_temp2 <- bind_rows(counts_temp, new_dat)

# Change necessary variables to scales and factors
counts_temp3 <- counts_temp2 %>%
  # Add covariates
  left_join(covs, by = c("Grid.ID")) %>% 
  # Sort the data
  arrange(Visit.ID, Grid.ID) %>% 
  mutate(
    # Numeric burned vs unburned
    Burned = as.numeric(factor(Grid.Type, levels = c("R", "B"))) - 1,
    # Time since Fire
    Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                 Year == "Y2" ~ 2023 - Fire.Year,
                                 Year == "Y3" ~ 2024 - Fire.Year)) %>%
  # Change the wind data to a factor with set levels
  mutate(Wind = case_when(is.na(Wind) ~ 'Unknown',
                          TRUE ~ Wind)) %>% 
  mutate(Wind = factor(Wind, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                        "8-12 mph", "13-18 mph", "Unknown"))) %>%
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID",
              "Observer.ID", "Year", "Aspect"
  ), factor) %>% 
  # Remove columns that are no longer needed
  dplyr::select(-Grid.Type) 

#...and view
glimpse(counts_temp3)

# Isolate the burned Grids as their own object so I can acurately scale them
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
  mutate(Sage.Cover = scale(Sage.Cover)[,1],
         Perennial.Cover = scale(Perennial.Cover)[,1],
         Elevation = scale(Elevation)[,1],
         Mean.MAS = scale(Mean.MAS)[,1],
         Ord.Date = scale(Ord.Date)[,1],
         Years.Since.Fire = (Years.Since.Fire - mean_burnY) / sd_burnY,
         mean.rdnbr = (mean.rdnbr - mean_rdnbr) / sd_rdnbr)

# 1.3) Prepare the observation level data ################################################################

# Define a bin size
bin_size <- 0.025

# Create an object with all observations of a single species for the detection function
observations_temp <- sobs %>% 
  mutate(Distance = Distance / 1000) %>%      # Switch from m to km
  filter(Species == soi) %>%                  # Only one species
  arrange(Year, Visit, Grid.ID) %>%           # Same order as the others
  filter(Distance <= trunc_dist) %>%          # Only close observations
  mutate(
    # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
    Observer.ID = case_when(Observer.ID == "Alex" ~ "Ben", TRUE ~ Observer.ID),
    # A unique ID for each visit
    Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  dplyr::select(Distance, Minute, 
                Grid.ID, Observer.ID, Year, Visit.ID) %>% 
  left_join(covs, by = "Grid.ID") %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  # Add in the bin sizes
  mutate(Dist.Bin = floor(Distance / bin_size)* bin_size) %>% 
  # Name the first and last distance bin
  mutate(Dist.Bin = case_when(Dist.Bin == 0 ~ 0.025,
                              TRUE ~ Dist.Bin)) %>% 
  # Calculate Midpoints
  mutate(Dist.Bin.Midpoint = Dist.Bin - bin_size/2) %>% 
  # Change to a factor
  mutate(Dist.Bin = case_when(Dist.Bin == 0.025 ~ 1,
                              Dist.Bin == 0.050 ~ 2,
                              Dist.Bin < 0.1 & Dist.Bin >= 0.075 ~ 3, # idk why it needs this specificity. It just does
                              Dist.Bin == 0.100 ~ 4,
                              Dist.Bin == 0.125 ~ 5
  )) %>% 
  # Asign each observation to a two minute time interval
  mutate(Time.Interval = case_when(Minute %in% c(1, 2) ~ 1,
                                   Minute %in% c(3, 4) ~ 2,
                                   Minute %in% c(5, 6) ~ 3)) %>% 
  dplyr::select(Grid.ID, Observer.ID, Dist.Bin, Dist.Bin.Midpoint, Time.Interval, Year, Visit.ID)

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

# View Observations
glimpse(observations)

# Plot a specific covariate against bird abundance
# counts %>% 
#   ggplot(aes(x = Sage.Cover, y = Count)) +
#   geom_smooth() +
#   geom_point()

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
# Build a storage matrix for the number of points visited during each survey
points_mat <- matrix(NA, nrow = nrows, ncol = ncols)
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
  points_mat[y,] <- count_visit$n.Points
  obsv_mat[y,] <- count_visit$Observer.ID.num
}

# View the matrices
count_mat  # Number of individuals recorded at each visit
year_mat   # Year during which each visit took place
time_mat   # Scaled mean time of day for each visit
date_mat   # Scaled date for each visit
points_mat # Number of points visited during each survey 
obsv_mat   # Who conducted each survey

# Loop sizes 
ngrids <- length(unique(counts$Grid.ID.num))          # Number of survey grids
nind <- nrow(observations)                            # Number of individuals detected 
nobsv <- length(unique(counts$Observer.ID.num))       # Number of unique observers
nbins <- length(unique(observations$Dist.Bin))        # Number of distance bins
nints <- length(unique(observations$Time.Interval))   # Number of time intervals
nyears <- length(unique(counts$Year))                 # Number of Years we surveyed
nvst <- length(unique(counts$Visit.ID))               # Number of visits in each year

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
ncap <- count_mat                                     # Matrix of the number of detected individuals per grid per survey 
year <- year_mat                                      # Matrix of year numbers
sage_cvr <- counts$Sage.Cover[1:length(unique(counts$Grid.ID))]        # Percent sagebrush cover
pern_cvr <- counts$Perennial.Cover[1:length(unique(counts$Grid.ID))]   # Percent perennial forb and grass cover4
tree_cvr <- counts$Tree.Cover[1:length(unique(counts$Grid.ID))]        # Number of 30m cells with trees
elevation <- counts$Elevation[1:length(unique(counts$Grid.ID))]        # Elevation (m)                             

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
sobs_const <- list (
  ## Misc. Constants
  area = points_mat,       # Number of points surveyed per grid per visit
  delta = bin_size,        # Bin width
  trunc_dist = trunc_dist, # Truncation distance
  
  #For loop sizes
  ngrids = ngrids,         # Number of survey grids
  nind = nind,             # Number of individuals detected 
  nobsv = nobsv,           # Number of unique observers
  nyears = nyears,         # Number of years we surveyed
  nvst = nvst,             # Number of times each grid was surveyed (6)
  nbins = nbins,           # Number of distance bins
  nints = nints,           # Number of time intervals
  
  # Random effects
  observers = observers,   # Random effect for observer associated with each survey
  year = year_mat,         # Year number (1-3)
  
  # Non-stochastic constants
  obs_visit  = obs_visit,  # year when each observation took place
  obs_grid  = obs_grid     # Grid of each observation 
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
  
  #Point level data
  ncap = ncap,            # Number of detected individuals per site
  sage_cvr = sage_cvr,    # Scaled percent sagebrush cover
  pern_cvr = pern_cvr,    # Perennial forb and grass cover
  elevation = elevation,  # Scaled elevation 
  time = time,            # Scaled mean time after sunrise 
  day = day               # Scaled date 
)
# View Nimble data 
str(sobs_dat)

#set seed
set.seed(123)

#Object dimensions
sobs_dims <- list(
  g = c(ngrids, nvst, nbins),        # Cell prob x radial distance
  f = c(ngrids, nvst, nbins),        # Cell prob x radial distance
  sigma = c(ngrids, nvst),           # Linear combination on detectability
  pi_pd = c(ngrids, nvst, nbins),    # Detection probability in each cell
  pi_pd_c = c(ngrids, nvst, nbins),  # Proportion of total detection probability in each cell
  p_cap = c(ngrids, nvst),           # Detection probability 
  p_a = c(ngrids, nvst),             # Linear combination on avalibility  
  pi_pa =  c(ngrids, nvst, nints),   # Availability cell prob in each time interval
  pi_pa_c = c(ngrids, nvst, nints),  # Proportion of total availability probability in each cell
  phi = c(ngrids, nvst),             # Overall Availability
  FTD = c(ngrids , nvst),            # Freeman Tukey Discrepancy
  FTD_new = c(ngrids, nvst),         # New Freeman Tukey Discrepancy
  phi = c(ngrids, nvst)              # Availability probability
)

# 2.3) Model definition ################################################################
# Add random noise in the detection function intercepts
# Note all distances are in units of 1km (and area in 1km2 units)

# Model code
sobs_model_code <- nimbleCode({
  # Priors for all parameters 
  # ------------------------------------------------------------------
  
  # Parameters in the availability component of the detection model
  gamma0 ~ dnorm(0, 10)         # Mean availability
  gamma_date ~ dnorm(0, 10)     # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, 10)    # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, 10)     # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, 10)    # Effect of time of day on singing rate (quadratic)
  
  # Parameters in the detection portion of the model
  
  # offset for each observer 
  for(o in 1:nobsv) {     # Loop over all observers
    alpha_obsv[o] ~  dnorm(0, 10)    
  }
  
  # Random intercept for each year
  mean_beta0 ~ dnorm(0, 10)     # Truncated so that initial value is within bounds
  sd_beta0 ~ dunif(0, 5)        # Magnitude of that noise on lambda intercept 
  # Random intercept for each year
  for(y in 1:nyears) {     # Loop over 3 years
    alpha0_year[y] ~ dnorm(mean_beta0, sd_beta0)
  }
  
  # Covariates on abundance:
  beta0 ~ dnorm(0, 10)          # Intercept on abundance
  beta_sage ~ dnorm(0, 10)      # Effect of sagebrush cover
  beta_sage2 ~ dnorm(0, 10)     # Effect of sagebrush cover squared
  beta_pern ~ dnorm(0, 10)      # Effect of Perennial Cover
  beta_pern2 ~ dnorm(0, 10)     # Effect of perennial cover squared
  beta_elv ~ dnorm(0, 10)       # Effect of elevation
  beta_elv2 ~ dnorm(0, 10)      # Effect of elevation squared
  
  # -------------------------------------------------------------------
  # Hierarchical construction of the likelihood
  
  # Iterate over all of the visits to each survey grid
  for(y in 1:nvst){ 
    
    # Loop over all survey grids
    for(s in 1:ngrids){
      
      # Construction of the cell probabilities for the nbins distance bands
      for(b in 1:nbins){       
        log(g[s, y, b]) <- - (midpt[b] * midpt[b]) / (2 * sigma[s, y] * sigma[s, y]) # Half-normal detection function
        f[s, y, b] <- ((2 * midpt[b] ) / trunc_dist^2) * delta                       # Prob density function out to max truncation distance 
        pi_pd[s, y, b] <- g[s, y, b] * f[s, y, b]                                    # Detection cell probability
        pi_pd_c[s, y, b] <- f[s, y, b] / p_cap[s, y]                                 # Proportion of total probability in each cell probability
      }
      # Rectangular integral approx. of integral that yields the Pr(capture)
      p_cap[s, y] <- sum(pi_pd[s, y, ])
      
      # Construction of the cell probabilities for the nints time intervals
      for (k in 1:nints){
        pi_pa[s, y, k] <- p_a[s, y] * (1 - p_a[s, y])^(k-1) # Availability cell probability
        pi_pa_c[s, y, k] <- pi_pa[s, y, k] / phi[s, y]      # Proportion of availability probability in each time interval class
      }
      
      # Rectangular integral approx. of integral that yields the Pr(available)
      phi[s, y] <- sum(pi_pa[s, y, ])
      
      # Log-linear models on abundance, detectability, and availability
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[s, y]) <- alpha0_year[year[s, y]] +         # Random intercept on detectability
        alpha_obsv[observers[s, y]]       # Effect of each observer
      
      # Availability (p_a) Log-linear model for availability
      logit(p_a[s, y]) <- gamma0 +                          # Intercept on availability 
        gamma_date * day[s, y] +          # Effect of scaled ordinal date 
        gamma_date2 * day[s, y]^2 +       # Effect of scaled ordinal date squared 
        gamma_time * time[s, y] +         # Effect of scaled time of day 
        gamma_time2 * time[s, y]^2        # Effect of scaled time of day squared 
      
      # Abundance (lambda) Log-linear model 
      log(lambda[s, y]) <- beta0 +                          # Intercept on abundance
        beta_sage * sage_cvr[s] +        # Effect of sagebrush cover
        beta_sage2 * sage_cvr[s]^2 +     # Effect of sagebrush cover squared
        beta_pern *  pern_cvr[s] +       # Effect of Perennial Cover
        beta_pern2 * pern_cvr[s]^2 +     # Effect of perennial cover squared
        beta_elv * elevation[s] +        # Effect of elevation
        beta_elv2 * elevation[s]^2       # Effect of elevation squared 
      
      # Multiply availability with capture probability to yield total detection probbability
      p_dct[s, y] <- p_cap[s, y] * phi[s, y]
      
      ### Binomial mixture part (in conditional multinomial specification)
      ncap[s, y] ~ dbin(p_dct[s, y], N_indv[s, y])                            # Number of individual birds captured
      N_indv[s, y] ~ dpois(area[s, y] * lambda[s, y] * (present[s] + 0.0001)) # Area (Number of points per survey) used as an offset 
      
      # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
      # Compute fit statistic for observed data 
      eval[s, y] <- p_dct[s, y] * N_indv[s, y] 
      FTD[s, y] <- (sqrt(ncap[s, y]) - sqrt(eval[s, y]))^2 
      
      # Generate replicate count data and compute same fit stats for them
      ncap_new[s, y] ~ dbin(p_dct[s, y], N_indv[s, y]) 
      FTD_new[s, y] <- (sqrt(ncap_new[s, y]) - sqrt(eval[s, y]))^2
      
    } # end loop through grids
  } # end loop through visits
  
  # Model for binned distance observations of every detected individual
  for(i in 1:nind){       # Loop over all detected individuals
    dclass[i] ~ dcat(pi_pd_c[obs_grid[i], obs_visit[i], ]) # linking distance class data
    tint[i] ~ dcat(pi_pa_c[obs_grid[i], obs_visit[i], ])   # linking time removal data
  } # end observation loop
  
  # Zero-inflation component on abundance
  for(s in 1:ngrids){
    psi[s] ~ dunif(0.0001, 0.9999)                       # Occupancy probability
    present[s] ~ dbern(psi[s])                           # Number of grids where that individual can be present
    mean_N_indv[s] <- mean(N_indv[s,])                   # Average true abundance across visits 
  } # end zero-inflation loop
  
  # Averages across grids and years
  mean_psi <- mean(psi[])                                  # Average occupancy probability
  mean_phi <- mean(phi[,])                                 # Average availability probability
  mean_p_cap <- mean(p_cap[,])                             # Average detection probability
  
  # Add up fit stats across sites and years
  fit <- sum(FTD[,]) 
  fit_new <- sum(FTD_new[,]) 
  
  # Compute Bayesian p-value for detection probability
  bpv <- step(fit_new - fit)
  
})

# 2.4) Configure and Run the model ###########################################################

# Params to save
sobs_params <- c("alpha0_year",
                 "alpha_obsv",
                 "mean_N_indv",
                 "mean_psi",
                 "mean_p_cap",
                 "mean_phi",
                 "gamma0", 
                 "gamma_date", 
                 "gamma_date2", 
                 "gamma_time", 
                 "gamma_time2", 
                 "beta0",
                 "beta_sage", 
                 "beta_pern", 
                 "beta_elv", 
                 "beta_sage2", 
                 "beta_pern2", 
                 "beta_elv2",
                 "fit", 
                 "fit_new", 
                 "bpv")

# Initial Values
sobs_inits <- list(
  # Detecability 
  alpha0_year = rnorm(nyears, 0, 5),
  alpha_obsv = rnorm(nobsv, 0, 0.5),
  # availability 
  gamma0 = runif(1, 0.1, 0.9),
  gamma_date = rnorm(1, 0, 0.5), 
  gamma_time = rnorm(1, 0, 0.5),
  gamma_date2 = rnorm(1, 0, 0.5),  
  gamma_time2 = rnorm(1, 0, 0.5),
  # Abudance 
  sd_beta0 = runif(1, 0, 1),
  mean_beta0 = runif(1, 0, 30),
  beta0 = rnorm(1, 0, 0.5),
  beta_sage = rnorm(1, 0, 0.5),
  beta_sage2 = rnorm(1, 0, 0.5),
  beta_pern = rnorm(1, 0, 0.5),
  beta_pern2 = rnorm(1, 0, 0.5),
  beta_elv = rnorm(1, 0, 0.5),
  beta_elv2 = rnorm(1, 0, 0.5),
  # Presence 
  psi = runif(ngrids, 0.1, 0.9),
  present = rbinom(ngrids, 1, 0.8),
  # Data and Simulated data
  ncap_new = count_mat,  # Initialize the new capture data at the existing data values
  N_indv = count_mat + 1 # start each grid with an individual present
)  
# View the initial values
str(sobs_inits)

# MCMC settings. Pick one, comment out the rest 
# nc <- 3  ;  ni <- 50  ;  nb <- 0  ;  nt <- 1          # Quick test to see if the model runs
# nc <- 3  ;  ni <- 30000  ;  nb <- 10000  ;  nt <- 1   # longer test where some things might converge
nc <- 4;  ni <- 150000;  nb <- 50000;  nt <- 1        # Run the model for real

#Run the sampler
start <- Sys.time() #start time for the sampler
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
                            setSeed = 123,
                            samples = TRUE,
                            summary = TRUE)
difftime(Sys.time(), start) # end time for the sampler

# Save model output to local drive
saveRDS(sobs_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     soi, "_distance_model_out.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

# Load the output back in
sobs_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       soi, "_distance_model_out.rds"))

# Define which parameters I want to view
sobs_params_view <- c(
  # "mean_N_indv",
  "mean_psi",
  "mean_p_cap",
  "mean_phi",
  "alpha0_year",
  "alpha_obsv",
  "gamma0", 
  "gamma_date", 
  "gamma_date2", 
  "gamma_time", 
  "gamma_time2", 
  "beta0",
  "beta_sage", 
  "beta_pern", 
  "beta_elv", 
  "beta_sage2", 
  "beta_pern2", 
  "beta_elv2",
  "fit",
  "fit_new",
  "bpv")

# View MCMC summary
MCMCsummary(object = sobs_mcmc_out$samples, 
            params = sobs_params_view,
            round = 2)

# View MCMC plot
MCMCplot(object = sobs_mcmc_out$samples,
         params = sobs_params_view)

# Traceplots and density graphs 
MCMCtrace(object = sobs_mcmc_out$samples,
          pdf = FALSE,
          ind = TRUE,
          params = sobs_params_view,
          iter = 10000)
