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
library(ggcorrplot)

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
  "SABS",
  # "GTTO",
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
  

# Scale each covariate
sobs_counts <- sobs_counts_temp2 %>% 
  mutate(Mean.MAS.scl = scale(Mean.MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1],
         Shrub.Cover.scl = scale(Shrub.Cover.125m)[,1],
         PFG.Cover.scl = scale(Perennial.Cover.125m)[,1],
         AFG.Cover.scl = scale(Annual.Cover.125m)[,1],
         BG.Cover.scl = scale(Bare.Ground.Cover.125m)[,1],
         Elevation.scl = scale(Elevation.125m)[,1],
         TRI.scl = scale(TRI.125m)[,1])
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

# Select variables for the model
sobs_cor <- sobs_counts %>%
  filter(Burned == 1) %>%
  select(Shrub.Cover.scl, PFG.Cover.scl, AFG.Cover.scl, BG.Cover.scl, TRI.scl) %>%
  distinct() %>%
  cor()

# P-value correlations
p_mat <- cor_pmat(sobs_cor)
p_mat

# Plot correlations
ggcorrplot(sobs_cor,
           title = "Correlation Matrix for Pre-Fire Vegetation Data",
           lab = TRUE,
           lab_size = 4,
           tl.cex = 10,
           p.mat = p_mat,
           type = "lower",
           method = "square",
           sig.level = 0.05,
           colors = c("red", "white", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1)
)

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
ntrts <- length(unique(sobs_counts$Treatment))             # Number of grid types

# Observation Level data 
midpt <- sort(unique(sobs_observations$Dist.Bin.Midpoint)) # Midpoints of distance bins (n = 5)
observers <- obsv_mat                                      # Random effect for observer associated with each survey
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
shrub_cvr <- sobs_counts$Shrub.Cover.scl[1:ngrids]         # Shrub cover 
pfg_cvr <- sobs_counts$PFG.Cover.scl[1:ngrids]             # Perennial forb and grass cover
tri <- sobs_counts$TRI.scl[1:ngrids]                       # Topographic ruggedness
trts <- sobs_counts$Treatment[1:ngrids]                    # Treatment type of each grid 

# n <- nrow(sobs_counts)
# size1 <- 5
# size2 <- 10
# size3 <- 20
# p1 <- 0.1
# 
# data.frame(nb1 = rnbinom(n = n, size = size1, p = p1),
#            nb2 = rnbinom(n = n, size = size2, p = p1),
#            nb3 = rnbinom(n = n, size = size3, p = p1),
#            pois = rpois(n = n, lambda = size1),
#            counts = sobs_counts$Count) %>%
#   ggplot() +
#   geom_histogram(aes(x = pois), fill = "red", alpha = 0.6) +
#   geom_histogram(aes(x = nb1), fill = "lightblue", alpha = 0.6) +
#   geom_histogram(aes(x = nb2), fill = "purple", alpha = 0.6) +
#   geom_histogram(aes(x = nb3), fill = "yellow", alpha = 0.6) +
#   geom_histogram(aes(x = counts) , fill = "green", alpha = 0.6)
# 
# data.frame(b1 = rbeta(n = n, shape1 = 1.3, shape2 = 1.3),
#            b2 = rbeta(n = n, shape1 = 0.7, shape2 = 1),
#            b3 = rbeta(n = n, shape1 = 0.9, shape2 = 1.3)) %>% 
#   ggplot() +
#   geom_histogram(aes(x = b1), fill = "red", alpha = 0.6) +
#   geom_histogram(aes(x = b2), fill = "lightblue", alpha = 0.6) +
#   geom_histogram(aes(x = b3) , fill = "green", alpha = 0.6)

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Model definition ################################################################

# Model code
sobs_model_code <- nimbleCode({
 
  # ------------------------------------------------------------------
  # Priors for all parameters 
  
  # Parameters in the detection portion of the model ----
  # Fixed effects on detecability
  
  # Effect of observer experience (Can't be greater than 0)
  for(o in 1:nobsv){ # nobsv = 17
    alpha0_obsv[o] ~ T(dnorm(-2, sd = 3), -9, 0) # Prior mean centered on approximately sigma = exp(-2) = 0.125km
  } # End loop through observers
  
  # Parameters in the availability component of the detection model ----
  # Fixed effects on avaiablility
  gamma0 ~ dnorm(0, sd = 3)           # Availability intercept
  gamma_date ~ dnorm(0, sd = 1.5)     # Effect of day of year on singing rate
  gamma_date2 ~ dnorm(0, sd = 1.5)    # Effect of day of year on singing rate (quadratic)
  gamma_time ~ dnorm(0, sd = 1.5)     # Effect of time of day on singing rate
  gamma_time2 ~ dnorm(0, sd = 1.5)    # Effect of time of day on singing rate (quadratic)
  
  # Parameters on the abundance component of the model ----
  
  # Single fixed effects on abundance 
  beta0 ~ dnorm(0, sd = 3)
  beta_shrub ~ dnorm(0, sd = 1.5) # Effect of shrub cover
  beta_pfg ~ dnorm(0, sd = 1.5)   # Effect of perennial forb and grass
  beta_tri ~ dnorm(0, sd = 1.5)   # Effect of ruggedness
  
  # # Abundance random effect hyper-parameters
  sd_eps_year ~ dgamma(shape = 0.5, scale = 0.5) # Random effect on abundance hyperparameter for each year

  # Random noise among the other years
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
      
      ### Imperfect availability portion of the model ###
      
      # Construction of the cell probabilities for the nints time intervals
      for (t in 1:nints){
        pi_pa[j, k, t] <- phi[j, k] * (1 - phi[j, k])^(t - 1)  # Availability cell probability
        pi_pa_c[j, k, t] <- pi_pa[j, k, t] / p_a[j, k]         # Proportion of availability probability in each time interval class
      }
      # Rectangular integral approx. of integral that yields the Pr(available)
      p_a[j, k] <- sum(pi_pa[j, k, ])
      
      ### Binomial portion of mixture ###
      
      # Number of individual birds detected (observations)
      n_dct[j, k] ~ dbin(p_d[j, k], n_avail[j, k]) 
      
      # Number of birds avaiable
      n_avail[j, k] ~ dbin(p_a[j, k], N_indv[j, k]) 
      
      # Poisson abundance portion of mixture
      N_indv[j, k] ~ dpois(lambda[j, k] * area[j, k]) # ZIP true abundance at site j during visit k
      # * (present[j, k] + 0.0001)
      
      # Availability (phi) Logit-linear model for availability
      logit(phi[j, k]) <- gamma0 +                       # Intercept on availability
                          gamma_date * day[j, k] +       # Effect of scaled ordinal date
                          gamma_time * time[j, k] +      # Effect of scaled time of day
                          gamma_date2 * day[j, k]^2 +    # Effect of scaled ordinal date squared
                          gamma_time2 * time[j, k]^2     # Effect of scaled time of day squared
      
      # Detectability (sigma) Log-Linear model 
      log(sigma[j, k]) <- alpha0_obsv[observers[j, k]]   # Intercept for of observer experience on detectability
        
      # Abundance (lambda) Log-linear model 
      log(lambda[j, k]) <- beta0 +                       # Intercept for each grid type
                           beta_shrub * shrub_cvr[j] +   # Effect of shrub cover
                           beta_pfg * pfg_cvr[j] +       # Effect of perennial cover  
                           beta_tri * tri[j] +           # Effect of ruggedness
                           eps_year[years[j, k]]         # Unexplained noise on abundance by year

      # -------------------------------------------------------------------------------------------------------------------
      # Assess model fit: compute Bayesian p-value for using a test statisitcs
      
      # Chi square statisitc for the availability portion of the model
      e_pa[j, k] <- p_a[j, k] * N_indv[j, k]                                      # Expected value for availability binomial portion of the model
      n_avail_new[j, k] ~ dbin(p_a[j, k], N_indv[j, k])                           # Draw new available birds from the same binomial
      Chi_pa[j, k] <- (n_avail[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5)         # Compute availability chi squared statistic for observed data
      Chi_pa_new[j, k] <- (n_avail_new[j, k] - e_pa[j, k])^2 / (e_pa[j, k] + 0.5) # Compute availability chi squared statistic for simulated data data
      
      # Chi square statisitc for the detection portion of the model
      e_pd[j, k] <- p_d[j, k] * n_avail[j, k]                                     # Expected value for detection binomial portion of the model
      n_dct_new[j, k] ~ dbin(p_d[j, k], n_avail[j, k])                            # Draw new detections from the same binomial
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
  nind = nind,                 # Number of individuals detected 
  nobsv = nobsv,               # Number of unique observers
  nvst = nvst,                 # Number of times each grid was surveyed (6)
  nbins = nbins,               # Number of distance bins
  nints = nints,               # Number of time intervals
  nyears = nyears,             # Number of years we surveyed (3)
  # ntrts = ntrts,               # Number of grid types (elevation x burned)
  # Non-stochastic constants
  # trts = trts,                 # Type of grid 
  years = years,               # Year when each survey took place
  obs_visit  = obs_visit,      # Visit when each observation took place
  obs_grid  = obs_grid,        # Grid of each observation 
  observers = observers        # Effect of observer associated with each survey
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
  shrub_cvr = shrub_cvr,       # Shrub cover at each grid
  pfg_cvr = pfg_cvr,           # Perennial cover at each grid
  tri = tri                    # Ruggedness  at each grid
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
  alpha0_obsv = runif(nobsv, -2, -0.1),   # Effect of each observer on detecability
  # Availability 
  gamma0 = rnorm(1, 0, 0.1),              # Intercept on availability
  gamma_date = rnorm(1, 0, 0.1),          # Effect of date on availability
  gamma_time = rnorm(1, 0, 0.1),          # Effect of time of day on availability
  gamma_date2 = rnorm(1, 0, 0.1),         # Effect of date on availability (quadratic)
  gamma_time2 = rnorm(1, 0, 0.1),         # Effect of time of day on availability (quadratic)
  # Abundance 
  beta0 = rnorm(1, 0, 0.1),               # Intercept
  beta_shrub = rnorm(1, 0, 0.1),          # Effect of shrub cover
  beta_pfg = rnorm(1, 0, 0.1),            # Effect of perennial cover
  beta_tri = rnorm(1, 0, 0.1),            # Effect of ruggedness
  sd_eps_year = runif(1, 0, 1),           # Magnitude of random noise (only positive)
  eps_year = rep(0, nyears),              # Random noise on abundance by year
  # Presence 
  # psi_trt = runif(ntrts, 0.4, 0.6),       # Probability of each grid being occupied for zero inflation
  # present = matrix(rbinom(ngrids*nvst, 1, 0.8), ngrids, nvst), # Binary presence absence for zero-inflation
  # Simulated counts
  n_avail = count_mat + 1,                # Number of available birds (helps to start each grid with an individual present)
  n_dct_new = count_mat,                  # Simulated detected birds 
  n_avail_new = count_mat + 1,            # Simulated available birds (helps to start each grid with an individual present)
  N_indv = count_mat + 1                  # "True" abundance (helps to start each grid with an individual present)
)
# View the initial values
str(sobs_inits)

# Params to save
sobs_params <- c(
  "fit_pd",          # Fit statistic for observed data
  "fit_pd_new",      # Fit statisitc for simulated detection  data
  "fit_pa",          # Fit statistic for first availability data
  "fit_pa_new",      # Fit statisitc for simulated availability data
  "beta0",           # Intercept
  "beta_shrub",      # Effect of shrub cover
  "beta_pfg",        # Effect of perennial cover
  "beta_tri",        # Effect of ruggedness
  # "psi_trt",         # Occupanci probability by grid type    
  "sd_eps_year",     # Random noise on abundance by year
  "gamma0",          # Intercept on availability
  "gamma_date",      # Effect of date on singing rate
  "gamma_date2",     # Quadratic effect of date on singing rate
  "gamma_time",      # Effect of time of day on singing rate
  "gamma_time2",     # Quadratic e of time of day on singing rate
  "alpha0_obsv"      # Intercept for each observer on detection rate
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
  # Fixed effects
  "beta0", "beta_shrub", "beta_pfg", "beta_tri",
  # Random noise by year
  "eps_year[2]", "eps_year[3]"
  )
sobs_mcmcConf$addSampler(target = c(
  # Fixed effects
  "beta0", "beta_shrub", "beta_pfg", "beta_tri",
  # Random noise by year
  "eps_year[2]", "eps_year[3]"
  ), type = 'RW_block')

# # Block all occupancy (psi) nodes together
# sobs_mcmcConf$removeSamplers(
#   # Occupancy prob by grid type
#   "psi_trt[1]", "psi_trt[2]", "psi_trt[3]", "psi_trt[4]"
# )
# sobs_mcmcConf$addSampler(target = c(
#   # Occupancy prob by grid type
#   "psi_trt[1]", "psi_trt[2]", "psi_trt[3]", "psi_trt[4]"
# ), type = 'RW_block')

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
hab_mcmc_out<- runMCMC(cMCMC,
                  niter = ni, 
                  nburnin = nb, 
                  thin = nt, 
                  nchains = nc,
                  samplesAsCodaMCMC = T,
                  summary = T)
difftime(Sys.time(), start)               # End time for the sampler

# Save model output to local drive
saveRDS(hab_mcmc_out, file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                     model_species, "_habitat_model.rds"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

# Load the output back in
hab_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//", 
                                       model_species, "_habitat_model.rds"))
  
# Traceplots and density graphs 
MCMCtrace(object = hab_mcmc_out$samples,
          params = sobs_params,
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files",
          filename = paste0(model_species, "_habitat_model_traceplot"),
          type = 'both')

# View MCMC summary
MCMCsummary(object = hab_mcmc_out$samples, 
            params = sobs_params,
            round = 2)

# View MCMC plot
MCMCplot(object = hab_mcmc_out$samples,
         excl = c("fit_pa", "fit_pa_new", "fit_pd", "fit_pd_new", "beta0"),
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
library(gridExtra)
library(ggpubr)
library(grid)

# 4.2) Graph each species response to fire ###############################################################

# List of Species to plot 
all_plot_species <- c(
  "SATH",
  "BRSP",
  "SABS",
  "VESP",
  "WEME",
  "HOLA"
)

# Add veg covariates
covs <- tibble(read.csv("Data/Outputs/grid_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# View covariates
glimpse(covs)

# Calculate covariate means and sd's
shrub_mean <- mean(covs$Shrub.Cover.125m)
shrub_sd <- sd(covs$Shrub.Cover.125m)
pfg_mean <- mean(covs$Perennial.Cover.125m)
pfg_sd <- sd(covs$Perennial.Cover.125m)
tri_mean <- mean(covs$TRI.125m)
tri_sd <- sd(covs$TRI.125m)

# Loop over all species 
for(s in 1:length(all_plot_species)) { # (Comment this out) ----

# Name the species to model again
plot_species <- all_plot_species[s]
# plot_species <- all_plot_species[3]

# Data frame for naming species
plot_species_df <- data.frame(Species.Code = plot_species) %>% 
  mutate(Species.ID = case_when(Species.Code == "SATH" ~ "(A) Sage Thrasher",
                                  Species.Code == "BRSP" ~ "(B) Brewer's Sparrow", 
                                  Species.Code == "SABS" ~ "(C) Sagebrush Sparrow",
                                  Species.Code == "VESP" ~ "(D) Vesper Sparrow",
                                  Species.Code == "WEME" ~ "(E) Western Meadowlark",
                                  Species.Code == "HOLA" ~ "(F) Horned Lark"),
         Species.Name = case_when(Species.Code == "SATH" ~ "Sage Thrasher",
                                Species.Code == "BRSP" ~ "Brewer's Sparrow", 
                                Species.Code == "SABS" ~ "Sagebrush Sparrow",
                                Species.Code == "VESP" ~ "Vesper Sparrow",
                                Species.Code == "WEME" ~ "Western Meadowlark",
                                Species.Code == "HOLA" ~ "Horned Lark")
         )
# Extract names
species_id <- plot_species_df$Species.ID
species_name <- plot_species_df$Species.Name

# View
species_id
species_name

# Load the output back in
hab_mcmc_out <- readRDS(file = paste0("C://Users//willh//Box//Will_Harrod_MS_Project//Model_Files//",
                                       plot_species, "_habitat_model.rds"))

# View MCMC summary
hab_mcmc_out$summary$all.chains

# Extract effect sizes
beta_shrub <- hab_mcmc_out$summary$all.chains[20,]
beta_pfg <- hab_mcmc_out$summary$all.chains[19,]
beta_tri <- hab_mcmc_out$summary$all.chains[21,]

# View Betas
bind_rows(beta_shrub,
          beta_pfg,
          beta_tri)

# Combine everything into a dataframe
beta_dat <- data.frame(bind_rows(beta_shrub,
                                 beta_pfg,
                                 beta_tri)
                                 ) %>% 
  mutate(Parameter = c("Shrub Cover", "Perennial Cover", "Ruggedness"
                       )) %>% 
  relocate(Parameter, .before = Mean) %>% 
  rename(CRI.lb = X95.CI_low,
         CRI.ub = X95.CI_upp)

# View output dataframe
head(beta_dat, n = Inf)

# Parameter estimate plot -------------------------------------------------------------------------

# Create the plot
params_plot <- beta_dat %>% 
  # Add a New column for whether or not the CRI crosses Zero
  mutate(Significant = factor(case_when(CRI.lb * CRI.ub <= 0 ~ "No",
                                        CRI.lb * CRI.ub > 0 ~ "Yes"), 
                              levels = c("No", "Yes"))) %>% 
  # Switch to a factor 
  mutate(Parameter = factor(Parameter, levels = c("Ruggedness",
                                                  "Perennial Cover", 
                                                  "Shrub Cover"
                                                  ))) %>% 
  # Open the plot
  ggplot(aes(y = Parameter)) +
  # Add points at the mean values for each parameters
  geom_point(aes(x = Mean, color = Significant), shape = 15, size = 4) +
  # Add whiskers for 95% Bayesian Credible intervals
  geom_linerange(aes(xmin = CRI.lb, xmax = CRI.ub, color = Significant), linewidth = 1.5) +
  # Add a vertical Line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  # Change the Labels
  labs(x = "Parameter Estimate", y = "", title = species_id) + 
  # Simple theme
  theme_classic() +
  # Custom colors
  scale_color_manual(values = c("No" = "lightsteelblue4", 
                                "Yes" = "navyblue")) +
  # Edit theme
  theme(legend.position = "none",
        plot.title = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16)) 


# View the plot
# params_plot

# Save the plot as a png
ggsave(plot = params_plot,
       paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                                       plot_species, "_params.png"),
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

# save the plot as an RDS
saveRDS(object = params_plot,
        file = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                      plot_species, "_params.rds"))

# Predicted abundance #######################################################################################

# Extract the intercept
beta0 <- hab_mcmc_out$summary$all.chains[18,]

# Pick a number of samples to predict over
n_samples <- 1000

# Make a new dataframe for the prediction pots
preds <- tibble(Pred = seq(from = -2, to = 2., length.out = n_samples),
                # Intercept
                Beta0.mean = rep(beta0[1], n_samples), 
                Beta0.lb = rep(beta0[4], n_samples), 
                Beta0.ub = rep(beta0[5], n_samples),
                # Effect of shrub cover
                Beta.Shrub.mean = rep(beta_shrub[1], n_samples), 
                Beta.Shrub.lb = rep(beta_shrub[4], n_samples),
                Beta.Shrub.ub = rep(beta_shrub[5], n_samples),
                # Effect of perennial cover
                Beta.PFG.mean = rep(beta_pfg[1], n_samples), 
                Beta.PFG.lb = rep(beta_pfg[4], n_samples), 
                Beta.PFG.ub = rep(beta_pfg[5], n_samples),
                # Effect of ruggedness
                Beta.TRI.mean = rep(beta_tri[1], n_samples), 
                Beta.TRI.lb = rep(beta_tri[4], n_samples), 
                Beta.TRI.ub = rep(beta_tri[5], n_samples))
# View
# glimpse(preds) 

# Maximum of based on shrub
min_shrub <- exp(beta0[5] + beta_shrub[4]*-2)
max_shrub <- exp(beta0[5] + beta_shrub[5]*2)
# Maximum of based on pfg
min_pfg <- exp(beta0[5] + beta_pfg[4]*-2)
max_pfg <- exp(beta0[5] + beta_pfg[5]*2)
# Maximum of based on tri
min_tri <- exp(beta0[5] + beta_tri[4]*-2)
max_tri <- exp(beta0[5] + beta_tri[5]*2)

# Maximum number of possible birds for that species
max_birds <- max(c(min_shrub, max_shrub,
                   min_pfg, max_pfg,
                   min_tri, max_tri))

# Shrub cover predictive plot --------------------------------------------------------------------------
shrub_pred_plot <- preds %>% 
  # Unscale the predictor 
  mutate(Pred.Naive = Pred * shrub_sd + shrub_mean) %>% 
  ggplot() +
  # Predicted mean and 95% CI abundance 
  geom_line(aes(x = Pred.Naive, y = exp(Beta0.mean + Beta.Shrub.mean * Pred)), 
            col = "navyblue", linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(Beta0.lb + Beta.Shrub.lb * Pred), 
                  ymax = exp(Beta0.ub + Beta.Shrub.ub * Pred)),
              fill = "navyblue", alpha = 0.2) +
  # Labels
  labs(x = "Shrub cover", 
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    # axis.title.y = element_blank(),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.title = element_text(size = 16)
  ) +
  # Add percent symbols
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# View the predicted response to shrub cover
# shrub_pred_plot 

# Perennial cover predictive plot ------------------------------------------------------------------------------------
pfg_pred_plot <- preds %>% 
  # Unscale the predictor 
  mutate(Pred.Naive = Pred * pfg_sd + pfg_mean) %>% 
  ggplot() +
  # Predicted mean and 95% CI abundance 
  geom_line(aes(x = Pred.Naive, y = exp(Beta0.mean + Beta.PFG.mean * Pred)), 
            col = "navyblue", linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(Beta0.lb + Beta.PFG.lb * Pred), 
                  ymax = exp(Beta0.ub + Beta.PFG.ub * Pred)),
              fill = "navyblue", alpha = 0.2) +
  # Labels
  labs(x = "Perennial cover", 
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(
    axis.title.x = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.title = element_text(size = 16)
  ) +
  # Add percent symbols
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# View the predicted response to perennial cover
# pfg_pred_plot 

# Ruggedness predictive plot ----------------------------------------------------------------------------------------
tri_pred_plot <- preds %>% 
  # Unscale the predictor 
  mutate(Pred.Naive = Pred * tri_sd + tri_mean) %>% 
  # No such thing as negetive ruggedness
  filter(Pred.Naive >= 0) %>% 
  ggplot() +
  # Predicted mean and 95% CI abundance 
  geom_line(aes(x = Pred.Naive, y = exp(Beta0.mean + Beta.TRI.mean * Pred)), 
             col = "navyblue", linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(Beta0.lb + Beta.TRI.lb * Pred), 
                  ymax = exp(Beta0.ub + Beta.TRI.ub * Pred)),
              fill = "navyblue", alpha = 0.2) +
  # Labels
  labs(x = "Topographic Ruggedness", 
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(
    axis.title.x = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.title = element_text(size = 16)
  ) 
# View the predicted response to ruggedness
# tri_pred_plot 

# Combine predictive plots ---------------------------------------------------------------------------

# Define a legend label
lngd_lbl <- paste("Predicted", species_name, "Abundance")

# Make a plot for its legend
legend_plot <- preds %>%
  mutate(Pred.Naive = Pred * shrub_sd + shrub_mean) %>%
  ggplot() +
  geom_line(aes(x = Pred.Naive, y = exp(Beta0.mean + Beta.Shrub.mean * Pred), color = lngd_lbl), linewidth = 1) +
  geom_ribbon(aes(x = Pred.Naive, ymin = exp(Beta0.lb + Beta.Shrub.lb * Pred),
                  ymax = exp(Beta0.ub + Beta.Shrub.ub * Pred), fill = lngd_lbl), alpha = 0.2) +
  labs(x = "Shrub cover",
       y = paste0(species_name, "s per km^2")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, max_birds)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.size = unit(1, "cm")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = setNames(c("navyblue"), lngd_lbl)) +
  scale_fill_manual(values = setNames(c("navyblue"), lngd_lbl))

# Pull out the legend
legend <- ggpubr::get_legend(legend_plot)

# Create a title
plot_title <- textGrob(species_name, 
                       gp = gpar(fontsize = 20, fontface = "bold"))


# Combine the three predictive plots
comb_hab_pred_plots <- grid.arrange(shrub_pred_plot, pfg_pred_plot, tri_pred_plot,
                                nrow = 1, ncol = 3)

# Add the legend
comb_hab_pred_plots_lgnd <- grid.arrange(plot_title, 
                                     comb_hab_pred_plots, 
                                     legend,
                                     nrow = 3, 
                                     heights = c(0.1, 0.8, 0.1))

# View the co,bined plots
# comb_pred_plots_lgnd

# Save the plot as a png
ggsave(plot = comb_hab_pred_plots_lgnd,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_pred_plot_",
                         plot_species, ".png"),
       width = 300,
       height = 150,
       units = "mm",
       dpi = 300)

} # End plotting loop over all species (Comment this out) ----

# 4.3) Plot groups of species together #######################################

# All species plots ---------------------------------------------------------------------------------------------

# Read in the sagebrush obligate parameter plots
sath_param_plot <- readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                               "SATH", "_params.rds"))
brsp_param_plot <- readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                               "BRSP", "_params.rds"))
sabs_param_plot <- readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                               "SABS", "_params.rds"))

# Read in the grassland assoicated species plots
vesp_param_plot <- readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                                  "VESP", "_params.rds"))
weme_param_plot <- readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                                  "WEME", "_params.rds"))
hola_param_plot <- readRDS(paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                                  "HOLA", "_params.rds"))

# Plot the grassland parameter estimate plots together
all_species_params <- grid.arrange(sath_param_plot, brsp_param_plot, sabs_param_plot,
                                     vesp_param_plot, weme_param_plot, hola_param_plot,
                                     nrow = 2, ncol = 3)

# Save the plot as a png
ggsave(plot = all_species_params,
       filename = paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\habitat_",
                         "all_species", "_params.png"),
       width = 375,
       height = 175,
       units = "mm",
       dpi = 300)

