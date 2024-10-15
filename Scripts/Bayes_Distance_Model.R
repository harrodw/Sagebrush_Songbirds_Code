#----------------------------------------------------------------
#Will Harrod
#Hierarchical distance sampling for sagebrush songbird point count data
#
# Model code Modefied from Kery et al. 2024: Integrated distance sampling models for simple point counts 
# Marc Kary, Nicolas Strebel, Tyler Hallman, Kenneth F. Kellner
# August 2024
#---------------------------------------------------------------

#add packages
library(nimble)
library(tidyverse)
library(MCMCvis)

#clear environments
rm(list = ls())

################################################################################
# 1.0) Data Prep  ################################################################
################################################################################

# 1.1) Read in data ################################################################
# #Add in Data from local drive 
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# #or from github
# sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_data.csv") %>%
#   dplyr::select(-X) %>% 
#   tibble()

#view the data
glimpse(sobs)

# add covariates from the local drive
# Aspect == -1 means that the area is flat other classes start at 1=NE clockwise
# Fire Distance == 1000000 mean that the area is outside of a fire
# Burn sevarity == 0 means that the area did not burn
# Burn sevarity == -1 means that no data was available 
# Fire year == 1800 means there are no recorded fires in the area
covs <- tibble(read.csv("Data/Outputs/grid_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/point_summaries.csv") %>%
#   dplyr::select(-X) %>% 
#   tibble()

#View covariates
glimpse(covs)

# 1.2) Prepare the count level data ################################################################
#define relevant species
soi <- "BRSP"

#define a truncation distance (km)
trunc_dist <- 0.125

# Custom function to find the mode
find_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]  # Find the most frequent value
}

#make a table of important species sightings by visit
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

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  distinct(Full.Point.ID, Grid.ID, Grid.Type, Year, Ord.Date, MAS, Wind.Start, Observer.ID, Year, Visit) %>% 
  mutate(
    # Switch Alex's two surveys to Ben (most similar based on preliminary unmarked analysis
    Observer.ID = case_when(Observer.ID == "Alex" ~ "Ben", TRUE ~ Observer.ID),
    #Each visit should be treated separately
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
# ID-C11, Y1-V1,
# ID-C22, Y1-V1

# For now I',m going to propagate the existing data to fill these in
# Probably should find a better way to do this
new_dat <- tibble(Grid.ID = c("ID-C11", "ID-C22"),
                  Grid.Type = c("R", "R"),
                  Year = c("Y1", "Y1"),
                  Visit.ID = c("Y1-V1", "Y1-V1"),
                  # Saying Rory filled in the data
                  Observer.ID = c("Rory", "Rory"),
                  # Average ordianal date across visits
                  Ord.Date = c(floor(mean(counts_temp$Ord.Date[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$Ord.Date[which(counts_temp$Grid.ID == "ID-C22")]))),
                  # average survey time across visits
                  Mean.MAS = c(mean(counts_temp$Mean.MAS[which(counts_temp$Grid.ID == "ID-C11")]),
                               mean(counts_temp$Mean.MAS[which(counts_temp$Grid.ID == "ID-C22")])),
                  # average count across visits
                  Count = c(floor(mean(counts_temp$Count[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$Count[which(counts_temp$Grid.ID == "ID-C22")]))),
                  # Average number of points surevyed
                  n.Points = c(floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C11")])),
                               floor(mean(counts_temp$n.Points[which(counts_temp$Grid.ID == "ID-C22")]))),
                  )

# Bind these to the exisitng data 
counts_temp2 <- bind_rows(counts_temp, new_dat)

#Change necessary variables to scales and factors
counts_temp3 <- counts_temp2 %>%
  #Add covariates
  left_join(covs, by = c("Grid.ID")) %>% 
  #Sort the data
  arrange(Visit.ID, Grid.ID) %>% 
  mutate(
    # Numeric burned vs unburned
    Burned = as.numeric(factor(Grid.Type, levels = c("R", "B"))) - 1,
    # Time since Fire
    Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>%
  #change the wind data to a factor with set levels
  mutate(Wind = case_when(is.na(Wind) ~ 'Unknown',
                                TRUE ~ Wind)) %>% 
  mutate(Wind = factor(Wind, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>%
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID",
              "Observer.ID", "Year", "Aspect"
  ), factor) %>% 
  #Remove columns that are no longer needed
  dplyr::select(-Grid.Type) 

#...and view
glimpse(counts_temp3)

#Isolate the burned Grids as their own object so I can acurately scale them
fire_stats <- counts_temp3 %>% 
  filter(Burned == 1) %>% 
  dplyr::select(Grid.ID, Years.Since.Fire, mean.rdnbr) 

#Find the mean and standard deviation of the "real" burns
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

#define a bin size
bin_size <- 0.025

#Create an object with all observations of a single species for the detection function
observations_temp <- sobs %>% 
  mutate(Distance = Distance / 1000) %>%            # Switch from m to km
  filter(Species == soi) %>%                        # only one species
  arrange(Year, Visit, Grid.ID) %>% # Same order as the others
  filter(Distance <= trunc_dist) %>%                # Only close observations
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
  #Add in the bin sizes
  mutate(Dist.Bin = floor(Distance / bin_size)* bin_size) %>% 
  #Name the first and last distance bin
  mutate(Dist.Bin = case_when(Dist.Bin == 0 ~ 0.025,
                              TRUE ~ Dist.Bin)) %>% 
  #Calculate Midpoints
  mutate(Dist.Bin.Midpoint = Dist.Bin - bin_size/2) %>% 
  # #Change to a factor
  mutate(Dist.Bin = case_when(Dist.Bin == 0.025 ~ 1,
                              Dist.Bin == 0.050 ~ 2,
                              Dist.Bin < 0.1 & Dist.Bin >= 0.075 ~ 3, #idk why it needs this specificity. It just does
                              Dist.Bin == 0.100 ~ 4,
                              Dist.Bin == 0.125 ~ 5
  )) %>% 
  dplyr::select(Grid.ID, Observer.ID, Dist.Bin, Dist.Bin.Midpoint, Year, Visit.ID)

#view the whole object
glimpse(observations_temp)
unique(observations_temp$Dist.Bin)
unique(observations_temp$Dist.Bin.Midpoint)

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

#Link the factor levels from the count dataset to the observation dataset
observations <- observations_temp %>% 
  left_join(point_ids, by = c("Grid.ID", "Year", "Visit.ID"))

# View Counts
glimpse(counts)

# View Observations
glimpse(observations)

# 1.4) prepare objects for NIMBLE ################################################################

# Build a storage matrix of observations by visit
count_mat <- matrix(NA, nrow = length(unique(counts$Grid.ID.num)), ncol = length(unique(counts$Visit.ID.num)))
# Build a storage matrix of years by visit
year_mat <- matrix(NA, nrow = length(unique(counts$Grid.ID.num)), ncol = length(unique(counts$Visit.ID.num)))
# Build a storage matrix of survey times by visit
time_mat <- matrix(NA, nrow = length(unique(counts$Grid.ID.num)), ncol = length(unique(counts$Visit.ID.num)))
# Build a storage matrix of survey dates by visit
date_mat <- matrix(NA, nrow = length(unique(counts$Grid.ID.num)), ncol = length(unique(counts$Visit.ID.num)))
# Build a storage matrix for the number of points visited during each survey
points_mat <- matrix(NA, nrow = length(unique(counts$Grid.ID.num)), ncol = length(unique(counts$Visit.ID.num)))
# Build a storage matrix for who conducted each survey
obsv_mat <- matrix(NA, nrow = length(unique(counts$Grid.ID.num)), ncol = length(unique(counts$Visit.ID.num)))

# Fill in the matrix
for(y in 1:nrow(count_mat)){

  # filter for a specific grid
  grid_count <- counts %>% 
    arrange(Visit.ID) %>% 
    filter(Grid.ID.num == y)

  # assign values to each row
  count_mat[y,] <- grid_count$Count
  year_mat[y,] <- grid_count$Year
  time_mat[y,] <- grid_count$Mean.MAS
  date_mat[y,] <- grid_count$Ord.Date
  points_mat[y,] <- grid_count$n.Points
  obsv_mat[y,] <- grid_count$Observer.ID.num
}

# view the matrices
count_mat  # number of individuals recorded at each visit
year_mat   # year during which each visit took place
time_mat   # scaled mean time of day for each visit
date_mat   # scaled date for each visit
points_mat # Number of points visited during each survey 
obsv_mat   # Who conducted each survey

# Loop sizes 
ngrids <- length(unique(counts$Grid.ID.num)) # number of survey grids
nind <- nrow(observations) # Number of individuals detected 
nobsv <- length(unique(counts$Observer.ID.num)) # Number of unique observers
nbins <- length(unique(observations$Dist.Bin)) # Number of distance bins
nyears <- length(unique(counts$Year)) # Number of Years we surveyed
nvst <- length(unique(counts$Visit.ID)) # number of visits in each year

# Observation Level data 
dist_class <- observations$Dist.Bin # Distance category for each observation
obs_grid <- observations$Grid.ID.num # grid of each observation 
obs_year <- observations$Year.num     # year when each observation took place
midpoints <- unique(observations$Dist.Bin.Midpoint) # Midpoints of distance bins
observers <- obsv_mat                               #Random effect for observer associated with each survey

# Grid level data
ncap <- count_mat       # matrix of the number of detected individuals per grid per survey 
year <- year_mat        # matrix of year numbers
sage_cvr <- counts$Sage.Cover[1:length(unique(counts$Grid.ID))] #scaled elevation covariate
pern_cvr <- counts$Perennial.Cover[1:length(unique(counts$Grid.ID))] #scaled elevation covariate
elevation <- counts$Elevation[1:length(unique(counts$Grid.ID))] #scaled elevation covariate
time <- time_mat # matrix of scaled time after sunrise
day <- date_mat # matrix of scaled dates

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
sobs_const <- list (
  ## Misc. Constants
  area = pi * trunc_dist^2 * points_mat, # Area associated with the DS data
  delta = bin_size, # Bin width
  newB = trunc_dist, # Truncation distance
  
  #For loop sizes
  ngrids = ngrids,   # number of survey grids
  nind = nind, # Number of individuals detected 
  nobsv = nobsv, # Number of unique observers
  nyears = nyears, # Number of years we surveyed
  nvst = nvst, # number of times each grid was surveyed (6)
  nbins = nbins, # Number of distance bins
  
  # Random effects
  observers = observers, #Random effect for observer associated with each survey
  year = year_mat, # year number
  obs_grid = obs_grid, # grid of each observation 
  obs_year = obs_year # year when each observation took place
)

# Data to be fed into Nimble 
sobs_dat <- list(
  # Observation Level data
  dclass = dist_class, # Distance category for each observation
  midpt = midpoints, # Midpoints of distance bins
  
  #Point level data
  ncap = ncap, # Number of detected individuals per site
  sage_cvr = sage_cvr,
  pern_cvr = pern_cvr,
  elevation = elevation, #scaled elevation covariate
  time = time, # scaled mean time after sunrise covariate
  day = day # scaled date covariate
)

#set seed
set.seed(123)

#inits
sobs_inits <- list(
  mean.sigma = runif(1, 0, 1), 
  mean.phi = runif(1, 0, 1),
  mean.psi = runif(1, 0.001, 0.999),
  gamma1 = rnorm(1, 0, 10), 
  gamma2 = rnorm(1, 0, 10), 
  gamma3 = rnorm(1, 0, 10), 
  gamma4 = rnorm(1, 0, 10),
  mean.lambda = rnorm(1, 0, 10), 
  beta1 = rnorm(1, 0, 10), 
  beta2 = rnorm(1, 0, 10),
  beta3 = rnorm(1, 0, 10),
  beta4 = rnorm(1, 0, 10),
  beta5 = rnorm(1, 0, 10),
  sd.eps = runif(1, 0, 1),
  
  N_indv = count_mat + 1 # start each grid with an individual present
  )  

#Object dimensions
sobs_dims <- list(
  f = c(ngrids, nvst, nbins),
  fc = c(ngrids, nvst, nbins),
  fit = nvst,
  fit.new = nvst, 
  EDS = c(ngrids, nvst),
  EDS.new = c(ngrids, nvst))

# 2.3) Model definition ################################################################
# Add random noise in the detection function intercepts
# Note all distances are in units of 1km (and area in 1km2 units)

#The model code
sobs_model_code <- nimbleCode({
      # Priors for all parameters in DS model
      # ------------------------------------------------------------------
    
      # Random noise on availability
      tau.eps <- sd.eps^-2
      # Magnitude of that noise
		  sd.eps ~ dunif(0, 10)

      # Parameters in the availability component of the detection model
      gamma0 <- log(mean.phi)  # phi intercept on log scale and ...
      mean.phi ~ dunif(0, 1)   # ... on natural scale
      gamma1 ~ dnorm(0, 100)     # Effect of day of year on singing rate
      gamma2 ~ dnorm(0, 100)     # Effect of day of year on singing rate (quadratic)
      gamma3 ~ dnorm(0, 100)     # Effect of time of day on singing rate
      gamma4 ~ dnorm(0, 100)     # Effect of time of day on singing rate (quadratic)
      
      # Parameters in the detection portion of the model
      # Random intercept for each observer
      for(o in 1:nobsv) {     # Loop over 18 observers
        alpha0[o] ~ dnorm(alpha0.int, sd.alpha0)
      }
      alpha0.int <- log(mean.sigma)  # sigma intercept on log scale and ...
      mean.sigma ~ dnorm(0, 100)      # ... on the natural scale (0 - 1 km)
      sd.alpha0 ~ dunif(0, 10)   # Magnitude of noise in sigma intercept
      
      # zero-inflation Bernoulli probability for each grid conserved across years
      for(s in 1:ngrids) {
        logit(psi[s]) ~ dnorm(psi0.int, sd.psi0)
      }
      psi0.int <- log(mean.psi) # average magnitude of occupancy probability on the natural scale
      mean.psi ~ dnorm(0, 1) # average magnitude of occupancy probability on the natural scale
      sd.psi0 ~ dunif(0, 10) # variation in magnitude of occupancy probability
      
      # Random intercept for each year
      for(y in 1:nyears) {     # Loop over 3 years
        beta0[y] ~ dnorm(beta0.int, sd.beta0)
      }
      # Priors for yearly abundance intercept
      beta0.int <- log(mean.lambda)  # lambda intercept on log scale and ...
      mean.lambda ~ dnorm(0, 100) # ... on natural scale
      sd.beta0 ~ dunif(0, 10)  # Magnitude of that noise on lambda intercept 
      
      # Covariates on abundance:
      beta1 ~ dnorm(0, 100)      # Effect of sagebrush cover
      beta2 ~ dnorm(0, 100)    # Effect of Perennial Cover
      beta3 ~ dnorm(0, 100)    # Effect of elevation
      beta4 ~ dnorm(0, 100)    # Effect of sagebrush cover squared
      beta5 ~ dnorm(0, 100)    # Effect of perennial cover squared
      beta6 ~ dnorm(0, 100)    # Effect of elevation squared

      # -------------------------------------------------------------------
      # Hierarchical construction of the likelihood
      
      # Iterate over all of the visits to each survey grip
      for(y in 1:nvst){
      
      # Model for binned distance observations of every detected individual
      for(i in 1:nind){       # Loop over all detected individuals
        dclass[i] ~ dcat(fc[obs_grid[i], obs_year[i], 1:nbins])      # distance classes follow a multinational distribution
      }
      
      # Construction of the cell probabilities for the nD distance bands
      for(s in 1:ngrids){  # Loop over all survey grids
        for(b in 1:nbins){       # midpt = mid-point of each distance band
          log(p[s, y, b]) <- -midpt[b] * midpt[b] / (2 * sigma[s, y]^2) # half-normal detection function
          pi[s, y, b] <- ((2 * midpt[b] ) / newB^2) * delta # prob density function out to max trucation distance 
          f[s, y, b] <- p[s, y, b] * pi[s, y, b] # scale cell probability with radial distances
          fc[s, y, b] <- f[s, y, b] / pcap[s, y] # combined c ell probability
        }
        # Rectangular integral approx. of integral that yields the Pr(capture)
        pcap[s, y] <- sum(f[s, y, ])
        
        ### Log-linear models on abundance, detectability, and availability
        # Abundance (lambda) Log-linear model for abundance
        log(lambda[s, y]) <- beta0[year[s, y]] +           # random effect of year
        beta1 * sage_cvr[s] +                        # Effect of sagebrush cover
        beta2 *  pern_cvr[s] +                       # Effect of Perennial Cover
        beta3 * elevation[s] +                       # Effect of elevation
        beta4 * sage_cvr[s]^2 +                      # Effect of sagebrush cover squared
        beta5 * pern_cvr[s]^2 +                      # Effect of perennial cover squared
        beta6 * elevation[s]^2                       # Effect of elevation squared 
        
        # Detectability (sigma)
        log(sigma[s, y]) <- alpha0[observers[s, y]] + eps[s, y]  # Log-Linear model for detection probability
		    eps[s, y] ~ dnorm(0, tau.eps)                    # Note here eps1 has one precision and below another

        # Availability (phi)
		    # Log-linear model for availability
		    logit(phi[s, y]) <- gamma0 +                    # Intercept
        gamma1 * day[s, y] +                            # Effect of scaled ordinal date
        gamma2 * day[s, y]^2 +                          # Effect of scaled ordinal date squared
        gamma3 * time[s, y] +                           # Effect of scaled time of day
        gamma4 * time[s, y]^2                           # Effect of scaled time of day aquared

        # Multiply availability with detection probability
        pDS[s, y] <- pcap[s, y] * phi[s, y]
        
        # Zero-inflation component on abundance
        present[s, y] ~ dbern(psi[s])

        ### Binomial mixture part (in conditional multinomial specification)
        N_indv[s, y] ~ dpois(area[s, y] * lambda[s, y] * present[s, y])            # Note use of area as an offset

		  # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
        # Compute fit statistic for observed data 
        eval[s, y] <- pDS[s, y] * N_indv[s, y]
        EDS[s, y] <- (sqrt(ncap[s, y]) - sqrt(eval[s, y]))^2

        # Generate replicate count data and compute same fit stats for them
        ncap.new[s, y] ~ dbin(pDS[s, y], N_indv[s, y])
        EDS.new[s, y] <- (sqrt(ncap.new[s, y]) - sqrt(eval[s, y]))^2
      } # end loop through grids
      
      # Add up fit stats across sites for DS data
      fit[y] <- sum(EDS[, y])
      fit.new[y] <- sum(EDS.new[, y])
      
      # Compute Bayesian p-value for distance sampling data
      bpv <- step(fit.new[y] - fit[y])
      } # end loop through visits
})

# 2.4) Configure and Run the model ###########################################################

# Params to save
sobs_params <- c("mean.sigma", 
            "alpha0",  
            "sd.eps", 
            "mean.sigma",
            "mean.phi",
            "mean.psi",
            "gamma0", 
            "gamma1", 
            "gamma2", 
            "gamma3", 
            "gamma4", 
            "mean.lambda", 
            "beta0", 
            "beta1", 
            "beta2", 
            "beta3", 
            "beta4", 
            "beta5", 
            "beta6",
            "sd.beta0", 
            "beta0.int",
            "fit", 
            "fit.new", 
            "bpv")

# MCMC settings. Pick one, comment out the rest 
 nc <- 3  ;  ni <- 50  ;  nb <- 2  ;  nt <- 2 # test, 30 sec
# nc <- 3  ;  ni <- 30000  ;  nb <- 10000  ;  nt <- 3 # longer test
# nc <- 4;  ni <- 120000;  nb <- 60000;  nt <- 60   # Run the model for real
# nc <- 10;  ni <- 400000;  nb <- 200000;  nt <- 200 # This takes a while...


#Run the sampler
start <- Sys.time() #start time for the sampler
mcmc.output <- nimbleMCMC(code = sobs_model_code,
                          data = sobs_dat,
                          constants = sobs_const,
                          dimensions = sobs_dims,
                          inits =sobs_inits,
                          monitors = sobs_params,
                          niter = ni,
                          nburnin = nb,
                          thin = nt,
                          nchains = nc,
                          setSeed = 123,
                          samples = TRUE,
                          summary = TRUE
                          )
difftime(Sys.time(),start) # end time for the sampler

#Save model output
save(sobs_mcmc_comp, file = paste0("Bayes_Files//", soi, "_distance_model_out.rda"))

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

#load the output back in
mcmc_out <- load(file = paste0("Bayes_Files//", soi, "_distance_model_out.rda"))

#Define which parameters I want to view
params <- c("mean.sigma", 
            "alpha0",  
            "sd.eps", 
            "rf.alpha0",
            "mean.phi", 
            "gamma0", 
            "gamma1", 
            "gamma2", 
            "gamma3", 
            "gamma4", 
            "mean.lambda", 
            "beta0", 
            "beta1", 
            "beta2", 
            "beta3", 
            "beta4", 
            "beta5", 
            "beta6",
            "sd.beta0", 
            "beta0.int",
            "fit", 
            "fit.new", 
            "bpv")

#View MCMC summary
MCMCsummary(object = mcmc_out, round = 2)

#View an MCMC plot
MCMCplot(object = mcmc_out,
         params = params)

#Traceplots and density graphs 
MCMCtrace(object = mcmc_out,
          pdf = FALSE,
          ind = TRUE, 
          params = params)

