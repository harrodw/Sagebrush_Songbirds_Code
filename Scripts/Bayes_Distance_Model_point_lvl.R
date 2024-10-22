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
library(plyr)
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
# Burn sevarity == 0 mean that the area did not burn
# Fire year == 1850 means there are no recorded fires in the area
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

#make a table of important species sightings by visit
counts_0inf <- sobs %>%
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Species  == soi) %>% # only one species
  filter(Distance <= trunc_dist) %>% # only observations closer than the truncation distancee
  group_by(Full.Point.ID, Route.Type, Route.ID, Observer.ID, Year, Visit, Date) %>% 
  reframe(Full.Point.ID, Route.Type, Route.ID, Observer.ID, Year, Visit, Date, 
          Count = n()) %>% 
  distinct()
#...and view
glimpse(counts_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  tidyr::expand(nesting(Full.Point.ID, 
                        Route.ID,
                        Route.Type, 
                        Point.Time, 
                        Year, 
                        Sky.Start,
                        MAS, 
                        Ord.Date, 
                        Wind.Start, 
                        Temp.Start, 
                        Observer.ID,
                        Visit, 
                        Date))

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
counts_temp <-  visit_count %>% 
  left_join(counts_0inf, by = c('Full.Point.ID', 
                                'Route.ID',
                                "Route.Type",
                                'Observer.ID',
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))

#...and view
glimpse(counts_temp)

#Change necessary variables to scales and factors
counts <- counts_temp %>%
  #Add covariates
  left_join(covs, by = c("Route.ID")) %>% 
  #Sort the data
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% 
  #Reclassify some of the variables
  mutate(Burned = as.numeric(factor(Route.Type, levels = c("R", "B"))) - 1,
         Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>%
  #change the wind data to a factor
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown',
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>%
  #Change the sky data to a factor
  mutate(Sky.Start = factor(Sky.Start, levels = c("Clear", "Partly Cloudy", "Cloudy",
                                                  "Drizzle", "Fog or Smoke"))) %>% 
  mutate_at(c("Aspect", "Route.ID", "Full.Point.ID",
              "Observer.ID", "Year", "Visit"
  ), factor) %>% 
  #Remove columns that are no longer needed
  dplyr::select(-Route.Type) 

#...and view
glimpse(counts)

#Isolate the burned routes as their own object
fire_stats <- counts %>% 
  filter(Burned == 1) %>% 
  dplyr::select(Full.Point.ID, Years.Since.Fire) 

#Find the mean and standard deviation of the "real" burns
mean_burnY <- mean(fire_stats$Years.Since.Fire)
sd_burnY <- sd(fire_stats$Years.Since.Fire)
#...and view
print(c(mean_burnY, sd_burnY))

# 1.3) Prepare the observation level data ################################################################

#define a bin size
bin_size <- 0.025

#Create an object with all observations of a single species for the detection function
observations_temp <- sobs %>% 
  # Going to remove this but it simplifies the model for now ----
  filter(Year == "Y3" & Visit == "V1") %>% 
  mutate(Distance = Distance / 1000) %>%            # Switch from m to km
  filter(Species == soi) %>%                        # only one species
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% # Same order as the others
  filter(Distance <= trunc_dist) %>%                # Only close observations
  mutate(Burned = as.numeric(factor(Route.Type, levels = c("R", "B"))) - 1) %>%
  dplyr::select(Distance, Minute, How.Detected, 
                Route.ID, Full.Point.ID, Observer.ID, Year, Visit,
                Burned, Ord.Date, 
                MAS) %>% 
  left_join(covs, by = "Route.ID") %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  #Add in the bin sizes
  mutate(Dist.Bin = round_any(Distance, bin_size, floor)) %>% 
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
  dplyr::select(Dist.Bin, Dist.Bin.Midpoint, Full.Point.ID, Year, Visit)

#view the whole object
glimpse(observations_temp)
unique(observations_temp$Dist.Bin)
unique(observations_temp$Dist.Bin.Midpoint)

# Make sure the same numeric values for factors are shaped between the two datasets
counts <- counts %>% 
  arrange(Year, Visit, Full.Point.ID, Route.ID) %>% 
  mutate(Full.Point.ID.num = as.numeric(Full.Point.ID),
         Route.ID.num = as.numeric(Route.ID),
         Year.num = as.numeric(Year),
         Visit.num = as.numeric(Visit)) %>% 
  # drop this later ----
  filter(Year == "Y3" & Visit == "V1")

#Pull out just the visit ID
point_ids <- counts %>% 
  mutate_at(c("Full.Point.ID", "Route.ID", "Year", "Visit"), as.character) %>% 
  dplyr::select(Full.Point.ID, Full.Point.ID.num, Route.ID, Route.ID.num, Year, Year.num, Visit, Visit.num)
#...and view
glimpse(point_ids)

#Link the factor levels from the count dataset to the observation dataset
observations <- observations_temp %>% 
  left_join(point_ids, by = c("Full.Point.ID", "Year", "Visit"))

# View Counts
print("Counts:")
glimpse(counts)

# View Observations
print("Observations:")
glimpse(observations)

# array to store points based on their route
point_mat <- array(NA, c(length(unique(counts$Route.ID)), 16))

# split up points based on which grid they are part of
for(i in 1:nrow(point_mat)) {
  points <- counts$Full.Point.ID.num[counts$Route.ID.num  ==  i]  
  point_mat[i,] <- points
}

# 1.4) prepare objects for NIMBLE ################################################################

# Loop sizes 
ngrids <- length(unique(counts$Route.ID.num)) # number of survey grids
npoints <- nrow(counts) # Number of sites
nind <- nrow(observations) # Number of individuals detected 
nobs <- length(unique(as.numeric(counts$Observer.ID))) # Number of unique observers
nbins <- length(unique(observations$Dist.Bin)) # Number of distance bins
nyears <- length(unique(counts$Year)) # Number of Years we surveyed
nvst <- length(unique(counts$Visit)) # number of visits in each year

# Observation Level data 
dist_class <- observations$Dist.Bin # Distance category for each observation
points <- observations$Full.Point.ID.num # point number of each observation 
midpoints <- unique(observations$Dist.Bin.Midpoint) # Midpoints of distance bins
observers <- as.numeric(counts$Observer.ID) #Random effect for observer associated with each survey

# Point level data
ncap <- counts$Count # Number of detected individuals per site
year <- counts$Year.num # year number
sage_cvr <- (counts$Elevation -mean(counts$Elevation)) / sd(counts$Elevation) #scaled elevation covariate
pern_cvr <- (counts$Elevation -mean(counts$Elevation)) / sd(counts$Elevation) #scaled elevation covariate
elevation <- (counts$Elevation -mean(counts$Elevation)) / sd(counts$Elevation) #scaled elevation covariate
time <- (counts$MAS - mean(counts$MAS)) / sd(counts$MAS) # scaled based on mean time after sunrise
day <- (counts$Ord.Date - mean(counts$Ord.Date)) / sd(counts$Ord.Date) #scaled data covariate

#########################################################################################################
# 2) Build and run the model ############################################################################
#########################################################################################################

# 2.1) Constants, data, Initial values, and dimensions #############################################

# Constants to be fed into Nimble
brsp_const <- list (
  ## Misc. Constants
  area = pi * trunc_dist^2, # Area associated with the DS data
  delta = bin_size, # Bin width
  newB = trunc_dist, # Truncation distance
  
  #For loop sizes
  ngrids = ngrids,   # number of survey grids
  npoints = npoints, # Number of sites
  nind = nind, # Number of individuals detected 
  nobs = nobs, # Number of unique observers
  nyears = nyears, # Number of years we surveyed
  # nvst = nvst, # number of visits per year
  nbins = nbins, # Number of distance bins
  
  # Random effects
  points = points, # point number of each observation
  observers = observers, #Random effect for observer associated with each survey
  year = as.numeric(as.factor(counts$Year)) # year number
)

# Data to be fed into Nimble 
brsp_dat <- list(
  # Observation Level data 
  dclass = dist_class, # Distance category for each observation
  midpt = midpoints, # Midpoints of distance bins
  
  #Point level data
  ncap = ncap, # Number of detected individuals per site
  sage_cvr = sage_cvr,
  pern_cvr = pern_cvr,
  elevation = elevation, #scaled elevation covariate
  time = time, # scaled based on mean time after sunrise
  day = day #scaled data covariate
)

#set seed
set.seed(123)

#inits
brsp_inits <- list(
  mean.sigma = runif(1, 0, 1), 
  mean.phi = runif(1, 0, 1), 
  gamma1 = rnorm(1, 0, 0.1), 
  gamma2 = rnorm(1, 0, 0.1), 
  gamma3 = rnorm(1, 0, 0.1), 
  gamma4 = rnorm(1, 0, 0.1),
  mean.lambda = runif(1, 1, 100), 
  beta1 = rnorm(1, 0, 0.1), 
  beta2 = rnorm(1, 0, 0.1),
  beta3 = rnorm(1, 0, 0.1),
  beta4 = rnorm(1, 0, 0.1),
  beta5 = rnorm(1, 0, 0.1),
  sd.eps = runif(1, 0, 1), 
  N_indv = counts$Count+1)  

#Object dimensions
brsp_dims <- list(
  f = c(npoints, nbins),
  fc = c(npoints, nbins),
  EDS = npoints,
  EDS.new = npoints)

# 2.3) Model definition ################################################################
# Add random noise in the detection function intercepts
# Note all distances are in units of 1km (and area in 1km2 units)

#The model code
brsp_model_code <- nimbleCode({
      # Priors for all parameters in DS model
      # ------------------------------------------------------------------
    
      # Random noise on availability
      tau.eps <- sd.eps^-2
      # Magnitude of that noise
		  sd.eps ~ T(dt(0, 1, 1), 0,)  #truncated to be greater than 0

      # Parameters in the availability component of the detection model
      gamma0 <- log(mean.phi)  # phi intercept on log scale and ...
      mean.phi ~ dunif(0, 1)   # ... on natural scale
      gamma1 ~ dnorm(0, 1)     # Effect of day of year on singing rate
      gamma2 ~ dnorm(0, 1)     # Effect of day of year on singing rate (quadratic)
      gamma3 ~ dnorm(0, 1)     # Effect of time of day on singing rate
      gamma4 ~ dnorm(0, 1)     # Effect of time of day on singing rate (quadratic)

      # Parameters for the abundance model
      # Random intercept for each year
      for (i in 1:nyears) {     # Loop over 3 years
        beta0[i] ~ dnorm(beta0.int, tau.beta0)
      }
      beta0.int <- log(mean.lambda)  # lambda intercept on log scale and ...
      mean.lambda ~ T(dnorm(0, 0.001), 0,) # ... on natural scale
      tau.beta0 <- sd.beta0^-2 
      # Magnitude of that noise on lambda intercept
      sd.beta0 ~ T(dt(0, 1, 1), 0,)  #truncated to be greater than 0
      
      # Random intercept for each observer
      for (o in 1:nobs) {     # Loop over 18 observers
        alpha0[o] ~ dnorm(alpha0.int, tau.alpha0)
      }
      alpha0.int <- log(mean.sigma)  # sigma intercept on log scale and ...
      mean.sigma ~ dunif(0, 1)      # ... on the natural scale (0 - 1 km)
      tau.alpha0 <- sd.alpha0^-2
      sd.alpha0 ~ T(dt(0, 1, 2), 0,) # Magnitude of noise in sigma intercept
      
      # Covariates:
      beta1 ~ dnorm(0, 1)      # Effect of sagebrush cover
      beta2 ~ dnorm(0, 0.1)    # Effect of Perennial Cover
      beta3 ~ dnorm(0, 0.1)    # Effect of elevation
      beta4 ~ dnorm(0, 0.1)    # Effect of sagebrush cover squared
      beta5 ~ dnorm(0, 0.1)    # Effect of perrenial cover squared
      beta6 ~ dnorm(0, 0.1)    # Effect of elevation squared

      # -------------------------------------------------------------------
      # Hierarchical construction of the likelihood
      # Model for binned distance observations of every detected individual
      for(i in 1:nind){       # Loop over all detected individuals
        dclass[i] ~ dcat(fc[points[i], ])      # distance classes follow a multinational distribution
      }
      
      # Construction of the cell probabilities for the nD distance bands
      for(s in 1:npoints){  # Loop over all survey points
        for(b in 1:nbins){       # midpt = mid-point of each distance band
          log(p[s, b]) <- -midpt[b] * midpt[b] / (2 * sigma[s]^2) # half-normal detection function
          pi[s, b] <- ((2 * midpt[b] ) / newB^2) * delta # prob density function out to max trucation distance 
          f[s, b] <- p[s, b] * pi[s, b] # scale cell probability with radial distances
          fc[s, b] <- f[s, b] / pcap[s] # combined c ell probability
        }
        # Rectangular integral approx. of integral that yields the Pr(capture)
        pcap[s] <- sum(f[s, ])
        
        ### Log-linear models on abundance, detectability, and availability
        # Abundance (lambda) Log-linear model for abundance
        log(lambda[s]) <- beta0[year[s]] +           # random effect of year
        beta1 * sage_cvr[s] +                        # Effect of sagebrush cover
        beta2 *  pern_cvr[s] +                       # Effect of Perennial Cover
        beta3 * elevation[s] +                       # Effect of elevation
        beta4 * sage_cvr[s]^2 +                      # Effect of sagebrush cover squared
        beta5 * pern_cvr[s]^2 +                      # Effect of perennial cover squared
        beta6 * elevation[s]^2                       # Effect of elevation squared 
        
        # Detectability/perceptability (sigma)
        log(sigma[s]) <- alpha0[observers[s]] + eps[s]  # Log-Linear model for detection probability
		    eps[s] ~ dnorm(0, tau.eps)                    # Note here eps1 has one precision and below another

        # Availability (phi)
		    # Log-linear model for availability
		    logit(phi[s]) <- gamma0 +                    # Intercept
        gamma1 * day[s] +                            # Effect of scaled ordinal date
        gamma2 * day[s]^2 +                          # Effect of scaled ordinal date squared
        gamma3 * time[s] +                           # Effect of scaled time of day
        gamma4 * time[s]^2                           # Effect of scaled time of day aquared

        # Multiply availability with detection probability
        pDS[s] <- pcap[s] * phi[s]

        ### Binomial mixture part (in conditional multinomial specification)
        n_point[s] ~ dbin(pDS[s], N_indv[s])            # number captured at each point
        
        N_indv[s] ~ dpois(area * lambda[s])            # Note use of area as an offset

		  # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
        # Compute fit statistic for observed data 
        eval[s] <- pDS[s] * N_indv[s]
        EDS[s] <- (sqrt(ncap[s]) - sqrt(eval[s]))^2

        # Generate replicate count data and compute same fit stats for them
        ncap.new[s] ~ dbin(pDS[s], N_indv[s])
        EDS.new[s] <- (sqrt(ncap.new[s]) - sqrt(eval[s]))^2
	   }
      # Add up fit stats across sites for DS data
      fit <- sum(EDS[])
      fit.new <- sum(EDS.new[])
      
      # Compute Bayesian p-value for distance sampling data
      bpv <- step(fit.new - fit)
})

# 2.4) Configure and Run the model ###########################################################

# Params to save
brsp_params <- c("mean.sigma", 
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

# MCMC settings. Pick one, comment out the rest 
 nc <- 3  ;  ni <- 50  ;  nb <- 2  ;  nt <- 2 # test, 30 sec
# nc <- 3  ;  ni <- 30000  ;  nb <- 10000  ;  nt <- 3 # longer test
# nc <- 4;  ni <- 120000;  nb <- 60000;  nt <- 60   # Run the model for real
# nc <- 10;  ni <- 400000;  nb <- 200000;  nt <- 200 # This takes a while...


#Run the sampler
start <- Sys.time() #start time for the sampler
mcmc.output <- nimbleMCMC(code = brsp_model_code,
                          data = brsp_dat,
                          constants = brsp_const,
                          dimensions = brsp_dims,
                          inits =brsp_inits,
                          monitors = brsp_params,
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
save(brsp_mcmc_comp, file = "Bayes_Files/brsp_distance_model_out.rda")

################################################################################
# 3) Model output and diagnostics ##############################################
################################################################################

# 3.1) View model output

#load the output back in
mcmc_out <- load(file = "Bayes_Files/brsp_distance_model_out.rda")

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

