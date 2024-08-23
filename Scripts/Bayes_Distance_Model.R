#----------------------------------------------------------------
#Will Harrod
#Hierarchical distance sampling for sagebrush songbird point count data
#
# Modefied from Kery et al. 2024: Integrated distance sampling models for simple point counts 
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
#Data Prep  ####################################################################
################################################################################

# #Add in Data from local drive
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# #or from github
# sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_data.csv") %>%
#   dplyr::select(-X) %>% 
#    tibble()

#view the data
glimpse(sobs)

#add covariates from the local drive
covs <- tibble(read.csv("Data/Outputs/point_summaries.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/point_summaries.csv") %>%
#   dplyr::select(-X) %>% 
#   tibble()

#View covariates
glimpse(covs)

#Prepare the count level data #####################################################################################################################

#define relevant species
soi <- "BRSP"

#define a truncation distance
trunc_dist <- 0.125

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>%
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Species  == soi) %>%
  filter(Distance <= trunc_dist) %>% 
  group_by(Full.Point.ID, Route.ID, Observer.ID, Year, Visit, Date, Species) %>% 
  reframe(Full.Point.ID, Route.ID, Observer.ID, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()

#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  tidyr::expand(nesting(Full.Point.ID, Route.ID, Point.Time, Year, Sky.Start,
                        MAS, Ord.Date, Wind.Start, Temp.Start, Observer.ID,
                        Visit, Date))

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Full.Point.ID', 'Route.ID', 'Observer.ID',
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  mutate(Survey.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))

#...and view
glimpse(sobs_count)

#Change necessary variables to scales and factors
brsp_count <- sobs_count %>%
  left_join(covs, by = c("Route.ID", "Full.Point.ID")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>%
  mutate(Burn.Sevarity = case_when(is.na(Burn.Sevarity) ~ 0,
                                   TRUE ~ Burn.Sevarity)) %>%
  mutate(Burned = as.numeric(factor(Route.Type, levels = c("R", "B"))) - 1) %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown',
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown', #change the wind data
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>%
  mutate(Sky.Start = factor(Sky.Start, levels = c("Clear", "Partly Cloudy", "Cloudy",
                                                  "Drizzle", "Fog or Smoke"))) %>%
  mutate_at(c("Aspect", "Burn.Sevarity", "Route.ID",
              "Fire.Name", "Visit.ID", "Observer.ID"
  ), factor) %>% 
  dplyr::select(-Species, -Route.Type, -Fire.Name, -Fire.Year) %>% 
  arrange(Year, Visit, Route.ID, Full.Point.ID)

# Fill in years since fire so there are no NA's for BUGs to get mad about
for(i in 1:nrow(brsp_count)) {
  if(is.na(brsp_count$Years.Since.Fire[i])){
    brsp_count$Years.Since.Fire[i] <- floor(rnorm(1, 115, 10))
  }
}

#...and view
glimpse(brsp_count)

#Isolate the burned routes as their own object
fire_stats <- brsp_count %>% 
  filter(Burned == 1) %>% 
  dplyr::select(Full.Point.ID, Years.Since.Fire, Burn.Sevarity)

#Find the mean and standard deviation of the "real" burns
mean_burnY <- mean(fire_stats$Years.Since.Fire)
sd_burnY <- sd(fire_stats$Years.Since.Fire)
#...and view
print(c(mean_burnY, sd_burnY))

#Prepare the observation level data ####################################################################################

#define a bin size
bin_size <- 0.025

#Create all observations of a single species for the detection function
sobs_obs <- sobs %>% 
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Species == soi) %>% #only one species
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% # Same order as the others
  filter(Distance <= trunc_dist) %>% #Only close observations
  mutate(Burned = as.numeric(factor(Route.Type, levels = c("R", "B"))) - 1) %>%
  dplyr::select(Distance, Minute, How.Detected, 
                Route.ID, Full.Point.ID, Observer.ID, Year, Visit,
                Burned, Ord.Date, 
                MAS) %>% 
  mutate(Visit.ID = paste(Year, Visit, sep = "-")) %>% 
  left_join(covs, by = c("Route.ID", "Full.Point.ID")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  mutate(Survey.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>% 
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
  dplyr::select(Dist.Bin, Dist.Bin.Midpoint, Survey.ID)

#view the whole object
glimpse(sobs_obs)
unique(sobs_obs$Dist.Bin)
unique(sobs_obs$Dist.Bin.Midpoint)

#Histogram of distances
# sobs_obs %>%
# ggplot(aes(x = Distance)) +
# geom_histogram(binwidth = 0.025, col = "darkgray", fill = "lightblue")
#That's a great looking detection histogram right there

#Reorder visits and observations so that they are shared between the two
brsp_count <- brsp_count %>% 
  arrange(Survey.ID) %>% 
  mutate(Survey.ID.Fact = as.factor(Survey.ID)) %>% 
  mutate(Survey.ID.num = as.numeric(Survey.ID.Fact)) %>% 
  dplyr::select(-Survey.ID.Fact) %>% 
  mutate(Visit.ID.num = as.numeric(Visit.ID))

#...and view
print("Counts:")
glimpse(brsp_count)

#Pull out just the visit ID
survey_ids <- brsp_count %>% 
  dplyr::select(Survey.ID, Survey.ID.num, Visit.ID.num)

#...and view
glimpse(survey_ids)

#Link the factor levels from the count dataset to the observation dataset
brsp_obs <- sobs_obs %>% 
  left_join(survey_ids, by = "Survey.ID")

#...and view
print("Observations:")
glimpse(brsp_obs)

################################################################################
#The model building part of the code ###########################################
################################################################################

#Define loop sizes -----
nsites <- nrow(brsp_count) # Number of sites
nind <- nrow(brsp_obs) # Number of individuals detected 
nobs <- length(unique(as.numeric(brsp_count$Observer.ID))) # Number of unique observers
nbins <- length(unique(brsp_obs$Dist.Bin)) # Number of distance bins
nvst <- length(unique(brsp_count$Visit.ID.num)) # number of visits in data

# Constants to be fed into Nimble ----
nimble_const <- list (
  ## Misc. Constants
  area = pi * trunc_dist^2, # Area associated with the DS data
  delta = bin_size, # Bin width
  newB = trunc_dist, # Truncation distance
  
  #For loop sizes
  nsites = nsites, # Number of sites
  nind = nind, # Number of individuals detected 
  nobs = nobs, # Number of unique observers
  nbins = nbins, # Number of distance bins
  nvst = nvst # number of visits in data
)

#Define my data objects ----
# Observation Level data 
dist_class <- brsp_obs$Dist.Bin # Distance category for each observation
points <- brsp_obs$Survey.ID.num # point number of each observation 
midpoints <- unique(brsp_obs$Dist.Bin.Midpoint) # Midpoints of distance bins
observers <- as.numeric(brsp_count$Observer.ID) #Random effect for observer associated with each survey

#Point level data
ncap <- brsp_count$Count # Number of detected individuals per site
year <- as.numeric(as.factor(brsp_count$Year)) # year number
burned <- brsp_count$Burned # Habitat covariate
fire_year <- (brsp_count$Years.Since.Fire -mean_burnY)/ sd_burnY
precip <- (brsp_count$Precipitation -mean(brsp_count$Precipitation)) / sd(brsp_count$Precipitation) #scaled precipitation covariate
time <- (brsp_count$MAS - mean(brsp_count$MAS)) / sd(brsp_count$MAS) # scaled based on mean time after sunrise
day <- (brsp_count$Ord.Date - mean(brsp_count$Ord.Date)) / sd(brsp_count$Ord.Date) #scaled data covariate

# Data to be fed into Nimble ----
nimble_dat <- list(
  # Observation Level data 
  dclass = dist_class, # Distance category for each observation
  point = points, # point number of each observation 
  midpt = midpoints, # Midpoints of distance bins
  observers = observers, #Random effect for observer associated with each survey
  
  #Point level data
  ncap = ncap, # Number of detected individuals per site
  year = as.numeric(as.factor(brsp_count$Year)), # year number
  burned = burned, # Habitat covariate
  f_year = fire_year,
  precip = precip, #scaled precipitation covariate
  time = time, # scaled based on mean time after sunrise
  day = day #scaled data covariate
)

# Model definition in the BUGS language ----
# Add random noise in the detection function intercepts and 
# Note all distances are in units of 1km (and area in 1km2 units)
model <- nimbleCode({
      # Priors for all parameters in DS model
      # ------------------------------------------------------------------
    
      # Random noise on availability
      tau.eps <- sd.eps^-2
      # Magnitude of that noise
		  sd.eps ~ T(dt(0, 1, 1), 0,)  #truncated to be greater than 0
		    
      # Covariates:
      alpha1 ~ dnorm(0, 1)     #shrub cover

      # Shared parameters in the availability component of the detection model
      gamma0 <- log(mean.phi)  # phi intercept on log scale and ...
      mean.phi ~ dunif(0, 1)   # ... on natural scale
	    # mean.phi ~ dbeta(2, 2)   # ... on natural scale
      gamma1 ~ dnorm(0, 1)     # Effect of day of year on singing rate
      gamma2 ~ dnorm(0, 1)     # Effect of day of year on singing rate (quadratic)
      gamma3 ~ dnorm(0, 1)     # Effect of time of day on singing rate
      gamma4 ~ dnorm(0, 1)     # Effect of time of day on singing rate (quadratic)

      # Shared parameters in the abundance model
      # Random intercept for each year
      for (i in 1:nvst) {     # Loop over 7 years
        beta0[i] ~ dnorm(beta0.int, tau.beta0)
      }
      beta0.int <- log(mean.lambda)  # lambda intercept on log scale and ...
      mean.lambda ~ T(dnorm(0, 0.001), 0,) # ... on natural scale
      tau.beta0 <- sd.beta0^-2 
      # Magnitude of that noise on lambda intercept
      sd.beta0 ~ T(dt(0, 1, 1), 0,)  #truncated to be greater than 0
      
      # Random intercept for each observer
      for (o in 1:nobs) {     # Loop over 18 observers
        rf.alpha0[o] ~ dnorm(alpha0, tau.alpha0)
      }
      alpha0 <- log(mean.sigma)  # sigma intercept on log scale and ...
      mean.sigma ~ dunif(0, 1) # ... on the natural scale (0 - 1 km)
      tau.alpha0 <- sd.alpha0^-2
      sd.alpha0 ~ T(dt(0, 1, 2), 0,) # Magnitude of noise in sigma intercept
      
      # Covariates:
      beta1 ~ dnorm(0, 1)      # Effect of burned vs unburned
      beta2 ~ dnorm(0, 0.1)    # Effect of years since fire only on burned routes
      beta3 ~ dnorm(0, 0.1)    # effect of years since fire squared only on burned routes
      beta4 ~ dnorm(0, 0.1)    # effect of precipitation
      beta5 ~ dnorm(0, 0.1)    # effect of years since fire and precipitation only on burned routes

      # -------------------------------------------------------------------
      # Hierarchical construction of the likelihood
      # Model for binned distance observations of every detected individual
      for(i in 1:nind){       # Loop over all detected individuals
        dclass[i] ~ dcat(fc[point[i], ])      # distance classes follow a multinational distribution
      }
      
      # Construction of the cell probabilities for the nD distance bands
      # This is for the truncation distance for the data (here, newB = 0.125 km)

      for(s in 1:nsites){  # Loop over all sites in data set 1
        for(g in 1:nbins){       # midpt = mid-point of each distance band
          log(p[s,g]) <- -midpt[g] * midpt[g] / (2 * sigma[s]^2)
          pi[s,g] <- ((2 * midpt[g] ) / newB^2) * delta # prob. per interval
          f[s,g] <- p[s,g] * pi[s,g]
          fc[s,g] <- f[s,g] / pcap[s] 
        }
        # Rectangular approx. of integral that yields the Pr(capture)
        pcap[s] <- sum(f[s, ])
        
        ### Log-linear models on abundance, detectability, and availability
        # Abundance (lambda) Log-linear model for abundance
        log(lambda[s]) <- beta0[year[s]] +           # random intercept for each year of the survey
        beta1 * burned[s] +                          # most important effect: burned vs unburned
        beta2 * f_year[s] * burned[s] +              # Effect of years since fire only on burned routes
        beta3 * f_year[s]^2 * burned[s] +            # effect of years since fire squared only on burned routes
        beta4 * precip[s] +                          # Effect of precipitation
        beta5 * f_year[s] * precip[s] * burned[s]    # Effect of years since fire and precipitation only on burned routes
        
        # Detectability/perceptability (sigma)
        log(sigma[s]) <- rf.alpha0[observers[s]] + eps[s]  # Log-Linear model for detection probability
		      eps[s] ~ dnorm(0, tau.eps)        # Note here eps1 has one precision and below another

        # Availability (phi)
		    # Log-linear model for availability
        log(phi[s]) <- gamma0 +                       #Intercept
        gamma1 * day[s] +                            # Effect of scaled ordinal date
        gamma2 * day[s]^2 +                    # Effect of scaled ordinal date squared
        gamma3 * time[s] +                           # Effect of scaled time of day
        gamma4 * time[s]^2                     # Effect of scaled time of day aquared
        
        theta[s] <- 1 - exp(-phi[s])                   # link function to convert linear comb to a probability

        # Multiply availability with detection probability
        pDS[s] <- pcap[s] * theta[s]

        ### Binomial mixture part (in conditional multinomial specification)
        ncap[s] ~ dbin(pDS[s], N_indv[s])            # Part 2 of HM: number captured
        N_indv[s] ~ dpois(area * lambda[s])          # Note use of area as an offset

		  # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
        # Compute fit statistic for observed data 
        eval[s] <- pDS[s] * N_indv[s]
        EDS[s] <- (sqrt(ncap[s]) - sqrt(eval[s]))^2

        # Generate replicate DS count data and compute same fit stats for them
        ncap.new[s] ~ dbin(pDS[s], N_indv[s])
        EDS.new[s] <- (sqrt(ncap.new[s]) - sqrt(eval[s]))^2
	   }
      # Add up fit stats across sites for DS data
      fit <- sum(EDS[])
      fit.new <- sum(EDS.new[])
      
      # Compute Bayesian p-value for distance sampling data
      bpv <- step(fit.new - fit)
})

#set seed
set.seed(123)

# Inits ----
inits <- function(){list(mean.sigma = runif(1, 0, 1), 
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
                         N_indv = brsp_count$Count+1)}  

#Object dimessions
dims <- list(
  f = c(nsites, nbins),
  fc = c(nsites, nbins),
  EDS = nsites,
  EDS.new = nsites
)

# Params to save ----
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
            "sd.beta0", 
            "beta0.int",
            "fit", 
            "fit.new", 
            "bpv")

# MCMC settings. Pick one, comment out the rest ----
#  nc <- 3  ;  ni <- 50  ;  nb <- 2  ;  nt <- 2 # test, 30 sec
nc <- 3  ;  ni <- 30000  ;  nb <- 10000  ;  nt <- 5 # longer test
# nc <- 4;  ni <- 120000;  nb <- 60000;  nt <- 60   # As for the paper
# nc <- 10;  ni <- 400000;  nb <- 200000;  nt <- 200 # takes a while...


# Launch Nimble, run my sampler, check convergence and summarize posteriors ----
start <- Sys.time() #start time for the model

# run the model
mcmc.out <- nimbleMCMC(code = model,
                       dimensions = dims,
                       data = nimble_dat,
                       constants = nimble_const,
                       inits = inits,
                       monitors = params,
                       niter = ni,
                       nburnin = nb,
                       thin = nt,
                       nchains = nc)

# end time for the model
difftime(Sys.time(),start)

#Save model output
save(mcmc.out, file = "Bayes_Files/brsp_distance_model_out.rda")

################################################################################
#Model diagnostics #############################################################
################################################################################

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
            "sd.beta0", 
            "beta0.int",
            "fit", 
            "fit.new", 
            "bpv")

#View MCMC summary
MCMCsummary(object = mcmc.out, round = 2)

#View an MCMC plot
MCMCplot(object = mcmc.output,
         params = params)

#Traceplots and density graphs 
MCMCtrace(object = mcmc.output,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = params)
