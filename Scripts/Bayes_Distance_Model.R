#----------------------------------------------------------------
#Will Harrod
#Hierarchical distance sampling for sagebrush songbird point count data
#
# Modefied from Kery et al. 2024: Integrated distance sampling models for simple point counts 
# Marc Kary, Nicolas Strebel, Tyler Hallman, Kenneth F. Kellner
# August 2024
#---------------------------------------------------------------

#add packages
library(jagsUI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(plyr)

#My data Prep  ###########################################################################################
# #Add in Data from local drive
sobs <- read.csv("Data//Outputs//sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# #or from github
# sobs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/sobs_data.csv") %>%
#   dplyr::select(-X)

#view the data
glimpse(sobs)

#add covariates from the local drive
covs <- tibble(read.csv("Data//Outputs//point_summaries.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/point_summaries.csv") %>%
#   dplyr::select(-X)

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

#Look into the fire year data
brsp_count %>% 
  group_by(Full.Point.ID, Years.Since.Fire) %>% 
  reframe(Full.Point.ID, Years.Since.Fire) %>% 
  distinct(Full.Point.ID, Years.Since.Fire) %>% 
  print(n = Inf)

# Fill in years since fire
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
  dplyr::select(-Burn.Sevarity, -Perennial.Cover, - Fire.Distance,
                -Fire.Name, - Fire.Year, - Route.Type, - Aspect) %>% 
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
  ))

#Fill in years since fire
for(i in 1:nrow(sobs_obs)) {
  if(is.na(sobs_obs$Years.Since.Fire[i])){
    sobs_obs$Years.Since.Fire[i] <- as.integer(rnorm(1, 115, 10))
  }
}

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
  select(-Survey.ID.Fact) %>% 
  mutate(Visit.ID.num = as.numeric(Visit.ID))

#...and view
print("Counts:")
glimpse(brsp_count)

#Pull out just the visit ID
survey_ids <- brsp_count %>% 
  select(Survey.ID, Survey.ID.num, Visit.ID.num)

#...and view
glimpse(survey_ids)

#Link the factor levels from the count dataset to the observation dataset
brsp_obs <- sobs_obs %>% 
  left_join(survey_ids, by = "Survey.ID")

#...and view
print("Observations:")
glimpse(brsp_obs)

# Dataset to be fed into JAGS ----
brsp_data <- list(
  ### Brewer's Sparrow data set:
  nsites = nrow(brsp_count), # Number of sites  
  nind = nrow(brsp_obs), # Number of individuals detected 
  nobs = length(unique(as.numeric(brsp_count$Observer.ID))), # Number of unique observers
  newB = trunc_dist, # Truncation distance
  nD = length(unique(brsp_obs$Dist.Bin)), # Number of distance bins
  midpt = unique(brsp_obs$Dist.Bin.Midpoint), # Midpoints of distance bins
  ncap = brsp_count$Count, # Number of detected individuals per site
  dclass = brsp_obs$Dist.Bin, # Distance category for each observation
  point = brsp_obs$Survey.ID.num, # point number of each observation 
  area = pi * trunc_dist^2, # Area associated with the DS data
  delta = bin_size, # Bin width
  nvst = length(unique(brsp_count$Visit.ID.num)), # number of visits in data
  #covariates on detecability
  observers = as.numeric(brsp_count$Observer.ID), #Random effect for observer associated with each survey
  #covariates on availability
  time = (brsp_count$MAS - mean(brsp_count$MAS))/sd(brsp_count$MAS), # scaled based on mean time after sunrise
  day = (brsp_count$Ord.Date - mean(brsp_count$Ord.Date)) / sd(brsp_count$Ord.Date), #scaled data covariate
  # Covariates on abundance
  year = as.numeric(as.factor(brsp_count$Year)), # year number
  burned = brsp_count$Burned, # Habitat covariate
  f_year = (brsp_count$Years.Since.Fire -mean_burnY)/ sd_burnY,
  precip = (brsp_count$Precipitation -mean(brsp_count$Precipitation)) / sd(brsp_count$Precipitation) #scaled precipitation covariate
)

# Model definition in the BUGS language ----
# Add random noise in the detection function intercepts and 
# Note all distances are in units of 1km (and area in 1km2 units)
cat(file="brsp_distance_model.txt",
    "
    model{
      
      # Priors for all parameters in DS model
      # ------------------------------------------------------------------
    
      # Random noise on availability
      tau.eps <- pow(sd.eps, -2)
		  sd.eps ~ dt(0, 1, 1)I(0,)     # Magnitude of that noise
		    
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
        rf.beta0[i] ~ dnorm(beta0, tau.beta0)
      }
      beta0 <- log(mean.lambda)  # lambda intercept on log scale and ...
      mean.lambda ~ dnorm(0, 0.001)I(0,) # ... on natural scale
      tau.beta0 <- pow(sd.beta0,-2) 
      sd.beta0 ~ dt(0, 1, 2)I(0,) # Magnitude of noise in lambda intercept
      
      # Random intercept for each observer
      for (o in 1:nobs) {     # Loop over 18 observers
        rf.alpah0[o] ~ dnorm(alpha0, tau.alpha0)
      }
      alpha0 <- log(mean.sigma)  # sigma intercept on log scale and ...
      mean.sigma ~ dunif(0, 1) # ... on the natural scale (0 - 1 km)
      tau.alpha0 <- pow(sd.alpha0,-2) 
      sd.alpha0 ~ dt(0, 1, 2)I(0,) # Magnitude of noise in sigma intercept
      
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
        dclass[i] ~ dcat(fc[point[i],])               # Part 1 of HM
      }
      
      # Construction of the cell probabilities for the nD distance bands
      # This is for the truncation distance for the data (here, newB = 0.125 km)

      for(s in 1:nsites){  # Loop over all sites in data set 1
        for(g in 1:nD){       # midpt = mid-point of each distance band
          log(p[s,g]) <- -midpt[g] * midpt[g] / (2 * sigma[s]^2)
          pi[s,g] <- ((2 * midpt[g] ) / newB^2) * delta # prob. per interval
          f[s,g] <- p[s,g] * pi[s,g]
          fc[s,g] <- f[s,g] / pcap[s] 
        }
        # Rectangular approx. of integral that yields the Pr(capture)
        pcap[s] <- sum(f[s,])
        
        ### Log-linear models on abundance, detectability, and availability
        # Abundance (lambda) Log-linear model for abundance
        log(lambda[s]) <- rf.beta0[year[s]]           # random intercept for each year of the survey
        + beta1 * burned[s]                           # most important effect: burned vs unburned
        + beta2 * f_year[s] * burned[s]               # Effect of years since fire only on burned routes
        + beta3 * f_year[s]^2 * burned[s]             # effect of years since fire squared only on burned routes
        + beta4 * precip[s]                           # Effect of precipitation
        + beta5 * f_year[s] * precip[s] * burned[s]   # Effect of years since fire and precipitation only on burned routes
        
        # Detectability/perceptability (sigma)
        log(sigma[s]) <- rf.alpah0[observers[s]] + eps[s]  # Log-Linear model for detection probability
		      eps[s] ~ dnorm(0, tau.eps)        # Note here eps1 has one precision and below another

        # Availability (phi)
		    # Log-linear model for availability
        log(phi[s]) <- gamma0                        #Intercept
        + gamma1 * day[s]                            # Effect of scaled ordinal date
        + gamma2 * pow(day[s], 2)                    # Effect of scaled ordinal date squared
        + gamma3 * time[s]                           # Effect of scaled time of day
        + gamma4 * pow(time[s], 2)                   # Effect of scaled time of day aquared
        
        theta[s] <- 1-exp(-phi[s])                  # link function to convert linear comb to a probability

        # Multiply availability with detection probability
        pDS[s] <- pcap[s] * theta[s]

        ### Binomial mixture part (in conditional multinomial specification)
        ncap[s] ~ dbin(pDS[s], N_indv[s])            # Part 2 of HM: number captured
        N_indv[s] ~ dpois(area * lambda[s])          # Note use of area as an offset

		  # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
        # Compute fit statistic for observed data 
        eval[s] <- pDS[s] * N_indv[s]
        EDS[s] <- pow((sqrt(ncap[s]) - sqrt(eval[s])), 2)

        # Generate replicate DS count data and compute same fit stats for them
        ncap.new[s] ~ dbin(pDS[s], N_indv[s])
        EDS.new[s] <- pow((sqrt(ncap.new[s]) - sqrt(eval[s])), 2)
	   }
      # Add up fit stats across sites for DS data
      fit <- sum(EDS[])
      fit.new <- sum(EDS.new[])
      
      # Compute Bayesian p-value for distance sampling data
      bpv <- step(fit.new - fit)
    }
    "
)

# Inits ----
inits <- function(){list(mean.sigma = runif(1, 0, 1), 
                         mean.phi = runif(1, 0, 1), 
                         gamma1 = rnorm(1, 0, 0.1), 
                         gamma2 = rnorm(1, 0, 0.1), 
                         gamma3 = rnorm(1, 0, 0.1), 
                         gamma4 = rnorm(1, 0, 0.1),
                         mean.lambda = runif(1, 1, 60), 
                         beta1 = rnorm(1, 0, 0.1), 
                         beta2 = rnorm(1, 0, 0.1),
                         beta3 = rnorm(1, 0, 0.1),
                         beta4 = rnorm(1, 0, 0.1),
                         beta5 = rnorm(1, 0, 0.1),
                         sd.eps = runif(1, 0.1, 1), 
                         N_indv = brsp_count$Count+1)}  

# Params to save ----
params <- c("mean.sigma", "alpha0",  "sd.eps", "rf.alpha0",
            "mean.phi", 
            "gamma0", "gamma1", "gamma2", "gamma3", "gamma4", 
            "mean.lambda", "beta0", "beta1", "beta2", "beta3", "beta4"," beta5", "sd.beta0", "rf.beta0",
            "fit", "fit.new", "bpv")

# MCMC settings ----
# na <- 10  ;  nc <- 3  ;  ni <- 50  ;  nb <- 2  ;  nt <- 2 # test, 30 sec
na <- 500  ;  nc <- 3  ;  ni <- 30000  ;  nb <- 10000  ;  nt <- 5 # longer test
# na <- 10000;  nc <- 4;  ni <- 120000;  nb <- 60000;  nt <- 60   # As for the paper
# na <- 10000;  nc <- 10;  ni <- 400000;  nb <- 200000;  nt <- 200 # takes a while...


# Launch JAGS, check convergence and summarize posteriors ----
library(jagsUI)
start <- Sys.time()
set.seed(123)
jags_out <- jags(brsp_data, inits, params, "brsp_distance_model.txt", n.adapt = na,
                 n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt, parallel = TRUE)
difftime(Sys.time(),start)
traceplot(jags_out)
print(jags_out, 2) 
