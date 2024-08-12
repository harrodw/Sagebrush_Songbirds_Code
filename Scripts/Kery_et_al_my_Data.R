#----------------------------------------------------------------
# Kery et al. 2024: Integrated distance sampling models for simple point counts 
# Case study
# Analyse Distance Sampling (DS) data from Oregon2020 together with 
# EBird Point Count (PC) data 
# Marc K?ry, Nicolas Strebel, Tyler Hallman, Kenneth F. Kellner
# March 2022 - June 2023 
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
#   select(-X)

#view the data
glimpse(sobs)

#add covariates from the local drive
covs <- tibble(read.csv("Data//Outputs//point_summaries.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/point_summaries.csv") %>%
#   select(-X)

#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
soi <- "BRSP"

#define a truncation distance
trunc_dist <- 0.125

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>%
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Species  == soi) %>%
  filter(Distance <= trunc_dist) %>% 
  group_by(Full.Point.ID, Route.ID, Year, Visit, Date, Species) %>% 
  reframe(Full.Point.ID, Route.ID, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()

#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  tidyr::expand(nesting(Full.Point.ID, Route.ID, Point.Time, Year, Sky.Start,
                        MAS, Ord.Date, Wind.Start, Temp.Start, 
                        Visit, Date))

#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Full.Point.ID', 'Route.ID', 
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  mutate(Survey.ID = paste(Full.Point.ID, Year, sep = "-"))

#...and view
glimpse(sobs_count)

#Change necessary variables to scales and factors
brsp_count <- sobs_count %>%
  filter(Visit == "V1") %>%  #Only the first visit from each year
  left_join(covs, by = c("Route.ID", "Full.Point.ID")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
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
              "Fire.Name"
              #, "Observer.ID"
              ), factor) %>% 
  dplyr::select(-Species, -Route.Type, -Fire.Name, -Fire.Year) %>% 
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% 
  #change minutes after sunrise to a scale
  mutate(MAS = scale(MAS)) %>% 
  #make route ID a factor
  mutate(Route.ID = as.numeric(as.factor(Route.ID))) 
  #make observer Id a factor 
  # mutate(Observer.ID = as.numeric(Observer.ID))

# Fill in years since fire
for(i in 1:nrow(brsp_count)) {
  if(is.na(brsp_count$Years.Since.Fire[i])){
    brsp_count$Years.Since.Fire[i] <- floor(rnorm(1, 100, 20))
  }
}

#...and view
glimpse(brsp_count)

#define a bin size
bin_size <- 0.025

#Create all observations of a single species for the detection function
brsp_obs <- sobs %>% 
  mutate(Distance = Distance/1000) %>% #Switch from m to km
  filter(Species == soi) %>% #only one species
  filter(Visit == "V1") %>%  #Only the first visit from each year
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% # Same order as the others
  filter(Distance <= trunc_dist) %>% #Only close observations
  mutate(Burned = factor(Route.Type, levels = c("R", "B"))) %>% 
  dplyr::select(Distance, Minute, How.Detected, 
                Route.ID, Full.Point.ID, Observer.ID, Year, Visit,
                Burned, Ord.Date, 
                MAS) %>% 
  left_join(covs, by = c("Route.ID", "Full.Point.ID")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  dplyr::select(-Burn.Sevarity, -Perennial.Cover, - Fire.Distance,
                -Fire.Name, - Fire.Year, - Route.Type, - Aspect) %>% 
  mutate(Survey.ID = paste(Full.Point.ID, Year, sep = "-")) %>% 
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
for(i in 1:nrow(brsp_obs)) {
  if(is.na(brsp_obs$Years.Since.Fire[i])){
    brsp_obs$Years.Since.Fire[i] <- as.integer(rnorm(1, 100, 20))
  }
}

#... and view
glimpse(brsp_obs)
unique(brsp_obs$Dist.Bin)
unique(brsp_obs$Dist.Bin.Midpoint)

#Histogram of distances
# brsp_obs %>%
  # ggplot(aes(x = Distance)) +
  # geom_histogram(binwidth = 0.025, col = "darkgray", fill = "lightblue")
#That's a great looking detection histogram right there

#view both data sets
# glimpse(brsp_obs)
# glimpse(brsp_count)

#Reorder visits and observations so that they are shared between the two
brsp_count <- brsp_count %>% 
  arrange(Survey.ID) %>% 
  mutate(Survey.ID.Fact = as.factor(Survey.ID)) %>% 
  mutate(Survey.ID.num = as.numeric(Survey.ID.Fact)) %>% 
  select(-Survey.ID.Fact)

#...and view
print("Counts:")
glimpse(brsp_count)

#Pull out just the visit ID
survey_ids <- brsp_count %>% 
  select(Survey.ID, Survey.ID.num)

#...and view
glimpse(visit_ids)

#only Y3 V1 observations
brsp_obs <- brsp_obs %>% 
  left_join(survey_ids, by = "Survey.ID")

#...and view
print("Observations:")
glimpse(brsp_obs)

# Data set to be fed into JAGS
str(brsp_data <- list(
  
  ### DS data set:
  nsites_DS = nrow(brsp_count), # Number of sites  
  nind = nrow(brsp_obs), # Number of individuals detected 
  newB = trunc_dist, # Truncation distance
  nD = length(unique(brsp_obs$Dist.Bin)), # Number of distance bins
  midpt = unique(brsp_obs$Dist.Bin.Midpoint), # Midpoints of distance bins
  ncap = brsp_count$Count, # Number of detected individuals per site
  dclass = brsp_obs$Dist.Bin, # Distance category for each observation
  siteDS = brsp_obs$Survey.ID.num, # Site number of each observation  
  A_DS = pi * trunc_dist^2, # Area associated with the DS data
  delta = bin_size, # Bin width
  DSduration = rep(6, nrow(brsp_count)), # Survey duration 
  nyear = length(unique(brsp_count$Year)), # number of years in data
  # Covariates on abundance
  year_DS = as.numeric(as.factor(brsp_count$Year)), # year number
  habitat_DS = brsp_count$Burned, # Habitat covariate
  # Covariates on detectability
  shrubcovdetect_DS = (brsp_count$Shrub.Cover - mean(brsp_count$Shrub.Cover)) / sd(brsp_count$Shrub.Cover), # Habitat covariate; scale so that 0 = -0.5, 50 = 0, 100 = 0.5 
  # Covariates on availability
  day_DS = (brsp_count$Ord.Date - mean(brsp_count$Ord.Date))/sd(brsp_count$Ord.Date), # 
  time_DS = (brsp_count$MAS[,1] - mean(brsp_count$MAS[,1]))/sd(brsp_count$MAS[,1])
))

# Model definition in the BUGS language
# Add random noise in the detection function intercepts and 
# allow the variance to be different by design (DS vs. PC)
# Note all distances are in units of 1km (and area in 1km2 units)
cat(file="IDS1_CaseStudy_model_5.txt",
"
    model{
      
      # Priors for all parameters in IDS model
      # -------------------------------------------------------------------
      # Partly different parameters in the detectability-perceptibility component 
	    # of the detection model for the DS and the PC portions of the data
      
        alpha0 <- log(mean.sigma)  # sigma intercept on log scale and ...
        mean.sigma ~ dunif(0, 1)    # ... on the natural scale (0 - 1 km)
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
      for (i in 1:nyear) {     # Loop over 7 years
        ann.beta0[i] ~ dnorm(beta0, tau.beta0)
      }
      beta0 <- log(mean.lambda)  # lambda intercept on log scale and ...
      mean.lambda ~ dnorm(0, 0.001)I(0,) # ... on natural scale
      tau.beta0 <- pow(sd.beta0,-2) 
      sd.beta0 ~ dt(0, 1, 2)I(0,) # Magnitude of noise in lambda intercept
      # Covariates:
      beta1 ~ dnorm(0, 1)      # Effect of habitat (burned) on abundance
      beta2 ~ dnorm(0, 0.1)    # Effect of habitat (burned) on abundance (quadratic)

      # Submodel for the DS data
      # -------------------------------------------------------------------
      # Hierarchical construction of the likelihood
      # Model for binned distance observations of every detected individual
      for(i in 1:nind){       # Loop over all detected individuals
        dclass[i] ~ dcat(fc[siteDS[i],])               # Part 1 of HM
      }
      
      # Construction of the cell probabilities for the nD distance bands
      # This is for the truncation distance for the DS data (here, newB = 0.3 km)

      for(s in 1:nsites_DS){  # Loop over all sites in data set 1
        for(g in 1:nD){       # midpt = mid-point of each distance band
          log(p[s,g]) <- -midpt[g] * midpt[g] / (2 * sigma[s]^2)
          pi[s,g] <- ((2 * midpt[g] ) / newB^2) * delta # prob. per interval
          f[s,g] <- p[s,g] * pi[s,g]
          fc[s,g] <- f[s,g] / pcap[s] 
        }
        # Rectangular approx. of integral that yields the Pr(capture)
        pcap[s] <- sum(f[s,])
        
        ### Log-linear models on abundance, detectability, and availability
        # Abundance (lambda)
        log(lambda[s]) <- ann.beta0[year_DS[s]] + beta1 * habitat_DS[s] + beta2 * pow(habitat_DS[s],2) # Log-linear model for abundance 
        
        # Detectability/perceptability (sigma)
        log(sigma[s]) <- alpha0 + alpha1 * shrubcovdetect_DS[s] + eps[s]  # Log-Linear model for detection probability
		      eps[s] ~ dnorm(0, tau.eps)        # Note here eps1 has one precision and below another

        # Availability (phi)
        log(phi[s]) <- gamma0 + gamma1 * day_DS[s] + gamma2 * pow(day_DS[s],2) + gamma3 * time_DS[s] + gamma4 * pow(time_DS[s],2)  # Log-linear model for availability
        theta[s] <- 1-exp(-DSduration[s]*phi[s])  # Effect of duration on availability

        # Multiply availability with detection probability
        pDS[s] <- pcap[s] * theta[s]

        ### Binomial mixture part (in conditional multinomial specification)
        ncap[s] ~ dbin(pDS[s], N_brsp[s])  # Part 2 of HM: number captured
        N_brsp[s] ~ dpois(A_DS * lambda[s])  # Note use of A_DS as an offset

		  # Assess model fit: compute Bayesian p-value for Freeman-Tukey discrepancy
        # Compute fit statistic for observed data (DS data portion)
        evalDS[s] <- pDS[s] * N_brsp[s]
        EDS[s] <- pow((sqrt(ncap[s]) - sqrt(evalDS[s])), 2)

        # Generate replicate DS count data and compute same fit stats for them
        ncap.new[s] ~ dbin(pDS[s], N_brsp[s])
        EDS.new[s] <- pow((sqrt(ncap.new[s]) - sqrt(evalDS[s])), 2)
	   }
      # Add up fit stats across sites for DS data
      fitDS <- sum(EDS[])
      fitDS.new <- sum(EDS.new[])
      
      # Compute Bayesian p-value for distance sampling data
      bpvDS <- step(fitDS.new - fitDS)
    }
    "
    )

# Inits
inits <- function(){list(mean.sigma = runif(1, 0, 1), alpha1 = rnorm(1, 0, 0.1), 
                         mean.phi = runif(1, 0, 1), gamma1 = rnorm(1, 0, 0.1), gamma2 = rnorm(1, 0, 0.1), 
                         gamma3 = rnorm(1, 0, 0.1), gamma4 = rnorm(1, 0, 0.1),
                         mean.lambda = runif(1, 1, 60), beta1 = rnorm(1, 0, 0.1), beta2 = rnorm(1, 0, 0.1), 
						 sd.eps = runif(1, 0.1, 1), N_brsp = brsp_count$Count+1)}  

# Params to save
params <- c("mean.sigma", "alpha0", "alpha1",  "sd.eps",
            "mean.phi", "gamma0", "gamma1", "gamma2", "gamma3", "gamma4", 
            "mean.lambda", "beta0", "beta1", "beta2", "sd.beta0", "ann.beta0",
			"fitDS", "fitDS.new", "fitPC", "fitPC.new", "bpvDS", "bpvPC")

# MCMC settings
# na <- 10  ;  nc <- 4  ;  ni <- 12  ;  nb <- 2  ;  nt <- 2 # test, 30 sec
na <- 100;  nc <- 3;  ni <- 10000;  nb <- 1000;  nt <- 10   # A little bit more thurough of a test
# na <- 10000;  nc <- 4;  ni <- 120000;  nb <- 60000;  nt <- 60   # As for the paper
# na <- 10000;  nc <- 10;  ni <- 400000;  nb <- 200000;  nt <- 200 # takes a while...


# Launch JAGS, check convergence and summarize posteriors ----
library(jagsUI)
start <- Sys.time()
set.seed(123)
jags_out <- jags(brsp_data, inits, params, "IDS1_CaseStudy_model_5.txt", n.adapt = na,
             n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt, parallel = TRUE)
difftime(Sys.time(),start)
traceplot(jags_out)
print(jags_out, 2) 

# Visualize and summarize results  ----
library(sf); library(terra)

# Load environmental data
head(centroids <- read.table("data/env_centroids.csv",header=T,sep=";",as.is = T))

#load in file with desired crs
polygon.grid <- st_read("data/geodata/grid_1km.shp")

# mask for study area
mask <- st_read("data/geodata/StudyArea.shp")

# adjust variables as they were in the model data
centroids$cancov.scaled <- (centroids$cancov - 50)/100
centroids$elev.scaled <- (centroids$elev - mean.elev)/sd.elev

# Create a function to make predictions based on model output
predict.abundances <- function(environmental = environmental, parameters = parameters){
  prediction <- exp(parameters[1] + parameters[2] * environmental$cancov.scaled + parameters[3] * I(environmental$cancov.scaled^2) + 
                      parameters[4] * environmental$elev.scaled + parameters[5] * I(environmental$elev.scaled^2))
  return(prediction)
}

# Extract model output samples as a matrix
samples.matrix <- as.matrix(out5A$samples)
test.raster <- rast(nlyrs=1, crs = crs(vect(polygon.grid)), extent = ext(polygon.grid), resolution = 1000)

### Plot map
centroids.predictions <- apply(samples.matrix[,16:20], 1, FUN = predict.abundances, environmental = centroids) # Make predictions for each km-square and iteration
centroids.predictions <- cbind(centroids, med=apply(centroids.predictions,1,median))
centroids.predictions.output_sf = st_as_sf(centroids.predictions, coords = c("X", "Y"), crs = st_crs(polygon.grid))
centroids.predictions.output_rast <- rasterize(vect(centroids.predictions.output_sf), test.raster, field = "med")
centroids.predictions.output_rast <- mask(centroids.predictions.output_rast, vect(mask)) 

# Plot density map
plot(centroids.predictions.output_rast,main="Estimates based on jags analysis")

### Estimate abundance throughout study area for each sample
# Select km-squares within study area 
within.study.area <- !(is.na(values(centroids.predictions.output_rast)))

# Make predictions for each km-square and iteration
centroids.predictions <- apply(samples.matrix[,16:20], 1, FUN = predict.abundances, environmental = centroids[within.study.area,])

# Summarize population estimates
pop.estimates <- apply(centroids.predictions,2,sum)

(mean.pop.estimate <- mean(pop.estimates)) # Mean for the entire study area
(uci.pop.estimate <- quantile(pop.estimates, probs = 0.975)) # upper
(lci.pop.estimate <- quantile(pop.estimates, probs = 0.025)) # lower

round(range(apply(centroids.predictions,1,median)),1) # range of median estimates per km-square

# Write raster
writeRaster(centroids.predictions.output_rast, "IDS_AMRO_PredictedAbundance.tif", filetype = "GTiff", overwrite = TRUE)