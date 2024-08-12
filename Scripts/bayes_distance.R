#Practicing Bayesian modeling using my songbird data

#Clear Environments
rm(list = ls())

#add packages
library(jagsUI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(plyr)

#Prep ------------------------------------------------------------------------------------
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
  tibble
# or from github
# covs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/point_summaries.csv") %>%
#   select(-X)

#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
soi <- "BRSP"

#define a truncation distance
trunc_dist <- 125

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species  == soi) %>%
  filter(Distance <= trunc_dist) %>% 
  group_by(Full.Point.ID, Route.ID, Year, Visit, Date, Species) %>% 
  reframe(Full.Point.ID, Route.ID, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()

#...and view
# glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  tidyr::expand(nesting(Full.Point.ID, Route.ID, Point.Time, Year, Sky.Start, MAS,
                        Wind.Start, Temp.Start, Visit, Date, Observer.ID))

#...and view
# glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Full.Point.ID', 'Route.ID', 
                                    'Year', 'Visit', 'Date')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))

#...and view
# glimpse(sobs_count)
  
#...and view
# glimpse(brsp_count)

#Change necessary variables to scales and factors
brsp_count <- sobs_count %>% 
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
  mutate_at(c("Aspect", "Burn.Sevarity", 
              "Fire.Name", "Observer.ID", "Route.ID"), factor) %>% 
  dplyr::select(-Species, -Route.Type, -Fire.Name, -Fire.Year) %>% 
  arrange(Year, Visit, Route.ID, Full.Point.ID) %>% 
  #change minutes after sunrise to a scale
  mutate(MAS = scale(MAS)) %>% 
  #make observer ID a factor
  mutate(Observer.ID = factor(Observer.ID)) %>% 
  #make route ID a factor
  mutate(Route.ID = as.numeric(as.factor(Route.ID))) %>% 
  #make observer Id a factor 
  mutate(Observer.ID = as.numeric(as.factor(Observer.ID)))
  
# Fill in years since fire
for(i in 1:nrow(brsp_count)) {
  if(is.na(brsp_count$Years.Since.Fire[i])){
    brsp_count$Years.Since.Fire[i] <- floor(rnorm(1, 100, 20))
  }
}

#...and view
glimpse(brsp_count)

#define a bin size
bin_size <- 25

#Create all observations of a single species for the detection function
brsp_obs <- sobs %>% 
  filter(Species == soi) %>% #only one species
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
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>% 
  #Add in the bin sizes
  mutate(Dist.Bin = round_any(Distance, bin_size, floor)) %>% 
  mutate(Dist.Bin = case_when(Dist.Bin == 0 ~ 25,
                              TRUE ~ Dist.Bin))%>% 
  mutate(Dist.Bin.Midpoint = Dist.Bin - floor(bin_size/2)) %>% 
  mutate(Dist.Bin = case_when(Dist.Bin == 25 ~ 1,
                              Dist.Bin == 50 ~ 2,
                              Dist.Bin == 75 ~ 3,
                              Dist.Bin == 100 ~ 4,
                              Dist.Bin == 125 ~ 5))

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
#   ggplot(aes(x = Distance)) +
#   geom_histogram(binwidth = 25, col = "darkgray", fill = "lightblue")
#That's a great looking detection histogram right there

#view both data sets
# glimpse(brsp_obs)
# glimpse(brsp_count)

#isolate just a single visit for counts
count_v1 <- brsp_count %>% 
  filter(Year == "Y3" & Visit == "V1") %>% 
  arrange(Visit.ID) %>% 
  mutate(Visit.ID = as.factor(Visit.ID)) %>% 
  mutate(Visit.ID.num = as.numeric(Visit.ID))

#...and view
print("Counts:")
glimpse(count_v1)

#Pull out just the visit ID
visit_ids <- count_v1 %>% 
  select(Visit.ID, Visit.ID.num)

#...and view
# glimpse(visit_ids)

#only Y3 V1 observations
obs_v1 <- brsp_obs %>% 
  filter(Year == "Y3" & Visit == "V1") %>% 
  left_join(visit_ids, by = "Visit.ID")

#...and view
print("Observations:")
glimpse(obs_v1)

#JAGS ##############################################################################################
#The model: Single parameter model for abundance at a single visit  with detentions binned by distance ---------------------
sink("bayes_files\\brsp_model_distance")
cat(
  "
  model{
    #Priors ################
    
    #Detection level priors ----
    sigma_observer ~ dunif(0, 1)   #between observer variation in detection
    sigma2_observer <- pow(sigma_observer, 2)
    tau_observer <- pow(sigma_observer, -2)
    beta_pd_mas ~ dnorm(0, 0.1) #offset in detection probability for minutes after sunrise               
   
    #Random intercepts for observer ----
    for(o in 1: n_observers){
      alpha_observer[o] ~ dnorm(0, tau_observer)
    }
    
    #Abundance level priors ----
    sigma_route ~ dunif(0, 50) #Between route variation in abundance
    sigma2_route <- pow(sigma_route, 2)
    tau_route <- pow(sigma_route, -2)
    beta_lambda_burn ~ dnorm(0, 0.1) #difference in abundance between burned and ref routes
    
    #Random intercept for route ID
    for(j in 1: n_routes){
      alpha_route[j] ~ dnorm(0, tau_route) 
    }
    
    #Likelihood ##############################################################
    
    ##DETECTION PROBABILITY ------------------------------
    #Linear combination of covariates
    for(k in 1:n_surveys){
      #Log linear combination of predictors for detection probability
      ln_sigma[k] <- alpha_observer[observers[k]] + beta_pd_mas * mas[k]
      #Log link
      sigma[k] <- exp(ln_sigma[k])

      #Distance sampling detection probability estimation -----------------------
      # Using summation technique - Pr(p of x)=exp(-x^2/2*sigma^2)*f(x)
      for(b in 1:n_bins){
        # half-normal detection function - first half of eq.,
        ln_g[k, b] <- -(bin_midpoints[b]*bin_midpoints[b]) / (2*sigma[k]*sigma[k])
        #Log link
        g[k, b] <- exp(ln_g[k, b])
        # this is f(x), the scaled radial density function
        f[k, b] <- (2*bin_midpoints[b]*delta) / (max_dist*max_dist)

        #this is the product Pr(detect)*Pr(distribution)
        pi_pd[k, b]<- g[b, k]*f[k, b]
        #standardizing based on overall capture probability - conditional formulation
        pi_pd_c[k, b] <- pi_pd[k, b]/pd[k]
      } #b

      #probability of detection is the sum of all rectangular areas
      pd[k] <- sum(pi_pd[k,])
    }#k

    ######## Observation-level model ----------------------------------
    for(i in 1:n_observations){
      #single trial with categorical distribution linking distance class to survey point
      dist_class[i] ~ dcat(pi_pd_c[surveys_obs[i],])
    } #i

    ################# Abundance level model  -----------------------------
    for(k in 1:n_surveys){
      # Abundance
      #Linear combination of abundance predictors
      lin_comb_lambda[k] <- alpha_route[surveys_count[k]] + beta_lambda_burn * burned[k]
      #Log link
      lambda[k] <- exp(lin_comb_lambda[k])
      #Poisson process of birds being distributed across the landscape
      N[k] ~ dpois(lambda[k])
      
      #Detection process likelihood
      n[k] ~ dbin(pd[k], N[k])
    } #k
  } #end model
"
  , fill = TRUE)


#Initial values
brsp_inits <- function(){list(
  sigma_route = runif(1, 0, 50),
  sigma_observer = runif(1, 0, 50),
  beta_lambda_burn = rnorm(1, 0, 0.001),
  beta_pd_mas = rnorm(1, 0, 0.001),
  N = count_v1$Count + 1
)}

#Parameters to save
brsp_params <- c("pd", "beta_lambda_burn", "beta_pd_mas", "N")

#Bundle data
brsp_data <- list(
  #Parameters
  n = count_v1$Count,
  routes = count_v1$Route.ID,
  surveys_count = count_v1$Visit.ID.num,
  surveys_obs = obs_v1$Visit.ID.num,
  observers = count_v1$Observer.ID,
  burned = count_v1$Burned,
  dist_class = obs_v1$Dist.Bin/1000,
  bin_midpoints = sort(unique(obs_v1$Dist.Bin.Midpoint)),
  mas = count_v1$MAS[, 1],
  delta = 0.025,
  max_dist = 0.125,
  
  #Length Objects
  n_surveys = length(count_v1$Visit.ID),               #k
  n_routes = length(unique(count_v1$Route.ID)),        #n
  n_observations = nrow(obs_v1),                       #i
  n_observers = length(unique(count_v1$Observer.ID)),  #o
  n_bins = length(unique(obs_v1$Dist.Bin))             #b
)

#MCMC settings
ni <- 1000
nt <- 5
nb <- 100
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_model_distance",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2)