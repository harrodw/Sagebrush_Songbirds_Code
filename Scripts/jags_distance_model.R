#Clear environments-------------------------------------------------------------------------------------------
rm(list = ls())

#Add packages--------------------------------------------------------------------------------------------------
library(jagsUI)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)

#Add data--------------------------------------------------------------------------------------------------------
#Counts during each survey
brsp_count <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/brsp_count_jags.csv") %>% 
  tibble() %>% 
  select(-X)
#View counts
glimpse(brsp_count)

#all observations
brsp_obs <- read.csv("https://raw.githubusercontent.com/harrodw/Sagebrush_Songbirds_Code/main/Data/Outputs/brsp_obs_jags.csv") %>% 
  tibble() %>% 
  select(-X)
#View counts
glimpse(brsp_obs)

##########################################################################################
#The Model: Single parameter model for abundance with detentions binned by distance
sink("bayes_files\\brsp_model_distance")
cat("
  model{
    #Priors #################
    #Detection level priors
    sigma_observer ~ dunif(0, 500)   #between observer variation in detection
    sigma2_observer <- pow(sigma_observer, 2)
    tau_observer <- pow(sigma_observer, -2)
    beta_pd_wind ~ dnorm(0, 0.001) #offset in detection probability for minutes after sunrise               
   
    #Random intercepts for observer
    for(o in 1: n_observers){
      alpha_observer[o] ~ dnorm(0, tau_observer)
    }
    
    #Abundance level priors ----------
    sigma_route ~ dunif(0, 50)           #Between route variation in abundance
    sigma2_route <- pow(sigma_route, 2)
    tau_route <- pow(sigma_route, -2)
    beta_lambda_burn ~ dnorm(0, 0.001) #difference in abundance between burned and ref routes
    
    #Random intercept for route ID
    for(j in 1: n_routes){
      alpha_route[j] ~ dnorm(0, tau_route) 
    }
    
    #Likelihood ##############################################################
    
    ##DETECTION PROBABILITY ------------------------------
    #Linear combination of covariates
    for(k in 1:n_surveys){
      #Log linear combination of predictors for detection probability
      log(sigma[k]) <- log(alpha_observer[surveys[k]]) + beta_pd_wind * wind[k]

      #Distance sampling detection probability estimation -----------------------
      # Using summation technique - Pr(p of x)=exp(-x^2/2*sigma^2)*f(x)
      for(b in 1:n_bins){
        # half-normal detection function - first half of eq., 
        ln_g[b,k] <- -bin_midpoint[b]*bin_midpoint[b] / (2*sigma[k]*sigma[k])  
        #Log link
        g[b, k] <- exp(ln_g[b, k])
        # this is f(x), the scaled radial density function
        f[b,k]<-  ( 2*bin_midpoint[b]*delta ) / (maxd*maxd) 
        
        #this is the product Pr(detect)*Pr(distribution)
        pi_pd[b,k]<- g[b,k]*f[b,k]  
        #standardizing based on overall capture probability - conditional formulation
        pi_pd_condi[b,k] <- pi.pd[b,k]/pd[k]  
      } #End detection function loop
      
      #probability of detection is the sum of all rectangular areas
      pd[k] <- sum(pi_pd[,k])  
    }#end surveys loop
    

    ######## Observation-level model ----------------------------------  
    for(i in 1:n_observations){  
      #single binomial trial with categorical distribution linking distance class to survey point
      dist_class[i] ~ dcat(pi_pd_condi[,surveys[i]]) #come back to this pieces -------------------------
    }
    
    ################# Abundance level model  -----------------------------
    for(k in 1:n_surveys){
      # Abundance
      #Linear combonation of abundance predictors
      lin_comb_lambda <- alpha_route[surveys[k]] + beta_lambda_burn * burned[k]
      #Log link
      lambda <- exp(lin_comb_lambda)
      #Poisson process of birds being distributed accross the landscape
      N[k] ~ dpois(lambda)
      
      #Detection process
      n[k] ~ dbin(pd[k], N[k])
    }
  }
, fill = TRUE)


#Initial values
brsp_inits <- function(){list(
  tau_route = runif(0, 50),
  tau_observer = runif(0, 50)
")}

#Parameters to save
brsp_params <- c("N", "pd", "beta_lambda_burn", "beta_pd_wind")

#Bundle data
brsp_data <- list(
  #Parameters
  n = brsp_count$Count,
  observations = brsp_count$Count,
  routes = as.numeric(as.factor(brsp_count$Route.ID)),
  surveys = as.numeric(as.factor(brsp_count$Visit.ID)),
  observers = as.numeric(as.factor(brsp_count$Observer.ID)),
  burned = as.numeric(brsp_count$Burned),
  dist_class = brsp_obs$Dist.Bin,
  bin_midpoint = brsp_obs$Dist.Bin.Midpoint,
  wind = as.numeric(brsp_count$Wind.Start),
  delta = 10,
  max_dist = max(brsp_obs$Dist.Bin),
  
  #Length Objects
  n_surveys = length(brsp_count$Visit.ID),               #k
  n_routes = length(unique(brsp_count$Route.ID)),        #n
  n_observations = nrow(brsp_obs),                       #i
  n_observers = length(unique(brsp_count$Observer.ID)),  #o
  n_bins = length(unique(brsp_obs$Dist.Bin))             #b
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
