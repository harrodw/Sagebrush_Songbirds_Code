#Simple N-Mixture from a single year of my point count data ############################################################

#Clear Environments
rm(list = ls())

#add packages
library(jagsUI)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)

#Add the data
brsp_count <- read.csv("Data\\Outputs\\brsp_count_jags.csv") %>%
  dplyr::select(-X)
#View the data
glimpse(brsp_count)

#Prep the data for N-mixture model
nmix_dat <- brsp_count %>% 
  mutate(Wind.Start = case_when(is.na(Wind.Start) ~ 'Unknown', #change the wind data
                                TRUE ~ Wind.Start)) %>% 
  mutate(Wind.Start = factor(Wind.Start, levels = c("<1 mph", "1-3 mph", "4-7 mph",
                                                    "8-12 mph", "13-18 mph", "Unknown"))) %>% 
  mutate(Wind.Start = as.numeric(Wind.Start)) %>% 
  dplyr::select(Full.Point.ID, Visit, Count, Shrub.Cover, Wind.Start) %>% #Pull out only the variables of interest
  pivot_wider(names_from = Visit, values_from = c('Count', 'Shrub.Cover', 'Wind.Start')) %>% #Pivot the data so it can be turned into a matrix
  drop_na(Count_V1, Count_V2) #Remove the sites that were only visited once

#View the new data
head(nmix_dat)
glimpse(nmix_dat)

#The model
sink("bayes_files\\brsp_count")
cat("
  model{
    #Fixed effect Priors ----
    beta_shrub ~ dnorm(0, 0.001) #Offset for burned or not
    #random effect hyperparameters
    # sigma_route ~ dunif(0, 50) 
    # sigma2_route <- pow(sigma_route, 2) 
    # tau_route <- pow(sigma_route, -2)
    beta_p_wind ~ dnorm(0, 0.001) 
    beta0_a ~ dnorm(0, 0.001)
    beta0_p ~ dnorm(0, 0.001)
     
    # #Random intercept priors ---
    # for(n in 1:n_routes){
      # beta0 ~ dnorm(0, tau_route)
    #}
    
    #State process Likelihood -----
    for(i in 1:n_points){ 
      lin_comb[i] <- beta0_a + beta_shrub * shrub[i] 
      #linear combination of predictors
      lambda[i] <- exp(lin_comb[i]) #log link
      n_brsp[i] ~ dpois(lambda[i]) 
     
    
    #Observation process Likelihood -----
     for(j in 1:n_visits){
       obs_brsp[i, j] ~ dbin(p[i, j], n_brsp[i])
       p[i, j] <- exp(lp[i, j]) /(1+ exp(lp[i, j]))
       lp[i, j] <- beta0_p + beta_p_wind * wind[i, j] 
     } #j
    } #i
    
    totalN <- sum(n_brsp[])
    
  }# end model
", fill = TRUE)
sink()

#Initial values
brsp_inits <- function(){list(
  beta_shrub = rnorm(1, 0, 0.001),
  beta_p_wind = rnorm(1, 0, 0.001),
  beta0_p = rnorm(1, 0, 0.001),
  beta0_a = rnorm(1, 0, 0.001),
  n_brsp = apply(as.matrix(nmix_dat[, 2:3]), 1, max) + 1
)}

#Bundle data
brsp_data <- list(obs_brsp = as.matrix(nmix_dat[, 2:3]),
                  shrub = nmix_dat$Shrub.Cover_V1,
                  wind = as.matrix(nmix_dat[, 6:7]),
                  #route = unique(as.numeric(as.factor(nmix_dat$Full.Point.ID))),
                  n_visits = 2,
                  n_points = nrow(nmix_dat)
)

#parameters to save
brsp_params <- c("beta_0", "beta_shrub", "totalN", "beta_p_wind")

#MCMC settings
ni <- 10000
nt <- 5
nb <- 1000
nc <- 3

#Send the data to BUGS
brsp_fit <- jagsUI(data = brsp_data,
                   inits = brsp_inits,
                   parameters.to.save = brsp_params,
                   model.file = "bayes_files\\brsp_count",
                   n.chains = nc,
                   n.iter = ni,
                   n.burnin = nb,
                   n.thin = nt)

#View model output
print(brsp_fit, digits = 2, n = Inf)

#View model traceplots
# par(mfrow = c(5, 5))
# traceplot(brsp_fit)
