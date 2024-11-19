## i. Nimble Model Statement ----
M1 <- nimbleCode({
  ## Priors 
  # constraining set of possible betas to what we think might be logically realistic
  # 1. beta coefficients for cluster being a kill
  beta0_kill ~ dnorm(0, 0.01)
  # beta_fidelity_kill ~ dnorm(0, 0.001)
  beta_clusdur_kill ~ dnorm(0, 0.01) 
  
  # 2. Priors on beta coefficients for prey type at cluster
  beta0_type[1] <- 0
  beta_elev_type[1] <- 0
  beta_slope_type[1] <- 0
  beta_diff_elevstd_type[1] <- 0 
  beta_log_dist_snbs_std_type[1] <- 0
  gamma_sex[1] <- 0 
  gamma_age[1] <- 0
  
  # loop over other options (option 2 through option J_options) and
  # give them diffuse normal priors. 
  for(j in 2:J_options){
    beta0_type[j] ~ dnorm(0, 0.01)
    beta_elev_type[j] ~ dnorm(0, 0.01)
    beta_slope_type[j] ~ dnorm(0, 0.01)
    beta_log_dist_snbs_std_type[j] ~ dnorm(0, 0.01)
    beta_diff_elevstd_type[j] ~ dnorm(0, 0.01)
    gamma_sex[j] ~ dnorm(0, 0.5) 
    gamma_age[j] ~ dnorm(0, 0.5) 
  } # end J_options prior loop
  
  # build random intercept prior for each lion
  for(l in 1:n_lion) { # loop over each lion
    beta_lion_mean[l, 1] <- 0
    for(j in 2:J_options){
      beta_lion_mean[l, j] <- gamma_sex[j] * lion_sex[l] + gamma_age[j] * lion_age[l]
      # track lion-to-lion variation
    }
    beta_lion[l, 1:J_options] ~ dmnorm(beta_lion_mean[l, 1:J_options], 
                                       cov = Sigma[1:J_options, 1:J_options])
  }
  
  # make prior in lion variance for each prey type
  for (j in 1:J_options) {
    # MAIN DIAGONAL
    sigma_lion[j] ~ T(dt(mu = 0, sigma = 1, df = 5), 0, ) # tried bumping up variance in REs to get them off 0 per Gelman here:
    # https://stats.stackexchange.com/questions/56525/standard-deviation-of-random-effect-is-0 and here:
    # https://stat.columbia.edu/~jcliu/paper/HierarchicalPrior.pdf
    # switching this to a half-t, per Gelman 2006. See the nimble help files here on t parameterization:
    # https://r-nimble.org/html_manual/cha-writing-models.html
    for(f in 1:J_options){
      # WHOLE COVARIANCE MATRIX
      Sigma[j, f] <- rho[j, f] * sigma_lion[j] * sigma_lion[f]
    }
  }
  
  # OFF DIAGONALS: make prior for covariance in option choices within a lion
  for(j in 2:J_options){
    for(f in 1:(j - 1)){
      rho[j, f] ~ dunif(-1, 1) # draws rhos (corrs among choices) for lower triangle
      rho[f, j] <- rho[j, f] # populates the upper triangle deterministically
    }
  }
  
  ## LIKELIHOOD LOOP: Linear Combination + Likelihood
  for (i in 1:N_clusters) { # for every cluster in 1 to total clusters
    
    ## 2. Linear prediction for prey type at cluster
    ## (this model vectorizes the calculations over all options, instead of looping.
    ## vectorizing reduces the number of dependent nodes and accelerates the step rate
    ## for the chains.)
    log(q[i, 1:J_options]) <- beta0_type[1:J_options] + 
      beta_lion[lion_num[i], 1:J_options] +          
      beta_elev_type[1:J_options] * elev_std[i] + 
      beta_slope_type[1:J_options] * log_slope_std[i] + 
      beta_log_dist_snbs_std_type[1:J_options] * log_dist_snbs_std[i] +
      beta_diff_elevstd_type[1:J_options] * diff_elevstd[i] 
    p[i, 1:J_options] <- q[i, 1:J_options] / sum(q[i, 1:J_options])
    
    # 1. Kills:
    # Linear prediction and transformation for cluster being a kill or not | terrain & cluster attributes
    logit(pi_kill[i]) <- beta0_kill + 
      #beta_fidelity_kill * fidelity[i] +
      beta_clusdur_kill * log_clusdur[i] 
    
    # likelihood for kill/no kill
    out_kill[i] ~ dbern(pi_kill[i]) # 1 = kill, 0 = no kill
    
    # assign a 5th state in p with probability of kill
    # for some reason you can't multiply pi_kill * p[i,1:J_options] in nimble
    p[i, 5] <- pi_kill[i] 
    
    # I think we should multiply p_final[i, 1:J_options] by p[i, 5];
    p_final[i, 1:J_options] <- p[i, 1:J_options] * p[i, 5] # (1-p[i, 5]) 
    # I think that this should be 1-p[i, 5]
    p_final[i, 5] <- 1-p[i, 5] # p[i, 5]]
    
    ## Likelihood for type
    # I think that this leaves room to sometimes draw the incorrect state (see example below model script)
    out_type[i] ~ dcat(p[i, 1:5]) # dcat = draw from categorical distrib. (multinomial)
    
  } # end N_clusters likelihood loop
})

## ii. Create Inputs ----
# look at sex and age category by prey type
table(dat$lion_sex, dat$lion_age_cat, dat$prey_choice)

# make vector of lion ids to feed into model
lions <- levels(factor(dat$lion_id))

# make vectors of lion sex and age to feed into model
lion_sex <- lion_age <- rep(NA, length(lions))
for(i in 1:length(lions)){
  k <- subset(dat, lion_id == levels(factor(dat$lion_id))[i])
  lion_sex[i] <- k$lion_sex[1]
  lion_age[i] <- k$lion_age_cat[1]
}

# make numeric vector of factored lion ids to feed into model
dat$lion_num <- as.numeric(factor(dat$lion_id))

# recode out_type so that category 5 = no kill
out_type <- dat$prey_choice
for(i in 1:nrow(dat)){
  out_type[i] <- ifelse(is.na(out_type[i]), 5, 
                        out_type[i])
}

# Data List
constants_list <- list("out_kill" = abs(as.numeric(as.character(dat$ind_clust_detect))), 
                       "out_type" = out_type, # vector of states
                       "elev_std" = dat$elev_std, # standardized
                       "log_slope_std" = dat$log_slope_std, # logged and standardized
                       "N_clusters" = nrow(dat), # number of clusters
                       "log_dist_snbs_std" = dat$dist_snbs_logstd, # log distance to nearest snbs herd
                       "diff_elevstd" = dat$diff_elevstd, # difference in mean snbs and deer elevation in a month
                       "log_clusdur" = dat$log_clusdur, # log cluster duration
                       "lion_sex" = as.numeric(factor(lion_sex)) - 1, # lion sex coded 0 and 1
                       "lion_age" = as.numeric(factor(lion_age)) - 1, # lion age cat coded 0 and 1
                       "n_lion" = length(levels(factor(dat$lion_id))), # index for making appropriate number of lion RE terms, 45 lions currently sampled
                       "lion_num" = as.numeric(as.factor(dat$lion_id)), # use as vector for set of indices for RE
                       "J_options" = length(levels(factor(dat$prey_choice)))) # number of options

# Build inits
make_rho <- function(){
  rhoIn <- matrix(runif(16, -1, 1), nrow = 4)
  diag(rhoIn) <- 1
  return(rhoIn)
}

nimble_inits <- function(){list("beta0_type" = c(0, rnorm(3, 0, 1)),
                                "beta_elev_type" = c(0, rnorm(3, 0, 1)),
                                "beta_slope_type" = c(0, rnorm(3, 0, 1)),
                                "beta_log_dist_snbs_std_type" = c(0, rnorm(3, 0, 1)),
                                "beta_diff_elevstd_type" = c(0, rnorm(3, 0, 1)),
                                "beta0_kill" = rnorm(1, 0, 1),
                                "gamma_sex" = c(0, rnorm(3, 0, 1)),
                                "gamma_age" = c(0, rnorm(3, 0, 1)),
                                "beta_clusdur_kill" = rnorm(1, 0, 1),
                                "sigma_lion" = runif(4, 0, 1),
                                "rho" = make_rho()
)
}

# Parameter List to Track in Model
monitor <- c("beta0_type", 
             "beta_elev_type", 
             "beta_slope_type",
             "beta_log_dist_snbs_std_type",
             "beta_diff_elevstd_type",
             "beta0_kill", 
             "beta_clusdur_kill",
             "gamma_sex", 
             "gamma_age",
             "beta_lion",
             "sigma_lion",
             "rho"
)

## iii. Build Nimble Model ----
## (this can be a medium-slow step) (specifies the graph associated with the model)
nimbleModel_vect <- nimbleModel(M1, 
                                constants = constants_list,
                                inits = nimble_inits())

# 2a. Next, run an MCMC with NIMBLE defaults; this can be a slow step.
## the just-do-it way to run an MCMC. this will take all steps to set up
#   and run an MCMC using NIMBLE’s default configuration. Handles everything from 
#   model building to compilation to running the MCMC
out <- nimbleMCMC(code = nimbleModel_vect,
                  constants = constants_list,
                  #data = data_list,
                  inits = nimble_inits(),
                  monitors = monitor,
                  niter = 1000, # 1000000,
                  nburnin = 500, # 500000,
                  thin = 10, # 500,
                  samplesAsCodaMCMC = T,
                  summary = T)

# 2b. configure and customize MCMC object
# An MCMC configuration holds the information on which samplers are included in 
# the MCMC, which nodes the samplers operate on, and any parameters they need.
# customize sampler to block betas for "Beta Blocking"

# sometimes betas are correlated with one another, and so to speed up sampling,
#   you should sample correlated betas jointly.
# Steps for identifying correlated betas:
#   1. Which betas are not converging?
#   2. Plot against each other to see if correlated and drop one if possible
#   3. After this step, if there are still issues, identify any multi-modalities in posterior

# assign default samplers to nodes
mcmcConf <- configureMCMC(nimbleModel_vect, monitors = monitor)

# make list of random effect betas for blocking
# n_lion <- length(levels(factor(dat$lion_id)))
# beta_lion <- sapply(n_lion, function(x) paste0("beta_lion[", 1:n_lion, "]"))
# dput(beta_lion)

# block all beta nodes together
mcmcConf$removeSamplers('beta0_type[2]', 'beta0_type[3]', 'beta0_type[4]',
                        'beta_elev_type[2]', 'beta_elev_type[3]', 'beta_elev_type[4]',
                        'beta_slope_type[2]', 'beta_slope_type[3]', 'beta_slope_type[4]',
                        'beta_log_dist_snbs_std_type[2]', 'beta_log_dist_snbs_std_type[3]', 'beta_log_dist_snbs_std_type[4]',
                        'beta_diff_elevstd_type[2]', 'beta_diff_elevstd_type[3]', 'beta_diff_elevstd_type[4]',
                        'beta0_kill', 'beta_clusdur_kill')

mcmcConf$addSampler(target = c('beta0_type[2]', 'beta0_type[3]', 'beta0_type[4]',
                               'beta_elev_type[2]', 'beta_elev_type[3]', 'beta_elev_type[4]',
                               'beta_slope_type[2]', 'beta_slope_type[3]', 'beta_slope_type[4]',
                               'beta_log_dist_snbs_std_type[2]', 'beta_log_dist_snbs_std_type[3]', 'beta_log_dist_snbs_std_type[4]',
                               'beta_diff_elevstd_type[2]', 'beta_diff_elevstd_type[3]', 'beta_diff_elevstd_type[4]',
                               'beta0_kill', 'beta_clusdur_kill'),
                    type = 'RW_block')

# block all sigma nodes together
mcmcConf$removeSamplers('sigma_lion[1]', 'sigma_lion[2]', 'sigma_lion[3]', 'sigma_lion[4]')
mcmcConf$addSampler(target = c('sigma_lion[1]', 'sigma_lion[2]', 'sigma_lion[3]', 'sigma_lion[4]'),
                    type = 'RW_block')

# block all gamma nodes together
mcmcConf$removeSamplers('gamma_sex[2]', 'gamma_sex[3]', 'gamma_sex[4]')
mcmcConf$addSampler(target = c('gamma_sex[2]', 'gamma_sex[3]', 'gamma_sex[4]'),
                    type = 'RW_block')

mcmcConf$removeSamplers('gamma_age[2]', 'gamma_age[3]', 'gamma_age[4]')
mcmcConf$addSampler(target = c('gamma_age[2]', 'gamma_age[3]', 'gamma_age[4]'),
                    type = 'RW_block')

mcmcConf$removeSamplers('rho[2, 1]', 'rho[3, 1]', 'rho[3, 2]', 'rho[4, 1]', 'rho[4, 2]', 'rho[4, 3]')
mcmcConf$addSampler(target = c('rho[2, 1]', 'rho[3, 1]', 'rho[3, 2]', 'rho[4, 1]', 'rho[4, 2]', 'rho[4, 3]'),
                    type = 'RW_block')

mcmcConf$printSamplers() # print samplers being used 
mcmcConf$unsampledNodes # look at unsampled nodes

# 3. build MCMC object: can customize thinning, monitors, etc.
modelMCMC <- buildMCMC(mcmcConf) # update mcmc configuration based on graph

# 4. compile Nimble object and MCMC object to one C++ object (gets an ~3 fold speed up, even 
#   if you do nothing else) spend time on getting this step right because this is the 
#   easiest way to accelerate convergence. 
cModel <- compileNimble(nimbleModel_vect) # compiled model
Cmcmc <- compileNimble(modelMCMC, project = cModel, resetFunctions = T) # compiled mcmc

## iv. Run Nimble Model ----
M1_out <- runMCMC(Cmcmc,
                  niter = 1000000, # 1000000,
                  nburnin = 500000, # 500000,
                  thin = 500, # 500,
                  nchains = 3,
                  samplesAsCodaMCMC = T,
                  summary = T)



# Run the sampler
start <- Sys.time()                                     # Start time for the sampler
start
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
                            # setSeed = 01151999,
                            samples = TRUE,
                            summary = TRUE)
difftime(Sys.time(), start)                             # End time for the sampler