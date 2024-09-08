# Build the model
brsp_model <- nimbleModel(code = brsp_model_code,
                          name = "Brewer's Sparrow Abondance",
                          data = brsp_dat,
                          constants = brsp_const,
                          inits = brsp_inits,
                          dimensions = brsp_dims)

#View noode
brsp_model$getNodeNames()

# See what did not initialize
brsp_model$initializeInfo()

# niter = ni,
# nburnin = nb,
# thin = nt,
# nchains = nc,
# setSeed = 123,
# samples = TRUE,
# summary = TRUE

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
                 "sd.beta0", 
                 "beta0.int",
                 "fit", 
                 "fit.new", 
                 "bpv")

#configure the sampler 
start <- Sys.time() #start time for the configuration
brsp_mcmc_config <- configureMCMC(brsp_model,           # The newly built model
                                  monitors = brsp_params,    # The parameters I care about
                                  print = TRUE)
difftime(Sys.time(),start) # end time for the configuration

#Build the MCMC sampler object
start <- Sys.time() #start time for building the sampler
brsp_mcmc_built <- buildMCMC(brsp_mcmc_config)
difftime(Sys.time(),start) # end time for building the sampler

#compile the model
start <- Sys.time() #start time for the compilation
brsp_mcmc_comp <- compileNimble(brsp_mcmc_built) 
difftime(Sys.time(),start) # end time for the compilation


