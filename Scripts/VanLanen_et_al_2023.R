#Abundance model


# This software has been approved for release by the U.S. Geological Survey 
# (USGS). Although the software has been subjected to rigorous review, the 
# USGS reserves the right to update the software as needed pursuant to further 
# analysis and review. No warranty, expressed or implied, is made by the USGS 
# or the U.S. Government as to the functionality of the software and related 
# material nor shall the fact of release constitute any such warranty. 
# Furthermore, the software is released on condition that neither the USGS 
# nor the U.S. Government shall be held liable for any damages resulting from
# its authorized or unauthorized use.

# Creative Commons Attribution 4.0 International (CC BY 4.0) 
# URL:http://creativecommons.org/licenses/by/4.0/
#   
#   You are free to:
#   Share - copy and redistribute the material in any medium or format for 
# any purpose, even commercially.
# Adapt - remix, transform, and build upon the material for any purpose, 
# even commercially.
# Attribution - You must give appropriate credit , provide a link to the license,
# and indicate if changes were made . You may do so in any reasonable manner, 
# but not in any way that suggests the licensor endorses you or your use.

library(igraph)
library(coda)
library(rjags)
library(dplyr)

#Enter path for the folder titled "Code_Data_Package"
setwd("/Code_Data_Package")

#### No edits should be required below this line ####
###############################
spp <- "BRSP"

#Bring in model data
data_path <- list.files(path = "./ModelData/ScaledData",
                        pattern = spp,
                        full.names = TRUE)

win_data <- readRDS(data_path)

sink("BRSP.landscape.model.jags")

cat(
# "
model {
  #Priors for detection

  #intercept for detection
    beta_p_0 ~ dnorm(0, 0.01)

    #slope of covariate (obsexp) influence on detection
    beta_p_obs ~ dnorm(0, 0.01)

    #slope of covariate (tssr) influence on detection
    beta_p_tssr ~ dnorm(0, 0.01)

    mu_tssr ~ dnorm(0, 0.01)
    sd_tssr ~ dunif(0, 5)
    tau_tssr <- pow(sd_tssr, -2)

  #Priors for availability
  #intercept for availability
    beta_avail_0 ~ dnorm(0, 0.01)

    #slope for availability covariate (ord_date)
    beta_avail_ord ~ dnorm(0, 0.01)

    #slope for availablility covariate (ord_date - quadratic form)
    beta_avail_ord2 ~ dnorm(0, 0.01)

    mu_ord ~ dnorm(0, 0.01)
    sd_ord ~ dunif(0, 5)
    tau_ord <- pow(sd_ord, -2)

  #Smoothing parameter priors
    gam <- exp(rho)
    rho ~ dunif(-12, 12)
    K1[1:(nknots-1), 1:(nknots-1)] <- S1[1:(nknots-1), 1:(nknots-1)] * gam

    gam_alpha0 ~ dnorm(0, 0.368)
    gam_alpha[1] <- gam_alpha0
    gam_alpha[2:nknots] ~ dmnorm(zero[2:nknots], K1[1:(nknots-1), 1:(nknots-1)])

    #Abundance priors
    mu_pdsi ~ dnorm(0, 0.01)
    sd_pdsi ~ dunif(0, 5)
    tau_pdsi <- pow(sd_pdsi, -2)

    #Survey-level random effect
    sd_surv ~ dunif(0, 5)
    tau_surv <- pow(sd_surv, -2)

    mu_elevation ~ dnorm(0, 0.01)
    sd_elevation ~ dunif(0, 5)
    tau_elevation <- pow(sd_elevation, -2)

    for(i in 1:nbcr){
      beta_r_0_pop[i,1:nre] ~ dmnorm(mu_wish[], omega1[1:nre,1:nre])
    }

    for(u in 1:nre){
        mu_wish[u] ~ dnorm(0, 0.01)
    }

  # nre = number random effects (should be 2)
    omega1[1:nre, 1:nre] ~ dwish(R[1:nre, 1:nre], df)

    sigma_omega1[1:nre, 1:nre] <- inverse(omega1[1:nre,1:nre])

    beta_r_a_herb ~ dnorm(0, 0.01)
    beta_r_herb ~ dnorm(0, 0.01)
    beta_r_pj_prop ~ dnorm(0, 0.01)
    beta_r_vrm ~ dnorm(0, 0.01)
    beta_r_ndvi ~ dnorm(0, 0.01)
    beta_r_ndvi2 ~ dnorm(0, 0.01)
    beta_r_l_dist ~ dnorm(0, 0.01)
    beta_r_litter ~ dnorm(0, 0.01)
    beta_r_p_dist ~ dnorm(0, 0.01)
    beta_r_sage ~ dnorm(0, 0.01)
    beta_r_elevation ~ dnorm(0, 0.01)
    beta_r_elevation2 ~ dnorm(0, 0.01)
    beta_r_crop ~ dnorm(0, 0.01)
    beta_r_pdsi ~ dnorm(0, 0.01)

    # Spatial Scale Selection
    # off Prevents SS from equaling highest scale (Frishkoff et al. 2019)
    off <- 0.00001

    for (b in 1:7) {
    SS[b] ~ dunif(1, (nscale - off))
    SS_trunc[b] <- trunc(SS[b])
    prop_scales[b] <- SS[b] - SS_trunc[b]
  }

    for (s in 1:nsites) {
    # Linear interpolation

      for (y in t1[s]:t2[s]){

      W_ndvi[s,y] <- ndvi[s, SS_trunc[1], y] * (1 - prop_scales[1]) +
                    ndvi[s, SS_trunc[1] + 1, y] * prop_scales[1]

      W_a_herb[s,y] <- a_herb[s, SS_trunc[2], y] * (1 - prop_scales[2]) +
                      a_herb[s, SS_trunc[2] + 1, y] * prop_scales[2]

      W_herb[s,y] <- herb[s, SS_trunc[3], y] * (1 - prop_scales[3]) +
                      herb[s, SS_trunc[3] + 1, y] * prop_scales[3]

      W_pj_prop[s,y] <- pj_prop[s, SS_trunc[4], y] * (1 - prop_scales[4]) +
                      pj_prop[s, SS_trunc[4] + 1, y] * prop_scales[4]

      W_litter[s,y] <- litter[s, SS_trunc[5], y] * (1 - prop_scales[5]) +
                       litter[s, SS_trunc[5] + 1, y] * prop_scales[5]

      W_crop[s,y] <- crop[s, SS_trunc[6], y] * (1 - prop_scales[6]) +
                      crop[s, SS_trunc[6] + 1, y] * prop_scales[6]
      }

        W_l_dist[s] <- l_dist[s, SS_trunc[7]] * (1 - prop_scales[7]) +
                      l_dist[s, SS_trunc[7] + 1] * prop_scales[7]

  }

    ##The model
    for(s in 1:nsites){
      for(y in t1[s]:t2[s]){
        tssr[s, y] ~ dnorm(mu_tssr, tau_tssr)
        orddate[s, y] ~ dnorm(mu_ord, tau_ord)

        log(sigma[s, y]) <- beta_p_0
        + beta_p_obs * obsexp[s, y]
        + beta_p_tssr * tssr[s, y]

        #Distance sampling portion of model
        for(b in 1:nD){
          log(g[s, y, b]) <- -mdpts[b] * mdpts[b]/(2*sigma[s, y] * sigma[s, y])
          f[s, y, b] <- (2*mdpts[b])/(B*B) * delta

          #product of detection prob and availability prob for distance band
          pi_pd[s, y, b] <- g[s, y, b] * f[s, y, b]
          pi_pd_c[s, y, b] <- pi_pd[s, y, b]/pdet[s, y]
        }

        #probability of detection at all
        pdet[s, y] <- sum(pi_pd[s, y, 1:nD])
        logit(p_a[s, y]) <- beta_avail_0
        + beta_avail_ord * orddate[s, y]
        + beta_avail_ord2 * orddate[s, y] * orddate[s, y]

        #Time removal
        for (k in 1:K[y]){
          pi_pa[s, y, k] <- p_a[s, y] * pow(1-p_a[s, y], (k-1))
          pi_pa_c[s, y, k] <- pi_pa[s, y, k]/phi[s, y]
        }

        #sum of all availability across all time periods
        phi[s, y] <- sum(pi_pa[s, y, 1:K[y]])
      }
    }

    #Conditional observation model
    for(o in 1:nobs){
      dclass[o] ~ dcat(pi_pd_c[site_detect[o], yr_detect[o],1:nD])
      tint[o] ~ dcat(pi_pa_c[site_detect[o], yr_detect[o], 1:K[yr_detect[o]]])
    }

    #linear model for zero-inflation
    mu_psi[1:nsites] <- A[1:nsites, 1:nknots] %*% gam_alpha[1:nknots]

    #Abundance model
     for(s in 1:nsites) {

       logit(psi[s]) <- mu_psi[s]
       active[s] ~ dbern(psi[s]) #zero inflation portion of model

       elevation[s] ~ dnorm(mu_elevation, tau_elevation)

      for (y in t1[s]:t2[s]){

         M[s, y] ~ dpois(lambda[s, y] * active[s])
         Mnew[s, y] ~ dpois(lambda[s, y] * active[s])

        #probability detected and available
         pmarg[s, y] <- phi[s, y] * pdet[s, y]

         # number of ind detected
         n[s, y] ~ dbin(pmarg[s, y], M[s, y])
         n_new[s, y] ~ dbin(pmarg[s, y], M[s, y])

        l_mu_lambda[s, y] <- beta_r_0_pop[bcr_index[s], 1]
        + beta_r_ndvi * W_ndvi[s, y]
        + beta_r_ndvi2 * W_ndvi[s, y] * W_ndvi[s, y]
        + beta_r_a_herb * W_a_herb[s, y]
        + beta_r_herb * W_herb[s, y]
        + beta_r_pj_prop * W_pj_prop[s, y]
        + beta_r_litter * W_litter[s, y]
        + beta_r_sage * sage[s, y]
        + beta_r_crop * W_crop[s, y]
        + beta_r_l_dist * W_l_dist[s]
        + beta_r_p_dist * p_dist[s]
        + beta_r_elevation * elevation[s]
        + beta_r_elevation2 * elevation[s] * elevation[s]
        + beta_r_vrm * vrm[s]
        + beta_r_pdsi * pdsi[s, y]
        + beta_r_0_pop[bcr_index[s], 2] * (y - 1)
        + offset[s, y]

        llambda[s, y] ~ dnorm(l_mu_lambda[s, y], tau_surv)
        log(lambda[s, y]) <- llambda[s, y]

       e_pd[s, y] <- pmarg[s, y] * M[s, y]
       E_pd[s, y] <- pow((n[s, y] - e_pd[s, y]), 2) / (e_pd[s, y] + 0.5)
       E_pd_new[s, y] <- pow((n_new[s, y] - e_pd[s, y]), 2) / (e_pd[s, y] + 0.5)

       FT1[s, y] <- pow(sqrt(M[s, y]) - sqrt(lambda[s, y] * active[s]), 2)
       FT1new[s, y] <- pow(sqrt(Mnew[s, y]) - sqrt(lambda[s, y] * active[s]), 2)
       }
       }

   for(s in 1:nsites){

    #Bayesian p-value for abundance
    fit_pd1[s] <- sum(E_pd[s, t1[s]:t2[s]])
    fit_new_pd1[s] <- sum(E_pd_new[s, t1[s]:t2[s]])
    }

    fit_pd2 <- sum(fit_pd1[1:nsites])
    fit_new_pd2 <- sum(fit_new_pd1[1:nsites])

    chat <- fit_pd2/fit_new_pd2

    bpvalue <- step(fit_new_pd2 - fit_pd2)

   }
   # "
   , fill = TRUE)

sink()

#Prepare initial values
activest <- rep(1, win_data$nsites)

#Get gam_alpha initial value from GAM model
jagam_mod <- mgcv::jagam(y ~ s(lon, lat,
                               k = win_data$nknots,
                               bs = "ds",
                               m = c(1, 0.5)),
                         data = win_data$gamdat,
                         family = "binomial",
                         sp.prior = "log.uniform",
                         file = "BRSP.gam.structure.txt")

gam_alpha <- jagam_mod$jags.ini$b

gam_alpha[1] <- NA

#Create initial detection values
n_inits <- as.matrix(win_data$n, ncol = win_data$nyears)

n_inits <- n_inits[, ] + 2
for (i in 1:dim(n_inits)[1]) {
  for (t in win_data$t1[i]:win_data$t2[i]) {
    if (is.na(n_inits[i, t])) {
      n_inits[i, t] <- round(median(n_inits[i, ], na.rm = TRUE))
    }
  }
}

#Functions to format initial covariate values - imputes missing covariate values
#and converts all existing values to NA

#Function for time-varying and spatial scale of selection (ss) covariates
cov_init_ss <- function(covariate) {
  cov_init_yr <- covariate
  cov_init_yr[which(!is.na(covariate))] <- NA
  cov_init_yr[which(is.na(covariate))] <- rnorm(length(which(is.na(covariate))),
                                                0, 1)

  for (s in 1:win_data$nsites) {
    for (b in 1:win_data$nscale){
      if (win_data$t1[s] > 1) {
        cov_init_yr[s, b, 1:(win_data$t1[s] - 1)] <- NA
      }

      if (win_data$t2[s] < win_data$nyears) {
        cov_init_yr[s, b, (win_data$t2[s] + 1):win_data$nyears] <- NA
      }
    }
  }
  return(cov_init_yr)
}

#For time varying covariates, without SS
cov_init_tv <- function(covariate) {
  cov_init_yr <- covariate
  cov_init_yr[which(!is.na(covariate))] <- NA
  cov_init_yr[which(is.na(covariate))] <- rnorm(length(which(is.na(covariate))),
                                                0, 1)

  for (s in 1:win_data$nsites) {

    if (win_data$t1[s] > 1) {
      cov_init_yr[s, 1:(win_data$t1[s] - 1)] <- NA
    }

    if (win_data$t2[s] < win_data$nyears) {
      cov_init_yr[s, (win_data$t2[s] + 1):win_data$nyears] <- NA
    }
  }
  return(cov_init_yr)
}

#Initial values function for non time varying and non SS covariates
cov_init_nosstv <- function(covariate) {

  cov_init <- covariate
  cov_init[which(!is.na(covariate))] <- NA
  cov_init[which(is.na(covariate))] <- rnorm(length(which(is.na(covariate))),
                                             0, 1)

  return(cov_init)
}

#### Generate initial values ####
inits <- function(chain) {
  list(M = n_inits,
       n_new = n_inits,
       mu_M = n_inits,
       beta_p_0 = rnorm(1, 0, 0.1),
       beta_p_obs = rnorm(1, 0, 0.3),
       beta_p_tssr = rnorm(1, 0, 0.4),
       mu_tssr = rnorm(1, 0, 0.1),
       sd_tssr = runif(1),
       tssr = as.matrix(cov_init_tv(win_data$tssr)),

       beta_avail_0 = rnorm(1, -0.6, 0.1),
       beta_avail_ord = rnorm(1),
       beta_avail_ord2 = rnorm(1, -0.1, 1),
       mu_ord = rnorm(1, 0, 0.1),
       sd_ord = runif(1),
       orddate = as.matrix(cov_init_tv(win_data$orddate)),

       beta_r_ndvi = rnorm(1, 0, 0.1),
       beta_r_ndvi2 = rnorm(1, 0, 0.1),
       ndvi = as.array(cov_init_ss(win_data$ndvi)),
       beta_r_a_herb = rnorm(1, 0, 0.1),
       a_herb = as.array(cov_init_ss(win_data$a_herb)),
       beta_r_herb = rnorm(1, 0, 0.1),
       herb = as.array(cov_init_ss(win_data$herb)),
       beta_r_pj_prop = rnorm(1, 0, 0.1),
       pj_prop = as.array(cov_init_ss(win_data$pj_prop)),

       beta_r_litter = rnorm(1, 0, 0.1),
       litter = as.array(cov_init_ss(win_data$litter)),
       beta_r_sage = rnorm(1, 0, 0.1),
       sage = as.matrix(cov_init_tv(win_data$sage)),
       beta_r_crop = rnorm(1, 0, 0.1),
       crop = as.array(cov_init_ss(win_data$crop)),
       beta_r_l_dist = rnorm(1, 0, 0.1),
       beta_r_p_dist = rnorm(1, 0, 0.1),
       beta_r_vrm = rnorm(1, 0, 0.1),
       beta_r_elevation = rnorm(1, 0, 0.1),
       beta_r_elevation2 = rnorm(1, 0, 0.1),
       elevation = as.vector(cov_init_nosstv(win_data$elevation)),

       gam_alpha0 = runif(1),
       gam_alpha = gam_alpha,
       rho = runif(1, -7, -3),
       active = activest,

       SS = runif(7, 1, win_data$nscale - 1),
       omega1 = matrix(c(1, 0, 0, 1),
                       ncol = win_data$nre),
       .RNG.seed = chain,
       .RNG.name = c("base::Wichmann-Hill",
                     "base::Marsaglia-Multicarry",
                     "base::Super-Duper",
                     "base::Mersenne-Twister")[chain])
  }

#### List of parameters to monitor ####
params <- c(
  "beta_avail_0",
  "beta_avail_ord",
  "beta_avail_ord2",

  "beta_p_0",
  "beta_p_obs",
  "beta_p_tssr",

  "rho",
  "gam",
  "beta_r_0_pop",
  "mu_wish",

  "SS",

  "beta_r_ndvi",
  "beta_r_ndvi2",
  "beta_r_a_herb",
  "beta_r_herb",
  "beta_r_pj_prop",
  "beta_r_litter",
  "beta_r_sage",
  "beta_r_crop",
  "beta_r_l_dist",
  "beta_r_p_dist",
  "beta_r_elevation",
  "beta_r_elevation2",
  "beta_r_vrm",
  "beta_r_pdsi",

  "bpvalue",
  "chat",

  "gam_alpha"
)

#Provide number of iterations (ni), thinning amount (nt),
#burnin (nb), and the # of chains (nc)
ni <- 10000
nt <- 1
nb <- 1
nc <- 4

library(dclone)
library(snow)
library(rjags)
library(parallel)

start <- Sys.time()

cl <- makeCluster(nc, timeout = 5184000)

clusterSetRNGStream(cl = cl, iseed = 22)
clusterExport(cl, c("win_data", "params", "ni", "nt", "nb"))
for (i in seq_along(cl)){
  init <- inits(i)
  clusterExport(cl[i], "init")
}

out <- clusterEvalQ(cl, {
  library(rjags)
  load.module("glm")
  load.module("bugs")
  out1 <- jags.model("BRSP.landscape.model.jags",
                   win_data,
                   init,
                   n.adapt = 5000,
                   n.chains = 1)
  update(out1, n.iter = nb)
  out2 <- coda.samples(out1, params, n.iter = ni, thin = nt)
  return(as.mcmc(out2))
})

elapsed <- Sys.time() - start
start
elapsed

out_p_mcmc <- as.mcmc(out)

out_mcmc_bind <- mcmc.list() # Combine previous iterations with new ones
out_mcmc_bind[[1]] <- mcmc(rbind(out_p_mcmc[[1]]))
out_mcmc_bind[[2]] <- mcmc(rbind(out_p_mcmc[[2]]))
out_mcmc_bind[[3]] <- mcmc(rbind(out_p_mcmc[[3]]))
out_mcmc_bind[[4]] <- mcmc(rbind(out_p_mcmc[[4]]))

saveRDS(out_mcmc_bind,
        paste0("./ModelOutput/Reviewer_Output/Initial_10K_", spp, ".rds"))

Sys.time()

#If not converged yet, run for more iterations
start <- Sys.time()
system.time({
  out <- clusterEvalQ(cl, {
    out2 <- coda.samples(out1, params, n.iter = 290000, thin = 1)
    return(as.mcmc(out2))
  })
})

elapsed <- Sys.time() - start
start
elapsed

out_p_mcmc2 <- as.mcmc(out)

out_mcmc_bind2 <- mcmc.list() # Combine previous iterations with new ones
out_mcmc_bind2[[1]] <- mcmc(rbind(out_p_mcmc[[1]], out_p_mcmc2[[1]]))
out_mcmc_bind2[[2]] <- mcmc(rbind(out_p_mcmc[[2]], out_p_mcmc2[[2]]))
out_mcmc_bind2[[3]] <- mcmc(rbind(out_p_mcmc[[3]], out_p_mcmc2[[3]]))
out_mcmc_bind2[[4]] <- mcmc(rbind(out_p_mcmc[[4]], out_p_mcmc2[[4]]))

Sys.time()

saveRDS(out_mcmc_bind2, 
        paste0("./ModelOutput/Reviewer_Output/FullModelRun_300K_",
               spp, ".rds"))
