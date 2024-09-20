#Hierarchical Distance Sampling using the Unmarked package
#Will Harrod
#Utah State University
#Creadted 09/08/2024

# 0) Clear environments, install, and load packages#####################################
rm(list = ls())
library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(raster)
library(sf)
library(tmap)
library(gridExtra)
library(viridis)

# 1) Data Prep #################################################################

# 1.1) Prepare point count data #########################################################

# Add point count data
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# View the data
glimpse(sobs)

# Make a new column for each survey visit
sobs <- sobs %>%
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))

# Define a single species of interest
species_to_model <- "VESP"

# Define a truncation distance
trunc_dist <- 125

#Define distance bin sizes
bin_size <- 25

# filter for only that species
ydist <- sobs %>%
  filter(Species == species_to_model) %>%
  dplyr::select(Visit.ID, Distance) %>%
  mutate(Distance = as.numeric(Distance)) %>%
  filter(Distance <= trunc_dist) %>%
  mutate(Distance.Class = NA)
#...and view
glimpse(ydist)

# Define distance bins
dist_breaks <- seq(from = 0, to = trunc_dist, by = bin_size)
#...and view
print(dist_breaks)

#Create an object of distance bin classes
for(j in 2:length(dist_breaks)){
  ydist <- ydist %>%
    mutate(Distance.Class = case_when(Distance == 0
                                      ~ paste0("[", dist_breaks[1], "- ", dist_breaks[2], "]"),
                                      Distance > dist_breaks[j - 1] & Distance <= dist_breaks[j]
                                      ~ paste0("(", dist_breaks[j -1], "- ", dist_breaks[j], "]"),
                                      TRUE ~ Distance.Class))
} #end loop
#... and view
glimpse(ydist)

#count up the observations by site
dist_dat_0inf <- ydist %>%
  group_by(Visit.ID, Distance.Class) %>%
  reframe(Visit.ID, Distance.Class, Count = n()) %>%
  distinct(Visit.ID, Distance.Class, Count) %>%
  pivot_wider(names_from = Distance.Class,
              values_from = Count)
#... and view
glimpse(dist_dat_0inf)

# Make a table of all survey visits
points <- sobs %>%
  distinct(Visit.ID)
#...and view'
head(points)
glimpse(points)

# Merge with your observations data
dist_dat <- points %>%
  left_join(dist_dat_0inf,
            by = "Visit.ID") %>%
  mutate(across(everything(), ~ replace_na(., 0)))%>%
  dplyr::select(Visit.ID, `(0- 25]`, `(25- 50]`, `(50- 75]`, `(75- 100]`, `(100- 125]`)

#... and view
glimpse(dist_dat)
dist_dat %>%
  filter(`(0- 25]` != 0)

# 1.2) Prepare covariate data  ###############################################################

# Add point level covariates
covs <- tibble(read.csv("Data\\Outputs\\point_summaries.csv")) %>%
  dplyr::select(-X, -Point.X, -Point.Y) %>%
  tibble() 

# Transform the observation data into an object that can receive the covariates
covs_tbl <- sobs %>%
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-")) %>%
  dplyr::select(Visit.ID, Year, Visit, Full.Point.ID, Route.ID, Observer.ID,
                Ord.Date, Date, MAS, Temp.Start, Wind.Start, Sky.Start,
                UTM.X, UTM.Y) %>%
  distinct(Visit.ID, Year, Visit, Full.Point.ID, Route.ID, Observer.ID,
           Ord.Date, Date, MAS, Temp.Start, Wind.Start, Sky.Start,
           UTM.X, UTM.Y)
#...and view
glimpse(covs_tbl)

#make sure this is the same as the actual number of points surveyed
nrow(covs_tbl) == length(unique(covs_tbl$Visit.ID))
# good good

# combine these with the covariates
hdm_dat <- covs_tbl %>%
  left_join(covs, by = c("Full.Point.ID", "Route.ID")) %>%
  # How long since each fire
  mutate(Years.Since.Fire = lubridate::year(Date) - Fire.Year) %>%
  #Update the variables based on my exploratory analysis
  mutate(# Log-transform the things that need to be
    ln.Sage.Cover = log(Sage.Cover),
    ln.Bare.Ground.Cover = log(Bare.Ground.Cover),
    #Combine Alex and Ben's Data
    Observer.ID = case_when(Observer.ID %in% c('Alex', "Ben") ~ "Alex & Ben",
                            TRUE ~ Observer.ID),
    #Now many fog/smoke days. I'll just remove them
    Sky.Start = case_when(Sky.Start %in% c("Fog or Smoke", "Cloudy") ~ "Cloudy",
                          TRUE ~ Sky.Start)) %>%
  # Fill in missing values for routes witout fire information
  mutate(Years.Since.Fire = case_when(is.na(Years.Since.Fire) ~ 115,
                                      TRUE ~ Years.Since.Fire)) # Random value from the lit. Not used in model


#...and view
glimpse(hdm_dat)

#Make sure there are still the proper number of points
nrow(hdm_dat) == length(unique(covs_tbl$Visit.ID))


# 1.3) Formating these into unmarked objects ######################################

# distance data
dist_mat <- as.matrix(dist_dat %>%
                        dplyr::select(`(0- 25]`, `(25- 50]`, `(50- 75]`, `(75- 100]`, `(100- 125]`))
# view
head(dist_mat)
dim(dist_mat)

#Distance breaks already exist, just need to view them
dist_breaks

# both site covs and detection covariates
names(hdm_dat)
site_covs <- hdm_dat %>%
  #Scale and factorize covariates
  mutate(area = pi * trunc_dist^2 * 0.0001, # calculate area
         # Abundance level covariates
         ln.Sage.Cover = scale(ln.Sage.Cover)[,1],
         Perennial.Cover = scale(Perennial.Cover)[,1],
         ln.Bare.Ground.Cover = scale(ln.Bare.Ground.Cover)[,1],
         Elevation = scale(Elevation)[,1],
         # Availability and detection level covariates
         Observer.ID = factor(Observer.ID, levels = sort(unique(Observer.ID))),
         MAS = scale(MAS)[,1],
         Ord.Date = scale(Ord.Date)[,1],
         # Fire ecology covariates
         Burned = as.numeric(factor(hdm_dat$Route.Type, levels = c("R", "B"))) -1,
         # Burn.Sevarity = case_when(is.na(Burn.Sevarity) ~ 0, TRUE ~ Burn.Sevarity),
         Years.Since.Fire = scale(Years.Since.Fire)[,1]
  ) %>%
  # mutate(Burn.Sevarity = factor(Burn.Sevarity)) %>%
  dplyr::select(#Pick which observation and site level covariates are interesting
    #These are for the process based model
    ln.Sage.Cover,
    ln.Bare.Ground.Cover,
    Elevation,
    Perennial.Cover,
    #and observation level covariates
    Observer.ID,
    MAS,
    Ord.Date,
    # For the fire ecology model
    Burned,
    # Burn.Sevarity, 
    Years.Since.Fire
  ) %>%
  as.data.frame()

#...and view
glimpse(site_covs)

#Combine these into an unmarked distance frame
umf <- unmarkedFrameGDS(y = dist_mat,
                        siteCovs = site_covs,
                        dist.breaks = dist_breaks,
                        survey = "point",
                        unitsIn = "m",
                        numPrimary = 1)
# View the unmarked data frame
str(umf)
head(umf)

# 2) Model Fitting #############################################################

# 2.1) Asses which key function fits the data best ##############################

# #Half-normal
# mod_kf_half_norm <- gdistsamp(lambdaformula = ~1,
#                               phiformula = ~1,
#                               pformula = ~1,
#                               data = umf,
#                               output = "density", unitsOut = "ha",
#                               mixture = "P",
#                               keyfun = "halfnorm")
# #save the model
# save(mod_kf_half_norm, file = paste0("Model_Files/umk_", species_to_model, "_brsp_mod_kf_half_norm.RData"))
#
# #hazard rate
# mod_kf_hazard <- gdistsamp(lambdaformula = ~1,
#                            phiformula = ~1,
#                            pformula = ~1,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "P",
#                            keyfun = "hazard")
# #save the model
# save(mod_kf_hazard, file = paste0("Model_Files/umk_", species_to_model, "_brsp_mod_kf_hazard.RData"))
#
# #exponential
# mod_kf_exp <- gdistsamp(lambdaformula = ~1,
#                         phiformula = ~1,
#                         pformula = ~1,
#                         data = umf,
#                         output ="density",
#                         mixture = "P",
#                         keyfun = "exp")
# #save the model
# save(mod_kf_exp, file = paste0("Model_Files/umk_", species_to_model, "_brsp_mod_kf_exp.RData"))
#
# # compare key-function models using AIC
# modlist_key <- list(mod_half_norm = mod_kf_half_norm,
#                     mod_hazard = mod_kf_hazard,
#                     mod_exp = mod_kf_exp)
# aictab(modlist_key)
# # The exponential key function seams to perform best

# 2.2) Assess which mixture process best fits the data ##############################

# # Poisson
# mod_mix_P <- gdistsamp(lambdaformula = ~1,
#                        phiformula = ~1,
#                        pformula = ~1,
#                        data = umf,
#                        output = "density", unitsOut = "ha",
#                        mixture = "P",
#                        keyfun = "exp")
# #save the model
# save(mod_mix_P, file = paste0("Model_Files/umk", species_to_model, "_mod_mix_P.RData"))
#
# # egative Binomial
# mod_mix_NB <- gdistsamp(lambdaformula = ~1,
#                         phiformula = ~1,
#                         pformula = ~1,
#                         data = umf,
#                         output = "density", unitsOut = "ha",
#                         mixture = "NB",
#                         keyfun = "exp")
# #save the model
# save(mod_mix_NB, file = paste0("Model_Files/umk", species_to_model, "_mod_mix_NB.RData"))
#
# # Zero-inflated Poisson
# mod_mix_ZIP <- gdistsamp(lambdaformula = ~1,
#                          phiformula = ~1,
#                          pformula = ~1,
#                          data = umf,
#                          output ="density",
#                          mixture = "ZIP",
#                          keyfun = "exp")
# #save the model
# save(mod_mix_ZIP, file = paste0("Model_Files/umk", species_to_model, "_mod_mix_ZIP.RData"))
#
# # compare key-function models using AIC
# modlist_mix <- list(mod_mix_P = mod_mix_P,
#                     mod_mix_NB = mod_mix_NB,
#                     mod_mix_ZIP = mod_mix_ZIP)
# aictab(modlist_mix)
# The ZIP model is by far the best but the negative binomial is okay
# Since the ZIP fitting is confusing, I'm going with a negative binomial for nwo

# # 2.3) Detection level covariate fitting #######################################
#
# # Null model is the best fitting model from the privious section of the heiarchy
# mod_dct_null <- mod_kf_exp
#
# # Observer model
# mod_dct_obs <- gdistsamp(lambdaformula = ~1,
#                          phiformula = ~1,
#                          pformula = ~ Observer.ID,
#                          data = umf,
#                          output = "density", unitsOut = "ha",
#                          mixture ="NB",
#                          keyfun  = "exp")
# #save the model
# save(mod_dct_obs, file = paste0("Model_Files/umk_", species_to_model, "_brsp_mod_dct_obs.RData"))
#
# # Time after sunrise model
# mod_dct_mas <- gdistsamp(lambdaformula = ~1,
#                          phiformula = ~1,
#                          pformula =  ~MAS,
#                          data = umf,
#                          output = "density", unitsOut = "ha",
#                          mixture = "NB",
#                          keyfun = "exp")
# #save the model
# save(mod_dct_mas, file = paste0("Model_Files/umk_", species_to_model, "_mod_dct_mas.RData"))
#
# #Date model
# mod_dct_date <- gdistsamp(lambdaformula = ~1,
#                           phiformula = ~1,
#                           pformula = ~Ord.Date,
#                           data = umf,
#                           output = "density", unitsOut = "ha",
#                           mixture = "NB",
#                           keyfun = "exp")
# #save the model
# save(mod_dct_date, file = paste0("Model_Files/umk_", species_to_model, "_mod_dct_date.RData"))
#
#
# # compare detection models using AIC
# modlist_dct = list(mod_null = mod_dct_null,
#                    mod_obs = mod_dct_obs,
#                    mod_mas = mod_dct_mas,
#                    mod_date = mod_dct_date)
# aictab(modlist_dct)
# # All of these perform better than the null but observer is the best predictor
#
# # Load the observation model back in
# load(paste0("Model_Files/umk_", species_to_model, "_mod_dct_obs.RData"))
#
# #Explore the observer data. I want to reclassify Alex's observations
# summary(mod_dct_obs)
# #I'll lump Alex in with Ben
#
# #View the model output
# par(mfrow = c(2, 2))
# plot(mod_dct_obs)
#
# # 2.4) abundance level covariate fitting #######################
#
# # Null model is the best fitting model from the privious section of the heiarchy
# mod_abd_null <- mod_dct_obs
#
# # sage cover model
# mod_abd_sage<- gdistsamp(lambdaformula = ~ln.Sage.Cover,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun  = "exp")
# #save the model
# save(mod_abd_sage, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_sage.RData"))
#
# # Perenial forb and grass cover model
# mod_abd_pern <- gdistsamp(lambdaformula = ~Perennial.Cover,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun  = "exp")
# #save the model
# save(mod_abd_pern, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_pern.RData"))
#
# # Bare ground cover model
# mod_abd_bg <- gdistsamp(lambdaformula = ~ln.Bare.Ground.Cover,
#                         phiformula = ~1,
#                         pformula = ~ Observer.ID,
#                         data = umf,
#                         output = "density", unitsOut = "ha",
#                         mixture = "NB",
#                         keyfun =  "exp")
# #save the model
# save(mod_abd_bg, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_bg.RData"))
#
# # Elevation Model model
# mod_abd_elv <- gdistsamp(lambdaformula = ~Elevation,
#                             phiformula = ~1,
#                             pformula = ~ Observer.ID,
#                             data = umf,
#                             output = "density", unitsOut = "ha",
#                             mixture = "NB",
#                             keyfun = "exp")
# #save the model
# save(mod_abd_elv, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_elc.RData"))
#
# # Load the single covariate abundance models back into R
# load(paste0("Model_Files/umk_", species_to_model, "_mod_dct_obs.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_sage.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_pern.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_bg.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_elv.RData"))
#
# #compare adundance models using AIC
# modlist_abd = list(mod_null = mod_dct_obs,
#                    mod_sage = mod_abd_sage,
#                    mod_pern = mod_abd_pern,
#                    mod_bg = mod_abd_bg,
#                    mod_annual = mod_abd_annu,
#                    mod_elv = mod_abd_elv)
# aictab(modlist_abd)
# # Everything has higher AIC than the null except for proportion of sagebrush
#
# # 2.5) multi-covariate abundance models ##################################################################
# mod_abd_process1 <- gdistsamp(lambdaformula = ~ ln.Sage.Cover + ln.Bare.Ground.Cover + Perennial.Cover + Elevation
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               unitsOut = "ha",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process1, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process1.RData"))
#
# #Remove Bare Ground Cover
# mod_abd_process2 <- gdistsamp(lambdaformula = ~ ln.Sage.Cover + Perennial.Cover + Elevation,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               unitsOut = "ha",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process2, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process2.RData"))
#
# #Remove perennial cover
# mod_abd_process3 <- gdistsamp(lambdaformula = ~ ln.Sage.Cover + ln.Bare.Ground.Cover + Elevation
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               unitsOut = "ha",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process3, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process3.RData"))
#
#
# #Remove Elevation
# mod_abd_process4 <- gdistsamp(lambdaformula = ~ ln.Sage.Cover + ln.Bare.Ground.Cover + Perennial.Cover 
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               unitsOut = "ha",
#                               mixture = "NB",
#                               keyfun = "exp")
# # #save the model
# save(mod_abd_process4, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process4.RData"))

# 3.1) Summaries and diagnostics on candidate process models #################################

#Load the candidate models
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_sage.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process1.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process2.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process3.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process4.RData"))

# Combine all candidate models
modlist_abd = list(
  # mod_null = mod_abd_sage,
                   mod_process1 = mod_abd_process1,
                   mod_process2 = mod_abd_process2,
                   mode_process3 = mod_abd_process3,
                   mod_process4 = mod_abd_process4)

# Check to confirm that all candidate models converged
sapply(modlist_abd, checkConv)

# Check the condition number from each model's Hessian matrix to see if they are overparameterized
sapply(modlist_abd, extractCN)

# Check the standard errors of the candidate models
lapply(modlist_abd, checkParms, se.max = 10, simplify = FALSE)

# Compare AIC scores among all candidate preocess models -------------
aictab(modlist_abd)

# A function that calculates overdisperision estimate (c-hat) from pearson residuals
c_hat_dist <- function(mod){
  # Extract the data from the fitted model
  mod_dat <- unmarked::getData(mod)
  #find the number of observations
  n_obs <- nrow(mod_dat@y)
  #Extract the Pearson residuals from the model
  resids <- residuals(mod, type = "pearson")
  # Sum of squared Pearson residuals
  sse <- sum(resids^2)
  # Residual degrees of freedom
  res_df <- n_obs - length(coef(mod))
  # Overdispersion parameter (c-hat)
  c_hat <- sse / res_df
  #Output
  return(c_hat)
}

# Calculate c-hat for all models (Not relevant if using negative binomial)
c_hat_modlist <- sapply(modlist_abd, c_hat_dist)
c_hat_modlist

# All of the Poisson models are overdispersed but they look good as negative binomials

#Model summaries corrected for overdispersion (Poisson only)
# summaryOD(mod_abd_sage, c.hat = c_hat_modlist[1])
# summaryOD(mod_abd_process1, c.hat = c_hat_modlist[2])
# summaryOD(mod_abd_process2, c.hat = c_hat_modlist[3])
# summaryOD(mod_abd_process3, c.hat = c_hat_modlist[4])

# Compare QAIC scores among all candidate models
aictab(modlist_abd, c.hat = mean(c_hat_modlist))

# Function returning three fit-statistics.
fitstats <- function(mod) {
  #observed data
  observed <- getY(mod@data)
  #Expected values
  expected <- fitted(mod)
  #Residuals
  resids <- residuals(mod)
  #Sum of squared erros
  sse <- sum(resids^2)
  #chi-squared test statisitc
  chisq <- sum((observed - expected)^2 / expected)
  #Freeman Tucky discrepancy
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  #combine the diagnostic test outputs
  out <- c(SSE = sse, Chisq = chisq, freemanTukey = freeTuke)
  #print the output
  return(out)
  
}

# Use parametric bootstrapping to view model outputs (this could take a while)
# pb_m8 <- unmarked::parboot(mod_abd_process8,
#                         fitstats,
#                         nsim = 5000,
#                         report = 1)


# 4.1) Load and view the best performing model  ##################################################

# Load the current best performing model
# I'm chosing the elevation, sage_cvr, pern, bg model based on a combination of prior knowledge, AIC, and other fit statistics
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process1.RData"))
mod_best <- mod_abd_process1

#View model output again
summary(mod_best)

# View model data
head(mod_best@data)

#Pull out the data to make predictions with
lam_dat <- mod_best@data@siteCovs
#...and view
head(lam_dat)

#Define the size of the simulated data set
N_sim <- 1000

# Pull out the original covariate means ----
mean_sage <- mean(hdm_dat$ln.Sage.Cover)
mean_pern <- mean(hdm_dat$Perennial.Cover)
mean_bg <- mean(hdm_dat$ln.Bare.Ground.Cover)
mean_elv <- mean(hdm_dat$Elevation)

# Pull out the original covariate sd's ----
sd_sage <- sd(hdm_dat$ln.Sage.Cover)
sd_pern <- sd(hdm_dat$Perennial.Cover)
sd_bg <- sd(hdm_dat$ln.Bare.Ground.Cover)
sd_elv <- sd(hdm_dat$Elevation)

#Write a function that undoes scaling
unscale <- function(x, mu, sd){
  y <- x * sd + mu
  return(y)
}

# 4.2) simulating data for model predictions #########################################################

#Simulate log sage cover
sim_sage <- data.frame(ln.Sage.Cover = seq(from = min(lam_dat$ln.Sage.Cover),
                                           to = max(lam_dat$ln.Sage.Cover), 
                                          length.out = N_sim),
                      Perennial.Cover = rep(mean(lam_dat$Perennial.Cover), N_sim),
                      ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                      Elevation = rep(mean(lam_dat$Elevation), N_sim))

# Make predictions based on sage cover
abund_est_sage <- unmarked::predict(object = mod_best,
                                     type = "lambda",
                                     newdata = sim_sage,
                                     appendData = TRUE)
abund_est_sage <- abund_est_sage %>%
  mutate(Sage.Cover = exp(unscale(x = abund_est_sage$ln.Sage.Cover,
                                   mu = mean_sage,
                                   sd = sd_sage)))

#Make a sage cover plot
sage_est_plot <- ggplot(data = abund_est_sage, aes(x = Sage.Cover, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity",
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = "birds/ha",
       title = "% sage Cover") +
  theme_minimal()

#-------------------------------------------------------------------------------------

# Simulate perennial forb and grass cover
sim_pern <- data.frame(ln.Sage.Cover = rep(mean(lam_dat$ln.Sage.Cover), N_sim),
                       Perennial.Cover = seq(from = min(lam_dat$Perennial.Cover),
                                             to = max(lam_dat$Perennial.Cover), length.out = N_sim),
                       ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                       Elevation = rep(mean(lam_dat$Elevation), N_sim))

# Make predictions based on perennial cover
abund_est_pern <- unmarked::predict(object = mod_best,
                                    type = "lambda",
                                    newdata = sim_pern,
                                    appendData = TRUE)
abund_est_pern <- abund_est_pern %>%
  mutate(Perennial.Cover.naiv = unscale(x = abund_est_pern$Perennial.Cover,
                                        mu = mean_pern,
                                        sd = sd_pern))

# Make a perennial grass cover plot
pern_est_plot <- ggplot(data = abund_est_pern, aes(x = Perennial.Cover.naiv, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity",
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = "birds/ha",
       title = "% Perrenial Grass Cover") +
  theme_minimal()

#-------------------------------------------------------------------------------------

# Simulate bare ground cover
sim_bg <- data.frame(ln.Sage.Cover = rep(mean(lam_dat$ln.Sage.Cover), N_sim),
                      Perennial.Cover = rep(mean(lam_dat$Perennial.Cover), N_sim),
                      ln.Bare.Ground.Cover = seq(from = min(lam_dat$ln.Bare.Ground.Cover),
                                      to = max(lam_dat$ln.Bare.Ground.Cover), length.out = N_sim),
                      Elevation = rep(mean(lam_dat$Elevation), N_sim))

# Make predictions based on bare ground cover
abund_est_bg <- unmarked::predict(object = mod_best,
                                   type = "lambda",
                                   newdata = sim_bg,
                                   appendData = TRUE)
abund_est_bg <- abund_est_bg %>%
  mutate(ln.Bare.Ground.Cover.naiv = exp(unscale(x = abund_est_bg$ln.Bare.Ground.Cover,
                                                 mu = mean_bg,
                                                 sd = sd_bg)))

# make a bare ground cover plot
bg_est_plot <- ggplot(data = abund_est_bg, aes(x = ln.Bare.Ground.Cover.naiv, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity",
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = NULL,
       title = "% Bare Ground Cover") +
  theme_minimal()

# ---------------------------------------------------------------------------------------------------------

# simulate Elevation
sim_elv <- data.frame(ln.Sage.Cover = rep(mean(lam_dat$ln.Sage.Cover), N_sim),
                      Perennial.Cover = rep(mean(lam_dat$Perennial.Cover), N_sim),
                      ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                      Elevation = seq(from = min(lam_dat$Elevation), to = max(lam_dat$Elevation), length.out = N_sim))


# Make predictions based on Elevation
abund_est_elv <- unmarked::predict(object = mod_best,
                                   type = "lambda",
                                   newdata = sim_elv,
                                   appendData = TRUE)
abund_est_elv <- abund_est_elv %>%
  mutate(Elevation.naive = unscale(x = abund_est_elv$Elevation,
                           mu = mean_elv,
                           sd = sd_elv))

#make a Elevationplot
elv_est_plot <- ggplot(data = abund_est_elv, aes(x = Elevation.naive, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity",
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = "birds/ha",
       title = "Elevation (m)") +
  theme_minimal()

# ----------------------------------------------------------------------------------------------------

# View prediction graphs
gridExtra::grid.arrange(sage_est_plot,
                        pern_est_plot,
                        bg_est_plot,
                        elv_est_plot,
                        nrow = 2, ncol = 2)

# 4.3) Making Spatial predictions from the best performing model ##################################################################

#primary file path for rasters
#This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add in raster layers
ras_sage <- raster(paste0(ras_path, "sage_cvr.tif"))
ras_pern <- raster(paste0(ras_path, "pern_cvr.tif"))
ras_bg <- raster(paste0(ras_path, "bg_cvr.tif"))
ras_elv <- raster(paste0(ras_path, "elevation.tif"))

# Write a function to log-transform rasters 
log_ras <- function(ras) {
  ras_out <- calc(ras, function(x) {
    ifelse(x == 0, 0, log(x))
  })
  return(ras_out)
}

# Log-transform the rasters that need to be logged 
ras_ln_sage <- log_ras(ras_sage)
ras_ln_bg <- log_ras(ras_bg)

# Change the rasters to the mean or mode of an 8x8 grid (240m x 240m)
ras_agg_sage <- raster::aggregate(ras_ln_sage, fact = 8, fun = mean)
ras_agg_pern <- raster::aggregate(ras_pern, fact = 8, fun = mean)
ras_agg_bg <- raster::aggregate(ras_ln_bg , fact = 8, fun = mean)
ras_agg_elv <- raster::aggregate(ras_elv , fact = 8, fun = mean)

# Write a function to scale all rasters based on the MODEL'S scaling 
scale_ras <- function(ras, mu, sd) {
  ras_out <- calc(ras, function(x) {
    (x - mu) / sd
  })
  return(ras_out)
} 

# scale all rasters base on the ORGIONAL DATA's mean and sd
ras_scl_sage <- scale_ras(ras_agg_sage, mean_sage, sd_sage)
ras_scl_pern <- scale_ras(ras_agg_pern, mean_pern, sd_pern)
ras_scl_bg <- scale_ras(ras_agg_bg, mean_bg, sd_bg)
ras_scl_elv <- scale_ras(ras_agg_elv, mean_elv, sd_elv)

# View model estimates
summary(mod_best)

# Extract model beta coefficients
beta0 <- mod_best@estimates@estimates$lambda@estimates[1]
beta_sage <- mod_best@estimates@estimates$lambda@estimates[2]
beta_pern <- mod_best@estimates@estimates$lambda@estimates[4]
beta_bg <- mod_best@estimates@estimates$lambda@estimates[3]
beta_elv <- mod_best@estimates@estimates$lambda@estimates[5]

# Make a raster for the intercept
ras_beta0 <- raster::calc(ras_scl_sage, function(x){x * 0 + beta0})

# a function that multiplies each raster by it's beta coeficient
ras_pred <- function(ras, beta){
  ras_out <- raster::calc(ras, function(x){x * beta})
  return(ras_out)
}


# Multiply each raster by it's coefficient
ras_sage_pred <- ras_pred(ras = ras_scl_sage, 
                          beta = beta_sage)
ras_pern_pred <- ras_pred(ras = ras_scl_pern, 
                          beta = beta_pern)
ras_bg_pred <- ras_pred(ras = ras_scl_bg, 
                          beta = beta_bg)
ras_elv_pred <- ras_pred(ras = ras_scl_elv, 
                          beta = beta_elv)

# Full linear combination of rasters
ras_ln_pred <- ras_beta0 + 
               ras_sage_pred +
               ras_pern_pred +
               ras_elv_pred

# Move back to the native scale
ras_pred <- raster::calc(ras_ln_pred, function(x){exp(x)})

# View a histogram of the raster output
hist(ras_pred)

# Add the study region polygon
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))

# Add the fire perimeter polygon 
fire_perims_multi <- st_read(paste0(ras_path, "Fire_Perimeters.shp"))

# Dissolve overlapping fire perimeters
fire_perims <- fire_perims_multi %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

# Switch between plotting and interactive modes
tmap_mode("plot")
# tmap_mode("view")

# Plot predicted Brewer's Sparrow abundance
brsp_umk_mod_abd <- tm_shape(ras_pred) +
  tm_raster(palette = "YlGnBu",
            title = paste0("(Birds/ha)"),
            n = 7,
            style = "cont") +
  tm_shape(study_region) +
  tm_fill(alpha = 0.1) + 
  tm_borders(lwd = 2, col = "black") +
  tm_shape(fire_perims) +
  tm_borders(lwd = 2.4, 
             alpha = 0.8,
             col = "red4") +
  tm_layout(frame = FALSE,
            legend.outside = FALSE,
            legend.title.size = 1.6,
            legend.position = c(-0.1, 0.2),
            legend.text.size = 1.3) +
  tm_add_legend(type = "line", 
                labels = paste0("Fires"), 
                col = "red4",
                lwd = 6) + 
  tm_scale_bar(
    breaks = c(0, 15, 30),
    position = c("right", "bottom"), 
    text.size = 1.5, 
    color.light = "gray95",
    color.dark = "black")
# View the map
brsp_umk_mod_abd

# Path to where I'm storing the plots
plot_path <- "C:\\Users\\willh\\Box\\Will Harrod MS Project\\Thesis_Documents\\Maps\\"

# Export map
tmap_save(brsp_umk_mod_abd, paste0(plot_path, "brsp_umk_mod_abd"))


# 5) Modeling fire effects ###############################################################

# 5.2) Combined fire models #############################################################################################################################

# #Load the null model
# load(paste0("Model_Files/umk_", species_to_model, "_mod_dct_obs.RData"))
# 
# # Full model
# mod_abd_fire1 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Years.Since.Fire^2 * Burned +
#                              Burn.Sevarity * Burned +
#                              Elevation * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire1, file = paste0("Model_Files/umk_", species_to_model,"_mod_abd_fire1.RData"))
# 
# # Take away burn severity
# mod_abd_fire2 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Years.Since.Fire^2 * Burned +
#                              Elevation * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire2, file = paste0("Model_Files/umk_", species_to_model,"_mod_abd_fire2.RData"))
# 
# # Take away Elevation
# mod_abd_fire3 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Years.Since.Fire^2 * Burned +
#                              Burn.Sevarity * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire3, file = paste0("Model_Files/umk_", species_to_model,"_mod_abd_fire3.RData"))
# 
# # Interactiuon between burn severity and elevation
# mod_abd_fire4 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Years.Since.Fire^2 * Burned +
#                              Burn.Sevarity * Elevation * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire4, file = paste0("Model_Files/umk_", species_to_model,"_mod_abd_fire4.RData"))
# 
# # Interaction between burn severity and elevation
# mod_abd_fire5 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Years.Since.Fire^2 * Burned +
#                              Burn.Sevarity * Burned +
#                              Elevation * Burned +
#                              Burn.Sevarity * Elevation * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire5, file = paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire5.RData"))
# 
# # Interaction between burn severity and time since fire
# mod_abd_fire5 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Elevation * Burned +
#                              Burn.Sevarity * Years.Since.Fire * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire5, file = paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire5.RData"))
# 
# # Interaction between burn severity elevation and time since fire
# mod_abd_fire6 <- gdistsamp(lambdaformula = ~ Years.Since.Fire * Burned +
#                              Burn.Sevarity * Years.Since.Fire * Elevation * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density", unitsOut = "ha",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire6, file = paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire6.RData"))
# 
# # Load these models
# load(paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire1.RData"))
# load(paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire2.RData"))
# load(paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire3.RData"))
# load(paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire4.RData"))
# load(paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire5.RData"))
# load(paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire6.RData"))
# 
# #View model summaries
# summary(mod_fire1)
# summary(mod_fire2)
# summary(mod_fire3)
# summary(mod_fire4)
# summary(mod_fire5)
# summary(mod_fire6)
# 
# # Make a list of these models
# modlist_fire <- liust(mod_null = mod_dct_obs,
#                       mod_fire1 = mod_abd_fire1,
#                       mod_fire2 = mod_abd_fire2,
#                       mod_fire3 = mod_abd_fire3,
#                       mod_fire4 = mod_abd_fire4,
#                       mod_fire5= mod_abd_fire5,
#                       mod_fire6 = mod_abd_fire6)
# 
# # Check to confirm that all candidate models converged
# sapply(modlist_fire, checkConv)
# 
# #compare AIC among models
# aictab(modlist_fire)

# 5.3) Model diagnostics on the "best" fire model ##############################################################################################

# Load the best model
load()



# 5.4 Graphical predictions based on the best performing fire model #####################################################################################