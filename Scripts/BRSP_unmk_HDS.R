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
species_to_model <- "BRSP"

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

# Add covariate data
covs <- tibble(read.csv("Data/Outputs/point_summaries.csv")) %>%
  dplyr::select(-X, -Point.X, -Point.Y) %>%
  tibble()
#...and view
glimpse(covs)

#Make a "percent sagebrush cover" layer

#View the initial data again
glimpse(sobs)

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
  left_join(covs, by = c("Full.Point.ID")) %>%
  # How long since each fire
  mutate(Years.Since.Fire = lubridate::year(Date) - Fire.Year) %>%
  #Update the variables based on my exploratory analysis
  mutate(
    #The variables that need to be log-transformed
    ln.Road.Distance = log(Road.Distance),
    ln.Shrub.Cover = log(Shrub.Cover),
    ln.Bare.Ground.Cover = log(Bare.Ground.Cover),
    ln.TRI = log(TRI),
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
         ln.Shrub.Cover = scale(ln.Shrub.Cover)[,1],
         ln.Bare.Ground.Cover = scale(ln.Bare.Ground.Cover)[,1],
         Annual.Cover = scale(Annual.Cover)[,1],
         ln.TRI = scale(TRI)[,1],
         Precipitation = scale(Precipitation)[,1],
         Sagebrush.Prop = scale(Sagebrush.Prop)[,1],
         Elevation = scale(Elevation)[1],
         Perennial.Cover = scale(Perennial.Cover)[1],
         # Availability and detection level covariates
         Observer.ID = factor(Observer.ID, levels = sort(unique(Observer.ID))),
         MAS = scale(MAS)[,1],
         Ord.Date = scale(Ord.Date)[,1],
         # Fire ecology covariates
         Burned = as.numeric(factor(hdm_dat$Route.Type, levels = c("R", "B"))) -1,
         Years.Since.Fire = scale(Years.Since.Fire)[,1],
         Burn.Sevarity = case_when(is.na(Burn.Sevarity) ~ 0, TRUE ~ Burn.Sevarity)
  ) %>%
  mutate(Burn.Sevarity = factor(Burn.Sevarity)) %>% 
dplyr::select(#Pick which observation and site level covariates are interesting
  #These are for the process based model
  ln.Shrub.Cover,
  ln.Bare.Ground.Cover,
  Annual.Cover,
  ln.TRI,
  Precipitation,
  Sagebrush.Prop,
  Elevation,
  Perennial.Cover,
  #and observation level covariates
  Observer.ID,
  MAS,
  Ord.Date,
  # For the fire ecology model
  Burned,
  Years.Since.Fire,
  Burn.Sevarity
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
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "halfnorm")
# #save the model
# save(mod_kf_half_norm, file = paste0("Model_Files/umk_", species_to_model, "_brsp_mod_kf_half_norm.RData"))
#
# #hazard rate
# mod_kf_hazard <- gdistsamp(lambdaformula = ~1,
#                            phiformula = ~1,
#                            pformula = ~1,
#                            data = umf,
#                            output = "density",
#                            mixture = "NB",
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
#                         mixture = "NB",
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
#                        output = "density",
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
#                         output = "density",
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
#                          output = "density",
#                          mixture ="P",
#                          keyfun  = "exp")
# #save the model
# save(mod_dct_obs, file = paste0("Model_Files/umk_", species_to_model, "_brsp_mod_dct_obs.RData"))
#
# # Time after sunrise model
# mod_dct_mas <- gdistsamp(lambdaformula = ~1,
#                          phiformula = ~1,
#                          pformula =  ~MAS,
#                          data = umf,
#                          output = "density",
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
#                           output = "density",
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
# # Shrub cover model
# mod_abd_shrub <- gdistsamp(lambdaformula = ~ln.Shrub.Cover,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density",
#                            mixture = "NB",
#                            keyfun  = "exp")
# #save the model
# save(mod_abd_shrub, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_shrub.RData"))
#
# # Bare ground cover model
# mod_abd_bg <- gdistsamp(lambdaformula = ~ln.Bare.Ground.Cover,
#                         phiformula = ~1,
#                         pformula = ~ Observer.ID,
#                         data = umf,
#                         output = "density",
#                         mixture = "NB",
#                         keyfun =  "exp")
# #save the model
# save(mod_abd_bg, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_bg.RData"))
#
#
# # Topographic ruggedness model
# mod_abd_tri <- gdistsamp(lambdaformula = ~ln.TRI,
#                          phiformula = ~1,
#                          pformula = ~ Observer.ID,
#                          data = umf,
#                          output = "density",
#                          mixture = "NB",
#                          keyfun = "exp")
# #save the model
# save(mod_abd_tri, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_tri.RData"))
#
# # Proportion of sagebrush cover model
# mod_abd_sage_prop <- gdistsamp(lambdaformula = ~Sagebrush.Prop * ln.Shrub.Cover,
#                          phiformula = ~1,
#                          pformula = ~ Observer.ID,
#                          data = umf,
#                          output = "density",
#                          mixture = "NB",
#                          keyfun = "exp")
# #save the model
# save(mod_abd_sage_prop, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_sage_prop.RData"))
#
# # Annual grass model model
# mod_abd_annu <- gdistsamp(lambdaformula = ~Annual.Cover,
#                                phiformula = ~1,
#                                pformula = ~ Observer.ID,
#                                data = umf,
#                                output = "density",
#                                mixture = "NB",
#                                keyfun = "exp")
# #save the model
# save(mod_abd_annu, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_annu.RData"))
#
# # Precipitation model
# mod_abd_precip <- gdistsamp(lambdaformula = ~Precipitation,
#                             phiformula = ~1,
#                             pformula = ~ Observer.ID,
#                             data = umf,
#                             output = "density",
#                             mixture = "NB",
#                             keyfun = "exp")
# #save the model
# save(mod_abd_precip, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_precip.RData"))
#
# # Load the abundance models back into R
# load(paste0("Model_Files/umk_", species_to_model, "_mod_dct_obs.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_shrub.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_bg.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_tri.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_sage_prop.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_annu.RData"))
# load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_precip.RData"))
#
# #compare adundance models using AIC
# modlist_abd = list(mod_null = mod_dct_obs,
#                    mod_shrub = mod_abd_shrub,
#                    mod_bg = mod_abd_bg,
#                    mod_tri = mod_abd_tri,
#                    mod_sage_prop = mod_abd_sage_prop,
#                    mod_annual = mod_abd_annu,
#                    mod_precip = mod_abd_precip)
# aictab(modlist_abd)
# # Everything has higher AIC than the null aexcept for proportion of sagebrush
#
# # 2.5) multi-covariate abundance models ##################################################################
# mod_abd_process1 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation + Annual.Cover + ln.TRI,
#                                   phiformula = ~1,
#                                   pformula = ~ Observer.ID,
#                                   data = umf,
#                                   output = "density",
#                                   mixture = "NB",
#                                   keyfun = "exp")
# #save the model
# save(mod_abd_process1, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process1.RData"))
#
# #Remove Ruggedness
# mod_abd_process2 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation + Annual.Cover,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process2, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process2.RData"))
#
# #Remove annual cover
# mod_abd_process3 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation + ln.TRI,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process3, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process3.RData"))
#
#
# #Remove Ruggedness and annual cover
# mod_abd_process4 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process4, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process4.RData"))

# #Remove bare ground cover. It fits the data well but is heavily correlated with Elevation, Precipitation, and perennial cover
# mod_abd_process5 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + Precipitation + Annual.Cover + ln.TRI,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process5, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process5.RData"))
# 
# # Swap elevation for precipitation
# mod_abd_process6 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + Elevation + Annual.Cover + ln.TRI,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process6, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process6.RData"))
# 
# # Swap perennial cover for annual cover
# mod_abd_process7 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + Precipitation + Perennial.Cover + ln.TRI,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process7, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process7.RData"))
# 
# # Swap perennial cover for annual cover and include elevation
# mod_abd_process8 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + Elevation + Perennial.Cover + ln.TRI,
#                               phiformula = ~1,
#                               pformula = ~ Observer.ID,
#                               data = umf,
#                               output = "density",
#                               mixture = "NB",
#                               keyfun = "exp")
# #save the model
# save(mod_abd_process8, file = paste0("Model_Files/umk_", species_to_model, "_mod_abd_process8.RData"))
# 
# # Combine all  candidate models
# modlist_abd = list(mod_null = mod_abd_shrub,
#                    mod_process1 = mod_abd_process1,
#                    mod_process2 = mod_abd_process2,
#                    mode_process3 = mod_abd_process3,
#                    mod_process4 = mod_abd_process4,
#                    mod_process5 = mod_abd_process5,
#                    mod_process6 = mod_abd_process6,
#                    mode_process7 = mod_abd_process7,
#                    mod_process8 = mod_abd_process8)
# 
# # Compare AIC scores among all candidate models
# aictab(modlist_abd)

# Combine all candidate models
modlist_mix = list(mod_abd_process5_P = mod_abd_process6,
                   mod_process5_NB = mod_abd_process5_NB,
                   mod_abd_process5_ZIP = mod_abd_process5_ZIP)

# Compare AIC scores among all candidate models
aictab(modlist_mix)

# 3.1) Summaries and diagnostics on candidate process models #################################

#Load the candidate models
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_shrub.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process1.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process2.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process3.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process4.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process5.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process6.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process7.RData"))
load(paste0("Model_Files/umk_", species_to_model, "_mod_abd_process8.RData"))

# Combine all candidate models
modlist_abd = list(mod_null = mod_abd_shrub,
                   mod_process1 = mod_abd_process1,
                   mod_process2 = mod_abd_process2,
                   mode_process3 = mod_abd_process3,
                   mod_process4 = mod_abd_process4,
                   mod_process5 = mod_abd_process5,
                   mod_process6 = mod_abd_process6,
                   mode_process7 = mod_abd_process7,
                   mod_process8 = mod_abd_process8)

# Check to confirm that all candidate models converged
sapply(modlist_abd, checkConv)

# Check the condition number from each model's Hessian matrix to see if they are overparameterized
sapply(modlist_abd, extractCN)

# Check the standard errors of the candidate models
lapply(modlist_abd, checkParms, se.max = 10, simplify = FALSE)

# A function that calculates overdisperision estimate (c-hat) fro  pearson residuals
c_hat_dist <- function(mod){
  # Extract the data from the fitted model
  mod_dat <- getData(mod)
  #find the number of observations
  n_obs <- nrow(mod_dat@y)
  #Extract the Pearson residuals from the model
  pearson_residuals <- residuals(mod, type = "pearson")
  # Sum of squared Pearson residuals
  sum_sq_pearson <- sum(pearson_residuals^2)
  # Residual degrees of freedom
  res_df <- n_obs - length(coef(mod))
  # Overdispersion parameter (c-hat)
  c_hat <- sum_sq_pearson / res_df
  #Output
  return(c_hat)
}

# Calculate c-hat for all models
c_hat_modlist <- sapply(modlist_abd, c_hat_dist)
c_hat_modlist
# All of the Poisson models are overdispersed

# Compare AIC scores among all candidate models
aictab(modlist_abd)

#Model summaries corrected for overdispersion
summaryOD(mod_abd_shrub, c.hat = c_hat_modlist[9])
summaryOD(mod_abd_process1, c.hat = c_hat_modlist[1])
summaryOD(mod_abd_process2, c.hat = c_hat_modlist[5])
summaryOD(mod_abd_process3, c.hat = c_hat_modlist[4])
summaryOD(mod_abd_process4, c.hat = c_hat_modlist[6])
summaryOD(mod_abd_process5, c.hat = c_hat_modlist[2])
summaryOD(mod_abd_process6, c.hat = c_hat_modlist[3])
summaryOD(mod_abd_process7, c.hat = c_hat_modlist[7])
summaryOD(mod_abd_process8, c.hat = c_hat_modlist[8])

# Compare QAIC scores among all candidate models
aictab(modlist_abd, c.hat = mean(c_hat_modlist)) # I need a better values for c.hat ----

# Function returning three fit-statistics.
fitstats <- function(fm) {
  #observed data
  observed <- getY(fm@data)
  #Expected values
  expected <- fitted(fm)
  #Residuals
  resids <- residuals(fm)
  #Sum of squared erros
  sse <- sum(resids^2)
  #chi-squared test statisitc
  chisq <- sum((observed - expected)^2 / expected)
  #Freeman Tucky discrepancy
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  #combine the diagnostic test outputs
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  #print the output
  return(out)
  
}

# Use parametric bootstrapping to view model outputs (this could take a while)
# pb <- parboot(mod_abd_process1,
#               fitstats,
#               nsim=25,
#               report=1)

# Assess the affect of including annual cover on model average shrinking
modavgShrink(cand.set = modlist_abd,
             parm = "Annual.Cover",
             parm.type = "lambda")

# Assess the affect of including ruggedness on model average shrinking
modavgShrink(cand.set = modlist_abd,
             parm = "ln.TRI",
             parm.type = "lambda")


# 4.1) Graphical predictions based on the best performing model ##################################################

# Load the current best performing model
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

# simulating data that for model predictions 

#Write a function that undoes scaling
unscale <- function(x, mu, sd){
  y <- x * sd + mu
  return(y)
}

# Pull out the origonal covariate means
mean_shrub <- mean(hdm_dat$ln.Shrub.Cover)
mean_bg <- mean(hdm_dat$ln.Bare.Ground.Cover)
mean_annu <- mean(hdm_dat$Annual.Cover)
mean_precip <- mean(hdm_dat$Elevation)
mean_tri <- mean(hdm_dat$ln.TRI)

# Pull out the origonal covariate sd's
sd_shrub <- sd(hdm_dat$ln.Shrub.Cover)
sd_bg <- sd(hdm_dat$ln.Bare.Ground.Cover)
sd_annu <- sd(hdm_dat$Annual.Cover)
sd_precip <- sd(hdm_dat$Precipitation)
sd_tri <- sd(hdm_dat$ln.TRI)

#-------------------------------------------------------------------------------------
#Simulate log shrub cover 
sim_shb <- data.frame(ln.Shrub.Cover = seq(from = min(lam_dat$ln.Shrub.Cover), 
                                           to = max(lam_dat$ln.Shrub.Cover), length.out = N_sim),
                      ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                      Annual.Cover = rep(mean(lam_dat$Annual.Cover), N_sim),
                      Elevation = rep(mean(lam_dat$Elevation), N_sim),
                      ln.TRI = rep(mean(lam_dat$Elevation), N_sim)) 

# Make predictions based on shrub cover
abund_est_shrub <- unmarked::predict(object = mod_best,
                                   type = "lambda",
                                   newdata = sim_shb,
                                   appendData = TRUE)
abund_est_shrub <- abund_est_shrub %>% 
  mutate(Shrub.Cover = exp(unscale(x = abund_est_shrub$ln.Shrub.Cover, 
                                   mu = mean_shrub, 
                                   sd = sd_shrub)))

#Make a shrub cover plot
shrub_est_plot <- ggplot(data = abund_est_shrub, aes(x = Shrub.Cover, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity", 
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = "birds/ha", 
       title = "% Shrub Cover") +
  theme_minimal()

#-------------------------------------------------------------------------------------
# Simulate annual forb and grass cover
sim_annual <- data.frame(ln.Shrub.Cover = rep(mean(lam_dat$ln.Shrub.Cover), N_sim),
                         ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                         Annual.Cover = seq(from = min(lam_dat$Annual.Cover), 
                                            to = max(lam_dat$Annual.Cover), length.out = N_sim),
                         Elevation = rep(mean(lam_dat$Elevation), N_sim),
                         ln.TRI = rep(mean(lam_dat$ln.TRI), N_sim))

# Make predictions based on annual cover
abund_est_annual <- unmarked::predict(object = mod_best,
                                   type = "lambda",
                                   newdata = sim_annual,
                                   appendData = TRUE)
abund_est_annual <- abund_est_annual %>% 
  mutate(Annual.Cover.naiv = unscale(x = abund_est_annual$Annual.Cover,
                                mu = mean_annu,
                                sd = sd_annu))
  
# amke a annual grass cover plot
annu_est_plot <- ggplot(data = abund_est_annual, aes(x = Annual.Cover.naiv, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity", 
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = "birds/ha", 
       title = "% Annual Grass Cover") +
  theme_minimal()

#-------------------------------------------------------------------------------------
# Simulate Elevation
sim_elv <- data.frame(ln.Shrub.Cover = rep(mean(lam_dat$ln.Shrub.Cover), N_sim),
                         ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                         Annual.Cover = rep(mean(lam_dat$Annual.Cover), N_sim),
                         Elevation = seq(from = min(lam_dat$Elevation), 
                                             to = max(lam_dat$Elevation), length.out = N_sim),
                         ln.TRI = rep(mean(lam_dat$ln.TRI), N_sim))

# Make predictions based on Elevation
abund_est_elv <- unmarked::predict(object = mod_best,
                                      type = "lambda",
                                      newdata = sim_precip,
                                      appendData = TRUE) 
abund_est_elv <- abund_est_precip %>% 
  mutate(Elevation.naiv = unscale(x = abund_est_precip$Elevation,
                                      mu = mean_precip,
                                      sd = sd_precip))

#make a Elevation plot
precip_est_plot <- ggplot(data = abund_est_precip, aes(x = Elevation.naiv, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity", 
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = NULL, 
       title = "Elevation (mm)") +
  theme_minimal()

# ---------------------------------------------------------------------------------------------------------
# simulate log of ruggedness
sim_tri <- data.frame(ln.Shrub.Cover = rep(mean(lam_dat$ln.Shrub.Cover), N_sim),
                      ln.Bare.Ground.Cover = rep(mean(lam_dat$ln.Bare.Ground.Cover), N_sim),
                      Annual.Cover = rep(mean(lam_dat$Annual.Cover), N_sim),
                      Elevation = rep(mean(lam_dat$Elevation), N_sim),
                      ln.TRI = seq(from = min(lam_dat$ln.TRI), to = max(lam_dat$ln.TRI), length.out = N_sim))


# Make predictions based on ruggedness
abund_est_tri <- unmarked::predict(object = mod_best,
                                   type = "lambda",
                                   newdata = sim_tri,
                                   appendData = TRUE) 
abund_est_tri <- abund_est_tri %>% 
  mutate(TRI = exp(unscale(x = abund_est_tri$ln.TRI,
                           mu = mean_tri,
                           sd = sd_tri)))
  
#make a ruggedness plot
tri_est_plot <- ggplot(data = abund_est_tri, aes(x = ln.TRI, y = exp(Predicted))) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), stat = "identity", 
              color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(x = NULL, y = "birds/ha", 
       title = "Topographic Ruggedness") +
  theme_minimal()

# ----------------------------------------------------------------------------------------------------
# View prediction graphs
gridExtra::grid.arrange(shrub_est_plot,
                        bg_est_plot,
                        annu_est_plot,
                        precip_est_plot,
                        tri_est_plot,
                        nrow = 3, ncol = 2)

# 4.3) Making Spatial predictions ##################################################################

#primary file path for rasters
#This is the geoprocessing outputs folder for my arc pro project
ras_path <- "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add in raster layers
ras_shrub <- raster(paste0(ras_path, "shrub_cvr.tif"))
ras_bg <- raster(paste0(ras_path, "bg_cvr.tif"))
ras_annu <- raster(paste0(ras_path, "anu_cvr.tif"))
ras_precip <- raster(paste0(ras_path, "precip.tif"))
ras_tri <- raster(paste0(ras_path, "tri_3x3.tif"))

# Write a function to log-transform rasters 
log_ras <- function(ras) {
  ras_out <- calc(ras, function(x) {
    ifelse(x == 0, 0, log(x))
  })
  return(ras_out)
}

# Log-transform the rasters that need to be logged 
ras_ln_shrub <- log_ras(ras_shrub)
ras_ln_bg <- log_ras(ras_bg)
ras_ln_tri <- log_ras(ras_tri)

# Change the rasters to the mean or mode of an 8x8 grid (240m x 240m)
ras_agg_shrub <- raster::aggregate(ras_ln_shrub, fact = 8, fun = mean)
ras_agg_bg <- raster::aggregate(ras_ln_bg, fact = 8, fun = mean)
ras_agg_annu <- raster::aggregate(ras_annu, fact = 8, fun = mean)
# ras_agg_elev <- raster::aggregate(ras_elv, fact = 8, fun = mean)
ras_agg_tri <- raster::aggregate(ras_ln_tri , fact = 8, fun = mean)
  
# line up other rasters with shrub cover
ras_res_bg <- resample(scale(ras_agg_bg), ras_agg_shrub)
ras_res_annu <- resample(scale(ras_agg_annu), ras_agg_shrub)
ras_res_precip <- resample(scale(ras_precip), ras_agg_shrub)
ras_res_tri <- resample(scale(ras_agg_tri), ras_agg_shrub)

# Write a function to scale all rasters based on the MODEL'S scaling 
scale_ras <- function(ras, mu, sd) {
  ras_out <- calc(ras, function(x) {
    (x - mu) / sd
  })
  return(ras_out)
} 

# scale all rasters and resample them to line up with shub cover
ras_scl_shrub <- scale_ras(ras_agg_shrub, mean_shrub, sd_shrub)
ras_scl_bg <- scale_ras(ras_res_bg, mean_bg, sd_bg)
ras_scl_annu <- scale_ras(ras_res_annu, mean_annu, sd_annu)
ras_scl_precip <- scale_ras(ras_res_precip, mean_precip, sd_precip)
ras_scl_tri <- scale_ras(ras_res_tri, mean_tri, sd_tri)

# Extract model beta coefficients
beta0 <- mod_best@estimates@estimates$lambda@estimates[1]
beta_shb <- mod_best@estimates@estimates$lambda@estimates[2]
beta_bg <- mod_best@estimates@estimates$lambda@estimates[3]
beta_precip <- mod_best@estimates@estimates$lambda@estimates[4]
beta_annu <- mod_best@estimates@estimates$lambda@estimates[5]
beta_tri <- mod_best@estimates@estimates$lambda@estimates[6]

# Make a raster for the intercept
ras_beta0 <- raster::calc(ras_scl_shrub, function(x){x * 0 + beta0})

# a function that multiplies each raster by it's beta coeficient
ras_pred <- function(ras, beta){
  ras_out <- raster::calc(ras, function(x){x * beta})
  return(ras_out)
}


# Multiply each raster by it's coefficient
ras_shrb_pred <- ras_pred(ras = ras_scl_shrub, 
                          beta = beta_shb)
ras_bg_pred <- ras_pred(ras = ras_scl_bg, 
                          beta = beta_bg)
ras_precip_pred <- ras_pred(ras = ras_scl_precip, 
                          beta = beta_precip)
ras_annu_pred <- ras_pred(ras = ras_scl_annu, 
                          beta = beta_annu)
ras_tri_pred <- ras_pred(ras = ras_scl_tri, 
                          beta = beta_tri)

# Full linear cobination of rasters
ras_ln_pred <- ras_beta0 + 
               ras_shrb_pred +
               ras_bg_pred +
               ras_annu_pred +
               ras_tri_pred

# Move back to the native scale
ras_pred <- raster::calc(ras_ln_pred, function(x){exp(x)})

# Add in the study region polygon
study_region <- st_read(paste0(ras_path, "Study_Region.shp"))

# Switch between plotting and interactive modes
tmap_mode("plot")
# tmap_mode("view")

# Plot predicted Brewer's Sparrow abundance
tm_shape(study_region) +
  tm_fill(alpha = 0.1) + 
  tm_borders() +
  tm_shape(ras_pred) +
  tm_raster(palette = "YlGnBu", 
            title = paste0("Estimated Brewer's\nSparrow Abundance\n(Birds/ha)"),
            style = "cont") +  
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            legend.text.size = 1.1,  
            legend.title.size = 2.0) +
  tm_basemap(leaflet::providers$Esri.WorldTopoMap) 

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
#                            output = "density",
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
#                            output = "density",
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
#                            output = "density",
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
#                            output = "density",
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
#                            output = "density",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire5, file = paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire5.RData"))
# 
# # Interaction between burn severity and time since fire
# mod_abd_fire5 <- gdistsamp(lambdaformula = ~ Years.Since.Fire^2 * Burned +
#                              Elevation * Burned +
#                              Burn.Sevarity * Years.Since.Fire * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density",
#                            mixture = "NB",
#                            keyfun = "exp")
# #save the model
# save(mod_abd_fire5, file = paste0("Model_Files/umk_", species_to_model,"Model_Files/_mod_abd_fire5.RData"))
# 
# # Interaction between burn severity elevation and time since fire
# mod_abd_fire6 <- gdistsamp(lambdaformula = ~ Years.Since.Fire^2 * Burned +
#                              Burn.Sevarity * Years.Since.Fire * Elevation * Burned,
#                            phiformula = ~1,
#                            pformula = ~ Observer.ID,
#                            data = umf,
#                            output = "density",
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