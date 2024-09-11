#Hierarchical Distance Sampling using the Unmarked package
#Will Harrod
#Utah State University
#Creadted 09/08/2024

# 0) Clear environments and load packages ######################################
rm(list = ls())
install.packages("tidyverse", "unmarked", "AICcmodavg", "DHARMa")
library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(DHARMa)

# 1) Data Prep #################################################################

# 1a) Prepare the data #########################################################

# Add point count data
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
  dplyr::select(-X) %>%
  tibble()
# View the data
glimpse(sobs)

# Make a new column for each survey visit
sobs <- sobs %>%
  mutate(Visit.ID = paste(Full.Point.ID, Year, Visit, sep = "-"))

# Define a single species of interest (SOI)
soi <- "BRSP"

# Define a truncation distance
trunc_dist <- 125

#Define distance bin sizes
bin_size <- 25

# filter for only that species
ydist <- sobs %>%
  filter(Species == soi) %>%
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

#### Extra code if I want to move to the Amundson 2024 model#####################
# # Calculate the time removal bins
# y_rmv <- sobs %>%
#   filter(Species == soi) %>%
#   filter(Distance <= trunc_dist) %>%
#   mutate(Minute.Interval = case_when(Minute %in% c(1, 2) ~ "1st",
#                                    Minute %in% c(3, 4) ~ "2nd",
#                                    Minute %in% c(5, 6) ~ "3rd")) %>%
#   mutate(Time.Interval = paste(Year, Visit, Minute.Interval, sep = "-")) %>%
#   dplyr::select(Visit.ID, Time.Interval)
# #... and view
# glimpse(y_rmv)
#
# #count up the time removal data by site
# rmv_dat_0inf <- y_rmv %>%
#   group_by(Visit.ID, Time.Interval) %>%
#   reframe(Visit.ID, Time.Interval, Count = n()) %>%
#   distinct(Visit.ID, Time.Interval, Count) %>%
#   pivot_wider(names_from = Time.Interval,
#               values_from = Count)
# #... and view
# glimpse(y_rmv)
#
# # Merge with your observations data
# rmv_dat <- points %>%
#   left_join(rmv_dat_0inf,
#             by = "Visit.ID") %>%
#   mutate(across(everything(), ~ replace_na(., 0))) %>%
#   dplyr::select(Visit.ID,
#          `Y1-V1-1st`,
#          `Y1-V1-2nd`,
#          `Y1-V1-3rd`,
#          `Y1-V2-1st`,
#          `Y1-V2-2nd`,
#          `Y1-V2-3rd`,
#          `Y2-V1-1st`,
#          `Y2-V1-2nd`,
#          `Y2-V1-3rd`,
#          `Y2-V2-1st`,
#          `Y2-V2-2nd`,
#          `Y2-V2-3rd`,
#          `Y3-V1-1st`,
#          `Y3-V1-2nd`,
#          `Y3-V1-3rd`,
#          `Y3-V2-1st`,
#          `Y3-V2-2nd`,
#          `Y3-V2-3rd`) %>%
#   arrange(Visit.ID)
# #... and view
# glimpse(rmv_dat)
# rmv_dat %>%
#   filter(`1st` != 0)

# 1b) Covariates ###############################################################

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
                Ord.Date, MAS, Temp.Start, Wind.Start, Sky.Start,
                UTM.X, UTM.Y) %>%
  distinct(Visit.ID, Year, Visit, Full.Point.ID, Route.ID, Observer.ID,
           Ord.Date, MAS, Temp.Start, Wind.Start, Sky.Start,
           UTM.X, UTM.Y) %>%
  #Update the variabls based on my exploratory analysis
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
                          TRUE ~ Sky.Start))

#...and view
glimpse(covs_tbl)

#make sure this is the same as the actual number of points surveyed
nrow(covs_tbl) == length(unique(covs_tbl$Visit.ID))
# good good

# combine these with the covariates
covs_dat <- covs_tbl %>%
  left_join(covs, by = c("Full.Point.ID")) %>%
  mutate(Years.Since.Fire = case_when())

#...and view
glimpse(covs_dat)
#Make sure there are still the proper number of points
nrow(covs_tbl) == length(unique(covs_tbl$Visit.ID))

# Combine covariates with data
hdm_dat <- dist_dat %>%
  # left_join(rmv_dat) %>%
  left_join(covs_dat)
#...and view
glimpse(hdm_dat)

# 1c) Formating these into unmarked objects ######################################

# distance data
dist_mat <- as.matrix(hdm_dat %>%
                        dplyr::select(`(0- 25]`, `(25- 50]`, `(50- 75]`, `(75- 100]`, `(100- 125]`))
# view
head(dist_mat)
dim(dist_mat)

#### Extra code if I want to move to the Amundson 2024 model#####################
# # Time interval data
# time_mat <- as.matrix(hdm_dat %>%
#                         dplyr::select(`Y1-V1-1st`,
#                                `Y1-V1-2nd`,
#                                `Y1-V1-3rd`,
#                                `Y1-V2-1st`,
#                                `Y1-V2-2nd`,
#                                `Y1-V2-3rd`,
#                                `Y2-V1-1st`,
#                                `Y2-V1-2nd`,
#                                `Y2-V1-3rd`,
#                                `Y2-V2-1st`,
#                                `Y2-V2-2nd`,
#                                `Y2-V2-3rd`,
#                                `Y3-V1-1st`,
#                                `Y3-V1-2nd`,
#                                `Y3-V1-3rd`,
#                                `Y3-V2-1st`,
#                                `Y3-V2-2nd`,
#                                `Y3-V2-3rd`))
# head(time_mat)
# dim(time_mat)

#Distance breaks already exist, just need to view them
dist_breaks

# both site covs and detection covariates
names(hdm_dat)
site_covs <- hdm_dat %>%
  #Scall and factoize covariates
  mutate(Area = pi * trunc_dist^2 * 0.0001, # calculate area
         # Abundance level covariates
         Shrub.Cover.scl = scale(Shrub.Cover)[,1],
         Bare.Ground.Cover.scl = scale(Bare.Ground.Cover)[,1],
         Annual.Cover.scl = scale(Annual.Cover)[,1],
         TRI.scl = scale(TRI)[,1],
         Precipitation.scl = scale(Precipitation)[,1],
         # Availability and detection level covariates
         Observer.ID.fct = factor(Observer.ID, levels = sort(unique(Observer.ID))),
         MAS.scl = scale(MAS)[,1],
         Ord.Date.scl = scale(Ord.Date)[,1]) %>%
  dplyr::select(#Pick which observation and site level covariates are interesting
    #These are for the process based model
    Shrub.Cover.scl,
    Bare.Ground.Cover.scl,
    Annual.Cover.scl,
    TRI.scl,
    Precipitation.scl,
    #and observation level covariates
    Observer.ID.fct,
    MAS.scl,
    Ord.Date.scl
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

#### Extra code if I want to move to the Amundson 2024 model#####################
#Combine these into an unmarked frame
# umf <- unmarkedFrameGDR(yDistance = dist_mat,
#                         yRemoval = time_mat,
#                         numPrimary = 6,
#                         siteCovs = site_covs,
#                         survey = "point",
#                         obsCovs = ??,
#                         yearlySiteCovs = ??,
#                         unitsIn = "m",
#                         dist.breaks = dist_breaks,
#                         numPrimary = 1) # I should probably change this to six later

# 2) Model Fitting #############################################################

# 2a) Asses which key function fits the data best ##############################

#Half-normal
mod_kf_half_norm <- gdistsamp(lambdaformula = ~1,
                              phiformula = ~1,
                              pformula = ~1,
                              data = umf,
                              output = "density",
                              mixture = "P",
                              keyfun = "halfnorm")
#save the model
save(mod_kf_half_norm, file = "Model_Files/umk_mod_kf_half_norm.RData")

#hazard rate
mod_kf_hazard <- gdistsamp(lambdaformula = ~1,
                           phiformula = ~1,
                           pformula = ~1,
                           data = umf,
                           output = "density",
                           mixture = "P",
                           keyfun = "hazard")
#save the model
save(mod_kf_hazard, file = "Model_Files/umk_mod_kf_hazard.RData")

#exponential
mod_kf_exp <- gdistsamp(lambdaformula = ~1,
                        phiformula = ~1,
                        pformula = ~1,
                        data = umf,
                        output="density",
                        mixture="P",
                        keyfun = "exp")
#save the model
save(mod_kf_exp, file = "Model_Files/umk_mod_kf_exp.RData")

# compare key-function models using AIC
modlist_key <- list(mod_half_norm = mod_kf_half_norm,
                    mod_hazard = mod_kf_hazard,
                    mod_exp = mod_kf_exp)
aictab(modlist_key)
# The exponential key function seams to perform best

# 2b) Detection level covariate fitting #######################################

# Null model is the best fitting model from the privious section of the heiarchy
mod_dct_null <- mod_kf_exp

# Observer model
mod_dct_obs <- gdistsamp(lambdaformula = ~1,
                         phiformula = ~1,
                         pformula = ~ Observer.ID.fct,
                         data = umf,
                         output = "density",
                         mixture ="P",
                         keyfun  = "exp")
#save the model
save(mod_dct_obs, file = "Model_Files/umk_mod_dct_obs.RData")

# Time after sunrise model
mod_dct_mas <- gdistsamp(lambdaformula = ~1,
                         phiformula = ~1,
                         pformula =  ~MAS.scl,
                         data = umf,
                         output = "density",
                         mixture = "P",
                         keyfun = "exp")
#save the model
save(mod_dct_mas, file = "Model_Files/umk_mod_dct_mas.RData")

#Date model
mod_dct_date <- gdistsamp(lambdaformula = ~1,
                          phiformula = ~1,
                          pformula = ~Ord.Date.scl,
                          data = umf,
                          output = "density",
                          mixture = "P",
                          keyfun = "exp")
#save the model
save(mod_dct_date, file = "Model_Files/umk_mod_dct_date.RData")


# compare detection models using AIC
modlist_dct = list(mod_null = mod_dct_null,
                   mod_obs = mod_dct_obs,
                   mod_mas = mod_dct_mas,
                   mod_date = mod_dct_date)
aictab(modlist_dct)
# All of these perform better than the null but observer is the best predictor

#Explore the observer data. I want to reclassify Alex's observations
mod_out <- summary(mod_dct_obs)$det

#Pull out the observer names
mod_out <- names(mod_dct_obs@estimates@estimates$det@estimates)[2:nrow(mod_out)]

#I'll lump Alex in with Ben

#View the model output
par(mfrow = c(2, 2))
plot(mod_dct_obs)

# 2c) abundance level covariate fitting #######################

# Null model is the best fitting model from the privious section of the heiarchy
mod_abd_null <- mod_dct_obs

# Shrub cover model
mod_abd_shrub <- gdistsamp(lambdaformula = ~Shrub.Cover.scl,
                           phiformula = ~1,
                           pformula = ~ Observer.ID.fct,
                           data = umf,
                           output = "density",
                           mixture = "P",
                           keyfun  = "exp")
#save the model
save(mod_abd_shrub, file = "Model_Files/umk_mod_abd_shrub.RData")

# Bare ground cover model
mod_abd_bg <- gdistsamp(lambdaformula = ~Bare.Ground.Cover.scl,
                        phiformula = ~1,
                        pformula = ~ Observer.ID.fct,
                        data = umf,
                        output = "density",
                        mixture = "P",
                        keyfun =  "exp")
#save the model
save(mod_abd_bg, file = "Model_Files/umk_mod_abd_bg.RData")


# Topographic ruggedness model
mod_abd_tri <- gdistsamp(lambdaformula = ~TRI.scl,
                         phiformula = ~1,
                         pformula = ~ Observer.ID.fct,
                         data = umf,
                         output = "density",
                         mixture = "P",
                         keyfun = "exp")
#save the model
save(mod_abd_tri, file = "Model_Files/umk_mod_abd_tri.RData")

# Precipitation model
mod_abd_precip <- gdistsamp(lambdaformula = ~Precipitation.scl,
                            phiformula = ~1,
                            pformula = ~ Observer.ID.fct,
                            data = umf,
                            output = "density",
                            mixture = "P",
                            keyfun = "exp")
#save the model
save(mod_abd_precip, file = "Model_Files/umk_mod_abd_precip.RData")

# Load the abundance models back into R
load("Model_Files/umk_mod_dct_obs.RData")
load("Model_Files/umk_mod_abd_shrub.RData")
load("Model_Files/umk_mod_abd_bg.RData")
load("Model_Files/umk_mod_abd_tri.RData")
load("Model_Files/umk_mod_abd_precip.RData")

#compare adundance models using AIC
modlist_abd = list(mod_null = mod_dct_obs,
                   mod_shrub = mod_abd_shrub,
                   mod_bg = mod_abd_bg,
                   mod_tri = mod_abd_tri,
                   mod_precip = mod_abd_precip)
aictab(modlist_abd)
#Everything except for TRI helps the mode. I still need to log-transform TRI