#Hierarchical Distance Sampling using the Unmarked package
#Will Harrod
#Utah State University
#Creadted 09/08/2024

# 0) Clear environments, install, and load packages#####################################
rm(list = ls())
# install.packages("tidyverse", "unmarked", "AICcmodavg", "DHARMa")
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
covs_dat <- covs_tbl %>%
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
                          TRUE ~ Sky.Start))

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

#Distance breaks already exist, just need to view them
dist_breaks

# both site covs and detection covariates
names(hdm_dat)
site_covs <- hdm_dat %>%
  #Scall and factoize covariates
  mutate(Area = pi * trunc_dist^2 * 0.0001, # calculate area
         # Abundance level covariates
         ln.Shrub.Cover = scale(ln.Shrub.Cover)[,1],
         ln.Bare.Ground.Cover = scale(ln.Bare.Ground.Cover)[,1],
         Annual.Cover = scale(Annual.Cover)[,1],
         ln.TRI = scale(TRI)[,1],
         Precipitation = scale(Precipitation)[,1],
         Sagebrush.Prop = scale(Sagebrush.Prop)[,1],
         # Availability and detection level covariates
         Observer.ID = factor(Observer.ID, levels = sort(unique(Observer.ID))),
         MAS = scale(MAS)[,1],
         Ord.Date = scale(Ord.Date)[,1]) %>%
  dplyr::select(#Pick which observation and site level covariates are interesting
    #These are for the process based model
    ln.Shrub.Cover,
    ln.Bare.Ground.Cover,
    Annual.Cover,
    ln.TRI,
    Precipitation,
    Sagebrush.Prop,
    #and observation level covariates
    Observer.ID,
    MAS,
    Ord.Date
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
                         pformula = ~ Observer.ID,
                         data = umf,
                         output = "density",
                         mixture ="P",
                         keyfun  = "exp")
#save the model
save(mod_dct_obs, file = "Model_Files/umk_mod_dct_obs.RData")

# Time after sunrise model
mod_dct_mas <- gdistsamp(lambdaformula = ~1,
                         phiformula = ~1,
                         pformula =  ~MAS,
                         data = umf,
                         output = "density",
                         mixture = "P",
                         keyfun = "exp")
#save the model
save(mod_dct_mas, file = "Model_Files/umk_mod_dct_mas.RData")

#Date model
mod_dct_date <- gdistsamp(lambdaformula = ~1,
                          phiformula = ~1,
                          pformula = ~Ord.Date,
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
summary(mod_dct_obs)

#I'll lump Alex in with Ben

#View the model output
par(mfrow = c(2, 2))
plot(mod_dct_obs)

# 2c) abundance level covariate fitting #######################

# Null model is the best fitting model from the privious section of the heiarchy
mod_abd_null <- mod_dct_obs

# Shrub cover model
mod_abd_shrub <- gdistsamp(lambdaformula = ~ln.Shrub.Cover,
                           phiformula = ~1,
                           pformula = ~ Observer.ID,
                           data = umf,
                           output = "density",
                           mixture = "P",
                           keyfun  = "exp")
#save the model
save(mod_abd_shrub, file = "Model_Files/umk_mod_abd_shrub.RData")

# Bare ground cover model
mod_abd_bg <- gdistsamp(lambdaformula = ~ln.Bare.Ground.Cover,
                        phiformula = ~1,
                        pformula = ~ Observer.ID,
                        data = umf,
                        output = "density",
                        mixture = "P",
                        keyfun =  "exp")
#save the model
save(mod_abd_bg, file = "Model_Files/umk_mod_abd_bg.RData")


# Topographic ruggedness model
mod_abd_tri <- gdistsamp(lambdaformula = ~ln.TRI,
                         phiformula = ~1,
                         pformula = ~ Observer.ID,
                         data = umf,
                         output = "density",
                         mixture = "P",
                         keyfun = "exp")
#save the model
save(mod_abd_tri, file = "Model_Files/umk_mod_abd_tri.RData")

# Proportion of sagebrush cover model
mod_abd_sage_prop <- gdistsamp(lambdaformula = ~Sagebrush.Prop * ln.Shrub.Cover,
                         phiformula = ~1,
                         pformula = ~ Observer.ID,
                         data = umf,
                         output = "density",
                         mixture = "P",
                         keyfun = "exp")
#save the model
save(mod_abd_sage_prop, file = "Model_Files/umk_mod_abd_sage_prop.RData")

# Annual grass model model
mod_abd_annu <- gdistsamp(lambdaformula = ~Annual.Cover,
                               phiformula = ~1,
                               pformula = ~ Observer.ID,
                               data = umf,
                               output = "density",
                               mixture = "P",
                               keyfun = "exp")
#save the model
save(mod_abd_annu, file = "Model_Files/umk_mod_abd_annu.RData")

# Precipitation model
mod_abd_precip <- gdistsamp(lambdaformula = ~Precipitation,
                            phiformula = ~1,
                            pformula = ~ Observer.ID,
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
load("Model_Files/umk_mod_abd_sage_prop.RData")
load("Model_Files/umk_mod_abd_annu.RData")
load("Model_Files/umk_mod_abd_precip.RData")

#compare adundance models using AIC
modlist_abd = list(mod_null = mod_dct_obs,
                   mod_shrub = mod_abd_shrub,
                   mod_bg = mod_abd_bg,
                   mod_tri = mod_abd_tri,
                   mod_sage_prop = mod_abd_sage_prop,
                   mod_annual = mod_abd_annu,
                   mod_precip = mod_abd_precip)
aictab(modlist_abd)
# Everything has higher AIC than the null aexcept for proportion of sagebrush

# A Full model that uses all of these covariates
mod_abd_process1 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation + Annual.Cover + ln.TRI, 
                                  phiformula = ~1,
                                  pformula = ~ Observer.ID,
                                  data = umf,
                                  output = "density",
                                  mixture = "P",
                                  keyfun = "exp")
#save the model
save(mod_abd_process1, file = "Model_Files/umk_mod_abd_process1.RData")

#Remove Ruggedness
mod_abd_process2 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation + Annual.Cover, 
                              phiformula = ~1,
                              pformula = ~ Observer.ID,
                              data = umf,
                              output = "density",
                              mixture = "P",
                              keyfun = "exp")
#save the model
save(mod_abd_process2, file = "Model_Files/umk_mod_abd_process2.RData")

#Remove annual cover
mod_abd_process3 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation + ln.TRI, 
                              phiformula = ~1,
                              pformula = ~ Observer.ID,
                              data = umf,
                              output = "density",
                              mixture = "P",
                              keyfun = "exp")
#save the model
save(mod_abd_process3, file = "Model_Files/umk_mod_abd_process3.RData")


#Remove Ruggedness and annual cover
mod_abd_process4 <- gdistsamp(lambdaformula = ~ ln.Shrub.Cover + ln.Bare.Ground.Cover + Precipitation, 
                              phiformula = ~1,
                              pformula = ~ Observer.ID,
                              data = umf,
                              output = "density",
                              mixture = "P",
                              keyfun = "exp")
#save the model
save(mod_abd_process4, file = "Model_Files/umk_mod_abd_process4.RData")

# Summaries and diagnostics on process model 1condidate models #################################

#Load the candidate models
load("Model_Files/umk_mod_abd_shrub.RData")
load("Model_Files/umk_mod_abd_process1.RData")
load("Model_Files/umk_mod_abd_process2.RData")
load("Model_Files/umk_mod_abd_process3.RData")
load("Model_Files/umk_mod_abd_process4.RData")

#Model summaries
summary(mod_abd_shrub)
summary(mod_abd_process1)
summary(mod_abd_process2)
summary(mod_abd_process3)
summary(mod_abd_process4)

# Combine all five candidate models
modlist_abd = list(mod_null = mod_abd_shrub,
                   mod_process1 = mod_abd_process1,
                   mod_process2 = mod_abd_process2,
                   mode_process3 = mod_abd_process3,
                   mod_process4 = mod_abd_process4)

# Check to confirm that all candidate models converged
sapply(modlist_abd, checkConv)

# Check the condition number from each model's Hessian matrix to see if they are overparameterized
sapply(modlist_abd, extractCN)

# Check the standard errors of thecandidate models
lapply(modlist_abd, checkParms)

# Compare goodness of fit between two models 
anovaOD(mod.simple = mod_abd_shrub,
                      mod.complex = mod_abd_process1)

# Compare AIC scores among all candidate models
aictab(modlist_abd)
