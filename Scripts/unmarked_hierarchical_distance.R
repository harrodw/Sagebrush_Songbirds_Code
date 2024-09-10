#Hierarchical Distance Sampling using the Unmarked package
#Will Harrod
#Utah State University
#Creadted 09/08/2024

# 0) Clear environments and load packages ######################################
rm(list = ls())
library(tidyverse)
library(unmarked)
library(AICcmodavg)

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
         UTM.X, UTM.Y)
#...and view 
glimpse(covs_tbl)

#make sure this is the same as the actual number of points surveyed
nrow(covs_tbl) == length(unique(covs_tbl$Visit.ID))
# good good

# combine these with the covariates
covs_dat <- covs_tbl %>% 
  left_join(covs, by = c("Full.Point.ID"))
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
  mutate(Area = pi * trunc_dist^2 * 0.0001, # calculate area
         #Scalle and factoize covariates
         Shrub.Cover.scl = scale(Shrub.Cover)[,1],
         Bare.Ground.Cover.scl = scale(Shrub.Cover)[,1],
         Annual.Cover.scl = scale(Annual.Cover)[,1],
         TRI.scl = scale(TRI)[,1],
         Precipitation.scl = scale(Precipitation)[,1],
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
mod_half_norm <- gdistsamp(lambdaformula = ~1, 
                           phiformula = ~1,
                           pformula = ~1,
                           data = umf,
                           output = "density",
                           mixture = "P",
                           keyfun = "halfnorm")
#hazard rate
mod_hazard <- gdistsamp(lambdaformula = ~1, 
                        phiformula = ~1,
                        pformula = ~1,
                        data = umf,
                        output="density",
                        mixture="P",
                        keyfun = "hazard")
#exponential
mod_exp <- gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1,
                     pformula = ~1,
                     data = umf,
                     output="density",
                     mixture="P",
                     keyfun = "exp")

#compare models using AIC
modlist_key <- list(mod.half.norm = mod_half_norm,
                 mod.hazard = mod_hazard,
                 mod.exp = mod_exp)
aictab(modlist_key)
# The exponential key function seams to perform best

# 2b) Detection level covariate fitting #######################################

# Null model
p_null <- gdistsamp(lambdaformula = ~1, 
                    phiformula = ~1, 
                    pformula = ~1, 
                    data = umf, 
                    output = "density",
                    mixture = "P",
                    keyfun = "exp")
# Observer model
p_obs <- gdistsamp(lambdaformula = ~1, 
                   phiformula = ~1,
                   pformula = ~ Observer.ID.fct, 
                   data = umf, 
                   output="density", 
                   mixture="P", 
                   keyfun = "exp")
# Time after sunrise model
p_mas <- gdistsamp(lambdaformula = ~1, 
                   phiformula = ~1, 
                   pformula =  ~MAS.scl, 
                   data = umf, 
                   output = "density", 
                   mixture = "P", 
                   keyfun = "exp")
#Date model
p_date <- gdistsamp(lambdaformula = ~1, 
                    phiformula = ~1,
                    pformula =  ~Ord.Date.scl, 
                    data = umf, 
                    output="density", 
                    mixture = "P", 
                    keyfun = "exp")

#compare models using AIC
modlist_dct_cov = list(p.null = p_null, 
                       p.obs = p_obs, 
                       p.mas = p_mas, 
                       p.date = p_date)
aictab(modlist_dct_cov)
# It doesn't seem like any models outperform the null

# 2c) abundance level covariate fitting #######################
