######################################################################################
# Sagebrush songbird community composition as a responce to fire
# Will Harrod
# Utah State University 
# Created 10/29/2024
########################################################################################

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(vegan)

# 1.0) ################################################################################

# Add the data 
sobs <- read.csv("Data/Outputs/sobs_data.csv") %>%
    dplyr::select(-X) %>%
    tibble()
# View
glimpse(sobs)

#