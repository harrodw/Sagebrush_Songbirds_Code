################################################################################
# Will Harrod
# Modeling shrub cover as a function of fire charactersitics 
# Created 02/10/2025
################################################################################

# 0) Prep ######################################################################

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)

# 1) Data Prep #################################################################

# 1.1) Add in exisitng data layers #############################################################

# Primary file path for rasters
ras_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Set a coordinate reference system (UTM Zone 12N)
target_crs <- "EPSG:26912"

# The study region
study_region <- st_read(paste0(ras_path, "Study_Region.shp")) %>% 
  st_transform(target_crs) 

# Add in the fire perimeters
fire_perms <- st_read(paste0(ras_path, "fire_perimeters.shp")) %>% 
  mutate(Ig.Year = as.numeric(str_extract(Ig_Date, "\\d{4}"))) %>% 
  dplyr::select(geometry, Ig.Year) %>% 
  st_transform(target_crs) %>% 
  st_intersection(study_region)

# Add in raster layers
shrub_cvr_rast <- rast(paste0(ras_path, "shrub_cvr.tif"))
pfg_cvr_rast <- rast(paste0(ras_path, "pfg_cvr.tif"))
afg_cvr_rast <- rast(paste0(ras_path, "afg_cvr.tif"))
elevation_rast <- rast(paste0(ras_path, "elevation.tif"))
tri_rast <- rast(paste0(ras_path, "tri.tif"))
aspect_rast <- rast(paste0(ras_path, "aspect.tif"))
trees_rast <- rast(paste0(ras_path, "tree_patches.tif"))

# 1.2) Prepare a fishnet grid to extract covariates #############################################################

# Set a rectangle size (meters)
cell_size <- 1000 

# Build a fishnet grid
fishnet <- st_make_grid(x = study_region,
                        cellsize = cell_size,
                        crs = target_crs,
                        square = TRUE) %>% 
  # Only fully inside fire perimeters
  st_intersection(fire_perms) %>%
  # Switch to an sf object
  st_as_sf() 

# Calculate Area
fishnet$Area = as.numeric(st_area(fishnet))
  
# Switch area to n

# # Remove the cells that are only partially in the fires
full_cells <- fishnet[which(fishnet$Area >= 900000),]

# Tmap mode
tmap_mode("view")

# View
tm_shape(study_region) +
  tm_polygons() +
  tm_shape(fire_perms) +
  tm_polygons(alpha = 0.3, col = "red3") +
  tm_shape(full_cells) +
  tm_polygons(alpha = 0.1, col = "gray87")

# Create a data frame or tibble from the fishnet
burn_dat <- tibble(full_cells) %>% 
  mutate(
    # Temporarty storage object covariates
    rdnbr.tmp = NA,
    # Permanent covariates
    rdnbr = 0,
    Fire.Year = 1800)

# 2) Extract covariates #############################################################

# 2.1) # Extract constant covariates ################################################

# Extract Shrub cover
mean_shrub <-  terra::extract(x = shrub_cvr_rast,
                                  y = full_cells,
                                  fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$Shrub.Cover <- mean_shrub[, 2]

# Extract perennial cover
mean_pfg <-  terra::extract(x = pfg_cvr_rast,
                              y = full_cells,
                              fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$PFG.Cover <- mean_pfg[, 2]

# Extract annual grass cover
mean_afg <-  terra::extract(x = afg_cvr_rast,
                            y = full_cells,
                            fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$AFG.Cover <- mean_afg[, 2]

# Extract Elevation 
mean_elevation <- terra::extract(x = elevation_rast,
                                  y = full_cells,
                                  fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$Elevation <- mean_elevation[, 2]

# Extract Ruggedness
mean_tri <- terra::extract(x = tri_rast,
                                  y = full_cells,
                                  fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$TRI <- mean_tri[, 2]

# Extract Aspect 
mode_aspect <- terra::extract(x = aspect_rast,
                                  y = full_cells,
                                  fun = function(x) modal(x, na.rm = TRUE)) 
burn_dat$Aspect <- mode_aspect[, 2]

# Extract tree cover
trees_count <- terra::extract(x = trees_rast,
                             y = full_cells,
                             fun = function(x) sum(x, na.rm = TRUE)) 
burn_dat$Trees.Count <- trees_count[, 2]

# View the data
glimpse(burn_dat)

# 2.2) Extract burn covariates ######################################################

# File path for burn sevarity data
mtbs_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\mtbs_cleaned\\"

# Define which years for which I have fire data
fire_years_df <- read.csv(paste0(mtbs_path, "fire_years.csv")) %>% 
  tibble() %>% 
  dplyr::select(-X) %>% 
  dplyr::rename(Year = "x") %>% 
  arrange(Year)

# Transform to a vector
fire_years <- fire_years_df$Year
#...and view
print(fire_years)

#Define the number of years to loop through
nyears <- length(fire_years)
#...and view
print(nyears)

# Loop through each year to extract burn severity and number of fires for each cell in the fishnet
for(i in 1:(nyears-1)){ # Skip 2022. For some reason it doesn't work
  # run a specific year
  # i <- which(fire_years == 1999)
  
  # define the year for the current itteration
  year <- fire_years[i]
  
  # read in rdnbr for that year
  rdnbr_ras <- rast(paste0(mtbs_path, "rdnbr_", year, ".tif")) 
  
  # replace negative values
  rdnbr_ras[rdnbr_ras < 0] <- NA
  
  # reset a temporary points object for the mean rdnbr at the 125m scale
  rdnbr.tmp <- terra::extract(x = rdnbr_ras, y = full_cells, fun = function(x) mean(x, na.rm = TRUE))
  burn_dat$rdnbr.tmp <- rdnbr.tmp[,2]
  
  # Pull out the values where there were fires at the point for that year
  burn_dat <- burn_dat %>% 
    mutate(rdnbr = case_when(!is.na(rdnbr.tmp) ~ rdnbr.tmp,
                                  TRUE ~ rdnbr),
           Fire.Year = case_when(!is.na(rdnbr.tmp) ~ year,
                                 TRUE ~ Fire.Year))
  
  # finished with one itteration
  message(paste("Extracted burn sevarity for year:", year))
  
} # end the loop through years

# Pull out the coords
boxes <- st_as_sf(burn_dat$x)

# Calculate Centroids
centroids <- st_centroid(boxes)

# Convert to character string
coords <- tibble(Coords = as.character(centroids$x)) %>%
mutate(Coords = str_remove_all(Coords, "c")) %>%
mutate(Coords = str_remove_all(Coords, ",")) %>%
mutate(Coords = str_remove_all(Coords, "[(]")) %>%
mutate(Coords = str_remove_all(Coords, "[])]")) %>%
mutate(X = as.numeric(str_split_i(Coords, " ", i = 1)),
       Y = as.numeric(str_split_i(Coords, " ", i = 2)))

# and View
coords

# Final formatting
burn_dat_final <- burn_dat %>% 
  # Add in the split coords
  mutate(X = coords$X, Y = coords$Y) %>%
  # Remove the temporary attributes
  dplyr::select(-rdnbr.tmp) %>%
  # Remove Grids with too much tree cover
  filter(Trees.Count < 300) %>% 
  # Subset some number of rows to remove spatial autocorrelation (Play around with n)
  slice_sample(n = 300) %>% 
  # Remove the extra column
  select(-x) %>% 
  # Add in fire year for the grid that does have info
  mutate(Fire.Year = case_when(Fire.Year == 1800 ~ 2017,
                               TRUE ~ Fire.Year))
  
# View the changes
glimpse(burn_dat_final)

# See where the points are 
burn_dat_final %>% 
  ggplot(aes(x = X, y = Y, col = Fire.Year)) +
  geom_point()

# Save the data
write.csv(burn_dat_final, "Data\\Outputs\\Fishnet_Grid_Covs.csv")

# 2) Modeling #################################################################

# 2.1) Prep and diagnostics ###############################################################
# Start here once the data have been extracted -------------------------------------------

# Add packages
library(tidyverse)
library(ggcorrplot)
library(DHARMa)
library(glmmTMB)

# Read the data back in
burn_dat_clean <- read.csv("Data\\Outputs\\Fishnet_Grid_Covs.csv") %>% 
  select(-X.1)
# View
glimpse(burn_dat_clean)

# Select the covariates to compare
model_covs <- burn_dat_clean %>% select(rdnbr, Fire.Year, AFG.Cover, PFG.Cover, Elevation, TRI, X, Y)

# Make a correlation matrix
cor_mat <- cor(model_covs)
cor_mat

# Find p-value correlations
p_mat <- cor_pmat(cor_mat)
p_mat

# Plot correlations
ggcorrplot(cor_mat, 
           title = "Correlation Matrix for Pre-Fire Vegetation Data", 
           lab = TRUE, 
           lab_size = 4,    
           tl.cex = 10,
           p.mat = p_mat, 
           type = "lower",
           method = "square",
           sig.level = 0.05,
           colors = c("red", "white", "blue")) + 
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text.y = element_text(angle = 0, hjust = 1)  
  )

# Model can include: Fire Year, Burn Sevarity, Elevation, X, and Y

# historgram of fire year
burn_dat_clean %>% 
  mutate(Years.Since.Fire = 2023 - Fire.Year) %>% 
  ggplot(aes(x = Years.Since.Fire)) +
  geom_histogram()

# historgram of elevation
burn_dat_clean %>% 
  ggplot(aes(x = Elevation)) +
  geom_histogram()

# historgram of burn sevarity
burn_dat_clean %>% 
  ggplot(aes(x = rdnbr)) +
  geom_histogram()

# Square and scale covariates
burn_dat_model <- burn_dat_clean %>% 
  mutate(Years.Since.Fire = 2023 - Fire.Year) %>% 
  select(Shrub.Cover, PFG.Cover, AFG.Cover, Years.Since.Fire, Elevation, rdnbr, X, Y) %>% 
  mutate(Years.Since.Fire2 = Years.Since.Fire^2,
         Elevation2 = Elevation^2) %>% 
  mutate(Elevation = scale(Elevation)[,1],
         Elevation2 = scale(Elevation2)[,1],
         Years.Since.Fire = scale(Years.Since.Fire)[,1],
         Years.Since.Fire2 = scale(Years.Since.Fire2)[,1],
         rdnbr = scale(rdnbr)[,1],
         X = scale(X)[,1],
         Y = scale(Y)[,1]) 

# View
glimpse(burn_dat_model)
  
# 2.2) Candidate models for shrub cover #############################################################

# View the data again
glimpse(burn_dat_model)

# Gaussian model with interactions
shrub_model_out_full <- glmmTMB(data = burn_dat_model,
                                formula = Shrub.Cover ~ Years.Since.Fire +          # Effect of time since fire
                                                        Years.Since.Fire2 +         # Effect of time since fire (Quadratic)
                                                        Elevation +                 # Effect of elevation
                                                     Elevation2 +                   # Effect of elevation (Quadratic)
                                                        rdnbr +                     # Effect of RdNBR burn severity
                                                     Elevation * Years.Since.Fire + # Effect of interaction between elevation and time
                                                     rdnbr * Years.Since.Fire +     # Effect of interaction between burn sevarity and time
                                                     X +                            # Explain extra variation with X coord
                                                     Y)                             # Explain extra variation with X coord

# Remove the burn sevarity time interaction
shrub_model_out2 <- glmmTMB(data = burn_dat_model,
                                  formula = Shrub.Cover ~ Years.Since.Fire +  # Effect of time since fire
                                           Years.Since.Fire2 +                # Effect of time since fire (Quadratic)
                                           Elevation +                        # Effect of elevation
                                           Elevation2 +                       # Effect of elevation (Quadratic)
                                           rdnbr +                            # Effect of RdNBR burn severity
                                           Elevation * Years.Since.Fire +     # Effect of interaction between elevation and time
                                           X +                                # Explain extra variation with X coord
                                           Y)                                 # Explain extra variation with X coords


# Remove X and Y
shrub_model_out3 <- glmmTMB(data = burn_dat_model,
                           formula = Shrub.Cover ~ Years.Since.Fire +            # Effect of time since fire
                                                   Years.Since.Fire2 +           # Effect of time since fire (Quadratic)
                                                   Elevation +                   # Effect of elevation
                                                   Elevation2 +                  # Effect of elevation (Quadratic)
                                                   rdnbr +                       # Effect of RdNBR burn severity
                                                   Elevation * Years.Since.Fire) # Effect of interaction between elevation and time

# Remove Quadratic effect from elevation
shrub_model_out4 <- glmmTMB(data = burn_dat_model,
                            formula = Shrub.Cover ~ Years.Since.Fire +            # Effect of time since fire
                              Years.Since.Fire2 +           # Effect of time since fire (Quadratic)
                              Elevation +                   # Effect of elevation
                              rdnbr +                       # Effect of RdNBR burn severity
                              Elevation * Years.Since.Fire) # Effect of interaction between elevation and time

# Remove quadratic effect from time since fire
shrub_model_out5 <- glmmTMB(data = burn_dat_model,
                            formula = Shrub.Cover ~ Years.Since.Fire + # Effect of time since fire
                              Elevation +                              # Effect of elevation
                              Elevation2 +                             # Effect of elevation (Quadratic)
                              rdnbr +                                  # Effect of RdNBR burn severity
                              Elevation * Years.Since.Fire)            # Effect of interaction between elevation and time

# Remove quadratic effect from time since fire and remove quadratic elevation
shrub_model_out6 <- glmmTMB(data = burn_dat_model,
                            formula = Shrub.Cover ~ Years.Since.Fire + # Effect of time since fire
                                      rdnbr +                          # Effect of RdNBR burn severity        
                                      Elevation +                      # Effect of elevation
                                      Elevation * Years.Since.Fire)    # Effect of interaction between elevation and time

# Remove RdNBR
shrub_model_out7 <- glmmTMB(data = burn_dat_model,
                             formula = Shrub.Cover ~ Years.Since.Fire + # Effect of time since fire
                               Elevation +                      # Effect of elevation
                               Elevation * Years.Since.Fire)    # Effect of interaction between elevation and time


# View model summarys
summary(shrub_model_out_full)
summary(shrub_model_out2)
summary(shrub_model_out3)
summary(shrub_model_out4)
summary(shrub_model_out5)
summary(shrub_model_out6)
summary(shrub_model_out7)

# Best model
# Try model 6
shrub_model_best <- shrub_model_out7

# Summary of the best model
summary(shrub_model_best)

# Test Dispersion
# testDispersion(shrub_model_best)

# Simulate Residuals
sim_out <- simulateResiduals(fittedModel = shrub_model_best, plot = F)

# View residuals in a plot
plot(sim_out)

# 2.3) Candidate models for shrub cover #############################################################

# View plot of PFG cover against a covariate
burn_dat_model %>% 
  ggplot(aes(x = rdnbr, y = PFG.Cover)) +
  geom_point() +
  geom_smooth(method = "lm")

# View the data again
glimpse(burn_dat_model)

# Gaussian model with interactions
pfg_model_out_full <- glmmTMB(data = burn_dat_model,
                                formula = PFG.Cover ~ Years.Since.Fire + # Effect of time since fire
                                  Years.Since.Fire2 +                  # Effect of time since fire (Quadratic)
                                  Elevation +                 # Effect of elevation
                                  Elevation2 +                   # Effect of elevation (Quadratic)
                                  rdnbr +                     # Effect of RdNBR burn severity
                                  Elevation * Years.Since.Fire + # Effect of interaction between elevation and time
                                  rdnbr * Years.Since.Fire +     # Effect of interaction between burn sevarity and time
                                  X +                            # Explain extra variation with X coord
                                  Y)                             # Explain extra variation with X coord

# Drop X and Y
pfg_model_out2<- glmmTMB(data = burn_dat_model,
                              formula = PFG.Cover ~ Years.Since.Fire +          # Effect of time since fire
                                Years.Since.Fire2 +         # Effect of time since fire (Quadratic)
                                Elevation +                 # Effect of elevation
                                Elevation2 +                   # Effect of elevation (Quadratic)
                                rdnbr +                     # Effect of RdNBR burn severity
                                Elevation * Years.Since.Fire + # Effect of interaction between elevation and time
                                rdnbr * Years.Since.Fire       # Effect of interaction between burn sevarity and time
)

# Remove quadratic terms
pfg_model_out3 <- glmmTMB(data = burn_dat_model,
                         formula = PFG.Cover ~ Years.Since.Fire +          # Effect of time since fire
                           Elevation +                 # Effect of elevation
                           rdnbr +                     # Effect of RdNBR burn severity
                           Elevation * Years.Since.Fire + # Effect of interaction between elevation and time
                           rdnbr * Years.Since.Fire       # Effect of interaction between burn sevarity and time
)

# Remove elv fyear interaction 
pfg_model_out4 <- glmmTMB(data = burn_dat_model,
                          formula = PFG.Cover ~ Years.Since.Fire +          # Effect of time since fire
                            Elevation +                 # Effect of elevation
                            rdnbr +                     # Effect of RdNBR burn severity
                            rdnbr * Years.Since.Fire       # Effect of interaction between burn sevarity and time
)

# Remove rdnbr fyear interaction 
pfg_model_out5 <- glmmTMB(data = burn_dat_model,
                          formula = PFG.Cover ~ Years.Since.Fire +          # Effect of time since fire
                            Elevation +                 # Effect of elevation
                            rdnbr                      # Effect of RdNBR burn severity
)


# View model summarys
summary(pfg_model_out_full)
summary(pfg_model_out2)
summary(pfg_model_out3)
summary(pfg_model_out4)
summary(pfg_model_out5)
# summary(pfg_model_out6)
# summary(pfg_model_out7)
# summary(pfg_model_out8)

# Best model
# Try model 4
pfg_model_best <- pfg_model_out4

# Summary of the best model
summary(pfg_model_best)

# Test Dispersion
# testDispersion(shrub_model_best)

# Simulate Residuals
pfg_sim_out <- simulateResiduals(fittedModel = shrub_model_best, plot = F)

# View residuals in a plot
plot(pfg_sim_out)

# 2.5) Plot Shrub model output #########################################################

# Extract the model coefficients
shrub_summary <- summary(shrub_model_best)
shrub_summary
shrub_coefs <- shrub_summary$coefficients$cond
shrub_coefs
shrub_coefs_tibble <- tibble(Parameter= c("Intercept", 
                                        "Years Since Fire",
                                        "Elevation (m)", 
                                        "Elevation * Years Since Fire"),
                           Value = as.numeric(shrub_coefs[,1]),
                           Std.Error = shrub_coefs[,2]) 

# and View
head(shrub_coefs_tibble, n = 10)

# Create the plot
shrub_params_plot <- shrub_coefs_tibble %>% 
  # Switch to a factor 
  mutate(Parameter = factor(Parameter, levels = c(
    "Elevation * Years Since Fire",
    "Elevation (m)", 
    "Years Since Fire", 
    "Intercept"
  )),
  CI.lb = Value - Std.Error,
  CI.ub = Value + Std.Error) %>% 
  # Open the plot
  ggplot(aes(y = Parameter)) +
  # Add points at the mean values for each parameters
  geom_point(aes(x = Value), shape = 15, size = 4, color = "navyblue") +
  # Add whiskers for mean plus standard error
  geom_linerange(aes(xmin = CI.lb, xmax = CI.ub), linewidth = 1.5, color = "navyblue") +
  # Add a vertical Line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  # Change the Labels
  labs(x = "Parameter Estimate", 
       y = "",
       title = "Effect on Percent Shrub Cover") +
  # Simple theme
  theme_classic() +
  # Edit theme
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 16)) 


# View the plot
shrub_params_plot

# Save the plot
ggsave(plot = shrub_params_plot,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\shrub_fire_output.png",
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)


# 2.5) Plot PFG model output #########################################################

# Extract the model coefficients
pfg_summary <- summary(pfg_model_best)
pfg_summary
pfg_coefs <- pfg_summary$coefficients$cond
pfg_coefs
pfg_coefs_tibble <- tibble(Parameter= c("Intercept", 
                                    "Years Since Fire",
                                    "Elevation (m)", 
                                    "RdNBR Burn Sevarity",
                                    "RdNBR * Years Since Fire"),
                       Value = as.numeric(pfg_coefs[,1]),
                       Std.Error = pfg_coefs[,2]) 

# and View
head(pfg_coefs_tibble, n = 10)

# Create the plot
pfg_params_plot <- pfg_coefs_tibble %>% 
  # Switch to a factor 
  mutate(Parameter = factor(Parameter, levels = c(
                                                  "RdNBR * Years Since Fire", 
                                                  "RdNBR Burn Sevarity",
                                                  "Elevation (m)", 
                                                  "Years Since Fire", 
                                                  "Intercept"
                                                  )),
         CI.lb = Value - Std.Error,
         CI.ub = Value + Std.Error) %>% 
  # Open the plot
  ggplot(aes(y = Parameter)) +
  # Add points at the mean values for each parameters
  geom_point(aes(x = Value), shape = 15, size = 4, color = "navyblue") +
  # Add whiskers for mean plus standard error
  geom_linerange(aes(xmin = CI.lb, xmax = CI.ub), linewidth = 1.5, color = "navyblue") +
  # Add a vertical Line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  # Change the Labels
  labs(x = "Parameter Estimate", 
       y = "",
       title = "Effect on Percent Perennial Cover") +
  # Simple theme
  theme_classic() +
  # Edit theme
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 16)) 


# View the plot
pfg_params_plot

# Save the plot
ggsave(plot = pfg_params_plot,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\pfg_fire_output.png",
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)
