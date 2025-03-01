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
target_crs <- "EPSG:32612"

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
trees_rast <- rast(paste0(ras_path, "tree_cvr.tif"))

# 1.2) Prepare a fishnet grid to extract covariates #############################################################

# Set a rectangle size (meters)
cell_size <- 1000 

# Number of cells to keep
n_cells <- 300

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

# # Remove the cells that are only partially in the fires
full_cells <- fishnet[which(fishnet$Area >= 900000),]

# Project tree cover
trees_prj <- project(x = trees_rast, y = target_crs)

# Extract tree cover
mean_trees <- terra::extract(x = trees_prj,
                             y = full_cells,
                             fun = function(x) mean(x, na.rm = TRUE)) 
full_cells$Tree.Cover <- mean_trees[, 2]


# Subset the cells
model_cells <- full_cells %>% 
  # Remove the cells with trees
  filter(Tree.Cover < 20) %>% 
  # Subset some number of rows to remove spatial autocorrelation 
  slice_sample(n = n_cells) 

# View the cells
glimpse(model_cells)

# Tmap mode
tmap_mode("view")

# View
tm_shape(study_region) +
  tm_polygons() +
  tm_shape(fire_perms) +
  tm_polygons(alpha = 0.3, col = "red3") +
  tm_shape(model_cells) +
  tm_polygons(alpha = 0.1, col = "gray87")

# Create a data frame or tibble from the fishnet
burn_dat <- tibble(model_cells) %>% 
  mutate(
    # Temporarty storage object covariates
    rdnbr.tmp = NA,
    dnbr.tmp = 0,
    # Permanent covariates
    rdnbr = 0,
    dnbr = 0,
    Fire.Year = 1800)

# Remove cells that have too many trees and then subset


# 2) Extract covariates #############################################################

# 2.1) # Extract constant covariates ################################################

# Extract Shrub cover
mean_shrub <-  terra::extract(x = shrub_cvr_rast,
                                  y = model_cells,
                                  fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$Shrub.Cover <- mean_shrub[, 2]

# Extract perennial cover
mean_pfg <-  terra::extract(x = pfg_cvr_rast,
                              y = model_cells,
                              fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$PFG.Cover <- mean_pfg[, 2]

# Extract annual grass cover
mean_afg <-  terra::extract(x = afg_cvr_rast,
                            y = model_cells,
                            fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$AFG.Cover <- mean_afg[, 2]

# Extract Elevation 
mean_elevation <- terra::extract(x = elevation_rast,
                                  y = model_cells,
                                  fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$Elevation <- mean_elevation[, 2]

# Extract Ruggedness
mean_tri <- terra::extract(x = tri_rast,
                                  y = model_cells,
                                  fun = function(x) mean(x, na.rm = TRUE)) 
burn_dat$TRI <- mean_tri[, 2]

# Extract Aspect 
mode_aspect <- terra::extract(x = aspect_rast,
                                  y = model_cells,
                                  fun = function(x) modal(x, na.rm = TRUE)) 
burn_dat$Aspect <- mode_aspect[, 2]

# 2.2) Extract burn covariates ######################################################

# File path for brn Severity data
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
  # read in dnbr for that year
  dnbr_ras <- rast(paste0(mtbs_path, "dnbr_", year, ".tif")) 
  
  # replace negative values
  rdnbr_ras[rdnbr_ras < 0] <- NA
  dnbr_ras[dnbr_ras < 0] <- NA
  
  # Project raster
  rdnbr_prj <- project(rdnbr_ras, y = target_crs)
  dnbr_prj <- project(dnbr_ras, y = target_crs)
  
  # reset a temporary points object for the mean rdnbr at the 125m scale
  rdnbr.tmp <- terra::extract(x = rdnbr_prj, y = model_cells, fun = function(x) mean(x, na.rm = TRUE))
  burn_dat$rdnbr.tmp <- rdnbr.tmp[,2]
  
  # reset a temporary points object for the mean dnbr at the 125m scale
  dnbr.tmp <- terra::extract(x = dnbr_prj, y = model_cells, fun = function(x) mean(x, na.rm = TRUE))
  burn_dat$dnbr.tmp <- dnbr.tmp[,2]
  
  
  # Pull out the values where there were fires at the point for that year
  burn_dat <- burn_dat %>% 
    mutate(rdnbr = case_when(!is.na(rdnbr.tmp) ~ rdnbr.tmp,
                                  TRUE ~ rdnbr),
           dnbr = case_when(!is.na(dnbr.tmp) ~ dnbr.tmp,
                             TRUE ~ dnbr),
           Fire.Year = case_when(!is.na(rdnbr.tmp) ~ year,
                                 TRUE ~ Fire.Year))
  
  # finished with one iteration
  message(paste("Extracted burn Severity for year:", year))
  
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

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(ggcorrplot)
library(gridExtra)
library(DHARMa)
library(glmmTMB)
library(AICcmodavg)
library(cowplot)

# Read the data back in
burn_dat_clean <- read.csv("Data\\Outputs\\Fishnet_Grid_Covs.csv") %>% 
  select(-X.1)
# View
glimpse(burn_dat_clean)

# Add covariates
covs <- tibble(read.csv("Data/Outputs/grid_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble()
# View
glimpse(covs)

covs %>% 
  filter(Grid.Type == "B") %>% 
  arrange(rdnbr.125m) %>% 
  select(Grid.ID, rdnbr.125m) %>% 
  print(n = Inf)

# Make a burn covariates object
burn_covs <- covs %>% 
  # Bunred vs unburned
  filter(Grid.Type == "B") %>% 
  # Binary high vs low elevation
  mutate(High.Elevation = case_when(Elevation.125m >= 1800 ~ 1,
                                    Elevation.125m < 1800 ~ 0),
         # Years since fire
         Years.Since.Fire = 2023 - Fire.Year) %>% 
  # Don't need all the columns
  select(Grid.ID, Shrub.Cover.125m, Perennial.Cover.125m, 
         Years.Since.Fire, rdnbr.125m, High.Elevation) %>% 
  rename(Shrub.Cover = Shrub.Cover.125m,
         PFG.Cover = Perennial.Cover.125m,
         rdnbr = rdnbr.125m)
# View
glimpse(burn_covs)

# Select the covariates to compare
model_covs <- burn_dat_clean %>% select(Shrub.Cover, rdnbr, Fire.Year, AFG.Cover, PFG.Cover, Elevation, TRI, X, Y)

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

# Model can include: Fire Year, Burn Severity, Elevation, X, and Y

# historgram of fire year
burn_dat_clean %>% 
  mutate(Years.Since.Fire = 2023 - Fire.Year) %>% 
  ggplot(aes(x = Years.Since.Fire)) +
  geom_histogram()

# historgram of elevation
burn_dat_clean %>% 
  ggplot(aes(x = Elevation)) +
  geom_histogram()

# historgram of burn Severity
burn_dat_clean %>% 
  ggplot(aes(x = rdnbr)) +
  geom_histogram()

# Square and scale covariates
burn_dat_model <- burn_dat_clean %>% 
  mutate(Years.Since.Fire = 2023 - Fire.Year) %>% 
  select(Shrub.Cover, PFG.Cover, AFG.Cover, Years.Since.Fire, Elevation, rdnbr, dnbr, X, Y) %>% 
  # Remove outlier rdnbr and fir year  
  filter(rdnbr < 2200 & Years.Since.Fire < 30) %>% 
  mutate(Years.Since.Fire2 = Years.Since.Fire^2,
         Elevation2 = Elevation^2,
         # Binary high vs low elevation
         High.Elevation = case_when(Elevation >= 1800 ~ 1,
                                    Elevation < 1800 ~ 0)
         ) %>% 
  mutate(Elevation.scl = scale(Elevation)[,1],
         Elevation2.scl = scale(Elevation2)[,1],
         Years.Since.Fire.scl = scale(Years.Since.Fire)[,1],
         Years.Since.Fire2.scl = scale(Years.Since.Fire2)[,1],
         dnbr.scl = scale(dnbr)[,1],
         rdnbr.scl = scale(rdnbr)[,1],
         X.scl = scale(X)[,1],
         Y.scl = scale(Y)[,1]) 

# View
glimpse(burn_dat_model)

#############################################################################################################
# 2.3) R-squared plots ######################################################################################
#############################################################################################################

# Time since fire shrub cover R-squared ##########################################################################

# Model shrub cover by time since fire at low elevations
fyear_shrub_model_low <- burn_covs %>%
  filter(High.Elevation == 0) %>%
  lm(formula = Shrub.Cover ~ Years.Since.Fire)

# View time since fire model output
sum_fyear_shrub_low <- summary(fyear_shrub_model_low)
sum_fyear_shrub_low

# Pull out the r squared
r_sq_fyear_shrub_low <- round(sum_fyear_shrub_low$r.squared, 3)
r_sq_fyear_shrub_low

# Model shrub cover by time since fire at high elevations
fyear_shrub_model_high <- burn_covs %>%
  filter(High.Elevation == 1) %>% 
  lm(formula = Shrub.Cover ~ Years.Since.Fire)

# View time since fire model output
sum_rndbr_shrub_high <- summary(fyear_shrub_model_high)
sum_rndbr_shrub_high

# Pull out the r squared
r_sq_fyear_shrub_high <- round(sum_rndbr_shrub_high$r.squared, 3)
r_sq_fyear_shrub_high

# Plot shrub cover against time since fire
fyear_shrub_plot <- burn_covs %>% 
  mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
                                      High.Elevation == 0 ~ "Below 1800m"))) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Shrub.Cover, col = Elevation, fill = Elevation)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic() +
  # Customize scales and labels
  scale_color_manual(values = c("Above 1800m" = "orange2",
                                "Below 1800m" =  "red3")) +
  scale_fill_manual(values = c("Above 1800m" = "orange2",
                               "Below 1800m" =  "red3")) +
  labs(x = "Years Since Fire",
       y = "Shrub Cover",
       title = "(A)") +
  # Custimize text
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  ylim(0, 50) +
  # Add the R squared
  annotate(geom="text", x = 6.7, y = 49, label = paste("low-elv R^2 =", r_sq_fyear_shrub_low),
           color = "black", size = 4.3) +
  annotate(geom = "text", x = 7, y = 43, label = paste("high-elv R^2 =", r_sq_fyear_shrub_high),
           color="black", size = 4.3) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# View the plot
fyear_shrub_plot

# Burn Severity R-squared ###############################################################################

# Model shrub cover by burn Severity at low elevations
rdnbr_shrub_model_low <- burn_covs %>%
  filter(High.Elevation == 0) %>%
  lm(formula = Shrub.Cover ~ rdnbr)

# View burn Severity model output
sum_rndbr_shrub_low <- summary(rdnbr_shrub_model_low)
sum_rndbr_shrub_low

# Pull out the r squared
r_sq_rdnbr_shrub_low <- round(sum_rndbr_shrub_low$r.squared, 3)
r_sq_rdnbr_shrub_low

# Model shrub cover by burn Severity at high elevations
rdnbr_shrub_model_high <- burn_covs %>%
  filter(High.Elevation == 1) %>% 
  lm(formula = Shrub.Cover ~ rdnbr)

# View burn Severity model output
sum_rndbr_shrub_high <- summary(rdnbr_shrub_model_high)
sum_rndbr_shrub_high

# Pull out the r squared
r_sq_rdnbr_shrub_high <- round(sum_rndbr_shrub_high$r.squared, 3)
r_sq_rdnbr_shrub_high

# Plot shrub cover against burn Severity
rdnbr_shrub_plot <- burn_covs %>% 
  mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
                                      High.Elevation == 0 ~ "Below 1800m"))) %>% 
  ggplot(aes(x = rdnbr., y = Shrub.Cover, col = Elevation, fill = Elevation)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic() +
  # Customize scales and labels
  scale_color_manual(values = c("Above 1800m" = "orange2",
                                "Below 1800m" =  "red3")) +
  scale_fill_manual(values = c("Above 1800m" = "orange2",
                               "Below 1800m" =  "red3")) +
  labs(x = "RdNBR Burn Severity",
       y = "Shrub Cover",
       title = "(C)") +
  # Customize text
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  ylim(0, 50) +
  # Add the R squared
  annotate(geom="text", x = 730, y = 49, label = paste("low-elv R^2 =", r_sq_rdnbr_shrub_low),
           color = "black", size = 4.3) +
  annotate(geom = "text", x = 740, y = 43, label = paste("high-elv R^2 =", r_sq_rdnbr_shrub_high),
           color="black", size = 4.3) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# View the plot
rdnbr_shrub_plot

# Combine the plots --------------------------------------------------------------------------

# # Plot for a template legend
# legend_plot <- burn_covs %>% 
#   mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
#                                       High.Elevation == 0 ~ "Below 1800m"))) %>% 
#   ggplot(aes(x = rdnbr, y = Shrub.Cover, col = Elevation, fill = Elevation)) +
#   geom_smooth(method = "lm") +
#   geom_jitter() +
#   theme_classic() +
#   # Customize scales and labels
#   scale_color_manual(values = c("Above 1800m" = "orange2",
#                                 "Below 1800m" =  "red3")) +
#   scale_fill_manual(values = c("Above 1800m" = "orange2",
#                                "Below 1800m" =  "red3")) +
#   theme(
#     plot.title = element_text(size = 20),
#     axis.title.y = element_text(size = 14),
#     axis.text.y = element_text(size = 16),
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 16),
#     legend.text = element_text(size = 16),
#     legend.position = "bottom", 
#     legend.title = element_blank())
# 
# # Extract the legend
# legend <- ggpubr::get_legend(legend_plot)

# # Combine the plots
# both_shrub_plots <- grid.arrange(fyear_shrub_plot, rdnbr_shrub_plot,
#                                  nrow = 1, ncol = 2)
# 
# # Add the legend
# both_shrub_plots_legend <- grid.arrange(both_shrub_plots, legend,
#                                         nrow = 2, heights = c(0.9, 0.1))
# 
# # Save the plot
# ggsave(plot = both_shrub_plots_legend,
#        file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\shrub_cvr_r_sq_plot.png",
#        width = 300,
#        height = 120,
#        units = "mm",
#        dpi = 300)

# Time since fire perennial cover R-squared ##########################################################################

# Model perennial cover by time since fire at low elevations
fyear_pfg_model_low <- burn_covs %>%
  filter(High.Elevation == 0) %>%
  lm(formula = PFG.Cover ~ Years.Since.Fire)

# View time since fire model output
sum_fyear_pfg_low <- summary(fyear_pfg_model_low)
sum_fyear_pfg_low

# Pull out the r squared
r_sq_fyear_pfg_low <- round(sum_rndbr_pfg_low$r.squared, 3)
r_sq_fyear_pfg_low

# Model pfg cover by time since fire at high elevations
fyear_pfg_model_high <- burn_covs %>%
  filter(High.Elevation == 1) %>% 
  lm(formula = PFG.Cover ~ Years.Since.Fire)

# View time since fire model output
sum_fyear_pfg_high <- summary(fyear_pfg_model_high)
sum_fyear_pfg_high

# Pull out the r squared
r_sq_fyear_pfg_high <- round(sum_fyear_pfg_high$r.squared, 3)
r_sq_fyear_pfg_high

# Plot pfg cover against time since fire
fyear_pfg_plot <- burn_covs %>% 
  mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
                                      High.Elevation == 0 ~ "Below 1800m"))) %>% 
  ggplot(aes(x = Years.Since.Fire, y = PFG.Cover, col = Elevation, fill = Elevation)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic() +
  # Customize scales and labels
  scale_color_manual(values = c("Above 1800m" = "orange2",
                                "Below 1800m" =  "red3")) +
  scale_fill_manual(values = c("Above 1800m" = "orange2",
                               "Below 1800m" =  "red3")) +
  labs(x = "Years Since Fire",
       y = "Perennial Cover",
       title = "(B)") + 
  # Custimize text
  theme(
    plot.title = element_text(size = 20),
    axis.title.y =  element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  ylim(0, 50) +
  # Add the R squared
  annotate(geom = "text", x = 19.8, y = 49, label = paste("low-elv R^2 =", r_sq_fyear_pfg_low),
           color = "black", size = 4.3) +
  annotate(geom = "text", x = 20, y = 45, label = paste("high-elv R^2 =", r_sq_fyear_pfg_high),
           color = "black", size = 4.3) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# View the plot
fyear_pfg_plot

# Burn Severity R-squared ###############################################################################

# Model pfg cover by burn Severity at low elevations
rdnbr_pfg_model_low <- burn_covs %>%
  filter(High.Elevation == 0) %>%
  lm(formula = PFG.Cover ~ rdnbr)

# View burn Severity model output
sum_rndbr_pfg_low <- summary(rdnbr_pfg_model_low)
sum_rndbr_pfg_low

# Pull out the r squared
r_sq_rdnbr_pfg_low <- round(sum_rndbr_pfg_low$r.squared, 3)
r_sq_rdnbr_pfg_low

# Model pfg cover by burn Severity at high elevations
rdnbr_pfg_model_high <- burn_covs %>%
  filter(High.Elevation == 1) %>% 
  lm(formula = PFG.Cover ~ rdnbr)

# View burn Severity model output
sum_rndbr_pfg_high <- summary(rdnbr_pfg_model_high)
sum_rndbr_pfg_high

# Pull out the r squared
r_sq_rdnbr_pfg_high <- round(sum_rndbr_pfg_high$r.squared, 3)
r_sq_rdnbr_pfg_high

# Plot pfg cover against burn Severity
rdnbr_pfg_plot <- burn_covs %>% 
  mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
                                      High.Elevation == 0 ~ "Below 1800m"))) %>% 
  ggplot(aes(x = rdnbr, y = PFG.Cover, col = Elevation, fill = Elevation)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic() +
  # Customize scales and labels
  scale_color_manual(values = c("Above 1800m" = "orange2",
                                "Below 1800m" =  "red3")) +
  scale_fill_manual(values = c("Above 1800m" = "orange2",
                               "Below 1800m" =  "red3")) +
  labs(x = "RdNBR Burn Severity",
       y = "Perennial Cover",
       title = "(D)") +
  # Customize text
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  ylim(0, 50) +
  # Add the R squared
  annotate(geom="text", x = 2050, y = 49, label = paste("low-elv R^2 =", r_sq_rdnbr_pfg_low),
           color = "black", size = 4.3) +
  annotate(geom = "text", x = 2070, y = 45, label = paste("high-elv R^2 =", r_sq_rdnbr_pfg_high),
           color="black", size = 4.3) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# View the plot
rdnbr_pfg_plot

# Combine the plots --------------------------------------------------------------------------

# Plot for a template legend
legend_plot <- burn_dat_model %>% 
  mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
                                      High.Elevation == 0 ~ "Below 1800m"))) %>% 
  ggplot(aes(x = rdnbr, y = PFG.Cover, col = Elevation, fill = Elevation)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic() +
  # Customize scales and labels
  scale_color_manual(values = c("Above 1800m" = "orange2",
                                "Below 1800m" =  "red3")) +
  scale_fill_manual(values = c("Above 1800m" = "orange2",
                               "Below 1800m" =  "red3")) +
  theme(
    plot.title = element_text(size = 22),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "bottom", 
    legend.title = element_blank())

# Extract the legend
legend <- ggpubr::get_legend(legend_plot)

# # Combine the plots
# both_pfg_plots <- grid.arrange(fyear_pfg_plot, rdnbr_pfg_plot,
#                                  nrow = 1, ncol = 2)

# # Add the legend
# both_pfg_plots_legend <- grid.arrange(both_pfg_plots, legend,
#                                         nrow = 2, heights = c(0.9, 0.1))
# 
# # Save the plot
# ggsave(plot = both_pfg_plots_legend,
#        file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\pfg_cvr_r_sq_plot.png",
#        width = 300,
#        height = 120,
#        units = "mm",
#        dpi = 300)

# Combine all four plots############################################################


# Arrange plots in a 2x2 grid
combined_cont_veg_plots <- plot_grid(
  fyear_shrub_plot, fyear_pfg_plot,
  rdnbr_shrub_plot, rdnbr_pfg_plot,
  nrow = 2, ncol = 2, align = "hv"
)
combined_cont_veg_plots

# # Add shared axis labels 
# cont_veg_plots_labs <- ggdraw(combined_cont_veg_plots) +
#   draw_label("Shrub Cover", x = 0, y = 0.7, hjust = 0.5, vjust = 0.5, angle = 90) +
#   draw_label("Perennial Cover", x = 0.5, y = 0.7, hjust = 1, vjust = 0.5, angle = 90) + 
#   draw_label("Years Since Fire", x = 0.4, y = 0.5, hjust = 0.5, vjust = 1.5) + 
#   draw_label("RdNBR Burn Severity", x = 0.4, y = 0, hjust = 0.5, vjust = 0.5)
# cont_veg_plots_labs

# Display the final plot
cont_veg_plots_labs

# # Add the legend
four_cont_plots_legend <- grid.arrange(combined_cont_veg_plots, legend,
                                        nrow = 2, heights = c(0.9, 0.1))

# # Save the plot
ggsave(plot = four_cont_plots_legend,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\veg_r_sq_plots_all.png",
       width = 250,
       height = 200,
       units = "mm",
       dpi = 300)


# Shrub cover by treatment ############################################)

# Run a model for shrub cover
shrub_burn_model <- covs %>% 
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation.125m < 1800 ~ 1,
                                    Grid.Type == "R" & Elevation.125m >= 1800 ~ 2,
                                    Grid.Type == "B" & Elevation.125m < 1800 ~ 3,
                                    Grid.Type == "B" & Elevation.125m >= 1800 ~ 4))) %>% 
  lm(formula = Shrub.Cover.125m ~ Burn.Elevation)

# View model summary
shrub_burn_model_sum <- summary(shrub_burn_model)
shrub_burn_model_sum

# Extract the R-Squared 
shrub_burn_model_r_sq <- round(shrub_burn_model_sum$r.squared, 4)
shrub_burn_model_r_sq

# Plot shrub cover against burn type
shrub_trt_plot <- covs %>%
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation.125m < 1800 ~ "Reference Below 1800m",
                                           Grid.Type == "R" & Elevation.125m >= 1800 ~ "Reference Above 1800m",
                                           Grid.Type == "B" & Elevation.125m < 1800 ~ "Burned Below 1800m",
                                           Grid.Type == "B" & Elevation.125m >= 1800 ~ "Burned Above 1800m"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Burn.Elevation, y = Shrub.Cover.125m, fill = Burn.Elevation, color = Burn.Elevation)) +
  theme_classic() +
  labs(x = "", y = "Shrub Cover", title = "(A) Shrub Cover") +
  # Manually adjust colors
  scale_color_manual(values = c("Reference Below 1800m" = "mediumseagreen",
                                "Reference Above 1800m" = "darkslategray4",
                                "Burned Below 1800m" = "red3",
                                "Burned Above 1800m" = "orange2")) +
  scale_fill_manual(values = c("Reference Below 1800m" = "mediumseagreen",
                                "Reference Above 1800m" = "darkslategray4",
                                "Burned Below 1800m" = "red3",
                                "Burned Above 1800m" = "orange2")) +
  # Customize text
  theme(
    plot.title = element_text(size = 22),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  ylim(0, 50) +
  # Add the R squared
  annotate(geom = "text",
           x = 3.5, 
           y = 45, 
           label = paste("R-Squared ", shrub_burn_model_r_sq),
           color = "black",
           size = 5) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# View the plot
shrub_trt_plot

# Perennial cover from being burned on our plots ############################################

# Run a model for pfg cover
pfg_burn_model <- covs %>% 
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation.125m < 1800 ~ 1,
                                           Grid.Type == "R" & Elevation.125m >= 1800 ~ 2,
                                           Grid.Type == "B" & Elevation.125m < 1800 ~ 3,
                                           Grid.Type == "B" & Elevation.125m >= 1800 ~ 4))) %>% 
  lm(formula = Perennial.Cover.125m ~ Burn.Elevation)

# View model summary
pfg_burn_model_sum <- summary(pfg_burn_model)
pfg_burn_model_sum

# Extract the R-Squared 
pfg_burn_model_r_sq <- round(pfg_burn_model_sum$r.squared, 4)
pfg_burn_model_r_sq

# Plot pfg cover against burn type
pfg_trt_plot <- covs %>%
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation.125m < 1800 ~ "Reference Below 1800m",
                                           Grid.Type == "R" & Elevation.125m >= 1800 ~ "Reference Above 1800m",
                                           Grid.Type == "B" & Elevation.125m < 1800 ~ "Burned Below 1800m",
                                           Grid.Type == "B" & Elevation.125m >= 1800 ~ "Burned Above 1800m"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Burn.Elevation, y = Perennial.Cover.125m, fill = Burn.Elevation, color = Burn.Elevation)) +
  theme_classic() +
  labs(x = "", y = "Perennial Cover", title = "(B) Perennnial Cover") +
  # Manually adjust colors
  scale_color_manual(values = c("Reference Below 1800m" = "mediumseagreen",
                                "Reference Above 1800m" = "darkslategray4",
                                "Burned Below 1800m" = "red3",
                                "Burned Above 1800m" = "orange2")) +
  scale_fill_manual(values = c("Reference Below 1800m" = "mediumseagreen",
                               "Reference Above 1800m" = "darkslategray4",
                               "Burned Below 1800m" = "red3",
                               "Burned Above 1800m" = "orange2")) +
  # Customize text
  theme(
    plot.title = element_text(size = 22),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  ylim(0, 50) +
  # Add the R squared
  annotate(geom = "text",
           x = 3.5, 
           y = 45, 
           label = paste("R-Squared ", pfg_burn_model_r_sq),
           color = "black",
           size = 5) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# View the plot
pfg_trt_plot

# combine plots #########################################################################################

# Plot for the legend
trt_leg_plot <-  covs %>%
   mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation.125m < 1800 ~ "Reference Below 1800m",
                                            Grid.Type == "R" & Elevation.125m >= 1800 ~ "Reference Above 1800m",
                                            Grid.Type == "B" & Elevation.125m < 1800 ~ "Burned Below 1800m",
                                            Grid.Type == "B" & Elevation.125m >= 1800 ~ "Burned Above 1800m"))) %>% 
   ggplot() +
   geom_boxplot(aes(x = Burn.Elevation, y = Perennial.Cover.125m, fill = Burn.Elevation, color = Burn.Elevation)) +
   theme_classic() +
   labs(x = "", y = "Perennial Cover", title = "Perennial Cover") +
   # Manually adjust colors
   scale_color_manual(values = c("Reference Below 1800m" = "mediumseagreen",
                                 "Reference Above 1800m" = "darkslategray4",
                                 "Burned Below 1800m" = "red3",
                                 "Burned Above 1800m" = "orange2")) +
   scale_fill_manual(values = c("Reference Below 1800m" = "mediumseagreen",
                                "Reference Above 1800m" = "darkslategray4",
                                "Burned Below 1800m" = "red3",
                                "Burned Above 1800m" = "orange2")) +
   # Customize text
   theme(
     legend.text = element_text(size = 16),
     legend.position = "bottom", 
     legend.title = element_blank()
   ) +
  # 2x2 legend
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))

# Extract the legend
trt_legend <- ggpubr::get_legend(trt_leg_plot)

# Combine the plots
both_trt_plots <- grid.arrange(shrub_trt_plot, pfg_trt_plot,
                               nrow = 1, ncol = 2)

# Add the legend
both_trt_plots_legend <- grid.arrange(both_trt_plots, trt_legend,
                                      nrow = 2, heights = c(0.9, 0.1))

# Save the plot
ggsave(plot = both_trt_plots_legend,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\treatment_veg_plot.png",
       width = 300,
       height = 170,
       units = "mm",
       dpi = 300)

# # 2.5) Candidate models for shrub cover #############################################################
# 
# # View the data again
# glimpse(burn_dat_model)
# 
# # Plot shrub cover against elevation
# ggplot(burn_dat_model, aes(x = Elevation, y = Shrub.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot shrub cover against fire year
# ggplot(burn_dat_model, aes(x = Years.Since.Fire, y = Shrub.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot shrub cover against burn severity
# ggplot(burn_dat_model, aes(x = rdnbr, y = Shrub.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot shrub cover against X coord
# ggplot(burn_dat_model, aes(x = X, y = Shrub.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot shrub cover against X coord
# ggplot(burn_dat_model, aes(x = Y, y = Shrub.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Gaussian model with interactions --------------------------------------------------------------
# shrub_model_out1 <- lm(data = burn_dat_model,
#                        formula = Shrub.Cover ~ Years.Since.Fire + 
#                          Elevation + rdnbr + Elevation * Years.Since.Fire + 
#                          rdnbr * Years.Since.Fire + X + Y)                             
# # Explain extra variation with X coord
# # Summary
# summary(shrub_model_out1)
# # Simulate Residuals
# shrub_sim_out1 <- simulateResiduals(fittedModel = shrub_model_out1, plot = F)
# # View residuals in a plot
# plot(shrub_sim_out1)
# 
# # Remove Interaction with rdnbr  -------------------------------------------------------------
# shrub_model_out2 <- lm(data = burn_dat_model,
#                        formula = Shrub.Cover ~ Years.Since.Fire + 
#                          Elevation + rdnbr + Elevation * Years.Since.Fire + X + Y)                             
# 
# # Summary
# summary(shrub_model_out2)
# # Simulate Residuals
# shrub_sim_out2 <- simulateResiduals(fittedModel = shrub_model_out2, plot = F)
# # View residuals in a plot
# plot(shrub_sim_out2)
# 
# # Remove rdnbr -------------------------------------------------------------
# shrub_model_out3 <- lm(data = burn_dat_model,
#                        formula = Shrub.Cover ~ Years.Since.Fire + 
#                          Elevation + Elevation * Years.Since.Fire + X + Y)                              
# 
# # Summary
# summary(shrub_model_out3)
# # Simulate Residuals
# shrub_sim_out3 <- simulateResiduals(fittedModel = shrub_model_out3, plot = F)
# # View residuals in a plot
# plot(shrub_sim_out3)
# 
# # Remove X  -------------------------------------------------------------
# shrub_model_out4 <- lm(data = burn_dat_model,
#                        formula = Shrub.Cover ~ Years.Since.Fire + 
#                          Elevation + Elevation * Years.Since.Fire + Y)                             
# 
# # Summary
# summary(shrub_model_out4)
# # Simulate Residuals
# shrub_sim_out4 <- simulateResiduals(fittedModel = shrub_model_out4, plot = F)
# # View residuals in a plot
# plot(shrub_sim_out4)
# 
# # Remove  X and Y -------------------------------------------------------------
# shrub_model_out5 <- lm(data = burn_dat_model,
#                       formula = Shrub.Cover ~ Years.Since.Fire + 
#                         Elevation + Elevation * Years.Since.Fire)                           
# 
# # Summary
# summary(shrub_model_out5)
# # Simulate Residuals
# shrub_sim_out5 <- simulateResiduals(fittedModel = shrub_model_out5, plot = F)
# # View residuals in a plot
# plot(shrub_sim_out5)
# 
# #--------------------------------------------------------------
# # Combine all models
# mod_list_shrub <- list(shrub_model_out1,
# shrub_model_out2,
# shrub_model_out3,
# shrub_model_out4, 
# shrub_model_out5)
# 
# # View model AIC Rankings
# aictab(mod_list_shrub)
# 
# # Define the best model
# shrub_model_best <- shrub_model_out3
# 
# 
# # 2.3) Candidate models for perennial cover #############################################################
# 
# # Subset the data further to prevent spatial autocorrelation
# pfg_dat_model <- burn_dat_model %>% 
#   slice_sample(n = 150)
# 
# # View the data again
# glimpse(pfg_dat_model)
# 
# # Plot pfg cover against elevation
# ggplot(pfg_dat_model, aes(x = Elevation, y = PFG.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot PFG cover against fire year
# ggplot(pfg_dat_model, aes(x = Years.Since.Fire, y = PFG.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot PFG cover against burn severity
# ggplot(pfg_dat_model, aes(x = rdnbr, y = PFG.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot PFG cover against X coord
# ggplot(pfg_dat_model, aes(x = X, y = PFG.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Plot PFG cover against X coord
# ggplot(pfg_dat_model, aes(x = Y, y = PFG.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()
# 
# # Gaussian model with interactions ----------------------------------------------------------------------------
# pfg_model_out1 <- lm(data = pfg_dat_model,
#                      formula = PFG.Cover ~ Years.Since.Fire + 
#                        Elevation + rdnbr + Elevation * Years.Since.Fire + 
#                        rdnbr * Years.Since.Fire + X + Y)                         
# # View model summarys
# summary(pfg_model_out1)
# # Simulate Residuals
# pfg_sim_out1 <- simulateResiduals(fittedModel = pfg_model_out1, plot = F)
# # View residuals in a plot
# plot(pfg_sim_out1)
# 
# # Remove X ----------------------------------------------------------------------------
# pfg_model_out2 <- lm(data = pfg_dat_model,
#                      formula = PFG.Cover ~ Years.Since.Fire +
#                        Elevation + rdnbr + Elevation * Years.Since.Fire +
#                        rdnbr * Years.Since.Fire + Y) 
# 
# # View model summarys
# summary(pfg_model_out2)
# # Simulate Residuals
# pfg_sim_out2 <- simulateResiduals(fittedModel = pfg_model_out2, plot = F)
# # View residuals in a plot
# plot(pfg_sim_out2)
# 
# # Remove both interactions ----------------------------------------------------------------------------
# pfg_model_out3 <- lm(data = pfg_dat_model,
#                      formula = PFG.Cover ~ Years.Since.Fire +
#                        Elevation + rdnbr + Y)     
# 
# # View model summarys
# summary(pfg_model_out3)
# # Simulate Residuals
# pfg_sim_out3 <- simulateResiduals(fittedModel = pfg_model_out3, plot = F)
# # View residuals in a plot
# plot(pfg_sim_out3)
# 
# # Remove years since fire ---------------------------------------------------------------------------
# pfg_model_out4 <- lm(data = pfg_dat_model,
#                      formula = PFG.Cover ~ Elevation + rdnbr + Y)     
# 
# # View model summarys
# summary(pfg_model_out4)
# # Simulate Residuals
# pfg_sim_out4 <- simulateResiduals(fittedModel = pfg_model_out4, plot = F)
# # View residuals in a plot
# plot(pfg_sim_out4)
# 
# # Remove Y ---------------------------------------------------------------------------
# pfg_model_out5 <- lm(data = pfg_dat_model,
#                      formula = PFG.Cover ~ Elevation + rdnbr)     
# 
# # View model summary
# summary(pfg_model_out5)
# # Simulate Residuals
# pfg_sim_out5 <- simulateResiduals(fittedModel = pfg_model_out5, plot = F)
# # View residuals in a plot
# plot(pfg_sim_out5)
# 
# 
# # AIC Ranking ----------------------------------------------------------------------
# 
# # Combine models into a list
# mode_list_pfg <- list(pfg_model_out1, 
#                       pfg_model_out2,
#                       pfg_model_out3,
#                       pfg_model_out4,
#                       pfg_model_out5) 
# 
# # View AIC Ranking
# aictab(mode_list_pfg)
# 
# # Define the best model
# pfg_model_best <- pfg_model_out4
# 
# # 2.4) Plot Shrub model output #########################################################
# 
# # Extract the model coefficients
# shrub_summary <- summary(shrub_model_best)
# shrub_summary
# shrub_coefs <- shrub_summary$coefficients
# shrub_coefs
# shrub_coefs_tibble <- tibble(Parameter= c("Intercept", 
#                                           "Years Since Fire",
#                                           "Elevation (m)",
#                                           "Latitude",
#                                           "Longitude",
#                                           "Elevation * Years Since Fire"),
#                              Value = as.numeric(shrub_coefs[,1]),
#                              Std.Error = shrub_coefs[,2]) 
# 
# # and View
# head(shrub_coefs_tibble, n = 10)
# 
# # Create the plot
# shrub_params_plot <- shrub_coefs_tibble %>% 
#   # Switch to a factor 
#   mutate(Parameter = factor(Parameter, levels = c(
#     "Elevation * Years Since Fire",
#     "Elevation (m)",
#     "Longitude",
#     "Latitude",
#     "Years Since Fire", 
#     "Intercept"
#   )),
#   CI.lb = Value - Std.Error,
#   CI.ub = Value + Std.Error) %>% 
#   # Open the plot
#   ggplot(aes(y = Parameter)) +
#   # Add points at the mean values for each parameters
#   geom_point(aes(x = Value), shape = 15, size = 4, color = "navyblue") +
#   # Add whiskers for mean plus standard error
#   geom_linerange(aes(xmin = CI.lb, xmax = CI.ub), linewidth = 1.5, color = "navyblue") +
#   # Add a vertical Line at zero
#   geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
#   # Change the Labels
#   labs(x = "Parameter Estimate", 
#        y = "",
#        title = "Shrub Cover") +
#   # Simple theme
#   theme_classic() +
#   # Edit theme
#   theme(legend.position = "none",
#         plot.title = element_text(size = 20),
#         axis.text.y = element_text(size = 16),
#         axis.title.x = element_text(size = 16),
#         axis.text.x = element_text(size = 16)) 
# 
# 
# # View the plot
# shrub_params_plot
# 
# # Save the plot
# ggsave(plot = shrub_params_plot,
#        file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\shrub_fire_output.png",
#        width = 200,
#        height = 120,
#        units = "mm",
#        dpi = 300)
# 
# # 2.5) Plot PFG model output #########################################################
# 
# # Extract the model coefficients
# pfg_summary <- summary(pfg_model_best)
# pfg_summary
# pfg_coefs <- pfg_summary$coefficients
# pfg_coefs
# pfg_coefs_tibble <- tibble(Parameter= c("Intercept", 
#                                     "Elevation (m)", 
#                                     "RdNBR Burn Severity",
#                                     "Latitude"),
#                        Value = as.numeric(pfg_coefs[,1]),
#                        Std.Error = pfg_coefs[,2]) 
# 
# # and View
# head(pfg_coefs_tibble, n = 10)
# 
# # Create the plot
# pfg_params_plot <- pfg_coefs_tibble %>% 
#   # Switch to a factor 
#   mutate(Parameter = factor(Parameter, levels = c("Latitude",
#                                                   "RdNBR Burn Severity",
#                                                   "Elevation (m)", 
#                                                   "Intercept"
#                                                   )),
#          CI.lb = Value - Std.Error,
#          CI.ub = Value + Std.Error) %>% 
#   # Open the plot
#   ggplot(aes(y = Parameter)) +
#   # Add points at the mean values for each parameters
#   geom_point(aes(x = Value), shape = 15, size = 4, color = "navyblue") +
#   # Add whiskers for mean plus standard error
#   geom_linerange(aes(xmin = CI.lb, xmax = CI.ub), linewidth = 1.5, color = "navyblue") +
#   # Add a vertical Line at zero
#   geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
#   # Change the Labels
#   labs(x = "Parameter Estimate", 
#        y = "",
#        title = "Perennial Cover") +
#   # Simple theme
#   theme_classic() +
#   # Edit theme
#   theme(legend.position = "none",
#         plot.title = element_text(size = 20),
#         axis.text.y = element_text(size = 16),
#         axis.title.x = element_text(size = 16),
#         axis.text.x = element_text(size = 16)) 
# 
# 
# # View the plot
# pfg_params_plot
# 
# # Save the plot
# ggsave(plot = pfg_params_plot,
#        file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\pfg_fire_output.png",
#        width = 200,
#        height = 120,
#        units = "mm",
#        dpi = 300)
# 
# # 2.6) Plot shrub and PFG model parameters together ###################################
# 
# # Load the two plots back in
# shrub_params_plot <- load("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\shrub_fire_output.png")
# pfg_params_plot <- load("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\pfg_fire_output.png")
# 
# # Two plots together
# double_plot <- grid.arrange(shrub_params_plot, pfg_params_plot, nrow = 1, ncol = 2)
# 
# # View the plot
# double_plot
# 
# # Save the plot
# ggsave(plot = double_plot,
#        file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\veg_cvr_params_plot.png",
#        width = 300,
#        height = 120,
#        units = "mm",
#        dpi = 300)
