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
library(grid)
library(cowplot)
library(extrafont)

#Load fonts
font_import()
loadfonts(device = "win")

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


# Make a burn covariates object
burn_covs <- covs %>% 
  # Bunred vs unburned
  filter(Grid.Type == "B") %>% 
  # Binary high vs low elevation
  mutate(High.Elevation = case_when(Elevation >= 1800 ~ 1,
                                    Elevation < 1800 ~ 0),
         # Years since fire
         Years.Since.Fire = 2023 - Fire.Year) %>% 
  # Binary old or recent fire (above or below the mean)
  mutate(Fire.Age = case_when(Years.Since.Fire >= 15 ~ "Old",
                              Years.Since.Fire < 15 ~ "Recent")) %>% 
  # Don't need all the columns
  select(Grid.ID, Shrub.Cover, Perennial.Cover, Fire.Age,
         Years.Since.Fire, rdnbr, High.Elevation, Elevation) %>% 
  rename(Shrub.Cover = Shrub.Cover,
         PFG.Cover = Perennial.Cover,
         rdnbr = rdnbr)
# View
glimpse(burn_covs)

burn_covs %>% 
  select(Grid.ID, rdnbr, Years.Since.Fire, High.Elevation, Elevation) %>% 
  arrange(High.Elevation, Years.Since.Fire) %>% 
  print(n = Inf)


##############################################################################################################
# 2.3) Vegetation cover by grid type #########################################################################
##############################################################################################################

# Shrub cover by treatment ------------------------------------------------------------------

# Run a model for shrub cover
shrub_burn_model <- covs %>% 
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation < 1800 ~ 1,
                                           Grid.Type == "R" & Elevation >= 1800 ~ 2,
                                           Grid.Type == "B" & Elevation < 1800 ~ 3,
                                           Grid.Type == "B" & Elevation >= 1800 ~ 4))) %>% 
  # filter(Burn.Elevation %in% c(2, 4)) %>% 
  mutate(High.Elevation = case_when(Elevation < 1800 ~ 1,
                                    Elevation >= 1800 ~ 2)) %>%
  lm(formula = Shrub.Cover ~ Burn.Elevation)

# View model summary
shrub_burn_model_sum <- summary(shrub_burn_model)
shrub_burn_model_sum

# Extract the R-Squared 
shrub_burn_model_r_sq <- round(shrub_burn_model_sum$r.squared, 4)
shrub_burn_model_r_sq

# Plot shrub cover against burn type
shrub_trt_plot <- covs %>%
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation < 1800 ~ "Reference Below 1800m",
                                           Grid.Type == "R" & Elevation >= 1800 ~ "Reference Above 1800m",
                                           Grid.Type == "B" & Elevation < 1800 ~ "Burned Below 1800m",
                                           Grid.Type == "B" & Elevation >= 1800 ~ "Burned Above 1800m"),
                                 levels = c("Reference Below 1800m", "Burned Below 1800m", 
                                            "Reference Above 1800m", "Burned Above 1800m"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Burn.Elevation, y = Shrub.Cover, fill = Burn.Elevation, color = Burn.Elevation)) +
  theme_classic() +
  labs(x = "Grid Type", y = "Shrub Cover", title = "(A) Shrub Cover") +
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
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  # Add the R squared
  annotate(geom = "text",
           x = 1.2,
           y = 45,
           label = paste0("R", "\u00B2", " = ", shrub_burn_model_r_sq), 
           color = "black",
           family = "Times New Roman",
           size = 6) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))

# View the plot
shrub_trt_plot

# Perennial cover from being burned on our plots ----------------------------------------------------

# Run a model for pfg cover
pfg_burn_model <- covs %>% 
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation < 1800 ~ 1,
                                           Grid.Type == "R" & Elevation >= 1800 ~ 2,
                                           Grid.Type == "B" & Elevation < 1800 ~ 3,
                                           Grid.Type == "B" & Elevation >= 1800 ~ 4))) %>% 
  mutate(High.Elevation = case_when(Elevation < 1800 ~ 1,
                                    Elevation >= 1800 ~ 2)) %>% 
  filter(Burn.Elevation %in% c(2, 4)) %>%
  lm(formula = Perennial.Cover ~ Burn.Elevation)

# View model summary
pfg_burn_model_sum <- summary(pfg_burn_model)
pfg_burn_model_sum

# Extract the R-Squared 
pfg_burn_model_r_sq <- round(pfg_burn_model_sum$r.squared, 4)
pfg_burn_model_r_sq

# Plot pfg cover against burn type
pfg_trt_plot <- covs %>%
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation < 1800 ~ "Reference Below 1800m",
                                           Grid.Type == "R" & Elevation >= 1800 ~ "Reference Above 1800m",
                                           Grid.Type == "B" & Elevation < 1800 ~ "Burned Below 1800m",
                                           Grid.Type == "B" & Elevation >= 1800 ~ "Burned Above 1800m"),
                                 levels = c("Reference Below 1800m", "Burned Below 1800m", 
                                            "Reference Above 1800m", "Burned Above 1800m"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Burn.Elevation, y = Perennial.Cover, fill = Burn.Elevation, color = Burn.Elevation)) +
  theme_classic() +
  labs(x = "Grid Type", y = "Perennial Cover", title = "(B) Perennnial Cover") +
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
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  # Add the R squared
  annotate(geom = "text",
           x = 1.2,
           y = 45,
           label = paste0("R", "\u00B2", " = ", shrub_burn_model_r_sq), 
           color = "black",
           family = "Times New Roman",
           size = 6) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))

# View the plot
pfg_trt_plot

# Combine treatment plots -------------------------------------------------------------------------------------

# Plot for the legend
trt_leg_plot <-  covs %>%
  mutate(Burn.Elevation = factor(case_when(Grid.Type == "R" & Elevation < 1800 ~ "Reference Below 1800m",
                                           Grid.Type == "R" & Elevation >= 1800 ~ "Reference Above 1800m",
                                           Grid.Type == "B" & Elevation < 1800 ~ "Burned Below 1800m",
                                           Grid.Type == "B" & Elevation >= 1800 ~ "Burned Above 1800m"),
                                 levels = c("Reference Below 1800m", "Burned Below 1800m", 
                                            "Reference Above 1800m", "Burned Above 1800m"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Burn.Elevation, y = Perennial.Cover, fill = Burn.Elevation, color = Burn.Elevation)) +
  theme_classic() +
  ylim(0, 50) +
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
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm")
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
                                      nrow = 2, heights = c(0.6, 0.1))

# Save the plot
ggsave(plot = both_trt_plots_legend,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\treatment_veg_plot.png",
       width = 275,
       height = 150,
       units = "mm",
       dpi = 300)

#############################################################################################################
# 2.4) Vegetation cover by time since fire ##################################################################
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
sum_fyear_shrub_high <- summary(fyear_shrub_model_high)
  sum_fyear_shrub_high

# Pull out the r squared
r_sq_fyear_shrub_high <- round(sum_fyear_shrub_high$r.squared, 3)
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
       title = "(A) Shrub Cover") +
  # Custimize text
  theme(
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  # Add the R squared
  annotate(geom="text", x = 7, y = 49, label = paste0("R", "\u00B2", " = ", r_sq_fyear_shrub_low),
           family = "Times New Roman", color = "black", size = 6) +
  annotate(geom = "text", x = 7, y = 46, label = paste0("R", "\u00B2", " = ", r_sq_fyear_shrub_high),
           family = "Times New Roman", color="black", size = 6) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))

# View the plot
fyear_shrub_plot


# Time since fire perennial cover R-squared ##########################################################################

# Model perennial cover by time since fire at low elevations
fyear_pfg_model_low <- burn_covs %>%
  filter(High.Elevation == 0) %>%
  lm(formula = PFG.Cover ~ Years.Since.Fire)

# View time since fire model output
sum_fyear_pfg_low <- summary(fyear_pfg_model_low)
sum_fyear_pfg_low

# Pull out the r squared
r_sq_fyear_pfg_low <- round(sum_fyear_pfg_low$r.squared, 3)
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
       title = "(B) Perennial Cover") + 
  # Custimize text
  theme(
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  # Add the R squared
  annotate(geom="text", x = 7, y = 49, label = paste0("R", "\u00B2", " = ", r_sq_fyear_pfg_low),
           family = "Times New Roman", color = "black", size = 6) +
  annotate(geom = "text", x = 7, y = 46, label = paste0("R", "\u00B2", " = ", r_sq_fyear_pfg_high),
           family = "Times New Roman", color="black", size = 6) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))

# View the plot
fyear_pfg_plot

# Combine both time since fire plots ############################################################

# Plot for a template legend
legend_plot <- burn_covs %>% 
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
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm"))

# Extract the legend
legend <- ggpubr::get_legend(legend_plot)

# Arrange plots in a grid
combined_cont_veg_plots <- grid.arrange(
  fyear_shrub_plot, fyear_pfg_plot,
  nrow = 1, ncol = 2
)
combined_cont_veg_plots


# # Add the legend
two_cont_plots_legend <- grid.arrange(combined_cont_veg_plots, legend,
                                        nrow = 2, heights = c(0.9, 0.1))

# # Save the plot
ggsave(plot = two_cont_plots_legend,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\veg_r_sq_plots_fyear.png",
       width = 275,
       height = 150,
       units = "mm",
       dpi = 300)

# Model shrub cover by burn severity #############################################################################

# Model shrub cover by burn Severity 
rdnbr_shrub_model<- burn_covs %>%
  lm(formula = Shrub.Cover ~ rdnbr)

# View burn Severity model output
sum_rndbr_shrub <- summary(rdnbr_shrub_model)
sum_rndbr_shrub

# Pull out the r squared
r_sq_rdnbr_shrub <- round(sum_rndbr_shrub$r.squared, 3)
r_sq_rdnbr_shrub

# Plot shrub cover against burn Severity
rdnbr_shrub_plot <- burn_covs %>%
  mutate(Elevation = factor(case_when(High.Elevation == 1 ~ "Above 1800m",
                                      High.Elevation == 0 ~ "Below 1800m"))) %>% 
  ggplot(aes(x = rdnbr, y = Shrub.Cover)) +
  geom_smooth(col = "orangered2", fill = "orangered2", method = "lm") +
  geom_jitter(col = "orangered2") +
  theme_classic() +
  labs(x = "RdNBR Burn Severity",
       y = "Shrub Cover",
       title = "(A) Shrub Cover") +
  # Customize text
  theme(
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_text(size = 18, family = "Times New Roman"),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  # Add the R squared
  annotate(geom="text", x = 730, y = 49, label = paste0("R", "\u00B2", " = ", r_sq_rdnbr_shrub),
           family = "Times New Roman", color = "black", size = 6) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))

# View the plot
rdnbr_shrub_plot

# Model Perennial cover by burn severity ############################################################################

# Model pfg cover by burn Severity 
rdnbr_pfg_model<- burn_covs %>%
  lm(formula = PFG.Cover ~ rdnbr)

# View burn Severity model output
sum_rndbr_pfg <- summary(rdnbr_pfg_model)
sum_rndbr_pfg

# Pull out the r squared
r_sq_rdnbr_pfg <- round(sum_rndbr_pfg$r.squared, 3)
r_sq_rdnbr_pfg

# Plot pfg cover against burn Severity
rdnbr_pfg_plot <- burn_covs %>%
  ggplot(aes(x = rdnbr, y = PFG.Cover, col = Fire.Age, fill = Fire.Age)) +
  geom_smooth(col = "orangered2", fill = "orangered2",method = "lm") +
  geom_jitter(col = "orangered2") +
  theme_classic() +
  labs(x = "RdNBR Burn Severity",
       y = "Perennial Cover",
       title = "(B) Perennial Cover") +
  # Customize text
  theme(
    plot.title = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.x = element_text(size = 18, family = "Times New Roman"),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  # Add the R squared
  annotate(geom="text", x = 730, y = 49, label = paste0("R", "\u00B2", " = ", r_sq_rdnbr_pfg),
           family = "Times New Roman", color = "black", size = 6) +
  # Add percent symbols
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))

# View the plot
rdnbr_pfg_plot


# Combine the Burn severity plots ---------------------------------------------------------------------------------------

# Make a new plot to steal a legend from 
rdnbr_legend_plot <- burn_covs %>%
  ggplot(aes(x = rdnbr, y = PFG.Cover)) + 
  geom_smooth(aes(color = "Trend and 95% CI"),
              fill = "orangered2", method = "lm") +
  theme_classic() +
  labs(x = "RdNBR Burn Severity",
       y = "Perennial Cover",
       title = "(B) Perennial Cover") +
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 18, family = "Times New Roman"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 18, family = "Times New Roman"),
    legend.text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm")
  ) +
  scale_color_manual(values = c("Trend and 95% CI" = "orangered2"), 
                     name = "")

# Extract the legend
rdnbr_legend <- ggpubr::get_legend(rdnbr_legend_plot)

# Arrange plots in a grid
combined_rdnbr_plots <- grid.arrange(
  rdnbr_shrub_plot, rdnbr_pfg_plot,
  nrow = 1, ncol = 2
)
combined_rdnbr_plots

# # Add the legend
two_rdnbr_plots_legend <- grid.arrange(combined_rdnbr_plots, rdnbr_legend,
                                      nrow = 2, heights = c(0.9, 0.1))

# # Save the plot
ggsave(plot = two_rdnbr_plots_legend,
       file = "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Thesis_Documents\\Graphs\\veg_r_sq_plots_rdnbr.png",
       width = 275,
       height = 150,
       units = "mm",
       dpi = 300)

