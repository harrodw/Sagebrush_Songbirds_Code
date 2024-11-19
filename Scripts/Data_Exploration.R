# Will Harrod
# Created: 06/18/2024
# Visualizing 2022-2024 songbird point count data 
# Start here ---------------------------------------------------------------------
rm(list = ls())

#Add packages
library(tidyverse)
library(gridExtra)
library(extrafont)
library(ggcorrplot)

#Load fonts
font_import()
loadfonts(device = "win")

# 1) Prep ################################################################################

#Add point count data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) %>% #Remove the column that excel generated
  tibble()
#and view the data
glimpse(sobs)

#add covariates

# Also, point level covariates
covs <- tibble(read.csv("Data\\Outputs\\grid_covs.csv")) %>%
  dplyr::select(-X) %>%
  tibble() 
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------
#define relevant species
important_species <- c("BRSP", "SATH", "SABS", "GTTO",
                       "WEME", "HOLA", "VESP")

#Truncate observations beyond a cirtain distance
trunc_dist <- 125

#Count of all target species
n_target_obs <- sobs %>% 
  filter(Species %in% important_species) %>% 
  nrow()

#count of close target species
n_close_target_obs <- sobs %>% 
  filter(Species %in% important_species) %>% 
  filter(Distance <= trunc_dist) %>%
  nrow()

#How many total observations?
print(c(n_target_obs, n_close_target_obs))

#How many observations does this cut?
print(n_target_obs - n_close_target_obs)
#20% of the observations isn't too bad

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  #only relevant species
  filter(Species %in% important_species) %>% 
  #only close observations
  filter(Distance <= trunc_dist) %>%
  #Reorganize
  group_by(Grid.ID, Visit, Date, Species) %>% 
  reframe(Grid.ID, Grid.Type, Year, Visit, Date, Species,
          Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)


#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  filter(Species %in% important_species) %>% 
  tidyr::expand(nesting(Grid.ID, Grid.Type, Year, Visit), Species)
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Grid.ID', "Grid.Type", 'Year', 'Visit','Species')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  left_join(covs, by = c('Grid.ID', "Grid.Type")) %>% 
  mutate(Visit.Count = paste(Grid.ID, Year, Visit, sep = "-")) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year,
                                      Year == "Y3" ~ 2024 - Fire.Year))
  
#...and view
glimpse(sobs_count)

#Create the object for relevant species observations
sobs_obs <- sobs %>% 
  filter(Species %in% important_species) %>% 
  filter(Distance <= trunc_dist)
#...and view
glimpse(sobs_obs)

# 2) View correlations among numeric variables #####################################################

#pick a single species
study_species <- "BRSP"

# Pull out all the variables I am interested in
# Define numeric covariates
num_covs_125m <- c("Shrub.Cover.125m", "Perennial.Cover.125m", "Annual.Cover.125m", "Bare.Ground.Cover.125m", 
                   "Elevation.125m", "TRI.125m",
                   "Prop.Burned.125m", "Avg.Shrub.Patch.Size.125m",
                   "n.Shrub.Patches.125m", "Avg.Tree.Patch.Size.125m", "n.Tree.Patches.125m",
                   "rdnbr.125m",  "Fire.Count", "Years.Since.Fire"
                   )

num_covs_1km <- c("Shrub.Cover.1km", "Perennial.Cover.1km", "Annual.Cover.1km", "Bare.Ground.Cover.1km",
                  "Elevation.1km", "TRI.1km", "Aspect.1km", "Prop.Burned.1km", "rdnbr.1km",
                  "Avg.Shrub.Patch.Size.1km", "n.Shrub.Patches.1km", "Avg.Tree.Patch.Size.1km", "n.Tree.Patches.1km")
               
num_covs_5km <-c("Shrub.Cover.5km", "Perennial.Cover.5km", "Annual.Cover.5km", "Bare.Ground.Cover.5km",
                 "Elevation.5km", "TRI.5km", "Prop.Burned.5km", "Avg.Shrub.Patch.Size.5km", 
                 "n.Shrub.Patches.5km", "Avg.Tree.Patch.Size.5km", "n.Tree.Patches.5km")

# Pick a scale 
num_covs <- num_covs_125m

#Pull out numeric data
num_cov_dat <- sobs_count %>% 
  filter(Species == study_species) %>% 
  mutate(Years.Since.Fire )
num_cov_dat <- num_cov_dat[,num_covs]

# Correlations 
cor_mat <- cor(num_cov_dat)
cor_mat

# P-value correlations
p_mat <- cor_pmat(num_cov_dat)
p_mat

# Plot correlations
ggcorrplot(cor_mat, 
           title = "Correlation Matrix for Songbird Spatial Data", 
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
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text.y = element_text(angle = 0, hjust = 1)  
  )


# View a specific correlation
sobs_count %>% 
ggplot(aes(x = Shrub.Cover.125m, y = Annual.Cover.125m)) +
  geom_point(color = "darkgreen", alpha = 0.7, size = 3) + 
  geom_smooth(method = "lm", color = "blue", se = TRUE, linetype = "dashed") + 
  labs(title = "Comparing Two Spatial Covariates") +
  ylim(0, max(50)) +
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_blank())

# 3) Histograms of numeric covariates ################################################################

#Pull out numeric data
num_cov_dat <- sobs_count %>% 
  filter(Species == study_species)

#Build a function to look at the data shape
view_num_dat <- function(dat, cov){
  # Histogram of the raw covariate
  naive_hist <- dat %>% 
    ggplot() +
    geom_histogram(aes(x = .data[[cov]]), fill = "lightblue") +
    theme_bw() +
    ggtitle(paste("Naive Histogram of", cov))
  # Histogram of scaled covariate
  scaled_hist <- dat %>% 
    mutate(across(cov, scale)) %>% 
    ggplot() +
    geom_histogram(aes(x = .data[[cov]]), fill = "lightblue") +
    theme_bw() +
    ggtitle(paste("Scaled Histogram of", cov))
  # Histogram of square-rooted covariate
  sqrt_hist <- dat %>% 
    mutate(across(cov, sqrt)) %>% 
    ggplot() +
    geom_histogram(aes(x = .data[[cov]]), fill = "lightblue") +
    theme_bw() +
    ggtitle(paste("Square Root Histogram of", cov))
  # Histogram of log-transformed covariate
  log_hist <- dat %>% 
    mutate(across(cov, log)) %>% 
    ggplot() +
    geom_histogram(aes(x = .data[[cov]]), fill = "lightblue") +
    theme_bw() +
    ggtitle(paste("Log-Transformed Histogram of", cov))
  # Plot all four
  grid.arrange(naive_hist, scaled_hist, sqrt_hist, log_hist, ncol = 2)
} # end function

# Loop over all covariates to make some plots
for(i in 1:length(num_covs)){
  # Define a particular covariate
  cov <- num_covs[i]
  #Plot them using my custom function
  view_num_dat(dat = num_cov_dat,
           cov = cov)
}

# Histogram of a specific covariate
view_num_dat(dat = num_cov_dat,
             cov = "Sage.Cover")


# Make a list of important catagoorical variables
cat_covs <- c("Observer.ID", "Wind.Start", "Sky.Start", "Grid.Type", "Fire.Name", "Burn.Sevarity")

#pull the catagorical variables out of the data
cat_cov_dat <- sobs_count %>% 
  filter(Species == soi)
cat_cov_dat <- cat_cov_dat[, cat_covs]
#... and view
glimpse(cat_cov_dat)

#make a function to view the catigorical data
view_cat_dat <- function(dat, cov){
  #Bar graph for number of cases of each variable
  cat_bars <- dat %>% 
    mutate(across(cov, factor)) %>% 
    drop_na(cov) %>% 
    ggplot() +
    geom_bar(aes(x = .data[[cov]]), fill = "pink") +
    theme_bw() +
    ggtitle(paste("Number of surveys in each class of", cov))
  #Plot
  grid.arrange(cat_bars)
} # end function

# Loop to make some plots
for(i in 1:length(cat_covs)){
  # Define a particular covariate
  cov <- cat_covs[i]
  #Plot them using my custom function
  view_cat_dat(dat = cat_cov_dat,
               cov = cov)
}

# Transformt he things that need to be transformed
# IMPORTANT: Here is the list of covariate adjustments I want to make ------------------------------
sobs_count <- sobs_count %>% 
  # Remove Fire Distance for things outside the fire
  filter(Fire.Distance < 500000) %>% 
  #log transform the things that need to be
  mutate(ln.Sage.Cover = log(Sage.Cover),
         ln.Fire.Distance = log(Fire.Distance),
          ln.Fire.Distance = log(Fire.Distance),
    #Combine Alex and Ben's Data
         Observer.ID = case_when(Observer.ID %in% c('Alex', "Ben") ~ "Alex & Ben",
                                 TRUE ~ Observer.ID),
         #Now many fog/smoke days. I'll just remove them
         Sky.Start = case_when(Sky.Start %in% c("Fog or Smoke", "Cloudy") ~ "Cloudy",
                               TRUE ~ Sky.Start))
#...and view
glimpse(sobs_count)

# 4) How does each covariate predict species abundance? ###########################################

# Build an object of transformed numeric covariates
num_covs_trans <- c("ln.Sage.Cover", "Perennial.Cover", "Elevation",  
                    "Burn.Sevarity", "Years.Since.Fire", "ln.Fire.Distance",
                    "MAS", "Ord.Date")


# Build a function that plots each numeric variable against the observed counts for each species
obs_scatter <- function(dat, cov) { 
  test_plots <- dat %>% 
    ggplot(aes(x = .data[[cov]], y = Count)) +
    geom_jitter(color = "cadetblue", size = 2, alpha = 0.7,
                height = 0.5, width = 0) +  
    geom_smooth(method = "glm",
                method.args = list(family = "poisson"),
                color = "deepskyblue3", 
                fill = "deepskyblue1", 
                linetype = "solid",  
                size = 1.2) +  
    facet_wrap(~Species, scales = "free_y") +  
    ggtitle(paste("Number of Observations as a Function of", cov)) +
    xlab(cov) + 
    ylab("Count of Observations") +  
    theme_bw() +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
      axis.title.x = element_blank(),         
      axis.title.y = element_text(size = 14), 
      axis.text = element_text(size = 12),  
      strip.text = element_text(size = 12),  
      legend.position = "none"  
    )
  
  grid.arrange(test_plots)
}  # end function

# Plot all of these with the function
for(i in 1:length(num_covs)){
  # Define a particular covariate
  cov <- num_covs_trans[i]
  #Plot them using my custom function
  obs_scatter(dat = sobs_count,
              cov = cov)
}

#View a covariate on its own
sobs_count %>% 
  filter(ln.Fire.Distance < 15) %>% 
  obs_scatter(cov = "ln.Fire.Distance")

#View a specific variable before and after the log transformation
scat_plot1 <- obs_scatter(dat = sobs_count,
                          cov = "Sage.Cover")
scat_plot2 <- obs_scatter(dat = sobs_count,
                     cov = "ln.Sage.Cover")
grid.arrange(scat_plot1, scat_plot2)

# Build a function that plots each categorical variable against the observed counts for each species
obs_boxp <- function(dat, cov) { 
  test_plots <- dat %>% 
    mutate(across(all_of(cat_covs), factor)) %>%  # Convert categorical variables to factors
    ggplot(aes(x = .data[[cov]], y = Count, fill = .data[[cov]])) +
    geom_boxplot(color = "black",  # Black border for boxes
                 outlier.shape = 21,  # Custom outlier shape
                 outlier.size = 2,  # Smaller outliers
                 outlier.fill = "gray", # all outliers the same color
                 lwd = 0.5) +  # Line width of the box borders
    facet_wrap(~Species, scales = "free_y") +  # Facet by species
    scale_fill_brewer(palette = "Dark2") +  
    ggtitle(paste("Number of Observations as a Function of", cov)) + 
    xlab(cov) +  # X-axis label
    ylab("Count of Observations") +  # Y-axis label
    theme_bw() +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold the title
      axis.title.x = element_blank(),          # no x-axis label
      axis.title.y = element_text(size = 14),  # Y-axis title size
      axis.text = element_text(size = 12),  # Axis text size
      strip.text = element_text(size = 12),  # Facet label size
      legend.position = "none"  
    )
  
  grid.arrange(test_plots)
}  # End function


# Plot these with the function
for(i in 1:length(cat_covs)){
  # Define a particular covariate
  cov <- cat_covs[i]
  #Plot them using my custom function
  obs_boxp(dat = sobs_count,
              cov = cov)
}

# 5) Other random useful plots #########################################################################

#View how many of each species were seen on each route
sobs_count %>% 
  mutate(Visit.ID = paste(Grid.ID, Year, Visit)) %>% 
  group_by(Species, Visit.ID) %>% 
  reframe(Grid.Count = sum(Count)) %>% 
  distinct(Species, Visit.ID, Grid.Count) %>% 
  mutate(Visit.ID = factor(Visit.ID)) %>% 
  mutate(Visit.ID = fct_reorder(Visit.ID, Grid.Count)) %>% 
  ggplot(aes(x = Visit.ID, y = Grid.Count)) +
  geom_col() +
  facet_wrap(~Species)

#Look at specific outlines -------------------------------------------
#there are a lot of WEME's on ID-B07 and ID-B22
sobs_obs %>% 
  filter(Grid.ID== "ID-B22") %>% 
  filter(Species == "WEME") %>% 
  dplyr::select(Species, Distance, Minute, Grid.ID, 
                Observer.ID, Year, Visit, Date, Distance) %>%
  ggplot(aes(x = Distance)) +
  geom_histogram() +
  facet_wrap(~Observer.ID)

#How many observations of each species
sobs_count %>% 
  ggplot(aes(x = Count)) + 
  geom_histogram() +
  facet_wrap(~Species)

#Histogram of observations by distance ------------------ ---------------
sobs_obs %>% 
  ggplot(aes(x = Distance)) +
  geom_histogram(binwidth = 25, col = "darkgray", fill = "lightblue") +
  facet_wrap(~Species)
#That's a great looking detection histogram right there

#At what elevation do we start seeing more birds on burned grids?
sobs_count %>% 
  ggplot(aes(x = Elevation, y = Count, col = Grid.Type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)

#What if elevation was binary?
sobs_count %>% 
  mutate(Sagebrush.Type = case_when(Elevation > 1800 ~ "Mountain",
                                    Elevation <= 1800 ~ "Wyoming")) %>% 
  mutate(Sagebrush.Type = factor(Sagebrush.Type, levels = c("Wyoming", "Mountain"))) %>% 
  ggplot(aes(x = Sagebrush.Type, y = Count, col = Grid.Type)) +
  geom_boxplot() +
  facet_wrap(~Species)

#View time since fire
sobs_count %>% 
  filter(Years.Since.Fire < 100) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Count)) +
  geom_point(color = "darkred") +      
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"),  
              formula = y ~ x,                        
              color = "darkred") +                    
  facet_wrap(~Species)

# plot species counts against burn sevarity (dnbr)
sobs_count %>% 
  filter(Years.Since.Fire < 100) %>% 
  ggplot(aes(x = mean.rdnbr, y = Count)) +
  geom_point(color = "darkred") +      
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"),  
              formula = y ~ x,                        
              color = "darkred") +                    
  facet_wrap(~Species)

# plot shrub cover against dnbr and rdnbr
sobs_count %>% 
  filter(mean.dnbr > 0) %>% 
  filter(Year == "Y3" & Visit == "V1") %>% 
  ggplot(aes(x = mean.dnbr, y = Sage.Cover)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),  
              formula = y ~ x,)

# plot shrub cover against dnbr and rdnbr
sobs_count %>% 
  filter(mean.dnbr > 0) %>% 
  filter(Year == "Y3" & Visit == "V1") %>% 
  ggplot(aes(x = mean.dnbr, y = Sage.Cover)) +
  geom_point() +
  geom_smooth()

hist(covs$mean.dnbr)
#And the interaction
sobs_count %>% 
  filter(!is.na(Fire.Year)) %>%
  mutate(Sagebrush.Type = case_when(Elevation > 1800 ~ "Mountain",
                                    Elevation <= 1800 ~ "Wyoming")) %>% 
  mutate(Sagebrush.Type = factor(Sagebrush.Type, levels = c("Wyoming", "Mountain"))) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Count, col = Sagebrush.Type)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  facet_wrap(~Species)

#Now Precipitation
sobs_count %>% 
  ggplot(aes(x = Precipitation, y = Count, col = Grid.Type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)