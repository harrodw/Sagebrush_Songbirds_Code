# --------------------------------------------------------------------------------------------
# Will Harrod
# Preparing the in person covariates
# Created 11/13/2024
# ----------------------------------------------------------------------------------------------

# 1.0) Covariates collected on the ground ###########################################################

# Add in the 2023 point data
points_24_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Points_2024_Raw.csv"))

# Object for renaming 2024 points
points_24_names <- c(Point.ID = "Point..",
                     GlobalID.Survey = "ParentGlobalID",
                     Point.Time = "Start.Time.at.Point",
                     Shrub.Cover = "Percent.of.the.area.within.50m.covered.by.any.shrub.species",
                     Trees.Count = "Count.the.number.of.trees.or.snags.within.50m.of.the.point",
                     Cheatgrass.Cover = "Percent.of.the.area.within.50m.of.the.point.where.cheatgrass.is.present")

# Pull out what I need from the points dataset and rename
points_24 <- points_24_raw %>% 
  dplyr::rename(all_of(points_24_names)) %>% 
  dplyr::select(Point.ID, Point.Time, GlobalID.Survey,
                Shrub.Cover, Trees.Count, Cheatgrass.Cover) %>%  
  mutate(Point.ID = as.character(Point.ID))

# Add in the2024 survey data
surveys_24_raw <- tibble(read.csv("Data\\Inputs\\Sobs_Surveys_2024_Raw.csv"))

#object for renaming 2024 surveys
surveys_24_names <- c(Grid.ID = "Route.ID",
                      GlobalID.Survey = "GlobalID",
                      Visit = "Visit.Number")

#rename and slect useful columns
surveys_24 <- surveys_24_raw %>% 
  dplyr::rename(all_of(surveys_24_names)) %>% 
  dplyr::select(Grid.ID,  Visit, GlobalID.Survey) #standardize some of the variables

# Join the two
dat_24 <- points_24 %>% 
  mutate(Point.ID = as.character(case_when(
    Point.ID == "1" ~ "01",
    Point.ID == "2" ~ "02",
    Point.ID == "3" ~ "03",
    Point.ID == "4" ~ "04",
    Point.ID == "5" ~ "05",
    Point.ID == "6" ~ "06",
    Point.ID == "7" ~ "07",
    Point.ID == "8" ~ "08",
    Point.ID == "9" ~ "09",
    Point.ID == "10" ~ "10",
    Point.ID == "11" ~ "11",
    Point.ID == "12" ~ "12",
    Point.ID == "13" ~ "13",
    Point.ID == "14" ~ "14",
    Point.ID == "15" ~ "15",
    Point.ID == "16" ~ "16", 
    TRUE ~ Point.ID
  ))) %>% 
  left_join(surveys_24, by = "GlobalID.Survey") %>% 
  mutate(Full.Point.ID = paste0(Grid.ID, "-P", Point.ID)) %>% 
  dplyr::select(Grid.ID, Full.Point.ID, Shrub.Cover, Cheatgrass.Cover, Trees.Count, Visit)

#View the cleaned 2024 point data
glimpse(dat_24)

# Average across visits
ground_covs <- dat_24 %>% 
  group_by(Full.Point.ID) %>% 
  reframe(Grid.ID, Full.Point.ID, 
          Cheatgrass.Cover = mean(Cheatgrass.Cover), 
          Trees.Count = mean(Trees.Count)) %>% 
  distinct(Grid.ID, Full.Point.ID, Cheatgrass.Cover, Trees.Count)
#...and view
glimpse(ground_covs)

# Summarize these covariates at the grid level
grid_ground_covs1 <- ground_covs %>% 
  group_by(Grid.ID) %>% 
  reframe(Grid.ID,
          Cheatgrass.Cover = mean(Cheatgrass.Cover),
          Trees.Count = sum(Trees.Count)) %>% 
  distinct(Grid.ID, Cheatgrass.Cover, Trees.Count)
#...and view
glimpse(grid_ground_covs1)

# Swap to binaries
grid_ground_covs <- grid_ground_covs1 %>% 
  mutate(Cheatgrass.Present = case_when(Cheatgrass.Cover <= 1 ~ 0,
                                        Cheatgrass.Cover > 1 ~ 1),
         Trees.Present = case_when(Trees.Count < 1 ~ 0,
                                   Trees.Count >= 1 ~ 1)) %>% 
  dplyr::select(Grid.ID, Cheatgrass.Present, Trees.Present)
#...and view
glimpse(grid_ground_covs)

# Join these to the exisitng covariates
grid_covs <- grid_covs_fire2 %>% 
  left_join(grid_ground_covs, by = "Grid.ID") 

# View the covariate trends
glimpse(grid_covs)
# ggplot(grid_covs, aes(x = mean.dnbr, y = Sage.Cover)) +
#   geom_smooth(method = "lm") +
#   geom_point()

#export the grid summaries
write.csv(grid_covs, "Data\\Outputs\\grid_covs.csv")
# And to my box data folder. Feel free to comment this out
write.csv(grid_covs, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Sagebrush_Songbirds_Code\\Data\\Outputs\\grid_covs.csv")

# # Export the point summaries
# write.csv(point_covs, "Data\\Outputs\\point_covs.csv")
# # And to my box data folder. Feel free to comment this out
# write.csv(point_covs, "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Sagebrush_Songbirds_Code\\Data\\Outputs\\pointcovs.csv")

