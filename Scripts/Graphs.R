#Will Harrod
#Created: 02/15/2024
#Start here ---------------------------------------------------------------------

#Add packages
library(tidyverse)
library(gridExtra)
library(extrafont)

#Load fonts
font_import()
loadfonts(device = "win")

#add in the data ---------------------------------------------------------------

#Add point count data
sobs <- tibble(read.csv("Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#and view the data
glimpse(sobs)

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("Data\\Outputs\\grid_covs.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------
#define relevant species
important_species <- c("BRSP", 
                       # "SATH",
                       # "SABS", "GTTO", "WEME", 
                       "HOLA", "VESP")

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

# Function to find the mode
find_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]  # Find the most frequent value
}

# Make a table of important species sightings by visit
counts_0inf <- sobs %>%
  filter(Distance <= trunc_dist) %>% 
  filter(Species %in% important_species) %>% 
  group_by(Grid.ID, Year, Visit, Ord.Date, Species) %>% 
  reframe(Grid.ID, Year, Visit, Ord.Date, Species,
          Count = n()) %>% 
  distinct()
#...and view
glimpse(counts_0inf)

# Make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  filter(Species %in% important_species) %>% 
  expand(nesting(Full.Point.ID, Grid.ID, Grid.Type, Ord.Date, MAS, Wind.Start, Observer.ID, Year, Visit), Species) %>% 
  group_by(Grid.ID, Species, Grid.Type, Year, Visit, Ord.Date, Observer.ID) %>% 
  reframe(Grid.ID, Species, Grid.Type, Year, Visit, Ord.Date, Observer.ID) %>% 
  mutate(Visit.ID = paste(Year, Visit, sep = "-"),
         Observer.ID = find_mode(Observer.ID),) %>% 
  distinct()
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count_tmp <-  visit_count %>% 
  left_join(counts_0inf, by = c("Grid.ID", "Year", "Visit", "Ord.Date", "Species")) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count))

#...and view
glimpse(sobs_count_tmp)

# Change necessary variables to scales and factors
sobs_count <- sobs_count_tmp %>%
  # Add covariates
  left_join(covs, by = c("Grid.ID", "Grid.Type")) %>% 
  # Sort the data
  arrange(Visit.ID, Grid.ID) %>% 
  mutate(# Numeric burned vs unburned
    Burned = as.numeric(factor(Grid.Type, levels = c("R", "B"))) - 1,
    # Time since Fire
    Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                 Year == "Y2" ~ 2023 - Fire.Year,
                                 Year == "Y3" ~ 2024 - Fire.Year)) %>%
  # Other things that should be factors
  mutate_at(c("Grid.ID", "Visit.ID", "Observer.ID", "Year"), factor)
#...and view
glimpse(sobs_count)

# 1.2) Summary stats ##########################################################

# #How many total species
# sobs %>% 
#   group_by(Species) %>% 
#   reframe(Species, Observation = n()) %>% 
#   distinct() %>% 
#   print(n = Inf)
# 
# #How many total observations
# sobs %>% 
#   filter(Species != "NOBI") %>% 
#   nrow()
# 
# #Table for Average number of observations between burned and unburned plots
# #Table for output
# burn_v_ref <- sobs_count %>% 
#   distinct(Year, Species) %>% 
#   mutate(Mean.B = NA,
#          Mean.R = NA,
#          t = NA,
#          p = NA,
#          CI.lb = NA,
#          CI.ub = NA)
# #..and view
# glimpse(burn_v_ref)
# 
# #For-loop to compare counts observed between B and R within each year
# for(i in 1:nrow(burn_v_ref)){
#   #Pull the info I want t=out of the data frame
#   soi_count <- sobs_count %>% 
#     group_by(Grid.ID, Burned, Year, Species) %>% 
#     reframe(Grid.ID, Burned, Year, Species, Total.Count = sum(Count)) %>% 
#     distinct(Grid.ID, Burned, Year, Species, Total.Count) %>% 
#     filter(Year == burn_v_ref$Year[i]) %>% 
#     filter(Species == burn_v_ref$Species[i])
#   #Compare species count within year
#   t_test <- t.test(Total.Count ~ Burned, data = soi_count)
#   #Pull what I need out of the 
#   burn_v_ref$Mean.B[i] <- t_test$estimate[1]
#   burn_v_ref$Mean.R[i] <- t_test$estimate[2]
#   burn_v_ref$t[i] <- t_test$p.value
#   burn_v_ref$p[i] <- t_test$p.value
#   burn_v_ref$CI.lb[i] <- t_test$conf.int[1]
#   burn_v_ref$CI.ub[i] <- t_test$conf.int[2]
# }
# 
# #Highlight significan p-values 
# burn_v_ref <- burn_v_ref %>% 
#   mutate(Signif = case_when(p <= 0.05 ~ T,
#                             p > 0.05 ~ F))
# 
# #View the output
# burn_v_ref %>% 
#   print(n = Inf)
# 
# #Export the tables
# write.csv(burn_v_ref, "Data\\Outputs\\burn_v_ref_20240308.csv")


# 2) Start plotting #######################################################################################

#Number of Species by year -----------------------------------------------------
sobs %>%  
  #Remove NOBI and the unknown codes
  filter(! Species %in% c("NOBI", "UKBB", "UNBB", "UNBI", "UNFA", "UNFI", "UNFL", "UNGR", 
                          "UNHA", "UNHU", "UNJA", "UNOW", "UNSP", "UNSW", "UNWO")) %>% 
  distinct(Year, Species) %>% 
  count(Year) %>% 
  rename(Species.Count = "n") %>% 
  mutate(Year = case_when(Year == "Y1" ~ "2022",
                          Year == "Y2" ~ "2023",
                          Year == "Y3" ~ "2024")) %>% 
  ggplot(aes(x = Year, y = Species.Count, fill = Year)) +
  geom_text(aes(label = Species.Count), vjust = -0.5, size = 5) +
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = c("deeppink", "blueviolet", "darkmagenta")) +
  labs(y = "Number of Species Observed") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, family = "sans"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "sans"),
        legend.position = "none",
        strip.text = element_text(size= 12, family = "sans")
  )

#Average number of observations between burned and unburned plots 
sobs_count %>% 
  mutate(Grid.Type = case_when(Grid.Type == "B" ~ "Burn",
                                Grid.Type == "R" ~ "Reference")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  mutate(Year = case_when(Year == "Y1" ~ "Year 1",
                          Year == "Y2" ~ "Year 2",
                          Year == "Y3" ~ "Year 3")) %>% 
  mutate(Year.Type = paste(Year, Grid.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Count, fill = Year.Type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("darkorange1", "lightblue",
                               "red1", "deepskyblue1",
                               "red4", "darkblue")) +
  labs(y = "Average Number of Observations",
       color = "") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, family = "sans"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "sans"),
        strip.text = element_text(size= 12, family = "sans"),
        legend.position = c(0.48, 0.17)) +
  ylim(0, 75) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Western Meadowlark",
                                         "Vesper Sparrow", "Horned Lark", 
                                         "Lark Sparrow"
                                         )))

#Total number of observations between burned and unburned plots ----------------------------
sobs_count %>% 
  mutate(Grid.Type = case_when(Grid.Type == "B" ~ "Burn Grids",
                                Grid.Type == "R" ~ "Reference Grids")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  mutate(Year.Type = paste(Year, Grid.Type, sep = " ")) %>%
  ggplot(aes(x = Year.Type, y = Count, fill = Year.Type)) +
  geom_col() +
  theme_bw() +
  ylim(0, 3300)+
  scale_fill_manual(values = c("darkorange1", "lightblue",
                               "red1", "deepskyblue1",
                               "red4", "darkblue")) +
  labs(y = "Total Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, family = "sans"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "sans"),
        legend.position = c(0.5, 0.18),
        strip.text = element_text(size= 12, family = "sans")
        ) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))


#proportion of plots with at least one observation 
sobs_count %>% 
  mutate(Grid.Type = case_when(Grid.Type == "B" ~ "Burn",
                                Grid.Type == "R" ~ "Reference")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  mutate(Year = case_when(Year == "Y1" ~ "Year 1",
                          Year == "Y2" ~ "Year 2",
                          Year == "Y3" ~ "Year 3")) %>% 
  group_by(Grid.ID, Grid.Type, Year, Species) %>% 
  reframe(Grid.ID, Grid.Type, Year, Species, Count = sum(Count)) %>% 
  distinct(Grid.ID, Grid.Type, Year, Species, Count) %>% 
  mutate(Observed = case_when(Count == 0 ~ 0,
                              TRUE ~ 1)) %>% 
  group_by(Grid.Type, Year, Species) %>% 
  reframe(Grid.Type, Year, Species, 
          Prop.Obs = case_when(Grid.Type == "Burn" ~sum(Observed)/31,
                               Grid.Type == "Reference" ~ sum(Observed)/29)) %>% 
  distinct(Grid.Type, Year, Species, Prop.Obs) %>% 
  mutate(Year.Type = paste(Year, Grid.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Prop.Obs, fill = Year.Type)) +
  geom_col() +
  theme_bw() +
  labs(y = "Proportion of routes with at least one observation") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(),
        legend.position = c(0.5, 0.19)) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Species counts and shrub cover plot ---------------------------------------
sobs_count %>% 
  filter(Species %in% c("BRSP", "SATH", "GTTO",  #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  ggplot(aes(x = Shrub.Cover, y = Count)) +
  geom_point(col = "cadetblue4", size = 2) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Percent Shrub Cover",
       y = "Number of Observations") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, family = "sans"),
        strip.text = element_text(size= 12, family = "sans")
  ) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Species counts and sagebrush cover
sobs_count %>% 
  filter(Species %in% c("BRSP", "SATH", "GTTO",  #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  ggplot(aes(x = Sagebrush.Cover, y = Count)) +
  geom_point(col = "cadetblue4", size = 2) +
  geom_smooth(col = "dodgerblue3", fill = "lightblue") +
  labs(x = "Percent Sagebrush Cover",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Species counts and ruggedness
sobs_count %>% 
  filter(Species %in% c("BRSP", "SATH", "GTTO",  #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  ggplot(aes(x = TRI, y = Count)) +
  geom_point(col = "cadetblue4", size = 1) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Topographic Ruggedness Index",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))


#Species counts and fire year ---------------------------------------------------------

# View the number of species
n_species <- length(important_species)

# Storage object for stats
ref_stats <- tibble(Species = rep(NA, n_species),
                    Mean = rep(NA, n_species),
                    sd = rep(NA, n_species),
                    lb = rep(NA, n_species),
                    ub = rep(NA, n_species))

# Pull out the mean and 95% confidence interval count on reference grids
for(i in 1:n_species){
  #Define a bird
  bird <- important_species[i]
  # Isolate the counts for that bird in reference grids
  counts <- sobs_count %>% 
    filter(Species == bird & Burned == 0) %>%
    dplyr::select(Species, Count) %>% 
    arrange(Count)
  #Pull out stats
  mean <- mean(counts$Count) 
  sd <- sd(counts$Count)
  lb <- counts$Count[floor(0.25*nrow(counts))]
  ub <- counts$Count[floor(0.75*nrow(counts))]
  
  # Add the variables to the stats object
  ref_stats$Species[i] <- bird
  ref_stats$Mean[i] <- mean
  ref_stats$sd[i] <- sd
  ref_stats$lb[i] <- lb
  ref_stats$ub[i] <- ub 
  
} # End loop

# View the stats object
print(ref_stats)

# Join these to the data 
tsf_count <- sobs_count %>% 
  filter(Species %in% important_species) %>% 
  left_join(ref_stats, by = "Species")
#... and view
glimpse(tsf_count)

# Graph time since fire
tsf_count  %>% 
  filter(Species == "BRSP") %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "HOLA" ~ "Horned Lark")) %>% 
  mutate(Species = factor(Species, levels = c("Brewer's Sparrow", 
                                              "Vesper Sparrow", 
                                              "Horned Lark"))) %>% 
  filter(Years.Since.Fire < 45) %>% 
  ggplot() +
  geom_jitter(aes(x = Years.Since.Fire, y = Count, color = "Burn Grid Counts"), 
              width = 0.2, height = 0.4, size = 1) +
  geom_smooth(aes(x = Years.Since.Fire, y = Count, color = "Trend"), 
              method = "glm", 
              fill = "red1",
              method.args = list(family = "quasipoisson"),
              se = TRUE,
              linewidth = 2) +
  geom_line(aes(x = Years.Since.Fire, y = Mean),
            linewidth = 2, col = "navyblue", alpha = 0.7) +
  geom_ribbon(aes(x = Years.Since.Fire, ymin = lb, ymax = ub, fill = "Mean and 75% CI for Reference Grid Counts"), 
              alpha = 0.2, linewidth = 0.5, color = "navyblue") +
  labs(x = "Years Since Fire", y = "Number of Observations on a Survey Grid", 
       color = "", fill = "") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 18, family = "sans"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 18, family = "sans"),
        strip.text = element_text(size= 22, family = "sans"),
        # legend.position = "right",
        legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        legend.key.size = unit(2, "cm")) +    
  ylim(0, 60) +
  facet_wrap(~Species, dir = "h") +
  scale_color_manual(values = c("Burn Grid Counts" = "black",
                                "Trend" = "red4")) +
  scale_fill_manual(values = c("Mean and 75% CI for Reference Grid Counts" = "lightblue")) +
  guides(color = guide_legend(ncol = 1),   
         fill = guide_legend(ncol = 1))

#Species counts and fire sevarety #################fill_()#Species counts and fire sevarety #################################################
fire_sev_cols <- c("yellow1", "darkorange1", "red1", "red4")
sobs_count %>% 
  filter(Species %in% c("BRSP", "SATH", "GTTO",  #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>%
  filter(Grid.Type == "B") %>% 
  filter(!is.na(Burn.Sevarity)) %>% 
  mutate(Burn.Sevarity = as.factor(Burn.Sevarity)) %>% 
  ggplot(aes(x = Burn.Sevarity, y = Count, fill = Burn.Sevarity)) +
  geom_boxplot() +
  labs(x = "Average Burn Sevarity",
       y = "Number of Observations") +
  theme_bw()+
  scale_fill_manual(values = fire_sev_cols) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#compare two species to each other -----------------
#Brewers sparrow vs sage thrasher
brsp_vs_sath <- sobs_count %>% 
  group_by(Grid.ID, Species) %>% 
  reframe(Grid.ID, Species, Mean.Count = mean(Count)) %>% 
  distinct(Grid.ID, Species, Mean.Count) %>% 
  pivot_wider(names_from = Species, values_from = Mean.Count) %>% 
  ggplot(aes(x = BRSP, y = SATH)) +
  geom_point(col = "darkmagenta", size = 1) +
  geom_smooth(col = "darkorchid4", fill = "orchid1") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  labs(x = "Brewers Sparrows Observed", y = "Sage Thrashers Observed")

#western meadowlark vs horned lark
brsp_vs_hola <- sobs_count %>% 
  group_by(Grid.ID, Species) %>% 
  reframe(Grid.ID, Species, Mean.Count = mean(Count)) %>% 
  distinct(Grid.ID, Species, Mean.Count) %>% 
  pivot_wider(names_from = Species, values_from = Mean.Count) %>% 
  ggplot(aes(x = BRSP, y = VESP)) +
  geom_point(col = "darkmagenta", size = 1) +
  geom_smooth(col = "darkorchid4", fill = "orchid1") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  labs(x = "Brewers Sparrows Observed", y = "Horned Larks Observed")

#View both plots
grid.arrange(brsp_vs_sath, brsp_vs_hola, ncol = 2)

#Species on burned plots at different elevations
sobs_count %>% 
  filter(Species == "VESP") %>% 
  mutate(Elevation = factor(case_when(Elevation < 1800 & Burned == 1  ~ "Burned below 1800m",
                                      Elevation >= 1800 & Burned == 1 ~ "Burned above 1800m",
                                      Elevation < 1800 & Burned == 0 ~ "Reference below 1800m",
                                      Elevation >= 1800 & Burned == 0 ~ "Reference above 1800m"),
                               levels = c("Burned below 1800m",
                                          "Burned above 1800m",
                                          "Reference below 1800m",
                                          "Reference above 1800m"))) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "HOLA" ~ "Horned Lark")) %>% 
  ggplot(aes(x = Elevation, y = Count)) +
  geom_boxplot(aes(color = Elevation, fill = Elevation), color = "black") +
  scale_fill_manual(values = c("darkgoldenrod2", "darkorange2", "cadetblue", "darkslategray")) +
  labs(x = "",
       y = "Average Number of Observations on a Survey Grid") +
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 18, family = "sans"),
        strip.text = element_text(size= 22, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"),
        legend.title = element_blank(),
        legend.key.size = unit(1.4, 'cm'),
        legend.position = "none") +    
  ylim(0, 60) +
  facet_wrap(~ factor(Species, levels = c("Brewer's Sparrow", "Vesper Sparrow", "Horned Lark")), 
             dir = "h")

#Combine grassland and shrubland species ##############################################################
grass_sp <- c("WEME", "HOLA", "VESP")
shrub_sp <- c("BRSP", "SATH", "SABS", "GTTO")

#Species counts and fire year ------------------------------------------------
sobs_count %>%
  filter(Species %in% c("BRSP", "SATH", "GTTO", "SABS", #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated Birds",
                                  Species %in% shrub_sp ~ "Sagebrush Associated Birds")) %>% 
  group_by(Grid.ID, Grid.Type, Fire.Year, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Grid.ID, Grid.Type, Fire.Year, Year, Visit, Species.Type, Total.Count) %>% 
  filter(Grid.Type == "B") %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year)) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Total.Count)) +
  geom_point(aes(col = Species.Type), size = 2) +
  geom_smooth(aes(color = Species.Type, fill = Species.Type)) +
  scale_fill_manual(values = c("darkgoldenrod1", "darkseagreen4"), name = "Species Niche") +
  scale_color_manual(values = c("darkgoldenrod1", "darkseagreen4"), name = "Species Niche") +
  labs(x = "Years Since Fire",
       y = "Number of Observations",
       color = "Species Type") + 
  theme_bw() +
  ylim(c(0, 110)) +
  theme(legend.position = c(0.29, 0.92),
        legend.text = element_text(size = 18,  family = "sans"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18, family = "sans"),
        axis.title.y = element_text(size = 18, family = "sans"))


#Species counts and shrub cover ------------------------------------------------
sobs_count %>%
  filter(Species %in% c(grass_sp, shrub_sp)) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated Birds",
                                  Species %in% shrub_sp ~ "Sagebrush Associated Birds")) %>% 
  group_by(Grid.ID, Shrub.Cover, Fire.Year, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Grid.ID, Shrub.Cover, Fire.Year, Year, Visit, Species.Type, Total.Count) %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2" ~ 2023 - Fire.Year)) %>% 
  ggplot(aes(x = Shrub.Cover, y = Total.Count)) +
  geom_point(aes(col = Species.Type), size = 2) +
  geom_smooth(aes(color = Species.Type, fill = Species.Type)) +
  scale_fill_manual(values = c("darkgoldenrod1", "darkseagreen4"), name = "Species Niche") +
  scale_color_manual(values = c("darkgoldenrod1", "darkseagreen4"), name = "Species Niche") +
  labs(x = "Percent Shrub Cover",
       y = "Number of Observations",
       color = "Species Type") + 
  theme_bw() +
  theme(legend.position = c(0.72, 0.88),
        legend.text = element_text(size = 18,  family = "sans"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18, family = "sans"),
        axis.title.y = element_text(size = 18, family = "sans")
  )


#Average number of observations between burned and unburned plots ------------------------
sobs_count %>% 
  filter(Species %in% c(grass_sp, shrub_sp)) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated",
                                  Species %in% shrub_sp ~ "Sagebrush Associated")) %>% 
  group_by(Grid.ID, Grid.Type, Fire.Year, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Grid.ID,  Grid.Type, Year, Visit, Species.Type, Total.Count) %>% 
  mutate(Grid.Type = case_when(Grid.Type == "B" ~ "Burn Grids",
                                Grid.Type == "R" ~ "Reference Grids")) %>% 
  ggplot(aes(x = Species.Type, y = Total.Count, fill = Species.Type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("darkgoldenrod1", "darkseagreen4"), name = "Species Niche") +
  labs(y = "Average Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size= 17, family = "sans"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size= 17, family = "sans"),
        axis.text.y = element_text(size= 16, family = "sans"),
        strip.text = element_text(size= 20, family = "sans"),
        legend.position = "none") +
  ylim(0, 75) +
  facet_wrap(~Grid.Type)

#Total number of observations between burned and unburned plots ----------------------------
sobs_count %>% 
  filter(Species %in% c(grass_sp, shrub_sp)) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated",
                                  Species %in% shrub_sp ~ "Sagebrush Associated")) %>% 
  mutate(Grid.Type = case_when(Grid.Type == "B" ~ "Burn Grids",
                                Grid.Type == "R" ~ "Reference Grids")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  ggplot(aes(x = Species.Type, y = Count, fill = Species)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() +
  labs(y = "Total Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, family = "sans"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 18, family = "sans"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, family = "sans"),
        legend.position = "right",
        strip.text = element_text(size= 20, family = "sans")
  ) +
  facet_wrap(~Grid.Type)

#Average number of observations between low and high elevation  ------------------------
sobs_count %>% 
  filter(Grid.Type == "B") %>% 
  mutate(Elevation = factor(case_when(Elevation < 1800 ~ "Below 1800m",
                                      Elevation >= 1800 ~ "Above 1800m"),
                            levels = c("Below 1800m", "Above 1800m"))) %>% 
  filter(Species %in% c(grass_sp, shrub_sp)) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated",
                                  Species %in% shrub_sp ~ "Sagebrush Associated")) %>% 
  group_by(Grid.ID, Grid.Type, Elevation, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Grid.ID, Grid.Type, Elevation, Year, Visit, Species.Type, Total.Count) %>% 
  mutate(Grid.Type = case_when(Grid.Type == "B" ~ "Burn Grids",
                                Grid.Type == "R" ~ "Reference Grids")) %>% 
  ggplot(aes(x = Species.Type, y = Total.Count, fill = Species.Type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("darkgoldenrod1", "darkseagreen4"), name = "Species Niche") +
  labs(y = "Average Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size= 15, family = "sans"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size= 15, family = "sans"),
        axis.text.y = element_text(size= 14, family = "sans"),
        strip.text = element_text(size= 20, family = "sans"),
        legend.position = "none") +
  ylim(0, 75) +
  facet_wrap(~Elevation)