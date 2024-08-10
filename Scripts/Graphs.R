#Will Harrod
#Created: 02/15/2024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
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
covs <- tibble(read.csv("Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X)
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
  group_by(Route.ID, Route.Type, Year, Visit, Date, Species) %>% 
  reframe(Route.ID, Route.Type, Year, Visit, Date, Species, Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zero counts
visit_count <- sobs %>% 
  expand(nesting(Route.ID, Route.Type, Year, Visit, Date), Species) %>% 
  filter(Species %in% important_species)
#...and view
glimpse(visit_count)

#Join the two, add zeros and average observations within year
sobs_count <-  visit_count %>% 
  left_join(sobs_count_0inf, by = c('Route.ID', 'Route.Type', 'Year', 'Visit', 'Date', 'Species')) %>% 
  mutate(Count = case_when(is.na(Count) ~ 0, TRUE ~ Count)) %>% 
  left_join(covs, by = c('Route.ID', 'Route.Type'))
#...and view
glimpse(sobs_count)

###########################################################################################
#How many total species
sobs %>% 
  group_by(Species) %>% 
  reframe(Species, Observation = n()) %>% 
  distinct() %>% 
  print(n = Inf)

#How many total observations
sobs %>% 
  filter(Species != "NOBI") %>% 
  nrow()

#Table for Average number of observations between burned and unburned plots --------------
#Table for output
burn_v_ref <- sobs_count %>% 
  distinct(Year, Species) %>% 
  mutate(Mean.B = NA,
         Mean.R = NA,
         t = NA,
         p = NA,
         CI.lb = NA,
         CI.ub = NA)
#..and view
glimpse(burn_v_ref)

#For-loop to compare counts observed between B and R within each year
for(i in 1:nrow(burn_v_ref)){
  #Pull the info I want t=out of the data frame
  soi_count <- sobs_count %>% 
    group_by(Route.ID, Route.Type, Year, Species) %>% 
    reframe(Route.ID, Route.Type, Year, Species, Total.Count = sum(Count)) %>% 
    distinct(Route.ID, Route.Type, Year, Species, Total.Count) %>% 
    filter(Year == burn_v_ref$Year[i]) %>% 
    filter(Species == burn_v_ref$Species[i])
  #Compare species count within year
  t_test <- t.test(Total.Count ~ Route.Type, data = soi_count)
  #Pull what I need out of the 
  burn_v_ref$Mean.B[i] <- t_test$estimate[1]
  burn_v_ref$Mean.R[i] <- t_test$estimate[2]
  burn_v_ref$t[i] <- t_test$p.value
  burn_v_ref$p[i] <- t_test$p.value
  burn_v_ref$CI.lb[i] <- t_test$conf.int[1]
  burn_v_ref$CI.ub[i] <- t_test$conf.int[2]
}

#Highlight significan p-values 
burn_v_ref <- burn_v_ref %>% 
  mutate(Signif = case_when(p <= 0.05 ~ T,
                            p > 0.05 ~ F))

#View the output
burn_v_ref %>% 
  print(n = Inf)

#Export the tables
write.csv(burn_v_ref, "Data\\Outputs\\burn_v_ref_20240308.csv")


#Start plotting #######################################################################################

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
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn",
                                Route.Type == "R" ~ "Reference")) %>% 
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
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
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
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Total number of observations between burned and unburned plots ----------------------------
sobs_count %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn Grids",
                                Route.Type == "R" ~ "Reference Grids")) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>%
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
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn",
                                Route.Type == "R" ~ "Reference")) %>% 
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
  group_by(Route.ID, Route.Type, Year, Species) %>% 
  reframe(Route.ID, Route.Type, Year, Species, Count = sum(Count)) %>% 
  distinct(Route.ID, Route.Type, Year, Species, Count) %>% 
  mutate(Observed = case_when(Count == 0 ~ 0,
                              TRUE ~ 1)) %>% 
  group_by(Route.Type, Year, Species) %>% 
  reframe(Route.Type, Year, Species, 
          Prop.Obs = case_when(Route.Type == "Burn" ~sum(Observed)/31,
                               Route.Type == "Reference" ~ sum(Observed)/29)) %>% 
  distinct(Route.Type, Year, Species, Prop.Obs) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
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


#Species counts and fire year
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
  filter(Route.Type == "B") %>% 
  mutate(Years.Since.Fire = case_when(Year == "Y1" ~ 2022 - Fire.Year,
                                      Year == "Y2"   ~ 2023 - Fire.Year,
                                       Year == "Y3" ~ 2024 - Fire.Year)) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Count)) +
  geom_point(col = "red", size = 1) +
  geom_smooth(fill = "red", col = "red4") +
  labs(x = "Years Since Fire",
       y = "Number of Observations") +
  theme_bw()+
  theme(axis.title.x = element_text(size = 14, family = "sans"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, family = "sans"),
        strip.text = element_text(size= 12, family = "sans")) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))
#Species counts and fire sevarety 
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
  filter(Route.Type == "B") %>% 
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
  group_by(Route.ID, Species) %>% 
  reframe(Route.ID, Species, Mean.Count = mean(Count)) %>% 
  distinct(Route.ID, Species, Mean.Count) %>% 
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
  group_by(Route.ID, Species) %>% 
  reframe(Route.ID, Species, Mean.Count = mean(Count)) %>% 
  distinct(Route.ID, Species, Mean.Count) %>% 
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

#Spercies on burned plots at different elevations
sobs_count %>% 
  filter(Species %in% c("BRSP", "SATH", "GTTO",  #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Elevation = factor(case_when(Elevation < 1800 & Route.Type == "B"  ~ "LB",
                                      Elevation >= 1800 & Route.Type == "B" ~ "HB",
                                      Elevation < 1800 & Route.Type == "R" ~ "LR",
                                      Elevation >= 1800 & Route.Type == "R" ~ "HR"),
                               levels = c("LB", 
                                          "HB", 
                                          "LR",
                                          "HR"))) %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  ggplot(aes(x = Elevation, y = Count)) +
  geom_boxplot(aes(color = Elevation, fill = Elevation), color = "black") +
  scale_fill_manual(values = c("darkgoldenrod2", "darkorange2", "cadetblue", "darkslategray")) +
  labs(x = "Grid Type",
       y = "Average Number of Observations") +
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.position = "none",
        strip.text = element_text(size= 12, family = "sans")) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Combine grassland and shrubland species ##############################################################
grass_sp <- c("WEME", "HOLA", "VESP")
shrub_sp <- c("BRSP", "SATH", "SABS", "GTTO")

#Species counts and fire year ------------------------------------------------
sobs_count %>%
  filter(Species %in% c("BRSP", "SATH", "GTTO", "SABS", #Only the species that have enough observations
                        "VESP", "WEME", "HOLA")) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated Birds",
                                  Species %in% shrub_sp ~ "Sagebrush Associated Birds")) %>% 
  group_by(Route.ID, Route.Type, Fire.Year, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Route.ID, Route.Type, Fire.Year, Year, Visit, Species.Type, Total.Count) %>% 
  filter(Route.Type == "B") %>% 
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
  group_by(Route.ID, Shrub.Cover, Fire.Year, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Route.ID, Shrub.Cover, Fire.Year, Year, Visit, Species.Type, Total.Count) %>% 
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
  group_by(Route.ID, Route.Type, Fire.Year, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Route.ID,  Route.Type, Year, Visit, Species.Type, Total.Count) %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn Grids",
                                Route.Type == "R" ~ "Reference Grids")) %>% 
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
  facet_wrap(~Route.Type)

#Total number of observations between burned and unburned plots ----------------------------
sobs_count %>% 
  filter(Species %in% c(grass_sp, shrub_sp)) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated",
                                  Species %in% shrub_sp ~ "Sagebrush Associated")) %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn Grids",
                                Route.Type == "R" ~ "Reference Grids")) %>% 
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
  facet_wrap(~Route.Type)

#Average number of observations between low and high elevation  ------------------------
sobs_count %>% 
  filter(Route.Type == "B") %>% 
  mutate(Elevation = factor(case_when(Elevation < 1800 ~ "Below 1800m",
                                      Elevation >= 1800 ~ "Above 1800m"),
                            levels = c("Below 1800m", "Above 1800m"))) %>% 
  filter(Species %in% c(grass_sp, shrub_sp)) %>% 
  mutate(Species.Type = case_when(Species %in% grass_sp ~ "Grassland Associated",
                                  Species %in% shrub_sp ~ "Sagebrush Associated")) %>% 
  group_by(Route.ID, Route.Type, Elevation, Year, Visit, Species.Type) %>% 
  reframe(Total.Count = sum(Count)) %>% 
  distinct(Route.ID, Route.Type, Elevation, Year, Visit, Species.Type, Total.Count) %>% 
  mutate(Route.Type = case_when(Route.Type == "B" ~ "Burn Grids",
                                Route.Type == "R" ~ "Reference Grids")) %>% 
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

