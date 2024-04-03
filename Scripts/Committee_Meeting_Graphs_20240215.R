#Basic Graphs to show what I have to my committee 
#Will Harrod
#Created: 02/15/2024
#Start here ---------------------------------------------------------------------

#Add packages
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#add in the data --------------------------------------------------

#Add point count data
sobs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\sobs_data.csv")) %>% 
  dplyr::select(-X) #Remove the column that excel generated
#and view the data
glimpse(sobs)

#add covariates
#I'm going to start with only the route level covariates
covs <- tibble(read.csv("C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Sagebrush_Songbirds_Code\\Data\\Outputs\\route_summaries.csv")) %>% 
  dplyr::select(-X)
#View covariates
glimpse(covs)

#Transform into a table with each species observations by visit ----------------------------
#define relevant species
important_species <- c("BRSP", "SATH", "SABS", "GTTO",
                       "WEME", "HOLA", "VESP", "LASP", "GRFL")

#make a table of important species sightings by visit
sobs_count_0inf <- sobs %>% 
  filter(Species %in% important_species) %>% 
  group_by(Route.ID, Route.Type, Year, Visit, Date, Species) %>% 
  reframe(Route.ID, Route.Type, Year, Visit, Date, Species, Count = n()) %>% 
  distinct()
#...and view
glimpse(sobs_count_0inf)

#make a table of all possible species visit combinations so we get the zreo counts
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

#Start plotting --------------------------------------------------------------------------

#Define colors for boxplot
count_cols <- c("darkorange3", "deepskyblue1", "red3", "dodgerblue3")

#Average number of observations between burned and unburned plots 
sobs_count %>% 
  mutate(Route.Type = case_when(Route.ID %in% c("UT-B24", "UT-B25") ~ "B",
                                Route.ID %in% c("ID-B03") ~ "R",
                                TRUE ~ Route.Type)) %>% 
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
                          Year == "Y2" ~ "Year 2")) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Count, fill = Year.Type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual("",
    values = count_cols) +
  labs(y = "Average Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text()) +
  ylim(0, 75) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Total number of observations between burned and unburned plots 
sobs_count %>% 
  mutate(Route.Type = case_when(Route.ID %in% c("UT-B24", "UT-B25") ~ "B",
                                Route.ID %in% c("ID-B03") ~ "R",
                                TRUE ~ Route.Type)) %>% 
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
                          Year == "Y2" ~ "Year 2")) %>%
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Count, fill = Year.Type)) +
  geom_col() +
  theme_bw() +
  scale_fill_manual("",
                    values = count_cols) +
  labs(y = "Total Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text()) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

sobs %>%
  mutate(Route.Type = case_when(Route.ID %in% c("UT-C24", "UT-C25") ~ "B",
                                Route.ID %in% c("ID-B03", "UT-C30") ~ "R", 
                                TRUE ~ Route.Type)) %>% 
  distinct(Route.ID, Route.Type, Year) %>% 
  arrange(Route.Type, Year, Route.ID) %>% 
  count(Route.Type, Year) %>%
  print(n = Inf)

#proportion of plots with at least one observation 
sobs_count %>% 
  mutate(Route.Type = case_when(Route.ID %in% c("UT-C24", "UT-C25") ~ "B",
                                Route.ID %in% c("ID-B03", "UT-C30") ~ "R",
                                TRUE ~ Route.Type)) %>% 
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
                          Year == "Y2" ~ "Year 2")) %>%
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
  scale_fill_manual("",
                    values = count_cols) +
  labs(y = "Proportion of routes with at least one observation") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text()) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Table for Average number of observations between burned and unburned plots --------------
#Table for output
burn_v_ref <- sobs_count %>% 
  mutate(Route.Type = case_when(Route.ID %in% c("UT-C24", "UT-C25") ~ "B",
                                Route.ID %in% c("ID-B03", "UT-C30") ~ "R",
                                TRUE ~ Route.Type)) %>% 
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
  mutate(Route.Type = case_when(Route.ID %in% c("UT-C24", "UT-C25") ~ "B",
                                Route.ID %in% c("ID-B03", "UT-C30") ~ "R",
                                TRUE ~ Route.Type)) %>% 
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

#View the output
burn_v_ref %>% 
  print(n = Inf)

#Export the tables
write.csv(burn_v_ref, "Data\\Outputs\\burn_v_ref_20240308.csv")


#Plot average number of observations between burned and unburned plots  ---------------------------
burn_v_ref %>% 
  pivot_longer(cols = c(Mean.B, Mean.R),
               values_to = "Mean.Count",
               names_to = "Route.Type") %>% 
  mutate(Route.Type = case_when(Route.Type == "Mean.B" ~ "Burn",
                                Route.Type == "Mean.R" ~ "Reference")) %>% 
  select(Year, Route.Type, Species, Mean.Count) %>% 
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
                          Year == "Y2" ~ "Year 2")) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " ")) %>% 
  ggplot(aes(x = Year.Type, y = Mean.Count, fill = Year.Type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual("",
                    values = count_cols) +
  labs(y = "Average Number of Observations") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text()) +
  ylim(0, 75) +
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

sobs_count %>% 
  mutate(Route.Type = case_when(Route.ID %in% c("UT-C24", "UT-C25") ~ "B",
                                Route.ID %in% c("ID-B03", "UT-C30") ~ "R",
                                TRUE ~ Route.Type)) %>% 
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
                          Year == "Y2" ~ "Year 2")) %>% 
  mutate(Year.Type = paste(Year, Route.Type, sep = " "))


#Species counts and shrub cover
sobs_count %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  mutate(Shrub.Cover = Shrub.Cover) %>%
  ggplot(aes(x = Shrub.Cover, y = Count)) +
  geom_point(col = "cadetblue4", size = 1) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Percent Shrub Cover",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))

#Species counts and sagebrush cover
sobs_count %>% 
  # mutate(Sagebrush.Cover = log(Sagebrush.Cover)) %>% 
  ggplot(aes(x =  Sagebrush.Cover, y = Count)) +
  geom_point(col = "cadetblue4", size = 1) +
  geom_smooth(col = "dodgerblue3") +
  labs(x = "Percent Sagebrush Cover",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~Species) 

#Species counts and ruggedness
sobs_count %>% 
  mutate(Species = case_when(Species == "BRSP" ~ "Brewer's Sparrow",
                             Species == "SATH" ~ "Sage Thrasher",
                             Species == "SABS" ~ "Sagebrush Sparrow",
                             Species == "GTTO" ~ "Green-Tailed Towhee",
                             Species == "VESP" ~ "Vesper Sparrow",
                             Species == "WEME" ~ "Western Meadowlark",
                             Species == "HOLA" ~ "Horned Lark",
                             Species == "GRFL" ~ "Gray Flycatcher",
                             Species == "LASP" ~ "Lark Sparrow")) %>% 
  mutate(Shrub.Cover = TRI) %>%
  ggplot(aes(x = Shrub.Cover, y = Count)) +
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
  mutate(Years.Since.Fire = 2023 - Fire.Year) %>% 
  # mutate(Years.Since.Fire = log(Years.Since.Fire)) %>% 
  ggplot(aes(x = Years.Since.Fire, y = Count)) +
  geom_point(col = "red3", size = 1) +
  geom_smooth(fill = "darkorange1", col = "red3") +
  labs(x = "Years Since Fire",
       y = "Number of Observations") +
  theme_bw()+
  facet_wrap(~factor(Species, levels = c("Brewer's Sparrow", "Sage Thrasher", 
                                         "Sagebrush Sparrow", "Green-Tailed Towhee",
                                         "Gray Flycatcher","Vesper Sparrow", 
                                         "Western Meadowlark", "Horned Lark", 
                                         "Lark Sparrow")))
#Species counts and fire sevarety 
fire_sev_cols <- c("yellow1", "darkorange1", "red1", "red4")
sobs_count %>% 
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

#compare two species to each other
sobs_count %>% 
  group_by(Route.ID, Species) %>% 
  reframe(Route.ID, Species, Mean.Count = mean(Count)) %>% 
  distinct(Route.ID, Species, Mean.Count) %>% 
  pivot_wider(names_from = Species, values_from = Mean.Count) %>% 
  ggplot(aes(x = WEME, y = SATH)) +
  geom_point() +
  geom_smooth()

#Fake date ----------------------------------------------------------
#Generate geometric data
min_dat <- tibble(x = seq(from = 1, to = 6, by = 0.01)) %>% 
  mutate(y = 25 + x^3)

#Plot these data
min_dat %>% 
  ggplot(aes(x = x, y = 0.06 + y/max(y))) +
  geom_point() +
  ylim(0, 1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13)) +
  labs(x = "Minute",
       y = "Probability of availability")

#simulate a half normal detection function
#Generate geometric data
dist_dat <- tibble(x = runif(n =10000, min = 0, max = 150),
                   y = exp(-0.02 * x))

#Plot these data
dist_dat %>% 
  ggplot(aes(x = x, y = y/max(y))) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  xlim(0, 150) + 
  labs(x = "Distance (m)",
       y = "Probability of detection")


