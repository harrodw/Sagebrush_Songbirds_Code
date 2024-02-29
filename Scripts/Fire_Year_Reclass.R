#Reclassifying fires 
#Code coppied from an older script
#Will Harrod
#Created 02/05/2024

#Code ------------------------------------------------------------------------------
#Change to years since fire
burn_count_data <- burn_count_data %>%
  mutate(Years.Since.Fire = (2022 - Fire.Year))
#Looks Good
unique(burn_count_data$Years.Since.Fire)

#Change to classes
burn_count_data <- burn_count_data %>%
  mutate(Years.Since.Fire.Class = factor(case_when(Years.Since.Fire < 5
                                                   ~ "0-4",
                                                   Years.Since.Fire < 10 &  
                                                     Years.Since.Fire >= 5
                                                   ~ "5-9",
                                                   Years.Since.Fire < 15 &  
                                                     Years.Since.Fire >= 10
                                                   ~ "10-14",
                                                   Years.Since.Fire < 20 &  
                                                     Years.Since.Fire >= 15
                                                   ~ "15-19",
                                                   Years.Since.Fire >= 20
                                                   ~ "20 or More"),
                                         levels = c("0-4", "5-9", "10-14", "15-19", "20 or More")))