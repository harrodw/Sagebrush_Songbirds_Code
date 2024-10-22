# Creator: Will Harrod
# Created: 10/07/2024
# Title: Preparing MTBS burn severity rasters

# 0) Clear environments and dowload packages #################################################
rm(list = ls())

#Load packages
library(tidyverse)
library(raster)
library(terra)
library(sf)
library(tmap)

# 1) Add existing spatial data ######################################################################

# Define the target CRS for NAD 1983 Zone 12N
target_crs <- CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

# Define the path to my existing spatial data
dat_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\Geoprocessing_Outputs\\"

# Add the study region polygon
study_region <- st_read(paste0(dat_path, "Study_Region.shp"))

# 2) Unzip the fire perimeters ########################################################################

# Set the directory containing the zipped files
zip_folder <- "C:/Users/willh/Downloads/mtbs"

# Set the directory where you want to extract the files
output_folder <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\mtbs_raw\\"

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Find all .zip files in the directory (including subdirectories)
zip_files <- list.files(path = zip_folder, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)

# Unzip each file
for (zip_file in zip_files) {
  # Define the output folder for each unzipped file (optional: create subfolders based on the .zip file's location)
  unzip_folder <- file.path(output_folder, tools::file_path_sans_ext(basename(zip_file)))
  
  # Create a directory for the unzipped files if it doesn't exist
  if (!dir.exists(unzip_folder)) {
    dir.create(unzip_folder, recursive = TRUE)
  }
  
  # Unzip the file into the folder
  unzip(zip_file, exdir = unzip_folder)
  
  message(paste("Unzipped:", zip_file, "to", unzip_folder))
}

message("All files have been unzipped.")

# 3) Assign where each dnbr, rdnbr, and fire perimeter shapefiles will go #############################################

# Set the directory containing the rasters
raster_folder <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\mtbs_raw\\"

# 3.1) dnbr files -----------------------------------------------------------------------------
# Find all dnbr files
dnbr_files <- list.files(path = raster_folder, pattern = "_dnbr.tif$", full.names = TRUE, recursive = TRUE)

# make an empty data frame
dnbr_df <- data.frame(File.Name = rep(NA, length(dnbr_files)),
                      Year =  rep(NA, length(dnbr_files)))# Find all dnbr files
dnbr_files <- list.files(path = raster_folder, pattern = "_dnbr.tif$", full.names = TRUE, recursive = TRUE)


# Assign the year and file path to a data frame
dnbr_df$File.Name <- dnbr_files
dnbr_df$Year <- str_extract(dnbr_files, "(?<=_)\\d{4}")

# View the dnbr files
glimpse(dnbr_df)
head(dnbr_df, n = 30)

# 3.2) rndbr files -----------------------------------------------------------------------------------
# Find all rdnbr files
rdnbr_files <- list.files(path = raster_folder, pattern = "_rdnbr.tif$", full.names = TRUE, recursive = TRUE)

# make an empty data frame
rdnbr_df <- data.frame(File.Name = rep(NA, length(rdnbr_files)),
                       Year =  rep(NA, length(rdnbr_files)))

# Assign the year and file path to a data frame
rdnbr_df$File.Name <- rdnbr_files
rdnbr_df$Year <- str_extract(rdnbr_files, "(?<=_)\\d{4}")

# View the dnbr files
glimpse(rdnbr_df)
head(rdnbr_df, n = 30)

# 3.3) fire perimeter shapefiles ------------------------------------------------------------------------------
# Find all perimeter files
perims_files <- list.files(path = raster_folder, pattern = "_bndy.shp$", full.names = TRUE, recursive = TRUE)

# make an empty data frame
perims_df <- data.frame(File.Name = rep(NA, length(perims_files)),
                        Year =  rep(NA, length(perims_files)))

# Assign the year and file path to a data frame
perims_df$File.Name <- perims_files
perims_df$Year <- str_extract(perims_files, "(?<=_)\\d{4}")

# View the dnbr files
glimpse(perims_df)
head(perims_df, n = 30)

# 4) Read in the burn layers and merge them by year ########################################

# file path to save the raster
save_path <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Spatial\\mtbs_cleaned\\"

# Define the years that we are examining
years <- sort(unique(perims_df$Year))
#...and view
print(years)

# start the loop at a certain year
# years <- years[which(years == 1999):37]

# export fire years
write.csv(years, paste0(save_path, "fire_years.csv"))

# Define how many years we have data for 
nyears <- length(years)
#...and view
print(nyears)

# Initialize a list to store merged rasters for each year
dnbr <- list(Year = rep(NA, length(years)),
             dnbr.raster = vector("list", length(years)),
             rdnbr.raster = vector("list", length(years)))

# Loop through each year and summarize burn sevarity
for (i in 1:nyears) {
  
  # collect 
  
  # In case I have issues with one year
  # i <- which(years == 2002)  
  
  # fire perimeters -----------------------------------------------------
  # Pull out a single year
  year <- years[i]
  
  # Define the fire perimeters in that year
  fires_year <- perims_df %>% 
    filter(Year == year)
  
  # Check if there were fires that year
  if (nrow(fires_year) == 0) {
    message(paste("No fires found for year:", year))
    next  # Skip to the next iteration
  }
  
  # Define the file paths to those shapefiles
  perims_files <- fires_year$File.Name
  
  # Check if perimeter files is empty
  if (length(perims_files) == 0) {
    message(paste("No dnbr files found for year:", year))
    next  # Skip to the next iteration
  }
  
  # read in all of the sf objects
  fires_many <- lapply(perims_files, function(file) {
    tryCatch(st_read(file), error = function(e) {
      message(paste("Error reading shapefile:", file, "-", e$message))
      return(NULL)  # Return NULL if there's an error
    })
  })
  
  # Merge all perimeter rows
  fires_single <- do.call(rbind, fires_many[!sapply(fires_many, is.null)])
  
  # Dissolve fire perimeters into one sf
  fires_mrg <- fires_single  %>%
    dplyr::select(Ig_Date, geometry) %>% 
    st_union() %>%
    st_make_valid() %>%
    st_cast("POLYGON") %>%
    st_as_sf() %>% 
    st_transform(crs = target_crs) 

  # Define the extent of the fires 
  reference_extent <- extent(fires_mrg)
  
  # dnbr rasters ------------------------------------------------------
  
  # # Define the dnbr burn sevarity layers in that year 
  # dnbr_year <- dnbr_df %>% 
  #   filter(Year == year)
  # 
  # # Define the file paths to those rasters
  # dnbr_files <- dnbr_year$File.Name
  # 
  # # Create a list of dnbr raster objects
  # dnbr_lg <- lapply(dnbr_files, function(file) {
  #   tryCatch(raster(file), error = function(e) {
  #     message(paste("Error reading raster file:", file, "-", e$message))
  #     return(NULL)  # Return NULL if there's an error
  #   })
  # })
  # 
  # # Remove any NULL rasters that failed to load
  # dnbr_lg <- dnbr_lg[!sapply(dnbr_lg, is.null)]
  # 
  # # Initialize a list to store clipped rasters
  # dnbr_ext <- vector("list", length(dnbr_lg))
  # 
  # # extend the dnbr rasters to match the fires from that year
  # for (j in 1:length(dnbr_lg)) {
  #   # Project the raster to the target CRS
  #   projected_raster <- projectRaster(dnbr_lg[[j]], crs = target_crs)
  #   
  #   # Extend the raster to match the reference extent
  #   extended_raster <- extend(projected_raster, reference_extent)
  #   message(paste0("extended dnbr raster from year ", year, ": ", j, " out of ", length(dnbr_lg)))
  #   
  #   # Save the rasters
  #   dnbr_ext[[j]] <- extended_raster 
  # }
  # 
  # # Use the first raster from that year as the reference raster
  # reference_ras <- dnbr_ext[[1]]
  # 
  # # Initialize a list to store clipped rasters
  # dnbr_res <- vector("list", length(dnbr_ext))
  # 
  # # Resample rasters
  # for (j in 1:length(dnbr_ext)) {
  #   # resample the raster
  #   resampled_raster <- resample(dnbr_ext[[j]], reference_ras, method = "bilinear")
  #   
  #   # Clip the extended raster to the fires
  #   dnbr_res[[j]] <- raster::mask(resampled_raster, fires_mrg)
  #   message(paste0("resampled and clipped dnbr raster from year ", year, ": ", j, " out of ", length(dnbr_ext)))
  # }
  # 
  #        
  # # Merge the rasters for the current year
  # if (length(dnbr_res) > 1) {
  #   # If there are multiple rasters, stack them first
  #   dnbr_stack <- raster::stack(dnbr_res)
  #   #then merge them
  #   dnbr_mrg <- terra::merge(dnbr_stack)
  # } else if (length(dnbr_res) == 1) {
  #   # If there is only one raster, assign it directly
  #   dnbr_mrg <- dnbr_res[[1]]
  # } else {
  #   # Handle the case where no rasters are available
  #   message(paste("Error merging dnbr raster in year:", year))
  # }
  # 
  # # Clip the combined raster to the study region
  # dnbr_clp <- raster::mask(dnbr_mrg, study_region)
  # 
  # # save the raster file
  # raster::writeRaster(dnbr_clp, filename = paste0(save_path, "dnbr_", year, ".tif"), overwrite = TRUE)
  # message(paste("successful dnbr raster created for year:", year, "with", nrow(fires_year), "fires found."))
  # 
  # # Remove the old rasters
  # remove(dnbr_lg, dnbr_ext, dnbr_res, dnbr_stack, dnbr_mrg, dnbr_clp)
  # 
  # rdnbr rasters ------------------------------------------------------

  # Define the rdnbr burn sevarity layers in that year
  rdnbr_year <- rdnbr_df %>%
    filter(Year == year)

  # Define the file paths to those rasters
  rdnbr_files <- rdnbr_year$File.Name

  # Create a list of rdnbr raster objects
  rdnbr_lg <- lapply(rdnbr_files, function(file) {
    tryCatch(raster(file), error = function(e) {
      message(paste("Error reading raster file:", file, "-", e$message))
      return(NULL)  # Return NULL if there's an error
    })
  })

  # Remove any NULL rasters that failed to load
  rdnbr_lg <- rdnbr_lg[!sapply(rdnbr_lg, is.null)]

  # Initialize a list to store clipped rasters
  rdnbr_ext <- vector("list", length(rdnbr_lg))

  # extend the rdnbr rasters to match the fires from that year
  for (j in 1:length(rdnbr_lg)) {
    # Project the raster to the target CRS
    projected_raster <- projectRaster(rdnbr_lg[[j]], crs = target_crs)

    # Extend the raster to match the reference extent
    extended_raster <- extend(projected_raster, reference_extent)
    message(paste0("extended rdnbr raster from year ", year, ": ", j, " out of ", length(rdnbr_lg)))

    # Save the rasters
    rdnbr_ext[[j]] <- extended_raster
  }

  # Use the first raster from that year as the reference raster
  reference_ras <- rdnbr_ext[[1]]

  # Initialize a list to store clipped rasters
  rdnbr_res <- vector("list", length(rdnbr_ext))

  # Resample rasters
  for (j in 1:length(rdnbr_ext)) {
    # resample the raster
    resampled_raster <- resample(rdnbr_ext[[j]], reference_ras, method = "bilinear")

    # Clip the extended raster to the fires
    rdnbr_res[[j]] <- raster::mask(resampled_raster, fires_mrg)
    message(paste0("resampled and clipped rdnbr raster from year ", year, ": ", j, " out of ", length(rdnbr_ext)))
  }

  # Merge the rasters for the current year
  if (length(rdnbr_res) > 1) {
    # If there are multiple rasters, stack them first
    rdnbr_stack <- raster::stack(rdnbr_res)
    #then merge them
    rdnbr_mrg <- terra::merge(rdnbr_stack)
  } else if (length(rdnbr_res) == 1) {
    # If there is only one raster, assign it directly
    rdnbr_mrg <- rdnbr_res[[1]]
  } else {
    # Handle the case where no rasters are available
    message(paste("Error merging rdnbr raster in year:", year))
  }

  # Clip the combined raster to the study region
  rdnbr_clp <- raster::mask(rdnbr_mrg, study_region)

  # save the raster file
  raster::writeRaster(rdnbr_clp, filename = paste0(save_path, "rdnbr_", year, ".tif"), overwrite = TRUE)
  message(paste("successful rdnbr raster created for year:", year, "with", nrow(fires_year), "fires found."))

  # Remove the old rasters
  remove(rdnbr_lg, rdnbr_res, rdnbr_stack, rdnbr_mrg, rdnbr_clp)
} # end overall loop
