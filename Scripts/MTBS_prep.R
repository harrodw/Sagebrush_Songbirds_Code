# Creator: Will Harrod
# Created: 10/07/2024
# Title: Preparing MTBS burn severity rasters

# 0) Clear environments and dowload packages #################################################
rm(list = ls())

#Load packages
library(tidyverse)
library(raster)
library(sf)
library(tmap)

# 1) Add existing spatial data ######################################################################

# Define the target CRS for NAD 1983 Zone 12N
target_crs <- CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

# Define the path to my existing spatial data
dat_path <- "C:/Users/willh/OneDrive/Documents/USU/SOBs/Data/Spatial/Geoprocessing_Outputs/"

# Add the fire perimeter polygons and pull out the nessesary information
fires <- st_read(paste0(dat_path, "Fire_Perimeters.shp")) %>% 
  mutate(Year = lubridate::year(Ig_Date),
         Incid.Name = Incid_Name) %>% 
  dplyr::select(Incid.Name, Year, geometry) %>% 
  filter(Year >= 1980) %>% 
  st_transform(target_crs)

# Add the study region polygon
study_region <- st_read(paste0(dat_path, "Study_Region.shp"))

# 2) Unzip the fire perimeters ########################################################################

# Set the directory containing the zipped files
zip_folder <- "C:/Users/willh/Downloads/mtbs"

# Set the directory where you want to extract the files
output_folder <- "C:/Users/willh/OneDrive/Documents/USU/SOBs/Data/Spatial/MTBS"

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

# 3) Define the years for each burn layer #########################################################

# Set the directory containing the rasters
raster_folder <- "C:/Users/willh/OneDrive/Documents/USU/SOBs/Data/Spatial/mtbs_raw"

# Find all dnbr files
dnbr_files <- list.files(path = raster_folder, pattern = "_dnbr.tif$", full.names = TRUE, recursive = TRUE)

# Make a data frame for dnbr file aths and their associated years
dnbr_df <- data.frame(File.Name = rep(NA, length(dnbr_files)),
                      Year = rep(NA, length(dnbr_files)))

# Loop through each dnbr file and extract the year from the folder name
for (i in 1:length(dnbr_files)) {

  #Pull out one file path
  file <- dnbr_files[i]
  
  # Extract the folder name, which contains the ID and year (e.g., id4199711370020000818)
  folder_name <- basename(dirname(dnbr_files))
  
  # Extract the year from the folder name (assumes the last four digits are the year)
  year <- as.numeric(substr(folder_name, nchar(folder_name) - 7, nchar(folder_name) - 4))
  
  # Store the raster and associated year in a list
  dnbr_df$File.Name <- file
  dnbr_df$Year <- year
  }


# Find all rdnbr files
rdnbr_files <- list.files(path = raster_folder, pattern = "_rdnbr.tif$", full.names = TRUE, recursive = TRUE)


# View the new data frames
glimpse(dnbr_df)
glimpse(rdnbr_df)


# 4) Read in the burn layers and merge them by year ########################################

# Define the years that we are examining
years <- sort(unique(dnbr_df$Year))
#...and view
print(years)

# Initialize a list to store merged rasters for each year
dnbr <- list(Year = years,
             dnbr.raster = vector("list", length(years)))

# Function to check if a raster has non-NA values
drop_na_ras <- function(ras) {
  if (is.null(ras)) {
    return(FALSE)  # Return FALSE for NULL rasters
  }
  return(!all(is.na(values(ras))))  # Return TRUE if not all values are NA
}

# Define a reference extent from the study region
reference_extent <- extent(study_region)

# file path to save the raster
save_path <- "C:\\Users\\willh\\OneDrive\\Documents\\USU\\SOBs\\Data\\Spatial\\mtbs_cleaned\\"

# Loop through each year and summarize burn sevarity
for (i in 1:length(years)) {
  # In case I have issues with one year
  # i <- which(years == 1999)

  # Pull out a single year
  year <- years[i]
  
  # Define the fire perimeters in that year
  fires_year <- fires %>% 
    filter(Year == year)
  
  # Check if fires_year is empty
  if (nrow(fires_year) == 0) {
    message(paste("No fires found for year:", year))
    next  # Skip to the next iteration
  }
  
  # Define the burn severity layers in that year 
  burn_sev_year <- dnbr_df %>% 
    filter(Year == year)
  
  # Define the file paths to those rasters
  dnbr_files <- burn_sev_year$File.Name
  
  # Check if dnbr_files is empty
  if (length(dnbr_files) == 0) {
    message(paste("No dnbr files found for year:", year))
    next  # Skip to the next iteration
  }
  
  # Create a list of raster objects
  dnbr_lg <- lapply(dnbr_files, function(file) {
    tryCatch(raster(file), error = function(e) {
      message(paste("Error reading raster file:", file, "-", e$message))
      return(NULL)  # Return NULL if there's an error
    })
  })
  
  # Remove any NULL rasters that failed to load
  dnbr_lg <- dnbr_lg[!sapply(dnbr_lg, is.null)]
  
  # Initialize a list to store clipped rasters
  dnbr_ext <- vector("list", length(dnbr_lg))

  # Clip the dnbr rasters to the fires from that year
  for (j in 1:length(dnbr_lg)) {
    # Project the raster to the target CRS
    projected_raster <- projectRaster(dnbr_lg[[j]], crs = target_crs)
    
    # Extend the raster to match the reference extent
    extended_raster <- extend(projected_raster, reference_extent)
    message(paste0("extended raster from year ", year, ": ", j, " out of ", length(dnbr_lg)))
    
    # Save the rasters
    dnbr_ext[[j]] <- extended_raster 
  }
  
  # Use the first raster from that year as the reference raster
  reference_ras <- dnbr_ext[[1]]
  
  # Initialize a list to store clipped rasters
  dnbr_res <- vector("list", length(dnbr_lg))
  
  # Resample rasters
  for (j in 1:length(dnbr_lg)) {
    # resample the raster
    resampled_raster <- resample(dnbr_ext[[j]], reference_ras)
    
    # Clip the extended raster to the fires
    dnbr_res[[j]] <- raster::mask(resampled_raster, fires_year)
    message(paste0("resampled and clipped raster from year ", year, ": ", j, " out of ", length(dnbr_lg)))
  }
  
  # Remove rasters that are completely NA
  dnbr_values <- dnbr_res[sapply(dnbr_res, drop_na_ras)]

  # Check if dnbr_sm is empty after filtering
  if (length(dnbr_values) == 0) {
    message(paste("No valid clipped rasters for year:", year))
    next  # Skip to the next iteration
  }
  
  # Merge the rasters for the current year
  if (length(dnbr_values) > 1) {
  dnbr_mrg <- do.call(raster::merge, dnbr_values)
  } else if(length(dnbr_values) == 1){
    # no need to merge if there's only one raster
    dnbr_mrg <- dnbr_values[[1]]
  } else{
    message(paste("error merging raster in year:", year))
  }
  
  # save the raster file
  raster::writeRaster(dnbr_mrg, filename = paste0(save_path, "burn_sev_", year, ".tif"), overwrite = TRUE)
  
  # save it as a data frame
  if(any(!is.na(values(dnbr_mrg)))){
  dnbr$dnbr.raster[[i]] <- dnbr_mrg
  dnbr$year <- year
  } else{
    message(paste("no fires for year:", year))
  }
  
  # Show how many clipped rasters were created for this year
  message(paste("successful raster created for year:", year, "with", nrow(fires_year), "fires found."))
  
} # end overall loop

# View the output 
dnbr
