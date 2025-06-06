library(tidyverse)
library(dplyr)
library(reshape2)
library(usethis)
library(devtools)
library(TMB)
library(patchwork) #THIS IS NEEDED FOR smsR to work!
library(abind) #THIS IS NEEDED FOR smsR to work!
library(smsR)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(sf)
library(scales)

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR")
source("C:/Users/chris/Desktop/DTU/R/ORIG/myfunctions.R")

# Step 1: Load the two CSV files and clean up the data
#The reason why I am using "square_to_sandeel_areas_WKSAND16" is because the vgt file does not contain 
#area information e.g. 1r, 2r, 3r etc. I am matching the area data with this existing data sheet to
#the vgt data to easily assign the area information to the vgt datasheet.
vessel_data <- read.csv("DATA/TBS_vrd_vgt_edit.csv", na.strings = c("", "NA")) %>%
  filter_all(all_vars(!is.na(.))) %>%  # Remove rows with NA
  filter_all(all_vars(trimws(.) != "")) %>%  # Remove rows with empty strings or spaces
  filter_all(all_vars(trimws(.) != ".")) %>%  # Remove rows with cells containing "     ."
  filter(year >= 2010 & year <= 2024, )  # Apply the year filter

key_data <- read.csv("DATA/square_to_sandeel_areas_WKSAND16.csv", na.strings = c("", "NA")) %>%
  filter_all(all_vars(!is.na(.))) %>%  # Remove rows with NA
  filter_all(all_vars(trimws(.) != "")) %>%  # Remove rows with empty strings or spaces
  filter_all(all_vars(trimws(.) != "."))  # Remove rows with cells containing "     ."

# Step 2: Merge the two datasets by matching the 'square' column
merged_data <- vessel_data %>%
  left_join(key_data %>% select(square, area), by = "square") %>%
  filter_all(all_vars(!is.na(.))) %>%  # Remove rows with NA
  filter_all(all_vars(trimws(.) != ""))  # Remove rows with empty strings or spaces

################################################################
############### FILTER, RENAME AND ADD ICES REC'S ##############
################################################################

vessel_data <- merged_data %>%
  rename(Year = year, VLength = oal, Square = square, GearType = redskb) %>%
  filter(area %in% c("1r")) %>%
  filter_all(all_vars(str_trim(.) != ".")) %>%   # Remove rows where any column has the value "."
  na.omit()  # Remove rows with NA values

# Step 4: Convert squares to lat/lon coordinates
coordinates <- ices.rect(vessel_data$Square)

# Step 5: Add the lat/lon coordinates to the vessel_data data frame
vessel_data <- bind_cols(vessel_data, coordinates)

################################################################
############## MERGE CATCHES WITH THE 3 GROUPS #################
################################################################

# Step 6: Load the EEZ shapefile
eez_data <- st_read('C:/Users/chris/Desktop/DTU/R/ORIG/DATA/SHAPEFILES/EEZ/EEZ_Land_v3_202030.shp', ) %>%
  filter(UNION %in% c('Denmark','Germany','Netherlands','Sweden','United Kingdom', 'Norway', 'France', 'Belgium', 'Jersey', 'Guernsey'))

# Filter for the United Kingdom EEZ
uk_eez <- eez_data %>% filter(UNION == "United Kingdom")

# Convert vessel_data to sf object for spatial operations
vessel_sf <- st_as_sf(vessel_data, coords = c("lon", "lat"), crs = st_crs(eez_data))

# Step 7: Classify vessel data points into the three groups

# Identify points inside the UK EEZ
points_in_uk_eez <- st_intersection(vessel_sf, uk_eez)

# Define a latitude threshold to split northern and southern stocks
latitude_threshold <- 54.5 # Changed to simulate more realistic group based on water currents and sand banks

# Identify points outside the UK EEZ
#st_intersects(): This function, from the sf package, checks whether the geometries in one sf object
#(in this case, vessel_sf) intersect with the geometries in another sf object (in this case, uk_eez).
#sparse = FALSE: When sparse = FALSE, the function returns a dense logical matrix where each element 
#is TRUE or FALSE, indicating whether each point in vessel_sf intersects with the UK EEZ. 
#The resulting matrix has TRUE where there is an intersection and FALSE where there is no intersection.
points_outside_uk_eez <- vessel_sf[!st_intersects(vessel_sf, uk_eez, sparse = FALSE), ]

# Split the non-UK points into northern and southern groups
northern_coords <- points_outside_uk_eez %>% filter(st_coordinates(.)[,2] >= latitude_threshold)
southern_coords <- points_outside_uk_eez %>% filter(st_coordinates(.)[,2] < latitude_threshold)

# Step 8: Add a "group" column to the vessel_data based on their location
vessel_data$group <- NA

# For UK EEZ points
vessel_data$group[st_intersects(vessel_sf, uk_eez, sparse = FALSE)] <- "Sub-Area 2"

# For Northern non-UK points
vessel_data$group[st_coordinates(vessel_sf)[,2] >= latitude_threshold & !st_intersects(vessel_sf, uk_eez, sparse = FALSE)] <- "Sub-Area 3"

# For Southern non-UK points
vessel_data$group[st_coordinates(vessel_sf)[,2] < latitude_threshold & !st_intersects(vessel_sf, uk_eez, sparse = FALSE)] <- "Sub-Area 1"

# Step 9: Merge with catch data and summarize by group
catch_summary <- vessel_data %>%
  group_by(group) %>%
  summarise(total_catch = sum(vgt, na.rm = TRUE))

# View the summary
print(catch_summary)

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")

catch_by_year <- vessel_data %>%
  filter(area == "1r") %>%  # Filter for area "1r" only
  group_by(Year, group) %>%
  summarise(total_catch = sum(vgt/1000, na.rm = TRUE)) %>%
  ungroup()

#CHANGE THE ORDER OF THE AREAS IN THE DATAFRAME FOR PLOTTING WITH UK FIRST
#I SET THIS UP SPECIFICALLY FOR PLOTTING THIS VGT AGAINST THE MODEL catch.save.age output
catch_by_year$group <- factor(catch_by_year$group,
                            levels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"))

