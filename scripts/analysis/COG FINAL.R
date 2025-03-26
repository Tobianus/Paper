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
vessel_data$group[st_intersects(vessel_sf, uk_eez, sparse = FALSE)] <- "UK EEZ"

# For Northern non-UK points
vessel_data$group[st_coordinates(vessel_sf)[,2] >= latitude_threshold & !st_intersects(vessel_sf, uk_eez, sparse = FALSE)] <- "EU North"

# For Southern non-UK points
vessel_data$group[st_coordinates(vessel_sf)[,2] < latitude_threshold & !st_intersects(vessel_sf, uk_eez, sparse = FALSE)] <- "EU South"

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

# CHANGE THE ORDER OF THE AREAS IN THE DATAFRAME FOR PLOTTING WITH UK FIRST
#I SET THIS UP SPECIFICALLY FOR PLOTTING THIS VGT AGAINST THE MODEL catch.save.age output
#catch_by_year$group <- factor(catch_by_year$group,
#levels = c("UK EEZ", "EU North", "EU South"))

base_size <- 16

# Step 10: Plot the total catches over time for the three groups
p <- ggplot(catch_by_year, aes(x = Year, y = total_catch, color = group)) +
  geom_line(size = 1) +  # Plot lines for each group
  labs(title = "Total Catches (vgt) Over Time",
       x = "Year",
       y = "Total Catch (vgt)",
       color = "Group") +
  
  scale_color_manual(values = c("UK EEZ" = "#35465A", "EU North" = "#CC3300", "EU South" = "#008000")) +  # Custom colors for groups
  # Use scale_y_continuous with label_number_si to format numbers in hundreds of thousands
  #scale_y_continuous(labels = label_number(scale = 1e-5, suffix = "T", accuracy = 1)) +
  # Adjust the x-axis to control the number of years shown
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +  # Shows every 2 years from 2010 to 2024
  theme(
    plot.title = element_text(size = base_size * 1.2, face = "bold"),
    axis.title = element_text(size = base_size),
    axis.text = element_text(size = base_size * 0.8),
    legend.title = element_text(size = base_size),
    legend.text = element_text(size = base_size * 0.8),
    legend.position = "bottom",                # Move the legend to the bottom
    legend.direction = "horizontal",           # Make the legend items display in a horizontal line
    strip.text = element_text(size = base_size * 1.1),
    panel.background = element_rect(fill = "#F0F2F2"),
    plot.background = element_rect(fill = "white")
  ) +
  guides(color = guide_legend(nrow = 1))

# Save the facet plot
ggsave("Total Catches (vgt) Over Time.png", plot = p, dpi = 300, width = 12, height = 8)


##################################### CHECKING GRAPH #######################################

getwd()
setwd("C:/Users/chris/Desktop/DTU/R/ORIG")
source("C:/Users/chris/Desktop/DTU/R/ORIG/myfunctions.R")

###########################################################
############## READ IN F AREA MAPS (1R, 2R, 4) ############
###########################################################

# Read the shapefile and filter for rows and columns covering labels 1r, 2r, and 4
ices_rec <- st_read('DATA/SHAPEFILES/ICES_REC/ICES_Statistical_Rectangles_Eco.shp', 
                    options = "ENCODING=UTF-8") %>%
  mutate(group = case_when(
    ICESNAME %in% c('36E9','37E9','34F0','35F0','36F0','37F0','38F0','39F0','40F0','31F1','32F1','33F1','34F1','35F1','36F1','37F1','38F1','39F1','40F1','31F2','32F2','33F2','34F2','35F2','36F2','37F2','38F2','39F2','40F2','31F3','32F3','33F3','34F3','35F3','36F3','37F3','38F3','39F3','40F3','32F4','33F4','34F4','35F4','36F4','37F4','38F4','39F4','40F4','41F4','34F5','35F5','36F5','37F5','38F5','39F5','40F5','41F5','35F6','36F6','37F6') ~ "1r",  # Light Cyan Group
    #ICESNAME %in% c('38F6','39F6','40F6','41F6','42F6','35F7','36F7','37F7','38F7','39F7','40F7','41F7','42F7','43F7','35F8','36F8','37F8','38F8','39F8','40F8','41F8','42F8','43F8','42F9','43F9','44F9','44G0','45G0','45G1','46G1') ~ "2r",  # Light Yellow Group
    #ICESNAME %in% c('40E6','41E6','44E6','45E6','46E6','40E7','41E7','42E7','43E7','44E7','45E7','46E7','38E8','39E8','40E8','41E8','42E8','43E8','44E8','45E8','46E8','38E9','39E9','40E9','41E9','42E9','43E9','44E9','45E9','46E9','41F0','42F0','43F0','44F0','45F0','46F0') ~ "4",   # Light Pink Group
    TRUE ~ NA_character_  # Assign NA to rows outside these groups
  )) %>%
  filter(!is.na(group))

###########################################################
#################### READ IN SAND BANKS ###################
###########################################################

# Step 1: Load the BANKS RData file
load("DATA/pol.RData")
pol_df <- data.frame()
for (p in 1:length(pol)) {
  print(pol[p])
  Bank_pols <-  cbind(pol[[p]], names(pol)[p])
  pol_df <- rbind(pol_df, Bank_pols) 
}
names(pol_df)[3] <- c("BankID")

###########################################################
################### READ IN DOGGER BANK ###################
###########################################################

dogger <- st_read('DATA/SHAPEFILES/DOGGER 2019/Natura2000_end2019_shp/Natura2000_end2019_epsg3035.shp', ) %>% 
  filter(SITECODE %in% c('DE1003301', 'UK0030352', 'NL2008001'))

###########################################################
################### READ IN ICES SQAURES ##################
###########################################################

ices <- (st_read('DATA/SHAPEFILES/ICES_AREAS/ICES_Areas_20160601_cut_dense_3857.shp', options = "ENCODING=UTF-8", ) %>%
           filter(SubArea %in% c('4'), Division %in% c('a','b','c')))

###########################################################
###################### READ IN EEZ'S ######################
###########################################################

eez <- st_read('DATA/SHAPEFILES/EEZ/EEZ_Land_v3_202030.shp', ) %>%
  filter(UNION %in% c('Denmark','Germany','Netherlands','Sweden','United Kingdom', 'Norway', 'France', 'Belgium', 'Jersey', 'Guernsey'))

###########################################################
################ READ IN COUNTRY BOUNDARIES ###############
###########################################################

country_boarders <- st_read('DATA/SHAPEFILES/EU COUNTRIES/Europe_SWAsia.shp', ) %>% 
  filter(NAME %in% c('DENMARK', 'GERMANY', 'NETHERLANDS', 'SWEDEN', 'UNITED KINGDOM', 'NORWAY', 'FRANCE', 'BELGIUM', 'JERSEY (UK)', 'GUERNSEY (UK)'))

###########################################################
############### FILTER OUT CRAP I DON'T NEED ##############
###########################################################

dogger <- dogger[c(2,7)]
ices <- ices[c(4,5, 11)]
country_boarders <- country_boarders[c(1,8)]
eez <- eez[c(1,31)]

sf_use_s2(FALSE)

###########################################################
################## MAKE GEODETIC THE SAME #################
###########################################################

ices <- st_transform(ices, crs = 4326)
eez <- st_transform(eez, crs = 4326)
country_boarders <- st_transform(country_boarders, crs = 4326)
ices_rec <- st_transform(ices_rec, crs = 4326)

###########################################################
################# CROP MAP FOR LOAD SPEED #################
###########################################################

cropped_ices <- st_crop(ices, xmin = -3, xmax = 11, ymin = 51, ymax = 59)
cropped_eez <- st_crop(eez, xmin = -3, xmax = 11, ymin = 51, ymax = 59)
cropped_country_boarders <- st_crop(country_boarders, xmin = -3, xmax = 11, ymin = 51, ymax = 59)
cropped_ices_rec <- st_crop(ices_rec, xmin = -3, xmax = 11, ymin = 51, ymax = 59)
pol_df_cropped <- pol_df %>% filter(x >= -3 & x <= 11, y >= 51 & y <= 59)
vessel_data_cropped <- vessel_data %>% filter(lon >= -3 & lon <= 11 & lat >= 51 & lat <= 59)

###############################################
################## MAPS END ###################
###############################################


############################################/////////////////##################################ggplot(cog_per_year_area, aes(x = Year)) +

labels_df <- data.frame(
  area = c("1r"),
  x = c(3.5),  # Longitude coordinates for each label
  y = c(54)  # Latitude coordinates for each label
)

#SUM VESSEL_DATA FOR PLOTTING

# Step 7: Calculate the center of gravity per year and per area
cog_per_year_area_vgt <- vessel_data %>%
  filter(area == "1r") %>%  # Filter for area "1r"
  mutate(area = "CoG") %>%  # Rename "1r" to "CoG"
  group_by(Year, area) %>%
  summarize(
    lon_cog = sum(lon * vgt) / sum(vgt),
    lat_cog = sum(lat * vgt) / sum(vgt)
  )

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/PLOTS/CATCHES")


#################### FACET FOR 2010 TO 2024 ######################

p <- ggplot() +
  # Plot ICES AREAS, EEZ, and country borders
  geom_sf(data = cropped_ices_rec, aes(fill = group), fill = "#c6e9e3", size = 0.5, alpha = 0.8, linewidth = 0.1, show.legend = FALSE) +
  
  # Plot the sand banks without adding to legend
  geom_polygon(data = pol_df_cropped, aes(x, y, group = BankID), fill = "#c2bdb9", alpha = 0.8, show.legend = FALSE) +
  
  # Plot EEZ, country borders, and Dogger Bank without adding to legend
  geom_sf(data = cropped_eez, size = 0.5, fill = NA, show.legend = FALSE) +
  geom_sf(data = cropped_country_boarders, fill = "#219175", linewidth = 0.1, show.legend = FALSE) +
  geom_sf(data = dogger, fill = "#c2bdb9", alpha = 0.10, show.legend = FALSE, linetype = "dashed") +
  
  # Plot center of gravity points for each year and area
  #geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch), alpha = 0.4) +  # Plot catch points with size based on total catch
  geom_point(data = cog_per_year_area_vgt, aes(x = lon_cog, y = lat_cog, fill = area), size = 1) +
  #geom_density_2d(data = catch_by_year, aes(x = lon, y = lat), color = "black", size = 0.3) +
  #geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch, shape = factor(Year))) +
  
  #scale_color_manual(values = c("UK EEZ" = "#f8766d", "EU North" = "#00ba38", "EU South" = "#619cff")) + # Custom colors for groups
  labs(title = "Center of Gravity for SA 1 (2010-2024)",
       x = "Longitude",
       y = "Latitude",
       fill = "Center of Gravity") + # Ensure the legend titles are explicit
  
  facet_wrap(~Year, ncol = 5) +
  theme(
    plot.title = element_text(size = 7, face = "bold"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.title = element_text(size = 7, face = "bold"),
    axis.text = element_text(size = 6, face = "bold", angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black"),
    
    # Legend positioning
    legend.position = "bottom",## <- here
    legend.direction = "horizontal",   # Titles and attributes stay in a single row
    legend.box = "vertical",           # Ensures stacked layout of different legend groups
    
    # Reduce spacing & padding
    legend.spacing.x = unit(0, 'cm'),  # Reduce horizontal spacing between attributes
    legend.spacing.y = unit(0, 'cm'),  # Reduce vertical spacing between rows
    legend.margin = margin(0, 0, 0, 0),   # Removes outer legend box padding
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 6, face = "bold"),  
    legend.text = element_text(size = 6),
    #scale_x_continuous(breaks = seq(min(cog_per_year_area_vgt$), max(cog_per_year_area_vgt$), by = 3)),
    # Remove white space
    plot.margin = margin(0, 0, 0, 0)) +  # Eliminates all margins
  
  guides(
    color = guide_legend(override.aes = list(size = 1)),
    size = guide_legend(override.aes = list(size = c(0.5, 5, 10)))) +
    coord_sf(expand = FALSE)

ggsave("Center of Gravity for SA 1 (2010-2024).png", plot = p, dpi = 600, width = 6, height = 5)

#################### FACET FOR 2023 TO 2024 ######################
################# REMEMBER TO CHANGE THE YEARS ON THE INITIAL DATA FEED ################## 

p <- ggplot() +
  # Plot ICES AREAS, EEZ, and country borders
  geom_sf(data = cropped_ices_rec, aes(fill = group), fill = "#c6e9e3", size = 0.5, alpha = 0.8, show.legend = FALSE) +
  
  # Plot the sand banks without adding to legend
  geom_polygon(data = pol_df_cropped, aes(x, y, group = BankID), fill = "#c2bdb9", alpha = 0.6, show.legend = FALSE) +
  
  # Plot EEZ, country borders, and Dogger Bank without adding to legend
  geom_sf(data = cropped_eez, size = 0.5, fill = NA, show.legend = FALSE) +
  geom_sf(data = cropped_country_boarders, fill = "#219175", show.legend = FALSE) +
  geom_sf(data = dogger, fill = "#c2bdb9", alpha = 0.10, show.legend = FALSE, linetype = "dashed") +
  
  # Plot center of gravity points for each year and area
  #geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch), alpha = 0.4) +  # Plot catch points with size based on total catch
  geom_point(data = cog_per_year_area_vgt, aes(x = lon_cog, y = lat_cog, fill = area), size = 4) +
  #geom_density_2d(data = catch_by_year, aes(x = lon, y = lat), color = "black", size = 0.3) +
  #geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch, shape = factor(Year))) +
  
  #scale_color_manual(values = c("UK EEZ" = "#f8766d", "EU North" = "#00ba38", "EU South" = "#619cff")) + # Custom colors for groups
  labs(title = "Center of Gravity for SA 1 (2023-2024)",
       x = "Longitude",
       y = "Latitude",
       fill = "Center of Gravity") + # Ensure the legend titles are explicit
  
  facet_wrap(~Year, ncol = 2) +
  theme(
    plot.title = element_text(size = 7, face = "bold"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.title = element_text(size = 7, face = "bold"),
    axis.text = element_text(size = 6, face = "bold", angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    theme(strip.text = element_text(color = "black")),
    
    # Legend positioning
    legend.position = "bottom",## <- here
    legend.direction = "horizontal",   # Titles and attributes stay in a single row
    legend.box = "vertical",           # Ensures stacked layout of different legend groups
    
    # Reduce spacing & padding
    legend.spacing.x = unit(0, 'cm'),  # Reduce horizontal spacing between attributes
    legend.spacing.y = unit(0, 'cm'),  # Reduce vertical spacing between rows
    legend.margin = margin(0, 0, 0, 0),   # Removes outer legend box padding
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 6, face = "bold"),  
    legend.text = element_text(size = 6),
    #scale_x_continuous(breaks = seq(min(cog_per_year_area_vgt$), max(cog_per_year_area_vgt$), by = 3)),
    # Remove white space
    plot.margin = margin(10, 0, 10, 0)  # Eliminates all margins
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size = guide_legend(override.aes = list(size = c(0.5, 5, 10)))) +
    coord_sf(expand = FALSE)

ggsave("Center of Gravity for SA 1 (2023-2024).png", plot = p, dpi = 600, width = 6, height = 4)


