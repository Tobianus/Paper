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

setwd("C:/Users/chris/Desktop/GIT/Paper")
source("C:/Users/chris/Desktop/DTU/R/ORIG/myfunctions.R")

# Step 1: Load the two CSV files and clean up the data
#The reason why I am using "square_to_sandeel_areas_WKSAND16" is because the vgt file does not contain 
#area information e.g. 1r, 2r, 3r etc. I am matching the area data with this existing data sheet to
#the vgt data to easily assign the area information to the vgt datasheet.

load("scripts/data/sandeel 1r/nage_space.Rdata")  # this loads an object into your environment

cpue_data <- d.export %>%
  filter(!is.na(cpue), cpue != 0,
    as.numeric(as.character(year)) >= 2010,
    as.numeric(as.character(year)) <= 2024,
    stock %in% c("SA 1")
  ) %>%
  rename(area = stock, age = Age)

###############################################################################
############### Apply EEZ and region classification to cpue_data ##############
###############################################################################

# Step 1: Load UK EEZ shapefile (same as before)
eez_data <- st_read('C:/Users/chris/Desktop/DTU/R/ORIG/DATA/SHAPEFILES/EEZ/EEZ_Land_v3_202030.shp') %>%
  filter(UNION %in% c('Denmark','Germany','Netherlands','Sweden','United Kingdom', 'Norway', 'France', 'Belgium', 'Jersey', 'Guernsey'))

uk_eez <- eez_data %>% filter(UNION == "United Kingdom")

# Step 2: Convert cpue_data to sf object
cpue_sf <- st_as_sf(cpue_data, coords = c("long", "lat"), crs = st_crs(eez_data))

# Step 3: Define latitude threshold
latitude_threshold <- 54.5

# Step 4: Assign group classification
cpue_data$group <- NA

# Points inside UK EEZ
cpue_data$group[st_intersects(cpue_sf, uk_eez, sparse = FALSE)] <- "UK EEZ"

# Points outside but north
cpue_data$group[st_coordinates(cpue_sf)[,2] >= latitude_threshold & !st_intersects(cpue_sf, uk_eez, sparse = FALSE)] <- "EU North"

# Points outside but south
cpue_data$group[st_coordinates(cpue_sf)[,2] < latitude_threshold & !st_intersects(cpue_sf, uk_eez, sparse = FALSE)] <- "EU South"

# Step 9: Merge with catch data and summarize by group
cpue_by_year <- cpue_data %>%
  group_by(year, group) %>%
  summarise(total_cpue = sum(cpue, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(as.character(year)))

# View the summary
print(cpue_summary)

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")

# CHANGE THE ORDER OF THE AREAS IN THE DATAFRAME FOR PLOTTING WITH UK FIRST
#I SET THIS UP SPECIFICALLY FOR PLOTTING THIS VGT AGAINST THE MODEL catch.save.age output
#catch_by_year$group <- factor(catch_by_year$group,
#levels = c("UK EEZ", "EU North", "EU South"))

# Step 10: Plot the total catches over time for the three groups
p <- ggplot(cpue_by_year, aes(x = year, y = total_cpue, color = group)) +
  geom_line(linewidth = 3) +  # Plot lines for each group
  labs(title = "Total CPUE Over Time",
       x = "Year",
       y = "Total Catch",
       color = "Group") +
  
  scale_color_manual(values = c("UK EEZ" = "#35465A", "EU North" = "#CC3300", "EU South" = "#008000")) +  # Custom colors for groups
  # Use scale_y_continuous with label_number_si to format numbers in hundreds of thousands
  #scale_y_continuous(labels = label_number(scale = 1e-5, suffix = "T", accuracy = 1)) +
  # Adjust the x-axis to control the number of years shown
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +  # Shows every 2 years from 2010 to 2024
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12 * 0.8),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12 * 0.8),
    legend.position = "bottom",                # Move the legend to the bottom
    legend.direction = "horizontal",           # Make the legend items display in a horizontal line
    strip.text = element_text(size = 12 * 1.1),
    panel.background = element_rect(fill = "#F0F2F2"),
    plot.background = element_rect(fill = "white")
  ) +
  guides(color = guide_legend(nrow = 1))

# Save the facet plot
ggsave("Total CPUE Over Time.png", plot = p, dpi = 300, width = 12, height = 8)


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
cpue_data_cropped <- cpue_data %>% filter(lon >= -3 & lon <= 11 & lat >= 51 & lat <= 59)

###############################################
################## MAPS END ###################
###############################################


############################################/////////////////##################################ggplot(cog_per_year_area, aes(x = Year)) +

labels_df <- data.frame(
  area = c("1r"),
  x = c(3.5),  # Longitude coordinates for each label
  y = c(54)  # Latitude coordinates for each label
)

#SUM cpue_data FOR PLOTTING

catch_by_year <- cpue_data %>%
  filter(area == "1r") %>%  # Filter for area "1r" only
  group_by(Year, group, lat, lon) %>%
  summarise(total_catch = sum(vgt/1000, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_catch = round(total_catch, 0)) %>%  # Round to 1 decimal place
  filter(total_catch >= 1)  # Remove values smaller than 1


# CHANGE THE ORDER OF THE AREAS IN THE DATAFRAME FOR PLOTTING WITH UK FIRST
catch_by_year$group <- factor(catch_by_year$group,
                              levels = c("UK EEZ", "EU North", "EU South"))

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")

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
  geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch), 
             position = position_jitter(width = 0.1, height = 0.1), alpha = 0.6) +
  #geom_density_2d(data = catch_by_year, aes(x = lon, y = lat), color = "black", size = 0.3) +
  #geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch, shape = factor(Year))) +
  
  #scale_color_manual(values = c("UK EEZ" = "#f8766d", "EU North" = "#00ba38", "EU South" = "#619cff")) + # Custom colors for groups
  labs(title = "Sandeel Sub-Areas in SA 1 (2010-2024)",
       x = "Longitude",
       y = "Latitude",
       color = "Group",
       size = "Weight Key - Catches (1000/tonnes)") + # Ensure the legend titles are explicit
  
  scale_size_continuous(
    range = c(0.5, 10),  # Define the smallest and largest point sizes
    limits = c(0, max(catch_by_year$total_catch)),  # Ensure the legend matches actual values
    breaks = c(min(catch_by_year$total_catch), 
               median(catch_by_year$total_catch), 
               max(catch_by_year$total_catch)),  # Define meaningful breakpoints
    labels = function(x) format(x, scientific = FALSE)  # Force scientific notation for consistency
  ) +
  
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    
    # Legend positioning
    legend.position = "bottom",## <- here
    legend.direction = "horizontal",   # Titles and attributes stay in a single row
    legend.box = "vertical",           # Ensures stacked layout of different legend groups
    
    # Reduce spacing & padding
    legend.spacing.x = unit(0, 'cm'),  # Reduce horizontal spacing between attributes
    legend.spacing.y = unit(0, 'cm'),  # Reduce vertical spacing between rows
    legend.margin = margin(0, 0, 0, 0),   # Removes outer legend box padding
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 7, face = "bold"),  
    legend.text = element_text(size = 7),
    
    # Remove white space
    plot.margin = margin(10, 0, 10, 0)  # Eliminates all margins
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(size = c(0.5, 5, 10))))

ggsave("Sandeel Sub-Areas in SA 1 (2010-2024).png", plot = p, dpi = 300, width = 8, height = 8)

#################### FACET ######################

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
  geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch), 
             position = position_jitter(width = 0.1, height = 0.1)) +
  #geom_density_2d(data = catch_by_year, aes(x = lon, y = lat), color = "black", size = 0.3) +
  #geom_point(data = catch_by_year, aes(x = lon, y = lat, color = group, size = total_catch, shape = factor(Year))) +
  
  #scale_color_manual(values = c("UK EEZ" = "#f8766d", "EU North" = "#00ba38", "EU South" = "#619cff")) + # Custom colors for groups
  labs(title = "Sandeel Sub-Areas in SA 1 (2010-2024)",
       x = "Longitude",
       y = "Latitude",
       color = "Group",
       size = "Size Key - Catches 1000/tonnes") + # Ensure the legend titles are explicit
  
  scale_size_continuous(
    range = c(0.5, 10),  # Define the smallest and largest point sizes
    limits = c(0, max(catch_by_year$total_catch)),  # Ensure the legend matches actual values
    breaks = c(min(catch_by_year$total_catch), 
               median(catch_by_year$total_catch), 
               max(catch_by_year$total_catch)),  # Define meaningful breakpoints
    labels = function(x) format(x, scientific = FALSE)  # Force scientific notation for consistency
  ) +
  facet_wrap(~Year, ncol = 5) +
  
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    
    # Legend positioning
    legend.position = "bottom",## <- here
    legend.direction = "horizontal",   # Titles and attributes stay in a single row
    legend.box = "vertical",           # Ensures stacked layout of different legend groups
    
    # Reduce spacing & padding
    legend.spacing.x = unit(0, 'cm'),  # Reduce horizontal spacing between attributes
    legend.spacing.y = unit(0, 'cm'),  # Reduce vertical spacing between rows
    legend.margin = margin(0, 0, 0, 0),   # Removes outer legend box padding
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 7, face = "bold"),  
    legend.text = element_text(size = 7),
    
    # Remove white space
    plot.margin = margin(10, 0, 10, 0)  # Eliminates all margins
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(size = c(0.5, 5, 10))))

ggsave("Sandeel Sub-Areas in SA 1 (2010-2024).png", plot = p, dpi = 300, width = 8, height = 8)


