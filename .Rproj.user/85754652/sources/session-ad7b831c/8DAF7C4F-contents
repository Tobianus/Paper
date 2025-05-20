
#################################################################################
################################ WITH OUT SCALING ###############################
#################################################################################

catch_df_filtered_noscale <- catch_df_filtered %>%
  mutate(Model = 'MODEL')

catch_by_year_noscale <- catch_by_year %>%
  mutate(Model = 'OBSERVED')

CAGE_CATCH <- rbind(catch_df_filtered_noscale, catch_by_year_noscale) 

#################################################################################
################################## WITH SCALING #################################
#################################################################################

#SCRIPT FOR PLOTTING MODEL AND VGT. REMEMBER TO RUN MODEL FIRST AND USE x$catch.save.age using OM_space_working_model.R
# Then run coords_areas_catches.R to get the data ready.

# Min-max scale function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply min-max scaling to total_catch in each data frame
catch_df_filtered_scale <- catch_df_filtered %>%
  mutate(total_catch = min_max_scale(total_catch),
         Model = 'MODEL')

catch_by_year_scale <- catch_by_year %>%
  mutate(total_catch = min_max_scale(total_catch),
         Model = 'OBSERVED')

# Combine the data
CAGE_CATCH <- rbind(catch_df_filtered_scale, catch_by_year_scale)

####################################################
####################### PLOT #######################
####################################################

library(ggplot2)

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")

p <- ggplot(CAGE_CATCH, aes(x = Year, y = total_catch, color = group, linetype = Model)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Unscaled Total Catches Over Time by Groups",
       x = "Year",
       y = "Catches in 1000 t",
       color = "Group",
       linetype = "Model") +
  #scale_color_manual(values = c("UK EEZ" = "#f8766d", "EU North" = "#00ba38", "EU South" = "#619cff")) +
  scale_color_manual(values = c("Sub-Area 1" = "#619cff", "Sub-Area 2" = "#f8766d", "Sub-Area 3" = "#00ba38")) +
  #theme_minimal() +
  scale_y_continuous(labels = scales::label_comma(suffix = " t")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 1)) +
  scale_linetype_manual(values = c("OBSERVED" = "solid", "MODEL" = "dashed")) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray80", linewidth = 0.3),
    strip.background = element_rect(fill = "black", color = NA),
    theme(strip.text = element_text(color = "black")),
    
    # Legend positioning
    legend.key.width = unit(2.5, "cm"),  # Increase width so dashes show clearly
    legend.position = "bottom",## <- here
    legend.direction = "horizontal",   # Titles and attributes stay in a single row
    legend.box = "vertical",           # Ensures stacked layout of different legend groups
    # Reduce spacing & padding
    legend.spacing.x = unit(0, 'cm'),  # Reduce horizontal spacing between attributes
    legend.spacing.y = unit(0, 'cm'),  # Reduce vertical spacing between rows
    legend.margin = margin(0, 0, 0, 0),   # Removes outer legend box padding
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10),
    # Remove white space
    plot.margin = margin(10, 0, 10, 0)  # Eliminates all margins
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size = guide_legend(override.aes = list(size = c(0.5, 5, 10))),
    linetype = guide_legend(title = "DATA", override.aes = list(size = 10, color = "black")))  # clearer in legend

# Save the facet plot
ggsave("Unscaled Total Catches Over Time by Group and Model.png", plot = p, dpi = 600, width = 10, height = 6)
