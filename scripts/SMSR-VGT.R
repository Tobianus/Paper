#SCRIPT FOR PLOTTING MODEL AND VGT. REMEMBER TO RUN MODEL FIRST AND USE x$catch.save.age using OM_space_working_model.R
# Then run coords_areas_catches.R to get the data ready.

# Min-max scale function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply min-max scaling to total_catch in each data frame
catch_df_filtered_scale <- catch_df_filtered %>%
  mutate(total_catch = min_max_scale(total_catch),
         Model = 'SMSR')

catch_by_year_scale <- catch_by_year %>%
  mutate(total_catch = min_max_scale(total_catch),
         Model = 'VGT')

# Combine the data
CAGE_CATCH <- rbind(catch_df_filtered_scale, catch_by_year_scale)

# Plot the scaled data
library(ggplot2)

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")

p <- ggplot(CAGE_CATCH, aes(x = Year, y = total_catch, color = group, linetype = Model)) +
  geom_line(size = 2) +
  labs(title = "Unscaled Total Catches Over Time by Group and Model",
       x = "Year",
       y = "Scaled Total Catch",
       color = "Group",
       linetype = "Model") +
  scale_color_manual(values = c("UK EEZ" = "#f8766d", "EU North" = "#00ba38", "EU South" = "#619cff")) +
  #theme_minimal() +
  #scale_y_continuous(breaks = seq(min(0), max(6e+05), by = 1)) + # Shows every 2 years from 2010 to 2024
  scale_x_continuous(breaks = seq(2010, 2024, by = 1)) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
     # Shows every 2 years from 2010 to 2024
  ) +
  guides(color = guide_legend(nrow = 1))

# Save the facet plot
ggsave("Unscaled Total Catches Over Time by Group and Model.png", plot = p, dpi = 300, width = 12, height = 8)

################################ WITH OUT SCALING ###############################

catch_df_filtered_noscale <- catch_df_filtered %>%
  mutate(Model = 'SMSR')

catch_by_year_noscale <- catch_by_year %>%
  mutate(Model = 'VGT')

CAGE_CATCH <- rbind(catch_df_filtered_noscale, catch_by_year_noscale) 
