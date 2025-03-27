# Function to calculate proportion of each group’s total_catch based on the grand total for specified years
calculate_proportion <- function(df, start_year = 2010, end_year = 2024) {
  # Filter the dataframe for the specified year range
  filtered_df <- subset(df, Year >= start_year & Year <= end_year)
  
  # Sum the total_catch for each group within the filtered data
  group_sums <- aggregate(total_catch ~ group, data = filtered_df, sum)
  
  # Calculate the grand total of all group sum totals
  grand_total <- sum(group_sums$total_catch)
  
  # Add a new column with the proportion of each group’s total_catch to the grand total
  group_sums$proportion <- group_sums$total_catch / grand_total
  
  if (nrow (group_sums) < length(unique(df$group))) {
    miss.group <- unique(df$group)[-which(unique(df$group) %in% group_sums$group)] 
    group_sums <- rbind(group_sums, data.frame(group = miss.group, total_catch = 0, proportion = 0))
  }
  
  group_sums <- group_sums %>% arrange(group)
  
  return(group_sums)
}

# Apply the function to the catch_by_year dataframe
catch_summary <- calculate_proportion(catch_by_year)

# Display the result
print(catch_summary)

years <- unique(catch_by_year$Year)

relative.catch <- matrix(0, nrow = 3, ncol = length(years))
 for (i in 1:length(years)) {
   relative.catch[,i] = calculate_proportion(catch_by_year, start_year = years[i], end_year = years [i])$proportion
   
 }
#changed from df$group to catch_by_year$group
rownames(relative.catch) <- (unique(catch_by_year$group))
colnames(relative.catch) <- (unique(years))

relative.catch

#########################################################
######################## CSV ############################
#########################################################

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/DATA")
# Example dataframe for years and spaces
years <- seq(2010,2024)
spaces <- 1:3  # Number of spaces

# Convert relative.catch to a dataframe for years and spaces
relative_catch_df <- data.frame(
  expand.grid(Year = years, Space = spaces),
  F = as.vector(t(relative.catch))
)

# Save to CSV
write.csv(relative_catch_df, "custom_F_data.csv", row.names = FALSE)
