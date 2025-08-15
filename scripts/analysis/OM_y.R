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
library(ggtext)

#Using the new area selection script 'Coords_areas_catches_area1.R', We have now assigned a new F in the OM which calculates 
#the relative distribution of catches from 2010 to 2024. We then need to run the script 'F_NIS gives' which outputs
#'relative.catch'. Relative catch is then fed into 'R/get_OM_parameters_NIS_fix.R' when the model is run.

#source(here("scripts/analysis", "OM_areas.R"))
#source(here("scripts/analysis", "F_distribution.R"))
#install_github("nissandjac/smsR")

source('R/matrices.R') # Normalized retention values

#NORMALISE THE NUMBERS SO THEY ADD UP TO 1. WE DO 
#THIS BECAUSE IF THIS DOES NOT ADD UP TO 1, WE ARE
#LOSING FISH FROM HOW THE RECRUITMENT IS FED INTO THE MODEL.
#source('R/retention.R')# Normalized retention values

# Pick which migration matrix to use
migration_matrix <- zero_migration_matrix
# migration_matrix <- ten_2_1_migration_matrix
# migration_matrix <- fourty_2_1_migration_matrix
# migration_matrix <- ten_2_3_migration_matrix
# migration_matrix <- fourty_2_3_migration_matrix
#migration_matrix <- asbjorne_migration_matrix

############ ADD OUTPUT TO rec.space AND THEN RUN MODEL

areas <- c("EU SOUTH","UK","EU NORTH")

migration_matrix <- matrix(c(
  # ----- ROW 1: TO EU SOUTH -----
  0.0,  # from EU SOUTH → to EU SOUTH   (stay; keep 0 for "leavers-only")
  0.01,  # from UK       → to EU SOUTH
  0.0,  # from EU NORTH → to EU SOUTH
  
  # ----- ROW 2: TO UK -----
  0.0,  # from EU SOUTH → to UK
  0.0,  # from UK       → to UK        (stay; keep 0 for "leavers-only")
  0.0,  # from EU NORTH → to UK
  
  # ----- ROW 3: TO EU NORTH -----
  0.0,  # from EU SOUTH → to EU NORTH
  0.20,  # from UK       → to EU NORTH
  0.0   # from EU NORTH → to EU NORTH  (stay; keep 0 for "leavers-only")
), nrow = 3, byrow = TRUE,
dimnames = list(to = areas, from = areas))

setwd("C:/Users/chris/Desktop/LATEST MODEL")
source('R/get_OM_parameters_move_y.R')
source('R/run_agebased_sms_OP_move_y.R')
source('R/addYear.R')

# Read parameters from stock assessment
parms <- readRDS('DATA/sandeel 1r/area1r.rds')
#parms <- readRDS(here("scripts/data/sandeel 1r", "sandeel_1r_parms.rds"))

sas <- parms[[2]]
df.tmb <- parms[[1]]
df.OM <- get_OM_parameters(df.tmb, sas,
                           nspace = 3,
                           rec.space = c(0.12, 0.65, 0.12),
                           #rec.space = c(0.33,0.33,0.33),
                           #moverecruit = c(0.1,0.7,0.2),
                           migration_matrix = migration_matrix,
                           movemax = c(0.1,0.1,0.1) # Max movement from 1+ groups
)
#df.OM$recruitment.fit <- list(mod1,mod2,mod3)

x <- run.agebased.sms.op(df.OM)


SSB <- as.data.frame(x$SSB) %>%
  mutate(years = df.OM$years) %>%
  pivot_longer(1:df.OM$nspace, values_to = 'SSB', names_to = 'area')

#dev.copy(png,'D:/Aquatic Engineering 2021/THESIS/Model/SSB/SSB.png', width=5000, height=3000, res=300)

setwd("C:/Users/chris/Desktop/LATEST MODEL/TEST/")

p <- ggplot(SSB, aes(x = years, y = SSB,color = area)) +
  geom_line(linewidth=5) +
  theme_classic() +
  labs(
    title = "  0.9   # from EU NORTH → to EU NORTH", 
    x = "Year", 
    y = "Total SSB"
  ) +
  scale_colour_manual(name = "area", labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38"))
ggsave("  0.9   # from EU NORTH → to EU NORTH.png", plot = p, dpi = 300, width = 12, height = 8)


str(df.OM)

####################################################################
########################### CPUE START #############################
####################################################################

Nsave <- as.data.frame.table(x$N.save.age, responseName = 'N') %>% 
  filter(!is.na(N), N != 0,
         as.numeric(as.character(year)) >= 2010,
         as.numeric(as.character(year)) <= 2024,
         age %in% 0:2,
         season == 2) %>% 
  mutate(
    Year = as.numeric(as.character(year)),
    Age = as.numeric(as.character(age)),
    Group = space
  ) %>%
  select(-year, -age, -space)

# Map space to group labels (like cpue_by_year$groups)
Nsave_filtered <- Nsave %>%
  mutate(Group = case_when(
    Group == 1 ~ "Sub-Area 1",
    Group == 2 ~ "Sub-Area 2",
    Group == 3 ~ "Sub-Area 3"
  ))

# Round the N values, remove small ones, keep spatial info
nage_by_agegroup <- Nsave_filtered %>%
  group_by(Group, Age, Year) %>%   # ✅ keep age, drop year
  summarise(total_cpue = sum(N, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(std_N = total_cpue / mean(total_cpue))

# Factor levels for consistent plotting order
nage_by_agegroup$Group <- factor(nage_by_agegroup$Group,
                                 levels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"))

####################################################################
#########################  CATCH START #############################
####################################################################

# Step 1: Filter for season 1 (4th dimension)
catch_season1 <- x$Catch.save.age[,,, "1"]
# Step 2: Sum over the age dimension (1st dimension)
# This will result in a 2D array with dimensions year and space

#The apply() function is used to apply a function (in this case, sum) to a specific set of dimensions of an array.
#c(2, 3) means that we are going to apply the sum() function across the 1st dimension (which is Age). We are keeping 
#the Year and Space dimensions (2nd and 3rd dimensions) intact.
catch_summed <- apply(catch_season1, c(2, 3), sum)  # c(2,3) means sum across the 1st dimension (age)

# Step 3: Convert to a data frame for easier manipulation and groupby operations
catch_df <- as.data.frame(as.table(catch_summed))  # Convert array to data frame
colnames(catch_df) <- c("Year", "space", "total_catch")  # Rename columns for clarity
# Step 4: Convert Year and Space to numeric (if necessary)
catch_df$Year <- as.numeric(as.character(catch_df$Year))
catch_df$space <- as.numeric(as.character(catch_df$space))
# Now catch_df has the total catch summed over age for each combination of year and space

# Step 5: Filter for years 2010 onwards and add descriptive space names
catch_df_filtered <- catch_df %>%
  filter(Year >= 2010) %>% 
  mutate(group = case_when(
    space == 1 ~ "Sub-Area 1",
    space == 2 ~ "Sub-Area 2",
    space == 3 ~ "Sub-Area 3",
    TRUE ~ as.character(space)
  )) %>%
  mutate(std_C = total_catch / mean(total_catch)) %>% 
  select(-space) %>% 
  select(Year, group, total_catch, std_C)

# CHANGE THE ORDER OF THE AREAS IN THE DATAFRAME FOR PLOTTING WITH UK FIRST
catch_df_filtered$group <- factor(catch_df_filtered$group,
                                  levels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"))

#############################################
################ PLOT CATCH #################
#############################################

setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")
# Step 6: Create the plot with ggplot2 and custom colors
p <- ggplot(catch_df_filtered, aes(x = Year, y = total_catch, color = group)) +
  geom_line(size = 1) +  # Plot lines for each space (area)
  geom_point(aes(shape = group), size = 3) +  # Add points with shapes for legend
  labs(
    title = "Total Catches Over Time (Season 1, from 2010 onwards)", 
    x = "Year", 
    y = "Total Catch", 
    color = "Area", 
    shape = "Area"  # Include shape legend for customization
  ) +
  theme(legend.position = "bottom") +  # Move the legend to the bottom
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +  # Shows every 2 years from 2010 to 2024
  scale_color_manual(values = c("Sub-Area 1" = "#35465A", "Sub-Area 2" = "#CC3300", "Sub-Area 3" = "#008000")) + # Color for each area
  scale_shape_manual(values = c("Sub-Area 1" = 16, "Sub-Area 2" = 16, "Sub-Area 3" = 16)) +  # Custom shapes
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",                # Move the legend to the bottom
    legend.direction = "horizontal",           # Make the legend items display in a horizontal line
    strip.text = element_text(size = 11),
    panel.background = element_rect(fill = "#F0F2F2"),
    plot.background = element_rect(fill = "white")
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))


ggsave("smsR - Total Catches Over Time (Season 1, from 2010 onwards).png", plot = p, dpi = 300, width = 12, height = 8)

SSB <- as.data.frame(x$SSB) %>%
  mutate(years = df.OM$years) %>%
  pivot_longer(1:df.OM$nspace, values_to = 'SSB', names_to = 'area')

#dev.copy(png,'D:/Aquatic Engineering 2021/THESIS/Model/SSB/SSB.png', width=5000, height=3000, res=300)
ggplot(SSB, aes(x = years, y = SSB,color = area))+geom_line()+theme_classic()
#dev.off()

SSB.tot <- SSB %>% group_by(years) %>% summarise(SSBtot = sum(SSB))

ssb.asses <- getSSB(df.tmb, sas)
r.assess <- getR(df.tmb, sas)

plot(rowSums(x$R.save))
lines(r.assess$R)

plot(SSB.tot$SSBtot/ssb.asses$SSB[1:df.tmb$nyears])
#lines()


###################################################
################### SUM SQUARED ###################
###################################################
#SSQ = sum((N_obs - N_modeled)^2)
#SSQ = sum((log(N_obs) - log(N_modeled))^2)
#residual = N_obs - N_modeled

SSQ = sum((allmeans_cpue - allmeans_nage)^2)
SSQ = sum((log(allmeans_cpue$std_cpue) - log(allmeans_nage$std_cpue))^2)
residual = allmeans_cpue - allmeans_nage

###################################################
###################### RICKER #####################
###################################################

lm_area1 <- data.frame(YEARS = df.OM$years, SSB = x$SSB[, 1], R = x$R.save[, 1])
lm_area2 <- data.frame(YEARS = df.OM$years, SSB = x$SSB[, 2], R = x$R.save[, 2])
lm_area3 <- data.frame(YEARS = df.OM$years, SSB = x$SSB[, 3], R = x$R.save[, 3])

# do a linear model of SSB (predictor) vs log(R/SSB) (response)
mod1 = lm(log(R/SSB)~SSB, lm_area1)
mod1
mod2 = lm(log(R/SSB)~SSB, lm_area2)
mod2
mod3 = lm(log(R/SSB)~SSB, lm_area3)
mod3

#############################################
#############  F DISTRIBUTION  ##############
#############################################

### Create a spatial operating model for sandeel ###

df.new <- addYear(df.OM,
                  new_years = 10, # Number of years to simulate into the future
                  #F_future = c(0.33, 0.33, 0.33) ### ADVICE
                  ### FROM LATEST ADVICE
                  #F_future = c(0.33, 0, 0.33) ### ADVICE BREXIT
                  ### FROM LATEST ADVICE ###Should be 0.36, 036, 0.00 but we drop uk bc 0 F
                  #F_future = c(0, 0.33, 0) ### ONLY UK
                  ### RELATIVE PROPORTION LAST 5 YEARS FROM 2024 VGT ### Should be 0.56, 044, 0.00 but we drop uk bc 0 F
                  
                  #F_future = c(0.0, 0.0, 0.0) ### NATURAL PUFFIN
                  ### YEAR 2024 VGT ONLY ### 0.56, 044, 0.00 but we drop uk bc 0 F
                  
                  #F_future = c(0.85, 0.85, 0.85) ### VULNERABLE PUFFIN
                  ### BEFORE BREXIT RELATIVE PROPORTION LAST 5 YEARS FROM 2024 VGT
                  
                  F_future = c(0.33, 0, 0.33) ### SUSTAINABLE PUFFIN
                  ### RELATIVE PROPORTION LAST 5 YEARS FROM 2024 VGT ### Should be 0.56, 044, 0.00 but we drop uk bc 0 F
                  
)

#ADD A AND B TO df.new for RICKER MODEL
df.new$mod <- list(mod1, mod2, mod3)

x <- run.agebased.sms.OP(df.new)

SSB <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  filter(years >= 2010) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

R.save <- as.data.frame(x$R.save) %>%
  mutate(years = df.new$years) %>%
  filter(years >= 2010) %>%
  pivot_longer(1:df.new$nspace, values_to = 'R.save', names_to = 'area')

######################################################
################## INDIVIDUAL PLOTS ##################
######################################################

######################################################
#################### SSB PER AREA ####################
######################################################
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/SSB P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/SSB P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/SSB P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/SSB P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(SSB, aes(x = years, y = SSB/1000, color = area)) +
  theme_bw() +
  geom_line(linewidth=1.5) + #change to 1.7 for individual plot
  geom_point(size=3.5) + #change to 4 for individual plot
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "SSB 2023-2032 YEARS", y = "SSB (Thousand Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) + #change to 20 for individual plot
  theme(legend.position = c(0.93, 0.86), legend.title=element_blank()) + #Use 0.93, 0.88 for individual plot
  theme(legend.box.background = element_rect(color="black", linewidth=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

#dev.off()

####################################################
##################### SSB SUM ######################
####################################################

#SUMMING THE 3 AREAS FOR BLIM COMPARISON

SSBsum <- SSB %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1')

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/SSB SUM - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/SSB SUM - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/SSB SUM - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/SSB SUM - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(data = SSBsum, aes(x = years, y = SSBtot/1000, fill = Area)) +
  geom_line(color = "#008822", linewidth=1.5) + 
  geom_point(color = "#008822", size=3.5) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "SSB (SUM) 2023-2032 YEARS", y = "SSB (Thousand Tonnes)", x = "Years") +
  geom_hline(yintercept=140824/1000, linetype=3, linewidth=1, color = "black") +
  annotate("text", x = 2032.3, y = 150000/1000, label = "B-escapement", size = 4, color = "black") +
  geom_hline(yintercept=105809/1000, linetype=3, linewidth=1, color = "red") +
  annotate("text", x = 2033.6, y = 120000/1000, label = "Blim", size = 4, color = "red") +
  scale_x_continuous(breaks = seq(min(SSBsum$years), max(SSBsum$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = c(0.93, 0.89), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

#dev.off()

####################################################
#################### R PER AREA ####################
####################################################

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/R P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/R P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/R P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/R P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(R.save, aes(x = years, y = R.save/1000000, color = area)) +
  theme_bw() +
  geom_line(linewidth=1.5) +
  geom_point(size=3.5) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "R (P/AREA) 2023-2032 YEARS", y = "Recruitment (Million Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Area", labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = c(0.93, 0.86), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

#dev.off()

####################################################
###################### R SUM #######################
####################################################

R.savesum <- R.save %>% group_by(years) %>% summarise(R.savetot = sum(R.save/1000000)) %>%  
  mutate(Area = 'SA1')

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/R SUM - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/R SUM - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/R SUM - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/R SUM - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(data = R.savesum, aes(x = years, y = R.savetot, fill = Area)) +
  geom_line(color = "#008822", linewidth=1.5) + 
  geom_point(color = "#008822", size=3.5) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "R (SUM) 2023-2032 YEARS", y = "Recruitment (Million Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(R.savesum$years), max(R.savesum$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = c(0.93, 0.89), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

#dev.off()

#############################################
############### CATCH PER AREA ##############
#############################################

Catch <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year >= 2010)

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/CATCH P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/CATCH P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/CATCH P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/CATCH P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(Catch,aes(x = year, y= Ctot/1000, color = space)) +
  theme_bw() +
  geom_line(linewidth=1.5) +
  geom_point(size=3.5) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "CATCH (P/AREA) 2023-2032 YEARS", y = "Catch (Thousand Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(name = "Area", labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38")) +
  theme(legend.position = c(0.93, 0.86), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm')) 

#dev.off()

#############################################
################# CATCH SUM #################
#############################################

CSUM <- Catch %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1')

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/CATCH SUM - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/CATCH SUM - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/CATCH SUM - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/CATCH SUM - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(data = CSUM, aes(x = year, y = Ctot/1000, fill = Area)) +
  geom_line(color = "#008822", linewidth=1.5) + 
  geom_point(color = "#008822", size=3.5) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "CATCH (SUM) 2023-2032 YEARS", y = "Catch (Thousand Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(CSUM$year), max(CSUM$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = c(0.93, 0.89), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

#dev.off()


#############################################
############### F PER AREA ##################
#############################################

#FIX FOR F IS USING df.new$F0 instead

Fsea_by_age_all <- as.data.frame.table(x$Fseason) %>%
  filter(year %in% 1983:2042, season == "1") %>%  # all spaces
  mutate(
    year = as.numeric(as.character(year)),
    age = as.numeric(as.character(age)),
    space = factor(space)  # treat space as a factor for plotting
  ) %>%
  filter(year >= 2010)

green_palette <- colorRampPalette(c("#e8f8f0", "#d0f0dc", "#a8ddb5", "#66c27c", "#008822"))(5)

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/F P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/F P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/F P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/F P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

FPAREA <-ggplot(Fsea_by_age_all, aes(x = year, y = Freq, group = factor(age), color = factor(age), linetype = factor(age))) +
    theme_bw() +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = green_palette, name = "Age", labels = paste("Age", 0:4)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dotted")) +
  geom_vline(aes(xintercept = 2025), linetype = "dashed") +
  labs(
    title = "Fishing Mortality (F) by Age Class and Sub-Area",
    subtitle = "Season 1, Years 2010–2034",
    y = "F (year⁻¹)", x = "Year",
    color = "Age"
  ) +
  facet_wrap(~ space, labeller = labeller(space = c("1" = "Sub-Area 1", "2" = "Sub-Area 2", "3" = "Sub-Area 3"))) +
  scale_x_continuous(breaks = seq(min(Fsea_by_age_all$year), max(Fsea_by_age_all$year), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    legend.position = c(0.852, 1.09),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.box.background = element_rect(color = "black", size = 0.3),
    legend.box.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(1, 'cm')
  ) +
  scale_x_continuous(breaks = seq(min(Fsea_by_age_all$year), max(Fsea_by_age_all$year), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  guides(
    fill = guide_legend(override.aes = list(size = 10, shape = 15)),
    linetype = "none",
    color = guide_legend(label.position = "left")) #this hides the extra linetype legend

#dev.off()

#############################################
#################### F SUM ##################
#############################################

# Convert Fseason into a tidy dataframe
F_by_age <- as.data.frame.table(x$Fseason) %>%
  mutate(
    age = as.numeric(as.character(age)),
    year = as.numeric(as.character(year)),
    F = Freq
  ) %>%
  filter(year >= 2010)

# Summarise total F per age and year (summing across space and season)
F_total_age_year <- F_by_age %>%
  group_by(year, age) %>%
  summarise(Ftot = sum(F), .groups = "drop")

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/F SUM - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/F SUM - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/F SUM - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/F SUM - 0, 0.44, 0.png', width=5000, height=3000, res=300)

FSUM <- ggplot(F_total_age_year, aes(x = year, y = Ftot, group = factor(age), color = factor(age), linetype = factor(age))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = green_palette, name = "Age", labels = paste("Age", 0:4)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dotted")) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(
    title = "Total Fishing Mortality (F) per Age Over Time",
    y = "F (year⁻¹)", x = "Year"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    legend.position = c(0.81, 0.89),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.box.background = element_rect(color = "black", size = 0.3),
    legend.box.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(1, 'cm')
  ) +
  scale_x_continuous(breaks = seq(min(F_total_age_year$year), max(F_total_age_year$year), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  guides(
    fill = guide_legend(override.aes = list(size = 10, shape = 15)),
    linetype = "none",
    color = guide_legend(label.position = "left"))

#dev.off()

######################################################
######################################################
######################################################
##################### GRID PLOTS #####################
######################################################
######################################################
######################################################

######################################################
#################### SSB PER AREA ####################
######################################################

SSBPAREA <- ggplot(SSB, aes(x = years, y = SSB/1000, color = area)) +
  theme_bw() +
  geom_line(linewidth=0.7) + #change to 1.7 for individual plot
  geom_point(size=1) + #change to 4 for individual plot
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "SSB (P/AREA) 2023-2032 YEARS", y = "SSB (Thousand Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_manual(name = "Area", labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38")) +
theme(
  text = element_text(size = 5),
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.text.y = element_text(angle = 45, hjust = 1),
  #legend.position = c(0.80, 0.84), ORIGINAL
  legend.position = c(0.80, 0.94),
  legend.direction = "horizontal",
  legend.title = element_blank(),
  legend.box.background = element_rect(color = NA, size = 0.15),
  legend.box.margin = margin(0, 0, 0, 0),
  legend.margin = margin(0, 0, 0, 0),
  legend.key.size = unit(0.1, 'cm')) +
guides(
    fill = guide_legend(override.aes = list(size = 10, shape = 15)),
    linetype = "none",
    color = guide_legend(label.position = "left")) #this hides the extra linetype legend

####################################################
##################### SSB SUM ######################
####################################################

#SUMMING THE 3 AREAS FOR BLIM COMPARISON

SSBSUM <- ggplot(data = SSBsum, aes(x = years, y = SSBtot/1000, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "SSB (SUM) 2023-2032 YEARS", y = "SSB (Thousand Tonnes)", x = "Years") +
  geom_hline(yintercept=140824/1000, linetype=3, linewidth=0.5, color = "black") +
  annotate("text", x = 2032.3, y = 440824/1000, label = "B-escapement", size = 2, color = "black") +
  geom_hline(yintercept=105809/1000, linetype=3, linewidth=0.5, color = "red") +
  annotate("text", x = 2033.6, y = 405809/1000, label = "Blim", size = 2, color = "red") +
  scale_x_continuous(breaks = seq(min(SSBsum$years), max(SSBsum$years), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  theme(
    text = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1))

####################################################
#################### R PER AREA ####################
####################################################

RPAREA <- ggplot(R.save, aes(x = years, y = R.save/1000000, color = area)) +
  theme_bw() +
  geom_line(linewidth=0.7) +
  geom_point(size=1) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "R (P/AREA) 2023-2032 YEARS", y = "Recruitment (Million Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_manual(name = "Area", labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38")) +
  theme(
    text = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    #legend.position = c(0.80, 0.84), ORIGINAL
    legend.position = c(0.80, 0.94),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.box.background = element_rect(color = NA, size = 0.15),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(0.1, 'cm')) +
  guides(
    fill = guide_legend(override.aes = list(size = 10, shape = 15)),
    linetype = "none",
    color = guide_legend(label.position = "left"))

####################################################
###################### R SUM #######################
####################################################

RSUM <- ggplot(data = R.savesum, aes(x = years, y = R.savetot, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "R (SUM) 2023-2032 YEARS", y = "Recruitment (Million Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(R.savesum$years), max(R.savesum$years), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  theme(
    text = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1))

#############################################
############### CATCH PER AREA ##############
#############################################

CPAREA <- ggplot(Catch,aes(x = year, y= Ctot/1000, color = space)) +
  theme_bw() +
  geom_line(linewidth=0.7) +
  geom_point(size=1) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "CATCH (P/AREA) 2023-2032 YEARS", y = "Catch (Thousand Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_manual(name = "Area", labels = c("Sub-Area 1", "Sub-Area 2", "Sub-Area 3"), values = c("#619cff", "#f8766d", "#00ba38")) +
  theme(
    text = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    #legend.position = c(0.80, 0.84), ORIGINAL
    legend.position = c(0.80, 0.94),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.box.background = element_rect(color = NA, size = 0.15),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(0.1, 'cm')) +
  guides(
    fill = guide_legend(override.aes = list(size = 10, shape = 15)),
    linetype = "none",
    color = guide_legend(label.position = "left"))


#############################################
################# CATCH SUM #################
#############################################

CASUM <- ggplot(data = CSUM, aes(x = year, y = Ctot/1000, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(title = "CATCH (SUM) 2023-2032 YEARS", y = "Catch (Thousand Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(CSUM$year), max(CSUM$year), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
theme(
  text = element_text(size = 5),
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.text.y = element_text(angle = 45, hjust = 1))



#############################################
############### F PER AREA ##################
#############################################

#FIX FOR F IS USING df.new$F0 instead

Fsea_by_age_all <- as.data.frame.table(x$Fseason) %>%
  filter(year %in% 1983:2042, season == "1") %>%  # all spaces
  mutate(
    year = as.numeric(as.character(year)),
    age = as.numeric(as.character(age)),
    space = factor(space)  # treat space as a factor for plotting
  ) %>%
  filter(year >= 2010)

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/F P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/F P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/F P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/F P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

custom_labeller <- labeller(
  space = c(
    "1" = "<span style='color:#619cff'><b>Sub-Area 1</b></span>",
    "2" = "<span style='color:#f8766d'><b>Sub-Area 2</b></span>",
    "3" = "<span style='color:#00ba38'><b>Sub-Area 3</b></span>"
  )
)

FPAREA <-ggplot(Fsea_by_age_all, aes(x = year, y = Freq, group = factor(age), color = factor(age), linetype = factor(age))) +
  theme_bw() +
  geom_line(linewidth=0.7) + #change to 1.7 for individual plot
  geom_point(size=0.5) + #change to 4 for individual plot
  scale_color_manual(values = green_palette, name = "Age", labels = paste("Age", 0:4)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dotted")) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(
    title = "Fishing Mortality (F) by Age Class and Sub-Area",
    y = "F (year⁻¹)", x = "Years",
    color = "Age"
  ) +
  facet_wrap(~ space, labeller = custom_labeller) +
  #facet_wrap(~ space, labeller = labeller(space = c("1" = "Sub-Area 1", "2" = "Sub-Area 2", "3" = "Sub-Area 3"))) +
  scale_x_continuous(breaks = seq(min(Fsea_by_age_all$year), max(Fsea_by_age_all$year), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  theme(
    strip.text = ggtext::element_markdown(face = "bold", size = 5),
    strip.background = element_rect(fill = "#ffffff"),
    text = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    #legend.position = c(0.756, 1.62), ORIGINAL
    legend.position = c(0.756, 1.19),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.box.background = element_rect(color = NA, size = 0.15),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(0.1, 'cm')
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5),  # Show round points, larger size
      label.position = "left"
    ),
    linetype = "none"  # Hide separate linetype legend
  )

#dev.off()

#############################################
#################### F SUM ##################
#############################################

# Convert Fseason into a tidy dataframe
F_by_age <- as.data.frame.table(x$Fseason) %>%
  mutate(
    age = as.numeric(as.character(age)),
    year = as.numeric(as.character(year)),
    F = Freq
  ) %>%
  filter(year >= 2010, season == "1")

# Summarise total F per age and year (summing across space and season)
F_total_age_year <- F_by_age %>%
  group_by(year, age) %>%
  summarise(Ftot = sum(F), .groups = "drop")

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/F SUM - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/F SUM - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/F SUM - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/F SUM - 0, 0.44, 0.png', width=5000, height=3000, res=300)

FSUM <- ggplot(F_total_age_year, aes(x = year, y = Ftot, group = factor(age), color = factor(age), linetype = factor(age))) +
  geom_line(linewidth=0.7) + #change to 1.7 for individual plot
  geom_point(size=0.5) + #change to 4 for individual plot
  scale_color_manual(values = green_palette, name = "Age", labels = paste("Age", 0:4)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dotted")) +
  geom_vline(aes(xintercept = 2024), linetype = "dashed") +
  labs(
    title = "Total Fishing Mortality (F) per Age Over Time",
    y = "F (year⁻¹)", x = "Years"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    legend.position = c(0.756, 1.18),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.box.background = element_rect(color = NA, size = 0.15),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(0.1, 'cm')
  ) +
  scale_x_continuous(breaks = seq(min(F_total_age_year$year), max(F_total_age_year$year), by = 3)) +
  scale_y_continuous(labels = label_comma()) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5),  # Show round points, larger size
      label.position = "left"
    ),
    linetype = "none"  # Hide separate linetype legend
  )

########################### GRID PLOTS #########################
setwd("C:/Users/chris/Desktop/LATEST MODEL/PLOTS")

#########################################################
#################### CLAUS STUFF ########################
#########################################################

############# ASHBJORN
bottom_title <- grid::textGrob(
  "40% from Area 2 to 3 - Matrix Scenario",
  gp = grid::gpar(fontsize = 20, fontface = "bold")
)

fourty_2_3_migration_matrix <- list(SSBPAREA, RPAREA, CPAREA, FPAREA)
fourty_2_3_migration_matrix_grid <- arrangeGrob(grobs = fourty_2_3_migration_matrix, ncol = 2, nrow = 2, bottom = bottom_title)
grid::grid.draw(fourty_2_3_migration_matrix_grid)
# Save to file
png("C:/Users/chris/Desktop/LATEST MODEL/PLOTS/CLAUS/40_2_3_plots.png", width = 5000, height = 3000, res = 600)
grid::grid.draw(fourty_2_3_migration_matrix_grid)
dev.off()

############# ASHBJORN
bottom_title <- grid::textGrob(
  "Asbjorne Migration Matrix Scenario",
  gp = grid::gpar(fontsize = 20, fontface = "bold")
)

asbjorne_migration_matrix <- list(SSBPAREA, RPAREA, CPAREA, FPAREA)
asbjorne_migration_matrix_grid <- arrangeGrob(grobs = asbjorne_migration_matrix, ncol = 2, nrow = 2, bottom = bottom_title)
grid::grid.draw(asbjorne_migration_matrix_grid)
# Save to file
png("C:/Users/chris/Desktop/LATEST MODEL/PLOTS/CLAUS/asbjorne_plots.png", width = 5000, height = 3000, res = 600)
grid::grid.draw(asbjorne_migration_matrix_grid)
dev.off()

#########################################################
#################### CLAUS STUFF ########################
#########################################################

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 1/ADVICE - 0.0, 0.0, 0.0.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 1/ADVICE - 0.33, 0.33, 0.33.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 2/ADVICEB - 0.85, 0.85, 0.85.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 3/ONLYBREX - 0, 0.33, 0.0.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 4/ONLYBREX - 0, 0.33, 0.0.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 5/ONLYBREX - 0, 0.33, 0.0.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/SCENARIO 6/ONLYBREX - 0, 0.33, 0.0.png', width=5000, height=3000, res=600)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CASUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()



######################### SENSITIVITY PLOT ###################

SSB <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSB <- filter(SSB, years %in% 2004:2042)

S2 <- ggplot(SSB, aes(x = years, y = SSB, color = area)) +
  theme_bw() +
  geom_line(linewidth=0.7) + #change to 1.7 for individual plot
  geom_point(size=1.5) + #change to 4 for individual plot
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "SENSITIVITY - 0.1, 0.8, 0.1", y = "SSB (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) + #change to 20 for individual plot
  theme(legend.position = c(0.98, 0.91), legend.title=element_blank()) + #Use 0.93, 0.88 for individual plot
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/SSB/SENSITIVITY/SENSITIVITY1.png', width=5000, height=3000, res=300)
grid.arrange(S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, ncol = 5, nrow = 2)
dev.off()

#########################################################################
########################### CATCH ALONE PLOTS ###########################
#########################################################################

CatchADVICE <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "ADVICE")

CatchADVICEB <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "ADVICEB")

CatchLASTYEAR <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "LASTYEAR")

CatchFIVE <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "FIVE")

CatchFIVEB <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "FIVEB")

########################################

NATURAL <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "NATURAL")

VULNERABLE <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "VULNERABLE")

SUSTAINABLE <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
  group_by(year, space) %>%
  summarise(Ctot = sum(Catch)) %>% 
  mutate(year = as.numeric(as.character(year)), scenario = "SUSTAINABLE")

#CatchNOF <- as.data.frame.table(x$Catch.save.age, responseName = 'Catch') %>% 
#  group_by(year, space) %>%
#  summarise(Ctot = sum(Catch)) %>% 
#  mutate(year = as.numeric(as.character(year)), scenario = "CNOF")

CADVICE <- CatchADVICE %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "ADVICE")
CADVICEB <- CatchADVICEB %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "ADVICEB")
CLASTYEAR <- CatchLASTYEAR %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "LASTYEAR")
CFIVE <- CatchFIVE %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "FIVE")
CFIVEB <- CatchFIVEB %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "FIVEB")
#CNOF <- CatchNOF %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
#  mutate(Area = 'SA1', scenario = "CNOF")

CNATURAL <- NATURAL %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "NATURAL")
CVULNERABLE <- VULNERABLE %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "VULNERABLE")
CSUSTAINABLE <- SUSTAINABLE %>% group_by(year) %>% summarise(Ctot = sum(Ctot)) %>%  
  mutate(Area = 'SA1', scenario = "SUSTAINABLE")

CATCHALLSUM <- data.frame(CADVICE, CADVICEB, CLASTYEAR, CFIVE, CFIVEB)
names(CATCHALLSUM) <- c('year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario')
CADVICEP <- CADVICE %>% filter(CADVICE$year %in% 2022:2032)
CADVICEBP <- CADVICEB %>% filter(CADVICEB$year %in% 2022:2032)
CLASTYEARP <- CLASTYEAR %>% filter(CLASTYEAR$year %in% 2022:2032) 
CFIVEP <- CFIVE %>% filter(CFIVE$year %in% 2022:2032) 
CFIVEBP <- CFIVEB %>% filter(CFIVEB$year %in% 2022:2032)

###################################

CNATURALP <- CNATURAL %>% filter(CNATURAL$year %in% 2022:2032)
CVULNERABLEP <- CVULNERABLE %>% filter(CVULNERABLE$year %in% 2022:2032)
CSUSTAINABLEP <- CSUSTAINABLE %>% filter(CSUSTAINABLE$year %in% 2022:2032)
CATCHALLSUMPLOT <- rbind(CNATURALP, CVULNERABLEP, CSUSTAINABLEP)

CATCHALLSUMPLOT <- rbind(CADVICEP, CADVICEBP, CLASTYEARP, CFIVEP, CFIVEBP)
#########################################################################
#########################CATCH AND SSB SCENARIOS ########################
#########################################################################

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/FORECAST/CATCHSCENARIOS.png', width=5000, height=3000, res=300)

CATCHGRID <- ggplot(data = CATCHALLSUMPLOT, aes(x = year, y = Ctot)) +
  geom_line(aes(color = scenario, linetype = scenario), linewidth=5) +
  #scale_color_manual(values=c("#00264A", "#00264A", "#66a103", "#66a103", "#f8766d")) +
  #scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed", "dotted"))+
  scale_color_manual(values=c("#00ba38", "#619cff", "#f8766d")) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "CATCH SCENARIOS 2022-2032 YEARS", y = "Catch (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(CATCHALLSUMPLOT$year), max(CATCHALLSUMPLOT$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  #theme(legend.position = c(0.92, 0.83), legend.title=element_blank()) + #INDIVIDUAL PLOT
  #theme(legend.position = c(0.83, 0.83), legend.title=element_blank()) + #GRID FROM 2004
  theme(legend.position = c(0.90, 0.17), legend.title=element_blank()) + #GRID FROM 2022
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))
#scale_color_manual(values=c("#9CB8E7", "#205098", "#5b8e23", "#A9C648", "#005800")) +

dev.off()
#########################################################################
########################### SSB PLOTS ###########################
#########################################################################

SSBADVICE <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBADVICEB <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBLASTYEAR <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBFIVE <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBFIVEB <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

#SSBNOF <- as.data.frame(x$SSB) %>%
#  mutate(years = df.new$years) %>%
#  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBNATURAL <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBVULNERABLE <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

SSBSUSTAINABLE <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')



SADVICE <- SSBADVICE %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "ADVICE")
SADVICEB <- SSBADVICEB %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "ADVICEB")
SLASTYEAR <- SSBLASTYEAR %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "LASTYEAR")
SFIVE <- SSBFIVE %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "FIVE")
SFIVEB <- SSBFIVEB %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "FIVEB")
#SNOF <- SSBNOF %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
#  mutate(Area = 'SA1', scenario = "NOF")

SNATURAL <- SSBNATURAL %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "NATURAL")
SVULNERABLE <- SSBVULNERABLE %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "VULNERABLE")
SSUSTAINABLE <- SSBSUSTAINABLE %>% group_by(years) %>% summarise(SSBtot = sum(SSB)) %>%  
  mutate(Area = 'SA1', scenario = "SUSTAINABLE")


SSBALLSUM <- data.frame(SADVICE, SADVICEB, SLASTYEAR, SFIVE, SFIVEB)
names(SSBALLSUM) <- c('years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario')
SADVICEP <- SADVICE %>% filter(SADVICE$years %in% 2022:2032)
SADVICEBP <- SADVICEB %>% filter(SADVICEB$years %in% 2022:2032) 
SLASTYEARP <- SLASTYEAR %>% filter(SLASTYEAR$years %in% 2022:2032) 
SFIVEP <- SFIVE %>% filter(SFIVE$years %in% 2022:2032)
SFIVEBP <- SFIVEB %>% filter(SFIVEB$years %in% 2022:2032)

#########################################

SNATURALP <- SNATURAL %>% filter(SNATURAL$years %in% 2022:2032)
SVULNERABLEP <- SVULNERABLE %>% filter(SVULNERABLE$years %in% 2022:2032)
SSUSTAINABLEP <- SSUSTAINABLE %>% filter(SSUSTAINABLE$years %in% 2022:2032)
SSBALLSUMPLOT <- rbind(SNATURALP,SVULNERABLEP,SSUSTAINABLEP)

SSBALLSUMPLOT <- rbind(SADVICEP,SADVICEBP,SLASTYEARP,SFIVEP,SFIVEBP)

#########################################################################
########################### SSB PLOTS #############################
#########################################################################
library(ggtext)

dev.copy(png,'C:/Users/chris/Desktop/LATEST MODEL/PLOTS/FORECAST/SSBSCENARIOS.png', width=5000, height=3000, res=300)

SSBGRID <- ggplot(data = SSBALLSUMPLOT, aes(x = years, y = SSBtot)) +
  geom_line(aes(color = scenario, linetype = scenario), linewidth=5) +
  #scale_color_manual(values=c("#00264A", "#00264A", "#66a103", "#66a103", "#f8766d")) +
  #scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed", "dotted"))+
  scale_color_manual(values=c("#00ba38", "#619cff", "#f8766d")) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024), linewidth=1) +
  labs(title = "SSB SCENARIOS 2022-2032 YEARS", y = "SSB (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(SSBALLSUMPLOT$years), max(SSBALLSUMPLOT$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  #theme(legend.position = c(0.93, 0.89), legend.title=element_blank()) + #INDIVIDUAL PLOT
  #theme(legend.position = c(0.825, 0.17), legend.title=element_blank()) + #FROM 2004
  theme(legend.position = c(0.25, 0.79), legend.title=element_blank()) + #FROM 2022
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm')) +
  geom_hline(yintercept=140824, linetype=3, linewidth=1, color = "black") +
  annotate("text", x = 2031.75, y = 150000, label = "B-escapement", size = 4, color = "black") +
  geom_hline(yintercept=105809, linetype=3, linewidth=1, color = "red") +
  annotate("text", x = 2032, y = 120000, label = "Blim", size = 4, color = "red")

dev.off()

dev.copy(png,'D:/Aquatic Engineering 2021/THESIS/Model/SSB/SCENARIOS/SSB.C.SCENARIOS.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/SSB/SCENARIOS/SSBCSCENARIO.png', width=5000, height=3000, res=300)
grid.arrange(SSBGRID, CATCHGRID, ncol = 2, nrow = 1)
dev.off()

################################################################################## NOT USING BELOW
scale_colour_manual(labels = c("Baseline", "Equal", "Fivefold", "Max", "Zero"), values = 2"#850000", "#a11000", "#bf3000", "#db4200", "#ff6700")) +
  scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed", "solid"))+



