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
library(here)

#Using the new area selection script 'Coords_areas_catches_area1.R', We have now assigned a new F in the OM which calculates 
#the relative distribution of catches from 2010 to 2024. We then need to run the script 'F_NIS gives' which outputs
#'relative.catch'. Relative catch is then fed into 'R/get_OM_parameters_NIS_fix.R' when the model is run.

source(here("scripts/analysis", "OM_areas.R"))
source(here("scripts/analysis", "F_distribution.R"))
source(here("scripts/functions", "get_OM_parameters_F_distribution.R"))
source(here("scripts/functions", "run_agebased_sms_OP.R"))
source(here("scripts/functions", "addYear.R"))

# Read parameters from stock assessment
#parms <- readRDS("~/Github/sandeel_space/sandeel 1r/sandeel_1r_parms.rds")
parms <- readRDS(here("data/sandeel 1r", "area1r.rds"))

sas <- parms[[2]]
df.tmb <- parms[[1]]

df.OM <- get_OM_parameters(df.tmb, sas, # Parameters can be changed in this function
                           nspace = 3, # Number of spatial areas
                           movemax = c(0.1,0.2,0.1), # Movement between areas
                           rec.space = c(0.6,0.3,0.1)  # Recruitment allocation to areas
)

#df.OM$recruitment.fit <- list(mod1,mod2,mod3)

x <- run.agebased.sms.op(df.OM)

str(df.OM)


#########################  CATCH START #############################
# Define font sizes
base_size <- 16
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
    space == 1 ~ "UK EEZ",
    space == 2 ~ "EU North",
    space == 3 ~ "EU South",
    TRUE ~ as.character(space)
  )) %>%
  select(-space) %>% 
  select(Year, group, total_catch)

# CHANGE THE ORDER OF THE AREAS IN THE DATAFRAME FOR PLOTTING WITH UK FIRST
catch_df_filtered$group <- factor(catch_df_filtered$group,
                                  levels = c("UK EEZ", "EU North", "EU South"))


setwd("C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS")
# Step 6: Create the plot with ggplot2 and custom colors
p <- ggplot(catch_df_filtered, aes(x = Year, y = Total_Catch, color = Space_Name)) +
  geom_line(size = 1) +  # Plot lines for each space (area)
  geom_point(aes(shape = Space_Name), size = 3) +  # Add points with shapes for legend
  labs(
    title = "Total Catches Over Time (Season 1, from 2010 onwards)", 
    x = "Year", 
    y = "Total Catch", 
    color = "Area", 
    shape = "Area"  # Include shape legend for customization
  ) +
  theme(legend.position = "bottom") +  # Move the legend to the bottom
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +  # Shows every 2 years from 2010 to 2024
  scale_color_manual(values = c("UK EEZ" = "#35465A", "EU North" = "#CC3300", "EU South" = "#008000")) + # Color for each area
  scale_shape_manual(values = c("UK EEZ" = 16, "EU North" = 16, "EU South" = 16)) +  # Custom shapes
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
                  new_years = 20, # Number of years to simulate into the future
                  #F_future = c(0.33, 0.33, 0) ### ADVICE
                  ### FROM LATEST ADVICE
                  F_future = c(0, 0.33, 0) ### ADVICE BREXIT
                  ### FROM LATEST ADVICE ###Should be 0.36, 036, 0.00 but we drop uk bc 0 F
                  #F_future = c(0, 0.99, 0.001) ### LAST YEAR
                  ### YEAR 2024 VGT ONLY ### 0.56, 044, 0.00 but we drop uk bc 0 F
                  #F_future = c(0.56, 0.44, 0) ### FIVE YEARS
                  ### BEFORE BREXIT RELATIVE PROPORTION LAST 5 YEARS FROM 2024 VGT
                  #F_future = c(0, 0.44, 0) ### FIVE YEARS BREXIT
                  ### RELATIVE PROPORTION LAST 5 YEARS FROM 2024 VGT ### Should be 0.56, 044, 0.00 but we drop uk bc 0 F
                  
)

#ADD A AND B TO df.new for RICKER MODEL
df.new$mod <- list(mod1, mod2, mod3)

x <- run.agebased.sms.op(df.new)

SSB <- as.data.frame(x$SSB) %>%
  mutate(years = df.new$years) %>%
  pivot_longer(1:df.new$nspace, values_to = 'SSB', names_to = 'area')

R.save <- as.data.frame(x$R.save) %>%
  mutate(years = df.new$years) %>%
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


ggplot(SSB, aes(x = years, y = SSB, color = area)) +
  theme_bw() +
  geom_line(linewidth=1.5) + #change to 1.7 for individual plot
  geom_point(size=3.5) + #change to 4 for individual plot
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "SSB 2023-2032 YEARS", y = "SSB (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
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

ggplot(data = SSBsum, aes(x = years, y = SSBtot, fill = Area)) +
  geom_line(color = "#008822", linewidth=1.5) + 
  geom_point(color = "#008822", size=3.5) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "SSB (SUM) 2023-2032 YEARS", y = "SSB (Tonnes)", x = "Years") +
  geom_hline(yintercept=140824, linetype=3, linewidth=1, color = "black") +
  annotate("text", x = 2040, y = 150000, label = "B-escapement", size = 4, color = "black") +
  geom_hline(yintercept=105809, linetype=3, linewidth=1, color = "red") +
  annotate("text", x = 2041.5, y = 120000, label = "Blim", size = 4, color = "red") +
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
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "R (P/AREA) 2023-2032 YEARS", y = "Recruitment (Millions)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Area", labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
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
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "R (SUM) 2023-2032 YEARS", y = "Recruitment (Millions)", x = "Years") +
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
  mutate(year = as.numeric(as.character(year)))

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/CATCH P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/CATCH P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/CATCH P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/CATCH P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(Catch,aes(x = year, y= Ctot, color = space)) +
  theme_bw() +
  geom_line(linewidth=1.5) +
  geom_point(size=3.5) +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "CATCH (P/AREA) 2023-2032 YEARS", y = "Catch (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(name = "Area", labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
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

ggplot(data = CSUM, aes(x = year, y = Ctot, fill = Area)) +
  geom_line(color = "#008822", linewidth=1.5) + 
  geom_point(color = "#008822", size=3.5) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "CATCH (SUM) 2023-2032 YEARS", y = "Catch (Tonnes)", x = "Years") +
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

Fsea <- as.data.frame.table(x$Fseason) %>% filter(year %in% 1983:2042, season == "1") %>% group_by(year, space, season) %>% 
  mutate(year = as.numeric(as.character(year))) %>%
  summarise(Ftot = sum(Freq))

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/F P.AREA - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/F P.AREA - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/F P.AREA - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/F P.AREA - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(Fsea,aes(x = year, y= Ftot/3, color = space)) +
  theme_bw() +
  geom_line(linewidth=1.5) +
  geom_point(size=3.5) +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "F (P/AREA) 2023-2032 YEARS", y = "F (year^(-1))", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Area", labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = c(0.93, 0.86), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

#dev.off()

#############################################
#################### F SUM ##################
#############################################

Fbar <- as.data.frame(x$Fbar) %>%
  mutate(years = df.new$years) %>%
  mutate(Area = 'SA1') %>% 
  rename('Ftot' = 'x$Fbar')

#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/F SUM - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/F SUM - 0, 0.36, 0.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/F SUM - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
#dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/F SUM - 0, 0.44, 0.png', width=5000, height=3000, res=300)

ggplot(data = Fbar, aes(x = years, y = Ftot/3, fill = Area)) +
  geom_line(color = "#008822", linewidth=1.5, show.legend = FALSE) + 
  geom_point(color = "#008822", size=3.5, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "Fbar (SUM) 2023-2032 YEARS", y = "F (year^(-1))", x = "Years") +
  scale_x_continuous(breaks = seq(min(Fbar$year), max(Fbar$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = c(0.93, 0.89), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(1, 'cm'))

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

SSBPAREA <- ggplot(SSB, aes(x = years, y = SSB, color = area)) +
  theme_bw() +
  geom_line(linewidth=0.7) + #change to 1.7 for individual plot
  geom_point(size=1.5) + #change to 4 for individual plot
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "SSB (P/AREA) 2023-2032 YEARS", y = "SSB (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) + #change to 20 for individual plot
  theme(legend.position = c(0.94, 0.74), legend.title=element_blank()) + #Use 0.93, 0.88 for individual plot
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

####################################################
##################### SSB SUM ######################
####################################################

#SUMMING THE 3 AREAS FOR BLIM COMPARISON

SSBSUM <- ggplot(data = SSBsum, aes(x = years, y = SSBtot, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1.5, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "SSB (SUM) 2023-2032 YEARS", y = "SSB (Tonnes)", x = "Years") +
  geom_hline(yintercept=140824, linetype=3, linewidth=1, color = "black") +
  annotate("text", x = 2040, y = 140824, label = "B-escapement", size = 4, color = "black") +
  geom_hline(yintercept=105809, linetype=3, linewidth=1, color = "red") +
  annotate("text", x = 2041.5, y = 105809, label = "Blim", size = 4, color = "red") +
  scale_x_continuous(breaks = seq(min(SSBsum$years), max(SSBsum$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.92, 0.69), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

####################################################
#################### R PER AREA ####################
####################################################

RPAREA <- ggplot(R.save, aes(x = years, y = R.save/1000000, color = area)) +
  theme_bw() +
  geom_line(linewidth=0.7) +
  geom_point(size=1.5) +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "R (P/AREA) 2023-2032 YEARS", y = "Recruitment (Millions)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Area", labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.94, 0.74), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

####################################################
###################### R SUM #######################
####################################################

RSUM <- ggplot(data = R.savesum, aes(x = years, y = R.savetot, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1.5, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "R (SUM) 2023-2032 YEARS", y = "Recruitment (Millions)", x = "Years") +
  scale_x_continuous(breaks = seq(min(R.savesum$years), max(R.savesum$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.92, 0.69), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

#############################################
############### CATCH PER AREA ##############
#############################################

CPAREA <- ggplot(Catch,aes(x = year, y= Ctot, color = space)) +
  theme_bw() +
  geom_line(linewidth=0.7) +
  geom_point(size=1.5) +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "CATCH (P/AREA) 2023-2032 YEARS", y = "Catch (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Area", labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.94, 0.74), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

#############################################
################# CATCH SUM #################
#############################################

CSUM <- ggplot(data = CSUM, aes(x = year, y = Ctot, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1.5, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "CATCH (SUM) 2023-2032 YEARS", y = "Catch (Tonnes)", x = "Years") +
  scale_x_continuous(breaks = seq(min(CSUM$year), max(CSUM$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.92, 0.69), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

#############################################
############### F PER AREA ##################
#############################################

FPAREA <- ggplot(Fsea,aes(x = year, y= Ftot, color = space)) +
  theme_bw() +
  geom_line(linewidth=0.7) +
  geom_point(size=1.5) +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "F (P/AREA) 2023-2032 YEARS", y = "F (year^(-1))", x = "Years") +
  scale_x_continuous(breaks = seq(min(df.new$years), max(df.new$years), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Area", labels = c("UK", "EU North", "EU South"), values = c("#f8766d", "#00ba38", "#619cff")) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.94, 0.74), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

#############################################
#################### F SUM ##################
#############################################

FSUM <- ggplot(data = Fbar, aes(x = years, y = Ftot/3, fill = Area)) +
  geom_line(color = "#008822", linewidth=0.7, show.legend = FALSE) + 
  geom_point(color = "#008822", size=1.5, show.legend = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = 2024)) +
  labs(title = "Fbar (SUM) 2023-2032 YEARS", y = "F (year^(-1))", x = "Years") +
  scale_x_continuous(breaks = seq(min(Fbar$year), max(Fbar$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 15))) +
  scale_y_continuous(labels = label_comma()) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = c(0.92, 0.69), legend.title=element_blank()) +
  theme(legend.box.background = element_rect(color="black", size=0.3), legend.box.margin = margin(1, 1, 1, 1)) +
  theme(legend.key.size = unit(0.1, 'cm'))

########################### GRID PLOTS #########################

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.36-0.36-0/ADVICE - 0.36, 0.36, 0.png', width=5000, height=3000, res=300)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CSUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.36-0/ADVICE - 0, 0.36, 0.png', width=5000, height=3000, res=300)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CSUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.99-0.001/LASTYEAR - 0, 0.99, 0.001.png', width=5000, height=3000, res=300)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CSUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0.56-0.44-0/LAST5YEARS - 0.56,0.44,0.png', width=5000, height=3000, res=300)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CSUM, FPAREA, FSUM, ncol = 2, nrow = 4)
dev.off()


dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/0-0.44-0/LAST5YEARS - 0, 0.44, 0.png', width=5000, height=3000, res=300)
grid.arrange(SSBPAREA, SSBSUM, RPAREA, RSUM, CPAREA, CSUM, FPAREA, FSUM, ncol = 2, nrow = 4)
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


CATCHALLSUM <- data.frame(CADVICE, CADVICEB, CLASTYEAR, CFIVE, CFIVEB)
names(CATCHALLSUM) <- c('year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario', 'year','Ctot','Area', 'scenario')
CADVICEP <- CADVICE %>% filter(CADVICE$year %in% 2022:2032)
CADVICEBP <- CADVICEB %>% filter(CADVICEB$year %in% 2022:2032)
CLASTYEARP <- CLASTYEAR %>% filter(CLASTYEAR$year %in% 2022:2032) 
CFIVEP <- CFIVE %>% filter(CFIVE$year %in% 2022:2032) 
CFIVEBP <- CFIVEB %>% filter(CFIVEB$year %in% 2022:2032)

CATCHALLSUMPLOT <- rbind(CADVICEP, CADVICEBP, CLASTYEARP, CFIVEP, CFIVEBP)           
#########################################################################
#########################CATCH AND SSB SCENARIOS ########################
#########################################################################

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/CATCHSCENARIOS.png', width=5000, height=3000, res=300)

CATCHGRID <- ggplot(data = CATCHALLSUMPLOT, aes(x = year, y = Ctot)) +
  geom_line(aes(color = scenario, linetype = scenario), linewidth=2.5) +
  scale_color_manual(values=c("#00264A", "#00264A", "#66a103", "#66a103", "#f8766d")) +
  scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed", "dotted"))+
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


SSBALLSUM <- data.frame(SADVICE, SADVICEB, SLASTYEAR, SFIVE, SFIVEB)
names(SSBALLSUM) <- c('years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario', 'years','SSBtot','Area', 'scenario')
SADVICEP <- SADVICE %>% filter(SADVICE$years %in% 2022:2032)
SADVICEBP <- SADVICEB %>% filter(SADVICEB$years %in% 2022:2032) 
SLASTYEARP <- SLASTYEAR %>% filter(SLASTYEAR$years %in% 2022:2032) 
SFIVEP <- SFIVE %>% filter(SFIVE$years %in% 2022:2032)
SFIVEBP <- SFIVEB %>% filter(SFIVEB$years %in% 2022:2032)

SSBALLSUMPLOT <- rbind(SADVICEP,SADVICEBP,SLASTYEARP,SFIVEP,SFIVEBP)

#########################################################################
########################### SSB PLOTS #############################
#########################################################################
library(ggtext)

dev.copy(png,'C:/Users/chris/Desktop/DTU/R/ORIG/SMSR/PLOTS/SSBSCENARIOS.png', width=5000, height=3000, res=300)

SSBGRID <- ggplot(data = SSBALLSUMPLOT, aes(x = years, y = SSBtot)) +
  geom_line(aes(color = scenario, linetype = scenario), linewidth=2.5) +
  scale_color_manual(values=c("#00264A", "#00264A", "#66a103", "#66a103", "#f8766d")) +
  scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed", "dotted"))+
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



