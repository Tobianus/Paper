###############################################################################
# Sandeel multi-area Operating Model (OM) — MAIN SCRIPT
# Date: 2025-09-09
# Files: 
#   - 01_main_sandeel_OM.R              (this file)
#   - R/02_get_OM_parameters.R          (sourced)
#   - R/03_run_agebased_sms_OP.R        (sourced)
#
# Save the two function files into an "R" subfolder, then run this main script.
# RStudio tip: fold sections defined with lines like "# ---- Section ----".
###############################################################################


# ---- 0) LIBRARIES ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(reshape2)
  library(usethis)
  library(devtools)
  library(TMB)
  library(patchwork) # required by smsR
  library(abind)     # required by smsR
  library(smsR)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(scales)
})

# # If you ever need to reinstall TMB or smsR:
# remove.packages("TMB")
# install.packages("TMB", type = "source")
# devtools::install_github("https://github.com/nissandjac/smsR", dependencies = TRUE)

# ---- NOTES: LIBRARIES ----
# - smsR supplies access to the fitted SMS stock assessment (e.g., getF, getSel, getCatchability).
# - TMB is the estimation backend used by smsR but here we mostly read parameters from a fitted model.
# - abind helps replicate arrays across spaces (areas).
# - ggplot2/patchwork/scales for plotting and combining plots.
# - tidyverse/dplyr/reshape2 for data wrangling.
# - Suppressing startup messages keeps the console tidy.


# ---- 1) PATHS & INPUTS ----
# setwd("D:/Aquatic Engineering 2021/THESIS/Model")
setwd("C:/Users/chris/Desktop/LATEST MODEL")

# Source function files (save them in ./R/ before running)
source('R/get_OM_parameters_10_09_2025.R')
source('R/run_agebased_sms_OP_10_09_2025.R')

# Read parameters from stock assessment (choose path as needed)
# parms <- readRDS("~/Github/sandeel_space/sandeel 1r/sandeel_1r_parms.rds")
# parms <- readRDS("D:/Aquatic Engineering 2021/THESIS/Model/sandeel 1r/sandeel_1r_parms.rds")
parms <- readRDS("DATA/sandeel 1r/Area1r.rds")

sas    <- parms[[2]]  # fitted smsR assessment
df.tmb <- parms[[1]]  # smsR input data (arrays for F, M, weights, maturity, etc.)

# ---- NOTES: PATHS & INPUTS ----
# - 'parms' is a list saved by your previous workflow; slot [[1]] holds df.tmb (arrays & metadata),
#   and slot [[2]] holds the fitted smsR object (sas).
# - df.tmb contains: years, nage, nseason, age, Mat, weca, west, M, propF, propM,
#   Fbarage (age range for Fbar), betaSR, recseason, survey timing, etc.
# - The fitted 'sas' object provides estimated parameters & derived quantities via smsR getters.

# ---- 2) LARVAL (AGE-0) ROUTING MATRIX ----
areas <- c("EU SOUTH","UK","EU NORTH")
move_age0 <- matrix(c(
  
  # ----- ROW 1: TO EU SOUTH -----
  1.0,  # from EU SOUTH → to EU SOUTH
  0.0,    # from UK → to EU SOUTH
  0.02,  # from EU NORTH → to EU SOUTH
  
  # ----- ROW 2: TO UK -----
  0.0,  # from EU SOUTH → to UK
  0.98,  # from UK → to UK
  0.02,  # from EU NORTH → to UK
  
  # ----- ROW 3: TO EU NORTH -----
  0.0,  # from EU SOUTH → to EU NORTH
  0.02,    # from UK → to EU NORTH
  0.96   # from EU NORTH → to EU NORTH

  ), nrow = 3, byrow = TRUE,
dimnames = list(to = areas, from = areas))

# ---- NOTES: LARVAL ROUTING ----
# - Controls how age-0 fish redistribute across areas per year.
# - Convention: rows are destinations (TO), columns are origins (FROM).
# - Example: 0.2143 in row "EU SOUTH", col "EU NORTH" → 21.43% of EU NORTH age-0 route to EU SOUTH.
# - Age-1+ movement is disabled in this OM; only larvae (age-0) move.


# ---- 3) BUILD OPERATING MODEL (OM) PARAMETER LIST ----
df.OM <- get_OM_parameters(
  df.tmb, sas,
  nspace    = 3,                    # number of spatial areas
  rec.space = c(0.11, 0.65, 0.23)   # relative fraction of recruits per space (sums to 1)
)
move_age0 <- check_move_matrix(move_age0, areas, normalize = FALSE)
df.OM$move_age0 <- move_age0  # inject larval routing matrix

# ---- NOTES: OM BUILD ----
# - nspace: how many areas (EU SOUTH, UK, EU NORTH). Life-history arrays are replicated across spaces.
# - movemax: an upper bound on adult movement *if* enabled; here kept at 0 → adults do not move.
# - rec.space: splits total recruits Rin[year] across areas; it is rescaled to sum 1 if needed.
# - recruitment mode defaults to "estimated": we replay historical Rin by year and allocate by rec.space.
# - df.OM$move_age0 installs the larval routing. Adults have NONE (movemat is zeros).


# ---- 4) RUN OM SIMULATION ----
x <- run.agebased.sms.op(df.OM)
str(df.OM)  # inspect OM structure

# ---- NOTES: RUN OM ----
# - The main loop iterates year × season × space.
# - Age-0 routing occurs twice: (a) very early at season 1 on any existing age-0, and
#   (b) immediately after recruitment assignment at df$rseason for the new cohort.
# - State tracked: N.save.age, SSB (year×space), Catch, CatchN, age comps, survey indices, Z, F.
# - End-of-year: ages advance with survival; plus-group accumulates oldest ages.
# - Fbar: average F over requested ages across space/season (see extra notes in function file).


# ---- 5) PLOT SSB BY AREA ----
SSB <- as.data.frame(x$SSB) %>%
  mutate(years = df.OM$years) %>%
  pivot_longer(1:df.OM$nspace, values_to = 'SSB', names_to = 'area')

p_ssb <- ggplot(SSB, aes(x = years, y = SSB, color = area)) +
  geom_line(linewidth = 2) +
  theme_classic() +
  scale_color_manual(values = c("1" = "#35465A", "2" = "#CC3300", "3" = "#008000"),
                     labels = areas) +
  labs(color = "Area")
print(p_ssb)

# ---- NOTES: PLOTTING ----
# - x$SSB is a year × space matrix; we reshape for ggplot.
# - Manual colors are mapped to area indices "1","2","3" and relabelled with 'areas'.
# - Adjust as desired (e.g., add theme elements, titles, etc.).
