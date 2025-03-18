# Author: David L. Pearce
# Description:
#             This script analyses 4 camera surveys to estimate white-tailed deer abundance, 
#             and extends the script and vignette written by Anna Moeller
#             for fitting Time to Event models using the spaceNtime R package.  
#             Which can be found here: 
#             https://github.com/annam21/spaceNtime
#                         

# Citation: 
#           TBD


# ------------------------------------------------------------------------------
#
#                               Load Packages
#
# ------------------------------------------------------------------------------

# Install packages (if needed)
# install.packages("tidyverse")
# remotes::install_github("annam21/spaceNtime")

# Load library
library(tidyverse)
library(spaceNtime)


# Set seed, scientific notation options, increase max print, and working directory
set.seed(123)
options(scipen = 9999)
options(max.print = 1000)  
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Camera trapping data
F23_wtd_cams <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2023.rds")
W24_wtd_cams <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Winter2024.rds")
F24_wtd_cams <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2024.rds")

# Deployment data
F23_deploy <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeploymentData_Fall2023.rds")
W24_deploy <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeploymentData_Winter2024.rds")
F24_deploy <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeploymentData_Fall2024.rds")

# Camera site covariates
site_covs <- read.csv("./Data/Survey_Data/Camera_Data/Camera_siteCovs.csv", row.names = 1)

# Read in site locations
site_dat <- read.csv("./Data/Survey_Data/Camera_Data/Cam_sites.csv")


# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# -------------------------------------------------------
# Deployment Information
# ------------------------------------------------------- 

# Camera deployment information
print(F23_deploy)

# ----------------------
# Calculating area
# ----------------------

# Area for a camera (a[i]) = 
# pi* r[i] ^2 * (theta[i]/360)
# theta is lens angle/field of view (FOV)

# Viewshed angle
theta = 38 # FOV degrees  
r = 10     # max view distance in meters **** this may change by camera

# Area in meters squared
area_m2 = ((theta/360) * (pi*r^2)) 

# Area in acres
# 1 square meter = 0.000247105 acres
area_acre = area_m2 * 0.000247105

# Area in hectares
# 1 square meter = 0.0001 hectares
areat_hectare = area_m2 * 0.0001

# Adding area measure
F23_deploy$area <- area_acre
W24_deploy$area <- area_acre
F24_deploy$area <- area_acre

# Renaming 
colnames(F23_deploy)[1] <- "cam" # Fall 2023
colnames(F23_deploy)[2] <- "start"
colnames(F23_deploy)[3] <- "end"
colnames(F23_deploy)[4] <- "area"

colnames(W24_deploy)[1] <- "cam" # Winter 2024
colnames(W24_deploy)[2] <- "start"
colnames(W24_deploy)[3] <- "end"
colnames(W24_deploy)[4] <- "area"

colnames(F24_deploy)[1] <- "cam" # Fall 2024
colnames(F24_deploy)[2] <- "start"
colnames(F24_deploy)[3] <- "end"
colnames(F24_deploy)[4] <- "area"

# Change sample start and end dates
# Note if a camera failed you would add a rows for when the 
# camera was operating
# All cameras ran the entire survey period. 
F23_start <-  as.POSIXct("2023-09-10 00:00:00", tz = "UTC") 
F23_end <- as.POSIXct("2023-10-10 23:59:59", tz = "UTC") 

W24_start <- as.POSIXct("2024-02-15 00:00:00", tz = "UTC") 
W24_end <- as.POSIXct("2024-03-15 23:59:59", tz = "UTC") 

F24_start <- as.POSIXct("2024-09-08 00:00:00", tz = "UTC") 
F24_end <- as.POSIXct("2024-10-08 23:59:59", tz = "UTC") 

F23_deploy$start <- F23_start
F23_deploy$end <- F23_end

# Take a look
print(F23_deploy)
print(W24_deploy)
print(F24_deploy)

# ----------------------
# Sample Period Length
# ----------------------

# Deer move at about 275m/hr, given that the view shed was
theta_rad <- theta * (pi / 180)
viewshed_width <- 2 * r * sin(theta_rad / 2)
print(viewshed_width)

# meters wide it would take a deer to cross the view shed about...
speed <- 275 # m/hr
speed_sec <- speed / 3600  # m/sec
time_sec <- viewshed_width / speed_sec # Time in seconds
time_min <- time_sec / 60  # Time in mins
print(time_sec)
print(time_min)

# 85 seconds or 1.4 mins
# so sampling periods should be around there
WTD_per <- time_sec
  
# -------------------------------------------------------
# Survey Data
# ------------------------------------------------------- 

colnames(F23_wtd_cams)

# Subset camera cam data to siteID, date_time, and count
F23_df <- F23_wtd_cams[,c(2, 6, 13)]
W24_df <- W24_wtd_cams[,c(2, 6, 13)]
F24_df <- F24_wtd_cams[,c(2, 6, 13)]

# Renaming 
colnames(F23_df)[1] <- "cam"
colnames(F23_df)[2] <- "datetime"
colnames(F23_df)[3] <- "count"

colnames(W24_df)[1] <- "cam"
colnames(W24_df)[2] <- "datetime"
colnames(W24_df)[3] <- "count"

colnames(F24_df)[1] <- "cam"
colnames(F24_df)[2] <- "datetime"
colnames(F24_df)[3] <- "count"

# Subset camera data to be between start and end
F23_df <- F23_df[which(F23_df$datetime >= F23_start & F23_df$datetime <= F23_end),]
W24_df <- W24_df[which(W24_df$datetime >= W24_start & W24_df$datetime <= W24_end),]
F24_df <- F24_df[which(F24_df$datetime >= F24_start & F24_df$datetime <= F24_end),]

# Take a look
head(F23_df, 5)
head(W24_df, 5)
head(F24_df, 5)

# Survey dates
F23_survey_dates <- c(F23_start, F23_end)
W24_survey_dates <- c(W24_start, W24_end)
F24_survey_dates <- c(F24_start, F24_end)


# -------------------------------------------------------
# Sampling Occasion
# ------------------------------------------------------- 

# Create TTE occasions
F23_occ <- tte_build_occ(
  per_length = WTD_per, # period length
  nper = 6,             # Number of periods
  time_btw = 4 * 3600,  # Time between periods in seconds
  study_start = F23_survey_dates[1],
  study_end = F23_survey_dates[2]
)

W24_occ <- tte_build_occ(
  per_length = WTD_per, # period length
  nper = 6,             # Number of periods
  time_btw = 4 * 3600,  # Time between periods in seconds
  study_start = W24_survey_dates[1],
  study_end = W24_survey_dates[2]
)

F24_occ <- tte_build_occ(
  per_length = WTD_per, # period length
  nper = 6,             # Number of periods
  time_btw = 4 * 3600,  # Time between periods in seconds
  study_start = F24_survey_dates[1],
  study_end = F24_survey_dates[2]
)



# Take a look
head(F23_occ, 5)
head(W24_occ, 5)
head(F24_occ, 5)

# -------------------------------------------------------
# Encounter Histories
# ------------------------------------------------------- 

# Creating encounter histories
F23_tte_eh <- tte_build_eh(F23_df, 
                           F23_deploy, 
                           F23_occ, 
                           WTD_per)

W24_tte_eh <- tte_build_eh(W24_df, 
                           W24_deploy, 
                           W24_occ, 
                           WTD_per)

F24_tte_eh <- tte_build_eh(F24_df, 
                           F24_deploy, 
                           F24_occ, 
                           WTD_per)

# Take a look
head(F23_tte_eh, 5)
head(W24_tte_eh, 5)
head(F24_tte_eh, 5)


# ------------------------------------------------------------------------------
#
#                           Time to Event Models
#
# ------------------------------------------------------------------------------


# Estimate abundance
F23_TTE <- tte_estN_fn(F23_tte_eh, study_area = 2710)
W24_TTE <- tte_estN_fn(W24_tte_eh, study_area = 2710)
F24_TTE <- tte_estN_fn(F24_tte_eh, study_area = 2710)

# Take a look
print(F23_TTE)
print(W24_TTE)
print(F24_TTE)


# -------------------------------------------------------
# Formatting Estimates for Exporting
# ------------------------------------------------------- 

F23_TTE_est <- data.frame(Model = "Cam TTE",
                         Season = "Fall 2023",
                         Season_Model = "F23 Cam TTE",
                         Data = "Camera",
                         N = F23_TTE$N,
                         LCI = F23_TTE$LCI,
                         UCI = F23_TTE$UCI
)


W24_TTE_est <- data.frame(Model = "Cam TTE",
                          Season = "Winter 2024",
                          Season_Model = "W24 Cam TTE",
                          Data = "Camera",
                          N = W24_TTE$N,
                          LCI = W24_TTE$LCI,
                          UCI = W24_TTE$UCI
)

F24_TTE_est <- data.frame(Model = "Cam TTE",
                          Season = "Fall 2024",
                          Season_Model = "F24 Cam TTE",
                          Data = "Camera",
                          N = F24_TTE$N,
                          LCI = F24_TTE$LCI,
                          UCI = F24_TTE$UCI
)


# Combine all the estimates
All_TTE <- rbind(F23_TTE_est, W24_TTE_est , F24_TTE_est)
print(All_TTE)

# Export l
saveRDS(All_TTE, "./Model_Objects/Cam_TTE_AbundEst.rds")



# ----------------------------- End of Script -----------------------------