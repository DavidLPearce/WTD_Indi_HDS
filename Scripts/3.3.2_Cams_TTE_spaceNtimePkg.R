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

# Take a look
print(F23_deploy)

# View shed width
theta_rad <- theta * (pi / 180)
viewshed_width <- 2 * r * sin(theta_rad / 2)
print(viewshed_width)

# ----------------------
# Sample Period Length
# ----------------------

# Deer move at about 275m/hr, given that the view shed was
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
WTD_per <- time_min
  
# -------------------------------------------------------
# Survey Data
# ------------------------------------------------------- 

colnames(F23_wtd_cams)

# Filter cam data to siteID, date_time, and count
F23_dat <- F23_wtd_cams[,c(2, 6, 13)]
head(F23_dat, 5)

# ------------------------------- 

 
library(dplyr)
library(lubridate)
library(tidyr)

# Define date range
F23_start <- as.Date("2023-09-10")
F23_end <- as.Date("2023-10-10")

# Convert date_time to POSIXct if not already
F23_dat <- F23_dat %>%
  mutate(date_time = as.POSIXct(date_time))

# Create a full site-date-period grid (only Period 1 & 2)
F23_grid <- expand.grid(
  site_number = unique(F23_dat$site_number),  # Get all sites
  date = seq.Date(F23_start, F23_end, by = "day"),  # All days in range
  period = c(1, 2)  # Only Period 1 (AM) and Period 2 (PM)
)

# Process detections (Only Period 1 & 2)
F23_sampling <- F23_dat %>%
  mutate(
    date = as.Date(date_time),  # Extract date
    hour = hour(date_time),     # Extract hour
    minute = minute(date_time), # Extract minutes
    period = case_when(  # Assign only Period 1 & 2
      hour == 7 & minute >= 30 | hour == 8 & minute < 30 ~ 1,  # 07:30-08:30 AM
      hour == 19 & minute >= 30 | hour == 20 & minute < 30 ~ 2, # 07:30-08:30 PM
      TRUE ~ NA_real_  # Remove anything outside the defined periods
    )
  ) %>%
  filter(!is.na(period)) %>%  # Keep only observations within defined periods
  group_by(site_number, date, period) %>%  # Group by site, date, and period
  slice_min(date_time, with_ties = FALSE) %>%  # Keep only first detection
  summarize(
    count = sum(group_size, na.rm = TRUE),  # Count from first detection only
    datetime = first(date_time),  # Store first detection datetime
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))  # Ensure date is Date type

# Merge with full site-date-period grid to ensure all site-date-periods exist
F23_final <- F23_grid %>%
  left_join(F23_sampling, by = c("site_number", "date", "period")) %>%
  mutate(
    count = ifelse(is.na(count), 0, count),  # Fill missing counts with 0
    datetime = case_when(
      !is.na(datetime) ~ datetime,  # Keep actual first detection time
      period == 1 ~ as.POSIXct(paste(date, "07:30:00"), tz = "UTC"),  # Fill missing with start time for Period 1
      period == 2 ~ as.POSIXct(paste(date, "19:30:00"), tz = "UTC")   # Fill missing with start time for Period 2
    ),
    Occasion = as.integer(date - F23_start + 1)  # Convert date to integer occasion
  ) %>%
  select(site_number, Occasion, Period = period, Count = count, datetime)  # Final column order

# Order by site and datetime
F23_final <- F23_final %>%
  arrange(site_number, datetime)

# View results
head(F23_final)

max(F23_final$Count)




# -------------------------------------------------------
# Sampling Occasion
# ------------------------------------------------------- 

# Once you have defined the length of your sampling period, you can build your sampling occasions. This can
# be done manually or with the function tte_build_occ(). The sampling occasions will be in a data.frame or tibble
# with the following structure:

WTD_study_dates <- as.POSIXct(c("2023-09-10 00:00:00", "2023-10-10 23:59:59"), tz = "GMT")

# -------------------------------------------------------
# Sampling Occasion
# ------------------------------------------------------- 

F23_occ <- tte_build_occ(
  per_length = 30,
  nper = 2,
  time_btw = 11.5 * 3600,
  study_start = WTD_study_dates[1],
  study_end = WTD_study_dates[2]
)

# -------------------------------------------------------
# Encounter Histories
# ------------------------------------------------------- 
F23_tte_eh <- tte_build_eh(F23_final, 
                           F23_deploy, 
                           F23_occ, 
                           WTD_per)
head(F23_tte_eh)




# ------------------------------------------------------------------------------
#
#                           Time to Event Models
#
# ------------------------------------------------------------------------------
