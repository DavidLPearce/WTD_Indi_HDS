# Author: David L. Pearce
# Description:
#             TBD

# Citation: 
#      TBD


# ------------------------------------------------------------------------------
#
#                               Loading R packages
#
# ------------------------------------------------------------------------------

## Install packages (if needed)
# install.packages("tidyverse")


# Load library
library(tidyverse)


# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")


# ------------------------------------------------------------------------------
#
#                                 Reading in data
#
# ------------------------------------------------------------------------------

# Read in camera trapping data
F23_cams <- read.csv("./Data/Survey_Data/Camera_Data/Raw_Data/LaCopita_Cams_Fall2023.csv")
W24_cams <- read.csv("./Data/Survey_Data/Camera_Data/Raw_Data/LaCopita_Cams_Winter2024.csv")
F24_cams <- read.csv("./Data/Survey_Data/Camera_Data/Raw_Data/LaCopita_Cams_Fall2024.csv")
 
# ------------------------------------------------------------------------------
#
#                                 Data Wrangling    
#
# ------------------------------------------------------------------------------

# -------------------------------------------------
# Format Date and Time  
# -------------------------------------------------

# Some datasets are in different formats. Creating afunction to standardize
cam_datetime_func <- function(df) {
df <- df %>%
  mutate(
    start_time = parse_date_time(start_time, orders = c("mdY HM", "mdY HMS", "Ymd HMS")),  # Standardize format
    end_time = parse_date_time(end_time, orders = c("mdY HM", "mdY HMS", "Ymd HMS")),      # Standardize format
    date = as.Date(start_time),                                                            # Extract date
    time = format(start_time, "%H:%M:%S"),                                                 # Extract time (HH:MM:SS)
    month = month(start_time, label = TRUE, abbr = FALSE),                                 # Extract month name
    day_of_year = yday(start_time),                                                        # Extract day of the year
    date_time = start_time                                                                 # Keep a full datetime column
  )
  
  return(df)
}

# Apply the function to each dataset
F23_cams <- cam_datetime_func(F23_cams)
W24_cams <- cam_datetime_func(W24_cams)
F24_cams <- cam_datetime_func(F24_cams)

# Check the first few rows
head(F23_cams, 5)
head(W24_cams, 5)
head(F24_cams, 5)


# -------------------------------------------------
# Column for Survey Start and End 
# -------------------------------------------------

# Survey Start
F23_cams$survey_start <- min(F23_cams$day_of_year, na.rm = TRUE)
W24_cams$survey_start <- min(W24_cams$day_of_year, na.rm = TRUE)
F24_cams$survey_start <- min(F24_cams$day_of_year, na.rm = TRUE)

# Survey End
F23_cams$survey_end <- max(F23_cams$day_of_year, na.rm = TRUE)
W24_cams$survey_end <- max(W24_cams$day_of_year, na.rm = TRUE)
F24_cams$survey_end <- max(F24_cams$day_of_year, na.rm = TRUE)

# Take a look
head(F23_cams, 5)
head(W24_cams, 5)
head(F24_cams, 5)

# -------------------------------------------------
# Extract site ID and site number  
# -------------------------------------------------

# Fall 2023
F23_cams <- F23_cams %>%
  mutate(
    site_id = str_extract(deployment_id, "Site\\d+"), # Extracts "SiteNumber"
    site_number = str_extract(deployment_id, "(?<=Site)\\d+")  # Extracts the number after "Site"
  )

# Take a look
head(F23_cams)

# Other seasons
W24_cams <- W24_cams %>%
  mutate(
    site_id = str_extract(deployment_id, "Site\\d+"), 
    site_number = str_extract(deployment_id, "(?<=Site)\\d+")   
  )
head(W24_cams)

F24_cams <- F24_cams %>%
  mutate(
    site_id = str_extract(deployment_id, "Site\\d+"), # Extracts "TX_Grassland_LaCopita"
    site_number = str_extract(deployment_id, "(?<=Site)\\d+")  # Extracts the number after "Site"
  )
head(F24_cams)

# -------------------------------------------------
# Deployment dataframe  
# -------------------------------------------------

# Creating a df for camera deployments using the min and max survey date_times for each camera
F23_deploy <- F23_cams %>%
  group_by(site_number) %>%   
  summarize(
    min_time = min(start_time, na.rm = TRUE),
    max_time = max(end_time, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  mutate(site_number = as.numeric(site_number)) %>%   
  arrange(site_number)  

head(F23_deploy)

W24_deploy <- W24_cams %>%
  group_by(site_number) %>%   
  summarize(
    min_time = min(start_time, na.rm = TRUE),
    max_time = max(end_time, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  mutate(site_number = as.numeric(site_number)) %>%   
  arrange(site_number)  

head(W24_deploy)
 
F24_deploy <- F24_cams %>%
  group_by(site_number) %>%   
  summarize(
    min_time = min(start_time, na.rm = TRUE),
    max_time = max(end_time, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  mutate(site_number = as.numeric(site_number)) %>%   
  arrange(site_number)   

head(F24_deploy)

# Convert to dataframes
F23_deploy <- as.data.frame(F23_deploy)
W24_deploy <- as.data.frame(W24_deploy)
F24_deploy <- as.data.frame(F24_deploy)

# -------------------------------------------------
# Subset to White-tailed Deer  
# -------------------------------------------------

F23_wtd_cams <- F23_cams[which(F23_cams$common_name == "White-tailed Deer"),]
W24_wtd_cams <- W24_cams[which(W24_cams$common_name == "White-tailed Deer"),]
F24_wtd_cams <- F24_cams[which(F24_cams$common_name == "White-tailed Deer"),]


# -------------------------------------------------
# Remove excess columns  
# -------------------------------------------------

# Columns to keep
keep_columns <- c("site_id", "site_number", "date",  "month", 
                  "day_of_year", "date_time", "start_time", "end_time", 
                  "time", "survey_start", "survey_end",
                  "common_name", "group_size", "age", "sex"   
)

# Subset 
F23_wtd_cams <- F23_wtd_cams[ , keep_columns]
W24_wtd_cams <- W24_wtd_cams[ , keep_columns]
F24_wtd_cams <- F24_wtd_cams[ , keep_columns]

# Take a look
head(F23_wtd_cams)
head(W24_wtd_cams)
head(F24_wtd_cams)


# -------------------------------------------------
# Order by Site, Date, Time 
# -------------------------------------------------

# Fall 2023
F23_wtd_cams <- F23_wtd_cams %>%
  mutate(site_number = as.numeric(site_number)) %>%  # site_number as numeric  
  arrange(site_number, date, time)  # Order by site number, then date, then time

head(F23_wtd_cams)

# Other Seasons
W24_wtd_cams <- W24_wtd_cams %>%
  mutate(site_number = as.numeric(site_number)) %>%  
  arrange(site_number, date, time)

head(W24_wtd_cams)

F24_wtd_cams <- F24_wtd_cams %>%
  mutate(site_number = as.numeric(site_number)) %>% 
  arrange(site_number, date, time)

head(F24_wtd_cams)

# -------------------------------------------------
# Export 
# -------------------------------------------------

# Export camera detection data
saveRDS(F23_wtd_cams, "./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2023.rds")
saveRDS(W24_wtd_cams, "./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Winter2024.rds")
saveRDS(F24_wtd_cams, "./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2024.rds")

# Export deployment data
saveRDS(F23_deploy, "./Data/Survey_Data/Camera_Data/LaCopita_DeploymentData_Fall2023.rds")
saveRDS(W24_deploy, "./Data/Survey_Data/Camera_Data/LaCopita_DeploymentData_Winter2024.rds")
saveRDS(F24_deploy, "./Data/Survey_Data/Camera_Data/LaCopita_DeploymentData_Fall2024.rds")

# ----------------------------- End of Script -----------------------------