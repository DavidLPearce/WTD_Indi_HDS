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

# Convert start_time to POSIXct format and extract Date, Time, Month, and Day of Year
F23_cams <- F23_cams %>%
  mutate(
    start_time = mdy_hm(start_time),  # Convert to date-time format (YYYY-MM-DD HH:MM)
    date = as.Date(start_time),       # Extract Date
    time = format(start_time, "%H:%M"),  # Extract Time (HH:MM)
    month = month(start_time, label = TRUE, abbr = FALSE),  # Extract Month Name
    day_of_year = yday(start_time)  # Extract Day of Year
  )

# Take a look
head(F23_cams)

# Other Seasons
W24_cams <- W24_cams %>%
  mutate(
    start_time = ymd_hms(start_time),       # Convert to date-time format (YYYY-MM-DD HH:MM:SS)
    date = as.Date(start_time),       
    time = format(start_time, "%H:%M:%S"),  # Extract Time in HH:MM:SS format
    month = month(start_time, label = TRUE, abbr = FALSE),  
    day_of_year = yday(start_time)   
  )
head(W24_cams)

F24_cams <- F24_cams %>%
  mutate(
    start_time = ymd_hms(start_time),       # Convert to date-time format (YYYY-MM-DD HH:MM:SS)
    date = as.Date(start_time),       
    time = format(start_time, "%H:%M:%S"),  # Extract Time in HH:MM:SS format
    month = month(start_time, label = TRUE, abbr = FALSE),  
    day_of_year = yday(start_time)   
  )
head(F24_cams)

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
# Subset to White-tailed Deer  
# -------------------------------------------------

F23_wtd_cams <- F23_cams[which(F23_cams$common_name == "White-tailed Deer"),]
W24_wtd_cams <- W24_cams[which(W24_cams$common_name == "White-tailed Deer"),]
F24_wtd_cams <- F24_cams[which(F24_cams$common_name == "White-tailed Deer"),]

# -------------------------------------------------
# Extract site ID and site number  
# -------------------------------------------------

# Fall 2023
F23_wtd_cams <- F23_wtd_cams %>%
  mutate(
    site_id = str_extract(deployment_id, "Site\\d+"), # Extracts "SiteNumber"
    site_number = str_extract(deployment_id, "(?<=Site)\\d+")  # Extracts the number after "Site"
  )

# Take a look
head(F23_wtd_cams)

# Other seasons
W24_wtd_cams <- W24_wtd_cams %>%
  mutate(
    site_id = str_extract(deployment_id, "Site\\d+"), 
    site_number = str_extract(deployment_id, "(?<=Site)\\d+")   
  )
head(W24_wtd_cams)

F24_wtd_cams <- F24_wtd_cams %>%
  mutate(
    site_id = str_extract(deployment_id, "Site\\d+"), # Extracts "TX_Grassland_LaCopita"
    site_number = str_extract(deployment_id, "(?<=Site)\\d+")  # Extracts the number after "Site"
  )
head(F24_wtd_cams)



# -------------------------------------------------
# Remove excess columns  
# -------------------------------------------------

# Columns to keep
keep_columns <- c("site_id", "site_number", "date",  "month", 
                  "day_of_year", "time", "survey_start", "survey_end",
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

# View the sorted dataframe
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

write.csv(F23_wtd_cams, "./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2023.csv")
write.csv(W24_wtd_cams, "./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Winter2024.csv")
write.csv(F24_wtd_cams, "./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2024.csv")

# ----------------------------- End of Script -----------------------------