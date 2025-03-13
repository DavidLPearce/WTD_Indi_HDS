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
# install.packages("activity")


# Load library
library(tidyverse)
library(activity) 



# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")


# ------------------------------------------------------------------------------
#
#                                 Reading in data
#
# ------------------------------------------------------------------------------

# Read in white-tailed deer camera trapping data
F23_cams <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2023.rds")
W24_cams <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Winter2024.rds")
F24_cams <- readRDS("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2024.rds")

# Read in site locations
site_dat <- read.csv("./Data/Survey_Data/Camera_Data/Cam_sites.csv")

# ------------------------------------------------------------------------------
#
#                                 Data Wrangling    
#
# ------------------------------------------------------------------------------

# Quick look at data
head(F23_cams, 5)
head(W24_cams, 5)
head(F24_cams, 5)

# -------------------------------------------------
# Stack Data For All Years
# -------------------------------------------------

#Allcams <- rbind(F23_cams, W24_cams, F24_cams)

# # -------------------------------------------------
# # Formatting Date Time 
# # -------------------------------------------------
# 
# # Format date time in POSIXct format  
# Allcams <- Allcams %>%
#   mutate(date_time = ymd_hms(date_time))
# 
# F23_cams <- F23_cams %>%
#   mutate(date_time = ymd_hms(date_time))
# 
# W24_cams <- W24_cams %>%
#   mutate(date_time = ymd_hms(date_time))
# 
# F24_cams <- F24_cams %>%
#   mutate(date_time = ymd_hm(date_time))

# -------------------------------------------------
# Adding Site Coordinates to Observation
# -------------------------------------------------

# Standardize site_id naming for merging
site_dat <- site_dat %>%
  mutate(SiteID = gsub(" ", "", SiteID)) %>%  # Remove spaces
  rename(site_id = SiteID)  # Rename column for merging

# # Adding in Coordinates
# Allcams <- Allcams %>%
#   left_join(site_dat, by = "site_id")

F23_cams <- F23_cams %>%
  left_join(site_dat, by = "site_id")

W24_cams <- W24_cams %>%
  left_join(site_dat, by = "site_id")

F24_cams <- F24_cams %>%
  left_join(site_dat, by = "site_id")

# -------------------------------------------------
# Calculate Solar Time 
# -------------------------------------------------

# # Calculate solar time using a temp object
# All_tmp <- solartime(Allcams$date_time,      # the date time column 
#                      Allcams$Lat,            # Latitude
#                      Allcams$Long,           # Longitude
#                      tz = -5,                # an offset in numeric hours to UTC
#                      format = "YYYY-MM-DD HH:MM:SS")

F23_tmp <- solartime(F23_cams$date_time,  
                     F23_cams$Lat,      
                     F23_cams$Long,   
                     tz = -5,           
                     format = "YYYY-MM-DD HH:MM:SS")

W24_tmp <- solartime(W24_cams$date_time,  
                     W24_cams$Lat,      
                     W24_cams$Long,   
                     tz = -5,           
                     format = "YYYY-MM-DD HH:MM:SS")

F24_tmp <- solartime(F24_cams$date_time,  
                     F24_cams$Lat,      
                     F24_cams$Long,   
                     tz = -5,           
                     format = "YYYY-MM-DD HH:MM:SS")

# Add in solar times
#Allcams$solar <- All_tmp$solar
F23_cams$solar <- F23_tmp$solar
W24_cams$solar <- W24_tmp$solar
F24_cams$solar <- F24_tmp$solar

# Add in clock times
#Allcams$clock <- All_tmp$clock
F23_cams$clock <- F23_tmp$clock
W24_cams$clock <- W24_tmp$clock
F24_cams$clock <- F24_tmp$clock

# -------------------------------------------------
# Fit Activity Model
# -------------------------------------------------

# Fit activity models using solar times (CI from data)
#All_actMod_solar <- fitact(Allcams$solar, sample = "data", reps = 100)
F23_actMod_solar <- fitact(F23_tmp$solar, sample = "data", reps = 100)
W24_actMod_solar <- fitact(W24_tmp$solar, sample = "data", reps = 100)
F24_actMod_solar <- fitact(F24_tmp$solar, sample = "data", reps = 100)
 
# Quick look
#plot(All_actMod_solar)
plot(F23_actMod_solar)
plot(W24_actMod_solar)
plot(F24_actMod_solar)

# # Fit activity models using clock times (CI from data)
# All_actMod_clock <- fitact(Allcams$clock, sample = "data", reps = 100)
# F23_actMod_clock <- fitact(F23_tmp$clock, sample = "data", reps = 100)
# W24_actMod_clock <- fitact(W24_tmp$clock, sample = "data", reps = 100)
# F24_actMod_clock <- fitact(F24_tmp$clock, sample = "data", reps = 100)
# 
# # Quick look
# plot(All_actMod_clock)
# plot(F23_actMod_clock)
# plot(W24_actMod_clock)
# plot(F24_actMod_clock)

# # Compare
# plot(All_actMod_solar)
# plot(All_actMod_clock)
# 
# plot(F23_actMod_solar)
# plot(F23_actMod_clock)
# 
# plot(W24_actMod_solar)
# plot(W24_actMod_clock)
# 
# plot(F24_actMod_solar)
# plot(F24_actMod_clock)

# -------------------------------------------------
# Extract Activity Estimates
# -------------------------------------------------


# Function to extract activity model data and convert to hours
ExtActDat <- function(model, season_name) {
  df <- as.data.frame(model@pdf)     # Extract PDF data
  df$Time <- df$x * (24 / (2 * pi))  # Convert radians to hours
  df$Season <- season_name           # Assign season label
  return(df)
}


# Extract PDF data for all seasons
#All_df <- ExtActDat(All_actMod_solar, "All Seasons")
F23_df <- ExtActDat(F23_actMod_solar, "Fall 2023")
W24_df <- ExtActDat(W24_actMod_solar, "Winter 2024")
F24_df <- ExtActDat(F24_actMod_solar, "Fall 2024")

# Combine all seasons into a single dataframe
act_df <- bind_rows( F23_df, W24_df, F24_df) #All_df,

 
# -------------------------------------------------
# Plot All data
# -------------------------------------------------

# Custom colors for each season
col_pallette <- c(#"All Seasons" = "black",
                  "Fall 2023" = "#E69F00",  
                  "Fall 2024" = "#6A3D9A",  
                  "Winter 2024" = "#E31A1C"   
                  
)

# Plot
Seasons_plot <- ggplot(act_df, aes(x = Time, y = y, color = Season)) +
                  geom_line(size = 1.2) +  # Activity density lines
                  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Season), 
                              alpha = 0.2,  # Increased opacity for better visibility
                              show.legend = FALSE) +  
                  scale_x_continuous(
                    breaks = seq(0, 24, by = 4),
                    labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM")
                  ) +
                  scale_color_manual(values = col_pallette) +  
                  scale_fill_manual(values = col_pallette) +   
                  labs(title = "",
                       x = "Time", y = "Density") +
                  theme_minimal() +
                  theme(
                    legend.position = c(0.05, 0.95), 
                    legend.justification = c(0, 1), 
                    legend.background = element_rect(fill = "white", color = "black"),
                    legend.key = element_rect(fill = "white"),
                    legend.title = element_blank(),
                    text = element_text(size = 14),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    panel.background = element_blank()   
)

# view plot
print(Seasons_plot)

# Export                
ggsave(plot = Seasons_plot, "./Figures/Activity_Plots/All_Seasons_Activity.jpeg",
       width = 8, height = 5, dpi = 300) 
