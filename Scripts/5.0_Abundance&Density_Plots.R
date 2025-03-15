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
# install.packages("Distance")
# install.packages("spAbundance")

# Load library
library(tidyverse)
library(Distance)

# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Reading in data
#
# ------------------------------------------------------------------------------

# Fall 2023 Estimates 
F23_Heli_DS_est <- readRDS("./Model_Objects/F23_Heli_DS_AbundEst.rds")
F23_Heli_HDS_est <- readRDS("./Model_Objects/F23_Heli_HDS_AbundEst.rds")
F23_Cam_Nmix_est <- readRDS("./Model_Objects/F23_Cam_Nmix_AbundEst.rds")

# Winter 2024 Estimates
W24_Heli_DS_est <- readRDS("./Model_Objects/W24_Heli_DS_AbundEst.rds")
W24_Heli_HDS_est <- readRDS("./Model_Objects/W24_Heli_HDS_AbundEst.rds")
W24_Cam_Nmix_est <- readRDS("./Model_Objects/W24_Cam_Nmix_AbundEst.rds")

# Fall 2024 Estimates
F24_Heli_DS_est <- readRDS("./Model_Objects/F24_Heli_DS_AbundEst.rds")
F24_Heli_HDS_est <- readRDS("./Model_Objects/F24_Heli_HDS_AbundEst.rds")
F24_Cam_Nmix_est <- readRDS("./Model_Objects/F24_Cam_Nmix_AbundEst.rds")


# Winter 2025 Estimates
W25_Heli_DS_est <- readRDS("./Model_Objects/W25_Heli_DS_AbundEst.rds")
W25_Heli_HDS_est <- readRDS("./Model_Objects/W25_Heli_HDS_AbundEst.rds")
# W25_Cam_Nmix <- readRDS()

# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# -------------------------------
# Organizing DS Estimates 
# -------------------------------

# Combine all estimates
All_DS_est <- rbind(F23_Heli_DS_est, W24_Heli_DS_est, F24_Heli_DS_est, W25_Heli_DS_est)

# Take a look
print(All_DS_est)

# -------------------------------
# Organizing HDS Estimates 
# -------------------------------

# Combining all Helicopter HDS estimates
All_Heli_HDS <- rbind(F23_Heli_HDS_est, W24_Heli_HDS_est, F24_Heli_HDS_est)

# Take a look
print(All_Heli_HDS)

# -------------------------------
# Organizing Nmix Estimates 
# -------------------------------

# Combining all Camera Nmix estimates
All_Cam_Nmix <- rbind(F23_Cam_Nmix_est, W24_Cam_Nmix_est, F24_Cam_Nmix_est)

# Take a look
print(All_Cam_Nmix)

# -------------------------------
# Combine Across Surveys and models 
# -------------------------------

All_estimates <- rbind(All_DS_est, All_Heli_HDS, All_Cam_Nmix) 

# Season and models are factors for plotting 
All_estimates <- All_estimates %>%
  mutate(Season_Model = factor(Season_Model, 
                               levels = c(
                                 
                        # Fall 2023
                        "F23 Heli DS", "F23 Heli HDS", "F23 Cam Nmix",
                        
                        # Winter 2024
                        "W24 Heli DS", "W24 Heli HDS", "W24 Cam Nmix",
                        
                        # Fall 2024
                        "F24 Heli DS", "F24 Heli HDS", "F24 Cam Nmix",
                        
                        # Winter 2025
                        "W25 Heli DS", "W25 Heli HDS"  
                        )) 
)  

# -------------------------------
# Abundance Estimate Plot  
# -------------------------------
 
# Define custom colors for each model
col_palette <- c("Heli CF" = "black", 
                 "Heli DS" = "orange",     
                 "Heli HDS" = "purple",
                 "Cam Nmix" = "blue",
                 "Cam TTE" = "red"
)   


# Abundance Estimates Fall 2023 - Winter 2025
ggplot(All_estimates, aes(x = Season_Model, y = N, color = Model)) +
  geom_point(size = 4, shape = 16) +  
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
  theme_minimal() +
  labs(title = "Fall 2023",
       x = "Model",
       y = "Estimate (N)") +
  scale_color_manual(values = col_palette) +   
  scale_y_continuous(limits = c(100, 900), breaks = seq(100, 900, by = 100)) +   
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),   
    axis.text.y = element_text(size = 12),   
    axis.title.x = element_text(size = 14, margin = margin(t = 10)), 
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    panel.grid = element_blank(),  
    axis.ticks = element_line(size = 0.8)  
)
