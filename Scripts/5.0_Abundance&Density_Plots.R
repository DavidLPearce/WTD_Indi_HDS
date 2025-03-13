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
library(spAbundance)

# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Reading in data
#
# ------------------------------------------------------------------------------

# Helicopter Data
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Formatted_Heli_Transect_Data.csv", row.names = 1)


# Fall 2023 Data & Models 
F23_Heli_DS <- readRDS("./Model_Objects/F23_Heli_DS_AbundEst.rds")
F23_Heli_HDS <- readRDS("./Model_Objects/F23_Heli_HDS_AbundEst.rds")
F23_Cam_Nmix <- readRDS("./Model_Objects/F23_Cam_Nmix_AbundEst.rds")

# Winter 2024 Data & Models
W24_Heli_DS <- readRDS("./Model_Objects/W24_Heli_DS_AbundEst.rds")
W24_Heli_HDS <- readRDS("./Model_Objects/W24_Heli_HDS_AbundEst.rds")
W24_Cam_Nmix <- readRDS("./Model_Objects/W24_Cam_Nmix_AbundEst.rds")

# Fall 2024 Data & Models
F24_Heli_DS <- readRDS("./Model_Objects/F24_Heli_DS_AbundEst.rds")
F24_Heli_HDS <- readRDS("./Model_Objects/F24_Heli_HDS_AbundEst.rds")
F24_Cam_Nmix <- readRDS("./Model_Objects/F24_Cam_Nmix_AbundEst.rds")


# Winter 2025 Models
W25_Heli_DS <- readRDS("./Model_Objects/W25_Heli_DS_AbundEst.rds")
W25_Heli_HDS <- readRDS("./Model_Objects/W25_Heli_HDS_AbundEst.rds")
# W25_Cam_Nmix <- readRDS()



# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# -------------------------------
# Extracting Helicopter Counts 
# -------------------------------

# Counts
F23_heli_counts <- nrow(heli_dat[which(heli_dat$Date == "9/8/2023" | heli_dat$Date == "9/9/2023"),])
W24_heli_counts <- nrow(heli_dat[which(heli_dat$Date ==  "2/9/2024" | heli_dat$Date == "2/10/2024"),])
F24_heli_counts <- nrow(heli_dat[which(heli_dat$Date ==  "9/6/2024" | heli_dat$Date == "9/7/2024"),])
W25_heli_counts <- nrow(heli_dat[which(heli_dat$Date ==  "2/7/2025" | heli_dat$Date == "2/8/2025"),])

# Print Counts
cat("Fall 2023 Helicopter Counts Were =", F23_heli_counts, "\n")
cat("Winter 2024 Helicopter Counts Were =", W24_heli_counts, "\n")
cat("Fall 2024 Helicopter Counts Were =", F24_heli_counts, "\n")
cat("Winter 2025 Helicopter Counts Were =", W25_heli_counts, "\n")

# Organize counts by Season
F23_cnts <- data.frame(Model = "Heli Counts", # Fall 2023
                         Season = "Fall 2023",
                         Data = "Helicopter",
                         N = F23_heli_counts,
                         LCI = NA,
                         UCI = NA
)

W24_cnts <- data.frame(Model = "Heli Counts", # Winter 2024
                       Season = "Winter 2024",
                       Data = "Helicopter",
                       N = W24_heli_counts,
                       LCI = NA,
                       UCI = NA
)

F24_cnts <- data.frame(Model = "Heli Counts", # Fall 2024
                       Season = "Fall 2024",
                       Data = "Helicopter",
                       N = F24_heli_counts,
                       LCI = NA,
                       UCI = NA
)

W25_cnts <- data.frame(Model = "Heli Counts", # Winter 2024
                       Season = "Winter 2025",
                       Data = "Helicopter",
                       N = W25_heli_counts,
                       LCI = NA,
                       UCI = NA
)

# Combine all counts
All_Cnts <- rbind(F23_cnts, W24_cnts, F24_cnts, W25_cnts)

# Take a look
print(All_Cnts)


# -------------------------------
# Extracting DS Estimates 
# -------------------------------

# Fall 2023
F23_DS_est <- data.frame(Model = "Heli DS",
                         Season = "Fall 2023",
                         Data = "Helicopter",
                         N = F23_Heli_DS$Abundance[3],
                         LCI = F23_Heli_DS$LCI[3],
                         UCI = F23_Heli_DS$UCI[3]
)

# Winter 2024
W24_DS_est <- data.frame(Model = "Heli DS",
                         Season = "Winter 2024",
                         Data = "Helicopter",
                         N = W24_Heli_DS$Abundance[3],
                         LCI = W24_Heli_DS$LCI[3],
                         UCI = W24_Heli_DS$UCI[3]
)

# Fall 2024
F24_DS_est <- data.frame(Model = "Heli DS",
                         Season = "Fall 2024",
                         Data = "Helicopter",
                         N = F24_Heli_DS$Abundance[3],
                         LCI = F24_Heli_DS$LCI[3],
                         UCI = F24_Heli_DS$UCI[3]
)

# Winter 2025
W25_DS_est <- data.frame(Model = "Heli DS",
                         Season = "Winter 2025",
                         Data = "Helicopter",
                         N = W25_Heli_DS$Abundance[3],
                         LCI = W25_Heli_DS$LCI[3],
                         UCI = W25_Heli_DS$UCI[3]
)


# Combine all estimates
All_DS_est <- rbind(F23_DS_est, W24_DS_est, F24_DS_est, W25_DS_est)

# Take a look
print(All_DS_est)

# -------------------------------
# Extracting HDS Estimates 
# -------------------------------

# Combining all Helicopter HDS estimates
All_Heli_HDS <- rbind(F23_Heli_HDS, W24_Heli_HDS, F24_Heli_HDS, W25_Heli_HDS)



# -------------------------------
# Combine  Across Surveys and models 
# -------------------------------

All_estimates <- rbind(All_Cnts, All_DS_est ) #, All_Heli_HDS)

# Season and models are factors for plotting 
All_estimates <- All_estimates %>%
  mutate(Season = factor(Season, levels = c("Fall 2023", "Winter 2024", "Fall 2024", "Winter 2025")),
         Model = factor(Model, levels = c("Heli Counts", "Heli DS", "Heli HDS")))  

# -------------------------------
# Filter by Season
# -------------------------------

# Fall 2023
F23_all_est <- All_estimates %>%
                    filter(Season == "Fall 2023")


# -------------------------------
# Plots  
# -------------------------------
 
# Define custom colors for each model
col_palette <- c("Heli Counts" = "black", 
                  "Heli DS" = "orange",     
                  "Heli HDS" = "purple")   


# Fall 2023
ggplot(F23_all_est, aes(x = Model, y = N, color = Model)) +
  geom_point(size = 4, shape = 16) +  
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
  theme_minimal() +
  labs(title = "Fall 2023",
       x = "Model",
       y = "Estimate (N)") +
  scale_color_manual(values = col_palette) +   
  scale_y_continuous(limits = c(50, 700), breaks = seq(100, 700, by = 100)) +   
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
