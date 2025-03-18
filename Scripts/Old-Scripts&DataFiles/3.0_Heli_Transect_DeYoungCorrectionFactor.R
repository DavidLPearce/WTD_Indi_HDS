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

# Helicopter Data
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Formatted_Heli_Transect_Data.csv", row.names = 1)



# # -------------------------------
# # Extracting Helicopter Counts 
# # -------------------------------
# 
# # Counts
# F23_heli_counts <- nrow(heli_dat[which(heli_dat$Date == "9/8/2023" | heli_dat$Date == "9/9/2023"),])
# W24_heli_counts <- nrow(heli_dat[which(heli_dat$Date ==  "2/9/2024" | heli_dat$Date == "2/10/2024"),])
# F24_heli_counts <- nrow(heli_dat[which(heli_dat$Date ==  "9/6/2024" | heli_dat$Date == "9/7/2024"),])
# W25_heli_counts <- nrow(heli_dat[which(heli_dat$Date ==  "2/7/2025" | heli_dat$Date == "2/8/2025"),])
# 
# # Print Counts
# cat("Fall 2023 Helicopter Counts Were =", F23_heli_counts, "\n")
# cat("Winter 2024 Helicopter Counts Were =", W24_heli_counts, "\n")
# cat("Fall 2024 Helicopter Counts Were =", F24_heli_counts, "\n")
# cat("Winter 2025 Helicopter Counts Were =", W25_heli_counts, "\n")
# 
# # Organize counts by Season
# F23_cnts <- data.frame(Model = "Heli Counts", # Fall 2023
#                          Season = "Fall 2023",
#                          Data = "Helicopter",
#                          N = F23_heli_counts,
#                          LCI = NA,
#                          UCI = NA
# )
# 
# W24_cnts <- data.frame(Model = "Heli Counts", # Winter 2024
#                        Season = "Winter 2024",
#                        Data = "Helicopter",
#                        N = W24_heli_counts,
#                        LCI = NA,
#                        UCI = NA
# )
# 
# F24_cnts <- data.frame(Model = "Heli Counts", # Fall 2024
#                        Season = "Fall 2024",
#                        Data = "Helicopter",
#                        N = F24_heli_counts,
#                        LCI = NA,
#                        UCI = NA
# )
# 
# W25_cnts <- data.frame(Model = "Heli Counts", # Winter 2024
#                        Season = "Winter 2025",
#                        Data = "Helicopter",
#                        N = W25_heli_counts,
#                        LCI = NA,
#                        UCI = NA
# )
# 
# # Combine all counts
# All_Cnts <- rbind(F23_cnts, W24_cnts, F24_cnts, W25_cnts)
# 
# # Take a look
# print(All_Cnts)

# -------------------------------------------------------
# Formatting Estimates for Exporting
# ------------------------------------------------------- 
# 
# # Combine all the estimates
# All_TTE <- rbind(F23_TTE_est, W24_TTE_est , F24_TTE_est)
# print(All_TTE)
# 
# # Export
# saveRDS(All_TTE, "./Model_Objects/Cam_TTE_AbundEst.rds")

# ----------------------------- End of Script -----------------------------