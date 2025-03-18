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

# Install packages (if needed)
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

# DS Estimates 
F23_Heli_DS_est <- readRDS("./Model_Objects/F23_Heli_DS_AbundEst.rds") # Helicopter DS
F23_Heli_625segDS_est <- readRDS("./Model_Objects/F23_625segHeli_DS_AbundEst.rds")
F23_Heli_1250segDS_est <- readRDS("./Model_Objects/F23_1250segHeli_DS_AbundEst.rds")
F23_Heli_HDS_est <- readRDS("./Model_Objects/F23_Heli_HDS_AbundEst.rds") # Helicopter HDS
F23_Heli_625segHDS_est <- readRDS("./Model_Objects/F23_625segHeli_HDS_AbundEst.rds") 
F23_Heli_1250segHDS_est <- readRDS("./Model_Objects/F23_1250segHeli_HDS_AbundEst.rds")

#F23_Cam_Nmix_est <- readRDS("./Model_Objects/F23_Cam_Nmix_AbundEst.rds") # Camera

# HDS Estimates
W24_Heli_DS_est <- readRDS("./Model_Objects/W24_Heli_DS_AbundEst.rds") # Helicopter DS
W24_Heli_1250segDS_est <- readRDS("./Model_Objects/W24_1250segHeli_DS_AbundEst.rds") 
W24_Heli_625segDS_est <- readRDS("./Model_Objects/W24_625segHeli_DS_AbundEst.rds")
W24_Heli_HDS_est <- readRDS("./Model_Objects/W24_Heli_HDS_AbundEst.rds") # Helicopter HDS
W24_Heli_625segHDS_est <- readRDS("./Model_Objects/W24_625segHeli_HDS_AbundEst.rds")
W24_Heli_1250segHDS_est <- readRDS("./Model_Objects/W24_1250segHeli_HDS_AbundEst.rds")
#W24_Cam_TTE_est <- readRDS("./Model_Objects/W24_Cam_Nmix_AbundEst.rds") # Camera

# Cam Estimates 
F24_Heli_DS_est <- readRDS("./Model_Objects/F24_Heli_DS_AbundEst.rds") # Helicopter DS
F24_Heli_1250segDS_est <- readRDS("./Model_Objects/F24_1250segHeli_DS_AbundEst.rds")
F24_Heli_625segDS_est <- readRDS("./Model_Objects/F24_625segHeli_DS_AbundEst.rds")
F24_Heli_HDS_est <- readRDS("./Model_Objects/F24_Heli_HDS_AbundEst.rds")
F24_Heli_625segHDS_est <- readRDS("./Model_Objects/F24_625segHeli_HDS_AbundEst.rds")
F24_Heli_1250segHDS_est <- readRDS("./Model_Objects/F24_1250segHeli_HDS_AbundEst.rds")
#F24_Cam_TTE_est <- readRDS("./Model_Objects/F24_Cam_Nmix_AbundEst.rds") # Camera


# Winter 2025 Estimates
W25_Heli_DS_est <- readRDS("./Model_Objects/W25_Heli_DS_AbundEst.rds") # Helicopter DS
W25_Heli_1250segDS_est <- readRDS("./Model_Objects/W25_1250segHeli_DS_AbundEst.rds")
W25_Heli_625segDS_est <- readRDS("./Model_Objects/W25_625segHeli_DS_AbundEst.rds")
W25_Heli_HDS_est <- readRDS("./Model_Objects/W25_Heli_HDS_AbundEst.rds") # Helicopter HDS
W25_Heli_625segHDS_est <- readRDS("./Model_Objects/W25_625segHeli_HDS_AbundEst.rds")
W25_Heli_1250segHDS_est <- readRDS("./Model_Objects/W25_1250segHeli_HDS_AbundEst.rds")
# W25_Cam_Nmix <- readRDS() # Camera

# Cam Estimates
Cam_TTE_est <- readRDS("./Model_Objects/Cam_TTE_AbundEst.rds")

# Renaming model this will need deleted  *********************************
F23_Heli_625segHDS_est$Model <- "Heli 625seg HDS"
W24_Heli_625segHDS_est$Model <- "Heli 625seg HDS"
F24_Heli_625segHDS_est$Model <- "Heli 625seg HDS"
W25_Heli_625segHDS_est$Model <- "Heli 625seg HDS"

F23_Heli_1250segHDS_est$Model <- "Heli 1250seg HDS"
W24_Heli_1250segHDS_est$Model <- "Heli 1250seg HDS"
F24_Heli_1250segHDS_est$Model <- "Heli 1250seg HDS"
W25_Heli_1250segHDS_est$Model <- "Heli 1250seg HDS"

W25_Heli_625segHDS_est$Season <- "Winter 2025"
W25_Heli_1250segHDS_est$Season <- "Winter 2025"

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
# Organizing 1250segT DS Estimates 
# -------------------------------

# Combine all estimates
All_1250segDS_est <- rbind(F23_Heli_1250segDS_est, W24_Heli_1250segDS_est, F24_Heli_1250segDS_est, W25_Heli_1250segDS_est)

# Take a look
print(All_1250segDS_est)

# -------------------------------
# Organizing 625segT DS Estimates 
# -------------------------------

# Combine all estimates
All_625segDS_est <- rbind(F23_Heli_625segDS_est, W24_Heli_625segDS_est, F24_Heli_625segDS_est, W25_Heli_625segDS_est)

# Take a look
print(All_625segDS_est)


# -------------------------------
# Organizing HDS Estimates 
# -------------------------------

# Combining all Helicopter HDS estimates
All_Heli_HDS <- rbind(F23_Heli_HDS_est, W24_Heli_HDS_est, F24_Heli_HDS_est, W25_Heli_HDS_est)

# Take a look
print(All_Heli_HDS)

# -------------------------------
# Organizing 1250segT HDS Estimates 
# -------------------------------

# Combine all estimates
All_1250segHDS_est <- rbind(F23_Heli_1250segHDS_est, W24_Heli_1250segHDS_est, F24_Heli_1250segHDS_est, W25_Heli_1250segHDS_est)

# Take a look
print(All_1250segHDS_est)

# -------------------------------
# Organizing 625segT HDS Estimates 
# -------------------------------

# Combine all estimates
All_625segHDS_est <- rbind(F23_Heli_625segHDS_est, W24_Heli_625segHDS_est, F24_Heli_625segHDS_est, W25_Heli_625segHDS_est)

# Take a look
print(All_625segHDS_est)


# -------------------------------
# Combine Across Surveys and models
# -------------------------------

# Combine all helicopter models
all_est <- rbind(All_DS_est, 
                  All_1250segDS_est, 
                  All_625segDS_est, 
                  All_Heli_HDS, 
                  All_1250segHDS_est, 
                  All_625segHDS_est,
                  Cam_TTE_est)

# Factor by models
all_est <- all_est %>%
  mutate(Model = factor(Model,
                        levels = c(
                         "Heli DS",
                         "1250seg Heli DS",
                         "625seg Heli DS",
                         "Heli HDS",
                         "Heli 1250seg HDS",
                         "Heli 625seg HDS",
                         "Cam TTE"
                       ))
  )

# Subset by season
F23_est <- all_est[which(all_est$Season == "Fall 2023"),]
W24_est <- all_est[which(all_est$Season == "Winter 2024"),]
F24_est <- all_est[which(all_est$Season == "Fall 2024"),]
W25_est <- all_est[which(all_est$Season == "Winter 2025"),]

# -------------------------------
# Abundance Estimate Plot By Season
# -------------------------------

# Define custom colors for each model # "Heli CF" = "#000000",# black forestgreen
# Other colors
# "#009E73",  # Bluish Green
col_palette <- c( 
                 "Heli DS" = "#6A3D9A", # Purple
                 "1250seg Heli DS" = "#E69F00", # Orange
                 "625seg Heli DS" = "#228B22",  # Forest Green
                 
                 "Heli HDS" = "#1F3A93", # Blue
                 "Heli 1250seg HDS" = "#E41A1C", # Red
                 "Heli 625seg HDS" = "#CC79A7", # Reddish Purple
                 
                 "Cam TTE" = "#009E73"  # Bluish Green
)   

# Define Custom shape 
# "Heli CF" = 16, # Solid circle
# Square for CT TTE = 15
# X for CT DS
shape_palette <- c(

  "Heli DS" = 17,  # Triangle for DS
  "1250seg Heli DS" = 17,  
  "625seg Heli DS" = 17,  
  
  "Heli HDS" = 18,  # Diamond for HDS
  "Heli 1250seg HDS" = 18, 
  "Heli 625seg HDS" = 18, 
  
  "Cam TTE" = 15 # Square for TTE
) 


# Fall 2023
F23_abund_plot <- ggplot(F23_est, aes(x = Model, y = N, color = Model, shape = Model)) +
                geom_point(size = 4) +  
                geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                theme_minimal() +
                labs(title = "Fall 2023",
                     x = "Model",
                     y = "Abundance Estimate") +
                scale_color_manual(values = col_palette) +   
                scale_shape_manual(values = shape_palette) +    
                scale_y_continuous(limits = c(0, 1700), breaks = seq(0, 1700, by = 100)) +    
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

# View
print(F23_abund_plot)


# Export
ggsave(plot = F23_abund_plot, 
       "./Figures/Abundance_Plots/F23_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 


# Winter 2024
W24_abund_plot <- ggplot(W24_est, aes(x = Model, y = N, color = Model, shape = Model)) +
                        geom_point(size = 4) +  
                        geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                        theme_minimal() +
                        labs(title = "Winter 2024",
                             x = "Model",
                             y = "Abundance Estimate") +
                        scale_color_manual(values = col_palette) +   
                        scale_shape_manual(values = shape_palette) +   
                        scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 100)) +   
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

# View
print(W24_abund_plot)


# Export
ggsave(plot = W24_abund_plot, 
       "./Figures/Abundance_Plots/W24_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 


# Fall 2024
F24_abund_plot <- ggplot(F24_est, aes(x = Model, y = N, color = Model, shape = Model)) +
                        geom_point(size = 4) +  
                        geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                        theme_minimal() +
                        labs(title = "Fall 2024",
                             x = "Model",
                             y = "Abundance Estimate") +
                        scale_color_manual(values = col_palette) +   
                        scale_shape_manual(values = shape_palette) +  
                        scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 100)) +   
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

# View
print(F24_abund_plot)


# Export
ggsave(plot = F24_abund_plot, 
       "./Figures/Abundance_Plots/F24_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 



# Winter 2025
W25_abund_plot <- ggplot(W25_est, aes(x = Model, y = N, color = Model, shape = Model)) +
                        geom_point(size = 4) +  
                        geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                        theme_minimal() +
                        labs(title = "Winter 2025",
                             x = "Model",
                             y = "Abundance Estimate") +
                        scale_color_manual(values = col_palette) +   
                        scale_shape_manual(values = shape_palette) +   
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

# View
print(W25_abund_plot)


# Export
ggsave(plot = W25_abund_plot, 
       "./Figures/Abundance_Plots/W25_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 

 
 

# ----------------------------- End of Script -----------------------------