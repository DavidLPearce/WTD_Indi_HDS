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

# Fall 2023 Estimates 
F23_Heli_DS_est <- readRDS("./Model_Objects/F23_Heli_DS_AbundEst.rds") # Helicopter DS
F23_Heli_625segDS_est <- readRDS("./Model_Objects/F23_625segHeli_DS_AbundEst.rds")
F23_Heli_1250segDS_est <- readRDS("./Model_Objects/F23_1250segHeli_DS_AbundEst.rds")
F23_Heli_HDS_est <- readRDS("./Model_Objects/F23_Heli_HDS_AbundEst.rds") # Helicopter HDS
F23_Heli_625segHDS_est <- readRDS("./Model_Objects/F23_625segHeli_HDS_AbundEst.rds") 
F23_Heli_1250segHDS_est <- readRDS("./Model_Objects/F23_1250segHeli_HDS_AbundEst.rds")

#F23_Cam_Nmix_est <- readRDS("./Model_Objects/F23_Cam_Nmix_AbundEst.rds") # Camera

# Winter 2024 Estimates
W24_Heli_DS_est <- readRDS("./Model_Objects/W24_Heli_DS_AbundEst.rds") # Helicopter DS
W24_Heli_1250segDS_est <- readRDS("./Model_Objects/W24_1250segHeli_DS_AbundEst.rds") 
W24_Heli_625segDS_est <- readRDS("./Model_Objects/W24_625segHeli_DS_AbundEst.rds")
W24_Heli_HDS_est <- readRDS("./Model_Objects/W24_Heli_HDS_AbundEst.rds") # Helicopter HDS
W24_Heli_625segHDS_est <- readRDS("./Model_Objects/W24_625segHeli_HDS_AbundEst.rds")
W24_Heli_1250segHDS_est <- readRDS("./Model_Objects/W24_1250segHeli_HDS_AbundEst.rds")
#W24_Cam_Nmix_est <- readRDS("./Model_Objects/W24_Cam_Nmix_AbundEst.rds") # Camera

# Fall 2024 Estimates 
F24_Heli_DS_est <- readRDS("./Model_Objects/F24_Heli_DS_AbundEst.rds") # Helicopter DS
F24_Heli_1250segDS_est <- readRDS("./Model_Objects/F24_1250segHeli_DS_AbundEst.rds")
F24_Heli_625segDS_est <- readRDS("./Model_Objects/F24_625segHeli_DS_AbundEst.rds")
F24_Heli_HDS_est <- readRDS("./Model_Objects/F24_Heli_HDS_AbundEst.rds")
F24_Heli_625segHDS_est <- readRDS("./Model_Objects/F24_625segHeli_HDS_AbundEst.rds")
F24_Heli_1250segHDS_est <- readRDS("./Model_Objects/F24_1250segHeli_HDS_AbundEst.rds")
#F24_Cam_Nmix_est <- readRDS("./Model_Objects/F24_Cam_Nmix_AbundEst.rds") # Camera


# Winter 2025 Estimates
W25_Heli_DS_est <- readRDS("./Model_Objects/W25_Heli_DS_AbundEst.rds") # Helicopter DS
W25_Heli_1250segDS_est <- readRDS("./Model_Objects/W25_1250segHeli_DS_AbundEst.rds")
W25_Heli_625segDS_est <- readRDS("./Model_Objects/W25_625segHeli_DS_AbundEst.rds")
W25_Heli_HDS_est <- readRDS("./Model_Objects/W25_Heli_HDS_AbundEst.rds") # Helicopter HDS
W25_Heli_625segHDS_est <- readRDS("./Model_Objects/W25_625segHeli_HDS_AbundEst.rds")
W25_Heli_1250segHDS_est <- readRDS("./Model_Objects/W25_1250segHeli_HDS_AbundEst.rds")
# W25_Cam_Nmix <- readRDS() # Camera

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


# # -------------------------------
# # Organizing Nmix Estimates 
# # -------------------------------
# 
# # Combining all Camera Nmix estimates
# All_Cam_Nmix <- rbind(F23_Cam_Nmix_est, W24_Cam_Nmix_est, F24_Cam_Nmix_est)
# 
# # Take a look
# print(All_Cam_Nmix)

# # -------------------------------
# # Combine Across Surveys and models 
# # -------------------------------
# 
# # Combine all estimates
# All_estimates <- rbind(All_DS_est, All_Heli_HDS, All_1250segDS_est, All_625segDS_est, All_Cam_Nmix) 
# 
# # Season and models are factors for plotting 
# All_estimates <- All_estimates %>%
#   mutate(Season_Model = factor(Season_Model, 
#                                levels = c(
#                                  
#                         # Fall 2023
#                         "F23 Heli DS", "F23 Heli HDS", "F23 1250seg Heli DS", "F23 625seg Heli DS", "F23 Cam Nmix",
#                         
#                         # Winter 2024
#                         "W24 Heli DS", "W24 Heli HDS", "W24 1250seg Heli DS", "W24 625seg Heli DS", "W24 Cam Nmix",
#                         
#                         # Fall 2024
#                         "F24 Heli DS", "F24 Heli HDS", "F24 1250seg Heli DS", "F24 625seg Heli DS", "F24 Cam Nmix",
#                         
#                         # Winter 2025
#                         "W25 Heli DS", "W25 Heli HDS", "W25 1250seg Heli DS", "W25 625seg Heli DS"
#                         )) 
# )  
# 
# # Take a look
# print(All_estimates)

# -------------------------------
# All heli 
# -------------------------------

# Combine all helicopter models
all_heli <- rbind(All_DS_est, All_1250segDS_est, All_625segDS_est, All_Heli_HDS, All_1250segHDS_est, All_625segHDS_est)

# Factor by models                 
all_heli <- all_heli %>%
  mutate(Model = factor(Model, 
                               levels = c(
                                 "Heli DS", 
                                 "1250seg Heli DS", 
                                 "625seg Heli DS", 
                                 "Heli HDS", 
                                 "Heli 1250seg HDS",
                                 "Heli 625seg HDS" 
                               )) 
  )

# Subset by season
F23_All_Heli <- all_heli[which(all_heli$Season == "Fall 2023"),]
W24_All_Heli <- all_heli[which(all_heli$Season == "Winter 2024"),]
F24_All_Heli <- all_heli[which(all_heli$Season == "Fall 2024"),]
W25_All_Heli <- all_heli[which(all_heli$Season == "Winter 2025"),]

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
                 "Heli 625seg HDS" = "#CC79A7" # Reddish Purple
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
  "Heli 625seg HDS" = 18  
) 


# Fall 2023
F23_heli_abund_plot <- ggplot(F23_All_Heli, aes(x = Model, y = N, color = Model, shape = Model)) +
                geom_point(size = 4) +  
                geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                theme_minimal() +
                labs(title = "Fall 2023",
                     x = "Model",
                     y = "Abundance Estimate") +
                scale_color_manual(values = col_palette) +   
                scale_shape_manual(values = shape_palette) +    
                scale_y_continuous(limits = c(100, 1700), breaks = seq(100, 1700, by = 100)) +    
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
print(F23_heli_abund_plot)


# Export
ggsave(plot = F23_heli_abund_plot, 
       "./Figures/Abundance_Plots/F23_heli_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 


# Winter 2024
W24_heli_abund_plot <- ggplot(W24_All_Heli, aes(x = Model, y = N, color = Model, shape = Model)) +
                        geom_point(size = 4) +  
                        geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                        theme_minimal() +
                        labs(title = "Winter 2024",
                             x = "Model",
                             y = "Abundance Estimate") +
                        scale_color_manual(values = col_palette) +   
                        scale_shape_manual(values = shape_palette) +   
                        scale_y_continuous(limits = c(100, 1100), breaks = seq(100, 1100, by = 100)) +   
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
print(W24_heli_abund_plot)


# Export
ggsave(plot = W24_heli_abund_plot, 
       "./Figures/Abundance_Plots/W24_heli_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 

# Fall 2024
F24_heli_abund_plot <- ggplot(F24_All_Heli, aes(x = Model, y = N, color = Model, shape = Model)) +
                        geom_point(size = 4) +  
                        geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                        theme_minimal() +
                        labs(title = "Fall 2024",
                             x = "Model",
                             y = "Abundance Estimate") +
                        scale_color_manual(values = col_palette) +   
                        scale_shape_manual(values = shape_palette) +  
                        scale_y_continuous(limits = c(100, 800), breaks = seq(100, 800, by = 100)) +   
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
print(F24_heli_abund_plot)


# Export
ggsave(plot = F24_heli_abund_plot, 
       "./Figures/Abundance_Plots/F24_heli_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 

# Winter 2025
W25_heli_abund_plot <- ggplot(W25_All_Heli, aes(x = Model, y = N, color = Model, shape = Model)) +
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
print(W25_heli_abund_plot)


# Export
ggsave(plot = W25_heli_abund_plot, 
       "./Figures/Abundance_Plots/W25_heli_abund_plot.jpeg", 
       width = 10, height = 5, dpi = 300) 

 
 








# -------------------------------
# Abundance Estimate Plot  
# -------------------------------
 
# Define custom colors for each model
col_palette <- c("Heli CF" = "black", 
                 "Heli DS" = "orange",     
                 "Heli HDS" = "purple",
                 "1250seg Heli DS" = "blue",
                 "625seg Heli DS" = "red",
                 "Cam Nmix" = "forestgreen",
                 "Cam TTE" = "yellow"
)   


# Abundance Estimates Fall 2023 - Winter 2025
abund_plot <- ggplot(all_heli, aes(x = Season_Model, y = N, color = Model)) +
                      geom_point(size = 4, shape = 16) +  
                      geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +   
                      theme_minimal() +
                      labs(title = "",
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

# View
print(abund_plot)


# Export
ggsave(plot = abund_plot, "./Figures/Abundance_Plots/Comparison_Abundance_Plot.jpeg", width = 10, height = 5, dpi = 300) 
