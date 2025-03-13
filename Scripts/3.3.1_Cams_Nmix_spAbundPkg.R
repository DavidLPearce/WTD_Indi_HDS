# Author: David L. Pearce
# Description:
#             This script analyses 4 camera surveys to estimate white-tailed deer abundance, 
#             and extends the script and vignette written by Jeff Doser
#             for fitting N-mixture models in the spAbundance R package.  
#             Which can be found here: 
#             https://doserlab.com/files/spabundance-web/articles/nmixturemodels
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
# install.packages("spAbundance")

# Load library
library(tidyverse)
library(spAbundance)

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

# Camera Trapping Data
F23_wtd_cams <- read.csv("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2023.csv", row.names = 1) 
W24_wtd_cams <- read.csv("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Winter2024.csv", row.names = 1) 
F24_wtd_cams <- read.csv("./Data/Survey_Data/Camera_Data/LaCopita_DeerCams_Fall2024.csv", row.names = 1) 

# Camera Site Covariates
site_covs <- read.csv("./Data/Survey_Data/Camera_Data/Camera_siteCovs.csv", row.names = 1)

# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# -------------------------------------------------------
# Creating Detection Matrices
# ------------------------------------------------------- 

# Initialize a site x survey matrix for each season
F23_grid <- expand.grid(site_number = 1:27,
                           day_of_year = min(F23_wtd_cams$survey_start):max(F23_wtd_cams$survey_end)
        
)

W24_grid <- expand.grid(site_number = 1:27,
                            day_of_year = min(W24_wtd_cams$survey_start):max(W24_wtd_cams$survey_end)
)

F24_grid <- expand.grid(site_number = 1:27,
                            day_of_year = min(F24_wtd_cams$survey_start):max(F24_wtd_cams$survey_end)
)

# Summarize counts by site and survey day
F23_det_mat <- F23_wtd_cams %>%
  group_by(site_number, day_of_year) %>%
  summarise(Detections = sum(group_size, na.rm = TRUE), .groups = 'drop') %>%
  full_join(F23_grid, by = c("site_number", "day_of_year")) %>%  # Ensure all sites are included
  mutate(Detections = replace_na(Detections, 0)) %>%  # Fill missing detections with 0
  pivot_wider(names_from = day_of_year, values_from = Detections, values_fill = 0) %>%
  column_to_rownames(var = "site_number")  # Set row names as site_number

W24_det_mat <- W24_wtd_cams %>%
  group_by(site_number, day_of_year) %>%
  summarise(Detections = sum(group_size, na.rm = TRUE), .groups = 'drop') %>%
  full_join(W24_grid, by = c("site_number", "day_of_year")) %>%  
  mutate(Detections = replace_na(Detections, 0)) %>%  
  pivot_wider(names_from = day_of_year, values_from = Detections, values_fill = 0) %>%
  column_to_rownames(var = "site_number") 

F24_det_mat <- F24_wtd_cams %>%
  group_by(site_number, day_of_year) %>%
  summarise(Detections = sum(group_size, na.rm = TRUE), .groups = 'drop') %>%
  full_join(F24_grid, by = c("site_number", "day_of_year")) %>%  
  mutate(Detections = replace_na(Detections, 0)) %>%  
  pivot_wider(names_from = day_of_year, values_from = Detections, values_fill = 0) %>%
  column_to_rownames(var = "site_number") 

# Take a look
head(F23_det_mat)
head(W24_det_mat)
head(F24_det_mat)

# -------------------------------------------------------
# Creating Observation Covariates Matrix
# ------------------------------------------------------- 

# Creating a day of year matrix

# Get day of year values
F23_doy_values <- as.numeric(colnames(F23_det_mat))
W24_doy_values <- as.numeric(colnames(W24_det_mat))
F24_doy_values <- as.numeric(colnames(F24_det_mat))

# Create a matrix of day of year values
F23_doy_mat <- matrix(rep(F23_doy_values, each = nrow(F24_det_mat)), 
                             nrow = nrow(F23_det_mat), 
                             ncol = ncol(F23_det_mat), 
                             byrow = FALSE)

W24_doy_mat <- matrix(rep(W24_doy_values, each = nrow(W24_det_mat)), 
                      nrow = nrow(W24_det_mat), 
                      ncol = ncol(W24_det_mat), 
                      byrow = FALSE)

F24_doy_mat <- matrix(rep(F24_doy_values, each = nrow(F24_det_mat)), 
                      nrow = nrow(F24_det_mat), 
                      ncol = ncol(F24_det_mat), 
                      byrow = FALSE)

# Take a look
head(F23_doy_mat)
head(W24_doy_mat)
head(F24_doy_mat)

# -------------------------------------------------------
# Creating Site Covariates Matrix
# ------------------------------------------------------- 

# Removing SiteID, Lat, Long
site_cov_mat <- site_covs[,-c(1:3)]

# -------------------------------------------------------
# Format for spAbundance N-mixture models
# ------------------------------------------------------- 

# spAbundance uses long format
F23_spA_dat <- list(y = F23_det_mat,
                    abund.covs = site_cov_mat,
                    det.covs = list(DOY = F23_doy_mat)
)

W24_spA_dat <- list(y = W24_det_mat,
                   abund.covs = site_cov_mat,
                   det.covs = list(DOY = W24_doy_mat)
)

F24_spA_dat <- list(y = F24_det_mat,
                    abund.covs = site_cov_mat,
                    det.covs = list(DOY = F24_doy_mat)
)


# Check structure
str(F23_spA_dat)
str(W24_spA_dat)
str(F24_spA_dat)

# ------------------------------------------------------------------------------
#
#                           N-Mixture Models
#
# ------------------------------------------------------------------------------

# ----------------------
# MCMC Specifications
# ----------------------
batch.length <- 25
n.batch <- 10000
batch.length * n.batch # Total number of MCMC samples per chain
n.burn <- 60000
n.thin <- 10
n.chains <- 3


# ----------------------
# Set Priors
# ----------------------
priors <- list(alpha.normal = list(mean = 0, var = 10),  
               beta.normal = list(mean = 0, var = 10),
               sigma.sq.p.ig = list(mean = 0, var = 2.72)
)

# ----------------------
# Tuning
# ----------------------
tuning <- list(
  alpha = 0.25,  
  beta = 0.25,
  alpha.star = 0.25,
  kappa = 0.25
) 


# -------------------------------------------------------
#                     Fall 2023
# -------------------------------------------------------

# # ----------------------
# # Initial values
# # ----------------------
# F23_inits <- list(alpha = 0.1,               
#                  beta = 0.1,                
#                  sigma.sq.p = 0.1,
#                  N = apply(F23_spA_dat$y, 1, sum)
# ) 
# 
# # ----------------------
# # Detection Covariates
# # ----------------------
# 
# # Fit 0: Null model 
# F23_fm0 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ 1, 
#                 data = F23_spA_dat,
#                 family = 'Poisson',
#                 inits = F23_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)
# 
# # Fit 1: DOY Random Effect 
# F23_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ (1|DOY), 
#                 data = F23_spA_dat,
#                 family = 'Poisson',
#                 inits = F23_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)
# 
# # Fit 2: DOY Random Effect - Negative Binomial 
# F23_fm2 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ (1|DOY), 
#                 data = F23_spA_dat,
#                 family = 'NB',
#                 inits = F23_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)

# ----------------------
# Checking Convergence
# ----------------------


# # Fit 0: Null Model
# plot(F23_fm0, 'beta', density = FALSE)  # Abundance parameters
# plot(F23_fm0, 'alpha', density = FALSE) # Detection parameters
# F23_fm0$rhat                            # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off()                               # Clear plots
# 
# # Fit 1: DOY Random Effect
# plot(F23_fm1, 'beta', density = FALSE)  
# plot(F23_fm1, 'alpha', density = FALSE) 
# plot(F23_fm1, 'sigma.sq.p', density = TRUE)  # Random Effect
# F23_fm1$rhat                            
# dev.off()                             
# 
# # Fit 2: DOY Random Effect - Negative Binomial
# plot(F23_fm2, 'beta', density = FALSE)  
# plot(F23_fm2, 'alpha', density = FALSE) 
# plot(F23_fm2, 'sigma.sq.p', density = TRUE)
# plot(F23_fm2, 'kappa', density = TRUE)      # Overdisperison parameter 
# F23_fm2$rhat                            
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# # Calculating WAIC
# F23_fm0_waic <- waicAbund(F23_fm0)
# F23_fm1_waic <- waicAbund(F23_fm1)
# F23_fm2_waic <- waicAbund(F23_fm2)
# 
# # Extract the WAIC values for each model
# F23_waic_values <- c(F23_fm0_waic["WAIC"],
#                      F23_fm1_waic["WAIC"],
#                      F23_fm2_waic["WAIC"]
# )
# 
# # Create a named vector with model names
# F23_names <- c("fm0", 
#                "fm1",
#                "fm2"
# )
# 
# # Combine model names and WAIC values into a data frame for ranking
# F23_waic_df <- data.frame(Model = F23_names, 
#                           WAIC = F23_waic_values)
# 
# # Rank models based on WAIC (lower WAIC is better)
# F23_waic_df <- F23_waic_df[order(F23_waic_df$WAIC), ]
# 
# # Print the ranked models
# print(F23_waic_df)
# 
# # Best model is ...
# 
# # Check fit
# F23_bm_ppc <- ppcAbund(F23_fm2, fit.stat = "chi-squared", group = 1)
# summary(F23_bm_ppc)
# 
# # Fit Nb is pretty good
# 
# # Export best model
# saveRDS(F23_fm2, "./Model_Objects/F23_Cam_Nmix_BestModel.rds")



# -------------------------------------------------------
#                     Winter 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W24_inits <- list(alpha = 0.1,               
                  beta = 0.1,                
                  sigma.sq.p = 0.1,
                  N = apply(W24_spA_dat$y, 1, sum)
) 

# ----------------------
# Detection Covariates
# ----------------------

# # Fit 0: Null model 
# W24_fm0 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ 1, 
#                 data = W24_spA_dat,
#                 family = 'Poisson',
#                 inits = W24_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)
# 
# # Fit 1: DOY Random Effect 
# W24_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ (1|DOY), 
#                 data = W24_spA_dat,
#                 family = 'Poisson',
#                 inits = W24_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)

# Fit 2: DOY Random Effect - Negative Binomial 
W24_fm2 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
                det.formula = ~ (1|DOY), 
                data = W24_spA_dat,
                family = 'NB',
                inits = W24_inits, 
                priors = priors,
                tuning = tuning,
                accept.rate = 0.43,
                n.batch = n.batch,
                batch.length = batch.length,
                n.burn = n.burn,
                n.thin = n.thin, 
                n.chains = n.chains,
                verbose = FALSE)

# ----------------------
# Checking Convergence
# ----------------------


# # Fit 0: Null Model
# plot(W24_fm0, 'beta', density = FALSE)  # Abundance parameters
# plot(W24_fm0, 'alpha', density = FALSE) # Detection parameters
# W24_fm0$rhat                            # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off()                               # Clear plots
# 
# # Fit 1: DOY Random Effect
# plot(W24_fm1, 'beta', density = FALSE)  
# plot(W24_fm1, 'alpha', density = FALSE) 
# plot(W24_fm1, 'sigma.sq.p', density = TRUE)  # Random Effect
# W24_fm1$rhat                            
# dev.off()                             
# 
# # Fit 2: DOY Random Effect - Negative Binomial
# plot(W24_fm2, 'beta', density = FALSE)  
# plot(W24_fm2, 'alpha', density = FALSE) 
# plot(W24_fm2, 'sigma.sq.p', density = TRUE)
# plot(W24_fm2, 'kappa', density = TRUE)      # Overdisperison parameter
# W24_fm2$rhat                            
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# # Calculating WAIC
# W24_fm0_waic <- waicAbund(W24_fm0)
# W24_fm1_waic <- waicAbund(W24_fm1)
# W24_fm2_waic <- waicAbund(W24_fm2)
# 
# # Extract the WAIC values for each model
# W24_waic_values <- c(W24_fm0_waic["WAIC"],
#                      W24_fm1_waic["WAIC"],
#                      W24_fm2_waic["WAIC"]
# )
# 
# # Create a named vector with model names
# W24_names <- c("fm0", 
#                "fm1",
#                "fm2"
# )
# 
# # Combine model names and WAIC values into a data frame for ranking
# W24_waic_df <- data.frame(Model = W24_names, 
#                           WAIC = W24_waic_values)
# 
# # Rank models based on WAIC (lower WAIC is better)
# W24_waic_df <- W24_waic_df[order(W24_waic_df$WAIC), ]
# 
# # Print the ranked models
# print(W24_waic_df)
# 
# # Best model is ...

# Check fit
W24_bm_ppc <- ppcAbund(W24_fm2, fit.stat = "chi-squared", group = 1)
summary(W24_bm_ppc)

# Fit is not the best

# Export best model
saveRDS(W24_fm2, "./Model_Objects/W24_Cam_Nmix_BestModel.rds")



# -------------------------------------------------------
#                     Fall 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F24_inits <- list(alpha = 0.1,               
                  beta = 0.1,                
                  sigma.sq.p = 0.1,
                  N = apply(F24_spA_dat$y, 1, sum)
) 

# ----------------------
# Detection Covariates
# ----------------------

# # Fit 0: Null model 
# F24_fm0 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ 1, 
#                 data = F24_spA_dat,
#                 family = 'Poisson',
#                 inits = F24_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)
# 
# # Fit 1: DOY Random Effect 
# F24_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ (1|DOY), 
#                 data = F24_spA_dat,
#                 family = 'Poisson',
#                 inits = F24_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)

# Fit 2: DOY Random Effect - Negative Binomial 
F24_fm2 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
                det.formula = ~ (1|DOY), 
                data = F24_spA_dat,
                family = 'NB',
                inits = F24_inits, 
                priors = priors,
                tuning = tuning,
                accept.rate = 0.43,
                n.batch = n.batch,
                batch.length = batch.length,
                n.burn = n.burn,
                n.thin = n.thin, 
                n.chains = n.chains,
                verbose = FALSE)

# ----------------------
# Checking Convergence
# ----------------------


# # Fit 0: Null Model
# plot(F24_fm0, 'beta', density = FALSE)  # Abundance parameters
# plot(F24_fm0, 'alpha', density = FALSE) # Detection parameters
# F24_fm0$rhat                            # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off()                               # Clear plots
# 
# # Fit 1: DOY Random Effect
# plot(F24_fm1, 'beta', density = FALSE)  
# plot(F24_fm1, 'alpha', density = FALSE) 
# plot(F24_fm1, 'sigma.sq.p', density = TRUE)  # Random Effect
# F24_fm1$rhat                            
# dev.off()                             
# 
# # Fit 2: DOY Random Effect - Negative Binomial
# plot(F24_fm2, 'beta', density = FALSE)  
# plot(F24_fm2, 'alpha', density = FALSE) 
# plot(F24_fm2, 'sigma.sq.p', density = TRUE)
# plot(F24_fm2, 'kappa', density = TRUE)      # Overdisperison parameter
# F24_fm2$rhat                            
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# # Calculating WAIC
# F24_fm0_waic <- waicAbund(F24_fm0)
# F24_fm1_waic <- waicAbund(F24_fm1)
# F24_fm2_waic <- waicAbund(F24_fm2)
# 
# # Extract the WAIC values for each model
# F24_waic_values <- c(F24_fm0_waic["WAIC"],
#                      F24_fm1_waic["WAIC"],
#                      F24_fm2_waic["WAIC"]
# )
# 
# # Create a named vector with model names
# F24_names <- c("fm0", 
#                "fm1",
#                "fm2"
# )
# 
# # Combine model names and WAIC values into a data frame for ranking
# F24_waic_df <- data.frame(Model = F24_names, 
#                           WAIC = F24_waic_values)
# 
# # Rank models based on WAIC (lower WAIC is better)
# F24_waic_df <- F24_waic_df[order(F24_waic_df$WAIC), ]
# 
# # Print the ranked models
# print(F24_waic_df)
# 
# # Best model is ...

# Check fit
F24_bm_ppc <- ppcAbund(F24_fm2, fit.stat = "chi-squared", group = 1)
summary(F24_bm_ppc)

# Fit is not the best

# Export best model
saveRDS(F24_fm2, "./Model_Objects/F24_Cam_Nmix_BestModel.rds")


# -------------------------------------------------------
#                     Winter 2025
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W25_inits <- list(alpha = 0.1,               
                  beta = 0.1,                
                  sigma.sq.p = 0.1,
                  N = apply(W25_spA_dat$y, 1, sum)
) 

# ----------------------
# Detection Covariates
# ----------------------

# # Fit 0: Null model 
# W25_fm0 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ 1, 
#                 data = W25_spA_dat,
#                 family = 'Poisson',
#                 inits = W25_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)
# 
# # Fit 1: DOY Random Effect 
# W25_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx), 
#                 det.formula = ~ (1|DOY), 
#                 data = W25_spA_dat,
#                 family = 'Poisson',
#                 inits = W25_inits, 
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin, 
#                 n.chains = n.chains,
#                 verbose = FALSE)
# 
# Fit 2: DOY Random Effect - Negative Binomial
W25_fm2 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx),
                det.formula = ~ (1|DOY),
                data = W25_spA_dat,
                family = 'NB',
                inits = W25_inits,
                priors = priors,
                tuning = tuning,
                accept.rate = 0.43,
                n.batch = n.batch,
                batch.length = batch.length,
                n.burn = n.burn,
                n.thin = n.thin,
                n.chains = n.chains,
                verbose = FALSE)

# ----------------------
# Checking Convergence
# ----------------------


# # Fit 0: Null Model
# plot(W25_fm0, 'beta', density = FALSE)  # Abundance parameters
# plot(W25_fm0, 'alpha', density = FALSE) # Detection parameters
# W25_fm0$rhat                            # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off()                               # Clear plots
# 
# # Fit 1: DOY Random Effect
# plot(W25_fm1, 'beta', density = FALSE)  
# plot(W25_fm1, 'alpha', density = FALSE) 
# plot(W25_fm1, 'sigma.sq.p', density = TRUE)  # Random Effect
# W25_fm1$rhat                            
# dev.off()                             
# 
# # Fit 2: DOY Random Effect - Negative Binomial
# plot(W25_fm2, 'beta', density = FALSE)  
# plot(W25_fm2, 'alpha', density = FALSE) 
# plot(W25_fm2, 'sigma.sq.p', density = TRUE)
# plot(W25_fm2, 'kappa', density = TRUE)      # Overdisperison parameter
# W25_fm2$rhat                            
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# # Calculating WAIC
# W25_fm0_waic <- waicAbund(W25_fm0)
# W25_fm1_waic <- waicAbund(W25_fm1)
# W25_fm2_waic <- waicAbund(W25_fm2)
# 
# # Extract the WAIC values for each model
# W25_waic_values <- c(W25_fm0_waic["WAIC"],
#                      W25_fm1_waic["WAIC"],
#                      W25_fm2_waic["WAIC"]
# )
# 
# # Create a named vector with model names
# W25_names <- c("fm0", 
#                "fm1",
#                "fm2"
# )
# 
# # Combine model names and WAIC values into a data frame for ranking
# W25_waic_df <- data.frame(Model = W25_names, 
#                           WAIC = W25_waic_values)
# 
# # Rank models based on WAIC (lower WAIC is better)
# W25_waic_df <- W25_waic_df[order(W25_waic_df$WAIC), ]
# 
# # Print the ranked models
# print(W25_waic_df)
# 
# # Best model is ...

# # Check fit
# W25_bm_ppc <- ppcAbund(W25_fm2, fit.stat = "chi-squared", group = 1)
# summary(W25_bm_ppc)
# 
# # Fit is not the best
# 
# # Export best model
# saveRDS(W25_fm2, "./Model_Objects/W25_Cam_Nmix_BestModel.rds")


# ----------------------------- End of Script -----------------------------