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
# install.packages("sf")

# Load library
library(tidyverse)
library(spAbundance)
library(sf)

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
# Subsetting Data to Peak Activity periods
# ------------------------------------------------------- 

# Subsetting times from 5-9am and 5-9pm
F23_wtd_cams <- F23_wtd_cams %>%
  filter((time >= "05:00:00" & time <= "09:00:00" ) | (time >= "17:00:00 "& time <= "21:00:00")
)

W24_wtd_cams <- W24_wtd_cams %>%
  filter((time >= "05:00:00" & time <= "09:00:00" ) | (time >= "17:00:00 "& time <= "21:00:00")
)

F24_wtd_cams <- F24_wtd_cams %>%
  filter((time >= "05:00:00" & time <= "09:00:00" ) | (time >= "17:00:00 "& time <= "21:00:00")
)

# -------------------------------------------------------
# Subsetting Data to Two Weeks
# ------------------------------------------------------- 

# Fall 2023
# min(F23_wtd_cams$day_of_year)
# min(F23_wtd_cams$day_of_year) + 4

F23_wtd_cams <- F23_wtd_cams %>%
  filter(day_of_year >= 251 & day_of_year <= 255)
 
# Winter 2024
# min(W24_wtd_cams$day_of_year)
# min(W24_wtd_cams$day_of_year) + 4

W24_wtd_cams <- W24_wtd_cams %>%
  filter(day_of_year >= 41 & day_of_year <= 45)

# Fall 2024
# min(F24_wtd_cams$day_of_year)
# min(F24_wtd_cams$day_of_year) + 4

F24_wtd_cams <- F24_wtd_cams %>%
  filter(day_of_year >= 251 & day_of_year <= 255)


 

# -------------------------------------------------------
# Creating Detection Matrices
# ------------------------------------------------------- 

# Initialize a site x survey matrix for each season
F23_grid <- expand.grid(site_number = 1:27,
                           day_of_year = min(F23_wtd_cams$day_of_year):max(F23_wtd_cams$day_of_year)
        
)

W24_grid <- expand.grid(site_number = 1:27,
                            day_of_year = min(W24_wtd_cams$day_of_year):max(W24_wtd_cams$day_of_year)
)

F24_grid <- expand.grid(site_number = 1:27,
                            day_of_year = min(F24_wtd_cams$day_of_year):max(F24_wtd_cams$day_of_year)
)

# Summarize counts by site and survey day
F23_det_mat <- F23_wtd_cams %>%
  group_by(site_number, day_of_year) %>%
  summarise(Detections = sum(group_size, na.rm = TRUE), .groups = 'drop') %>%
  full_join(F23_grid, by = c("site_number", "day_of_year")) %>%  # Ensure all sites are included
  mutate(Detections = replace_na(Detections, 0)) %>%  # Fill missing detections with 0
  pivot_wider(names_from = day_of_year, values_from = Detections, values_fill = 0) %>%
  arrange(as.numeric(site_number)) %>%  # Ensure sites are in ascending order
  column_to_rownames(var = "site_number")  # Set row names as site_number

W24_det_mat <- W24_wtd_cams %>%
  group_by(site_number, day_of_year) %>%
  summarise(Detections = sum(group_size, na.rm = TRUE), .groups = 'drop') %>%
  full_join(W24_grid, by = c("site_number", "day_of_year")) %>%  # Ensure all sites are included
  mutate(Detections = replace_na(Detections, 0)) %>%  # Fill missing detections with 0
  pivot_wider(names_from = day_of_year, values_from = Detections, values_fill = 0) %>%
  arrange(as.numeric(site_number)) %>%  # Ensure sites are in ascending order
  column_to_rownames(var = "site_number")  # Set row names as site_number

F24_det_mat <- F24_wtd_cams %>%
  group_by(site_number, day_of_year) %>%
  summarise(Detections = sum(group_size, na.rm = TRUE), .groups = 'drop') %>%
  full_join(F24_grid, by = c("site_number", "day_of_year")) %>%  # Ensure all sites are included
  mutate(Detections = replace_na(Detections, 0)) %>%  # Fill missing detections with 0
  pivot_wider(names_from = day_of_year, values_from = Detections, values_fill = 0) %>%
  arrange(as.numeric(site_number)) %>%  # Ensure sites are in ascending order
  column_to_rownames(var = "site_number")  # Set row names as site_number

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
# Getting Camera Coordinates
# ------------------------------------------------------- 

coords <- site_dat[,c("Lat", "Long")]

# Create an sf object with Lat/Long (WGS84)
coords <- st_as_sf(site_dat, coords = c("Long", "Lat"), crs = 4326)

# Transform to UTM Zone 14N (EPSG:32614)
coords <- st_transform(coords, crs = 32614)

# Extract UTM coordinates from geometry
coords <- coords %>%
  mutate(UTM_East = st_coordinates(.)[,1], 
         UTM_North = st_coordinates(.)[,2]) %>%
  select(UTM_East, UTM_North) 

# Convert to a dataframe
coords <- as.data.frame(coords)

# Remove geometry
coords <- coords[,-3]

# Print coordinates
print(coords)

# ----------------------
# Offset
# ----------------------

# Viewshed angle
theta = 38 # degrees, field of view (FOV)
r = 10     # max view distance 

# Offset in meters squared
offset_m2 = ((theta/360) * (pi*r^2)) 

# Offset in acres
# 1 square meter = 0.000247105 acres
offset_acre = offset_m2 * 0.000247105

# Offset in hectarews
# 1 square meter = 0.0001 hectares
offset_ha = offset_m2 * 0.0001

# -------------------------------------------------------
# Format for spAbundance N-mixture models
# ------------------------------------------------------- 

# spAbundance uses long format
F23_spA_dat <- list(y = F23_det_mat,
                    abund.covs = site_cov_mat,
                    det.covs = list(DOY = F23_doy_mat),
                    coords = coords,
                    offset = offset_ha
)

W24_spA_dat <- list(y = W24_det_mat,
                   abund.covs = site_cov_mat,
                   det.covs = list(DOY = W24_doy_mat),
                   coords = coords,
                   offset = offset_ha
)

F24_spA_dat <- list(y = F24_det_mat,
                    abund.covs = site_cov_mat,
                    det.covs = list(DOY = F24_doy_mat),
                    coords = coords,
                    offset = offset_ha
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
n.batch <- 40000
batch.length * n.batch # Total number of MCMC samples per chain
n.burn <- 100000
n.thin <- 10
n.chains <- 3
n.omp.threads <- 9

# ----------------------
# Site Distance
# ----------------------

# Pair-wise distances between all sites
dist_mat <- dist(coords)

# ----------------------
# Set Priors
# ----------------------
priors <- list(alpha.normal = list(mean = 0, var = 100),
               beta.normal = list(mean = 0, var = 100), 
               kappa.unif = c(0, 100), 
               sigma.sq.mu.ig = list(0.1, 0.1),
               sigma.sq.p.ig = list(0.1, 0.1),
               sigma.sq.ig = c(2, 1),
               phi.unif = c(3 / max(dist_mat), 3 / min(dist_mat)))

# ----------------------
# Tuning
# ----------------------
tuning <- list(beta = 0.5,
               alpha = 0.5,
               kappa = 0.5,
               beta.star = 0.5,
               alpha.star = 0.5,
               w = 0.5,
               phi = 0.5)

# -------------------------------------------------------
#                     Fall 2023
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F23_inits <- list(alpha = 5,
                  beta = 5,
                  kappa = 0.5,
                  sigma.sq.p = 0.5,
                  sigma.sq.mu = 0.5,
                  N = apply(F23_spA_dat$y, 1, max, na.rm = TRUE), 
                  sigma.sq = 1, 
                  phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(F23_spA_dat$y)))


# ----------------------
# Fit Model
# ----------------------




F23_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx),
                  det.formula = ~ (1|DOY),
                  data = F23_spA_dat,
                  family = 'NB',
                  inits = F23_inits,
                  priors = priors,
                  tuning = tuning,
                  accept.rate = 0.43,
                  n.batch = n.batch,
                  batch.length = batch.length,
                  n.burn = n.burn,
                  n.thin = n.thin,
                  n.chains = n.chains,
                  verbose = TRUE,
                  n.omp.threads = n.omp.threads,
                  n.report = 5000
)


# F23_fm1 <- spNMix(abund.formula = ~ scale(woody_lrgPInx),
#                 det.formula = ~ (1|DOY),
#                 data = F23_spA_dat,
#                 family = 'Poisson',
#                 cov.model = 'exponential',
#                 NNGP = TRUE,
#                 n.neighbors = 15,
#                 search.type = 'cb',
#                 inits = F23_inits,
#                 priors = priors,
#                 tuning = tuning,
#                 accept.rate = 0.43,
#                 n.batch = n.batch,
#                 batch.length = batch.length,
#                 n.burn = n.burn,
#                 n.thin = n.thin,
#                 n.chains = n.chains,
#                 verbose = TRUE,
#                 n.omp.threads = n.omp.threads,
#                 n.report = 5000
# )
# 


# ----------------------
# Checking Convergence
# ----------------------

# # Rhat values of 1.0 to 1.1 indicate good mixing
# F23_fm1$rhat
# F23_fm1sp$rhat
# 
# # Trace Plots
# plot(F23_fm1, 'beta', density = FALSE)       # Abundance parameters
# plot(F23_fm1, 'alpha', density = FALSE)      # Detection parameters
# plot(F23_fm1, 'sigma.sq.p', density = FALSE) # Random effect
# 
# # Check fit
# F23_ppc1 <- ppcAbund(F23_fm1, fit.stat = "chi-squared", group = 1)
# summary(F23_ppc1)

# ----------------------
# Abundance Estimates 
# ----------------------

# w.means <- apply(F23_fm1sp$w.samples, 2, mean)
# hist(w.means)

# F23_fm1$N.samples is abundance estimate per site
print(F23_fm1$N.samples)


# Summarizing estimates by site
F23_all_site_ests <- rowSums(F23_fm1$N.samples)

# Total number of survey days
survey_days <-  max(F23_wtd_cams$day_of_year) - min(F23_wtd_cams$day_of_year)
tot_survey_days <- survey_days * 27

# Area surveyed = area surveyed by camera * number of cameras
area_surveyed = offset_acre * 27

# Total surveyed area = total area by camera * total days surveyed * total hours surveyed
tot_surveyed_area = area_surveyed * tot_survey_days * 10

# mean(F23_fm1$N.samples) / area_surveyed * 2710

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
F23_dens_vec <- F23_all_site_ests / tot_surveyed_area

# Correcting desity estimates to total abundance in the area
F23_abund_vec <- F23_dens_vec * 2710

# Compute summary statistics
F23_abund_summary <- data.frame(Model = "Cam Nmix",
                                Season = "Fall 2023",
                                Data = "Camera",
                                Season_Model = "F23 Cam Nmix",
                                N = mean(F23_abund_vec, na.rm = TRUE),
                                LCI = as.numeric(quantile(F23_abund_vec, probs = 0.025, na.rm = TRUE)),
                                UCI = as.numeric(quantile(F23_abund_vec, probs = 0.975, na.rm = TRUE))
)

# Print Abundance Summary
print(F23_abund_summary)

# Export abundance estimates
saveRDS(F23_abund_summary, "./Model_Objects/F23_Cam_Nmix_AbundEst.rds")

# -------------------------------------------------------
#                     Winter 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W24_inits <- list(alpha = 5,
                  beta = 5,
                  kappa = 0.5,
                  sigma.sq.p = 0.5,
                  sigma.sq.mu = 0.5,
                  N = apply(W24_spA_dat$y, 1, max, na.rm = TRUE), 
                  sigma.sq = 1, 
                  phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(W24_spA_dat$y)))


# ----------------------
# Fit Model
# ----------------------

W24_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx),
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
                verbose = TRUE,
                n.omp.threads = n.omp.threads,
                n.report = 5000
)

# 
# W24_fm1 <- spNMix(abund.formula = ~ scale(woody_lrgPInx),
#                   det.formula = ~ (1|DOY),
#                   data = W24_spA_dat,
#                   family = 'Poisson',
#                   cov.model = 'exponential',
#                   NNGP = TRUE,
#                   n.neighbors = 15,
#                   search.type = 'cb',
#                   inits = W24_inits,
#                   priors = priors,
#                   tuning = tuning,
#                   accept.rate = 0.43,
#                   n.batch = n.batch,
#                   batch.length = batch.length,
#                   n.burn = n.burn,
#                   n.thin = n.thin,
#                   n.chains = n.chains,
#                   verbose = TRUE,
#                   n.omp.threads = n.omp.threads,
#                   n.report = 5000
# )


# ----------------------
# Checking Convergence
# ----------------------

# # Rhat values of 1.0 to 1.1 indicate good mixing
# W24_fm1$rhat 
# 
# # Trace Plots
# plot(W24_fm1, 'beta', density = FALSE)       # Abundance parameters
# plot(W24_fm1, 'alpha', density = FALSE)      # Detection parameters
# plot(W24_fm1, 'sigma.sq.p', density = FALSE) # Random effect
# 
# # Check fit
# W24_ppc1 <- ppcAbund(W24_fm1, fit.stat = "chi-squared", group = 1)
# summary(W24_ppc1)

# ----------------------
# Abundance Estimates 
# ----------------------

# w.means <- apply(W24_fm1sp$w.samples, 2, mean)
# hist(w.means)

# W24_fm1$N.samples is abundance estimate per site
print(W24_fm1$N.samples)


# Summarizing estimates by site
W24_all_site_ests <- rowSums(W24_fm1$N.samples)

# Total number of survey days
survey_days <-  max(W24_wtd_cams$day_of_year) - min(W24_wtd_cams$day_of_year)
tot_survey_days <- survey_days * 27

# Area surveyed = area surveyed by camera * number of cameras
area_surveyed = offset_acre * 27

# Total surveyed area = total area by camera * total days surveyed * total hours surveyed
tot_surveyed_area = area_surveyed * tot_survey_days * 10

# mean(W24_fm1$N.samples) / area_surveyed * 2710

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
W24_dens_vec <- W24_all_site_ests / tot_surveyed_area

# Correcting desity estimates to total abundance in the area
W24_abund_vec <- W24_dens_vec * 2710

# Compute summary statistics
W24_abund_summary <- data.frame(Model = "Cam Nmix",
                                Season = "Winter 2024",
                                Data = "Camera",
                                Season_Model = "W24 Cam Nmix",
                                N = mean(W24_abund_vec, na.rm = TRUE),
                                LCI = as.numeric(quantile(W24_abund_vec, probs = 0.025, na.rm = TRUE)),
                                UCI = as.numeric(quantile(W24_abund_vec, probs = 0.975, na.rm = TRUE))
)

# Print Abundance Summary
print(W24_abund_summary)

# Export abundance estimates
saveRDS(W24_abund_summary, "./Model_Objects/W24_Cam_Nmix_AbundEst.rds")

# -------------------------------------------------------
#                     Fall 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F24_inits <- list(alpha = 5,
                  beta = 5,
                  kappa = 0.5,
                  sigma.sq.p = 0.5,
                  sigma.sq.mu = 0.5,
                  N = apply(F24_spA_dat$y, 1, max, na.rm = TRUE), 
                  sigma.sq = 1, 
                  phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(F24_spA_dat$y)))


# ----------------------
# Fit Model
# ----------------------

F24_fm1 <- NMix(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx),
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
                verbose = TRUE,
                n.omp.threads = n.omp.threads,
                n.report = 5000
)

# F24_fm1 <- spNMix(abund.formula = ~ scale(woody_lrgPInx),
#                   det.formula = ~ (1|DOY),
#                   data = F24_spA_dat,
#                   family = 'Poisson',
#                   cov.model = 'exponential',
#                   NNGP = TRUE,
#                   n.neighbors = 15,
#                   search.type = 'cb',
#                   inits = F24_inits,
#                   priors = priors,
#                   tuning = tuning,
#                   accept.rate = 0.43,
#                   n.batch = n.batch,
#                   batch.length = batch.length,
#                   n.burn = n.burn,
#                   n.thin = n.thin,
#                   n.chains = n.chains,
#                   verbose = TRUE,
#                   n.omp.threads = n.omp.threads,
#                   n.report = 5000
# )
# 


# ----------------------
# Checking Convergence
# ----------------------

# # Rhat values of 1.0 to 1.1 indicate good mixing
# F24_fm1$rhat 
# 
# # Trace Plots
# plot(F24_fm1, 'beta', density = FALSE)       # Abundance parameters
# plot(F24_fm1, 'alpha', density = FALSE)      # Detection parameters
# plot(F24_fm1, 'sigma.sq.p', density = FALSE) # Random effect
# 
# # Check fit
# F24_ppc1 <- ppcAbund(F24_fm1, fit.stat = "chi-squared", group = 1)
# summary(F24_ppc1)

# ----------------------
# Abundance Estimates 
# ----------------------

# w.means <- apply(F24_fm1sp$w.samples, 2, mean)
# hist(w.means)

# F24_fm1$N.samples is abundance estimate per site
print(F24_fm1$N.samples)


# Summarizing estimates by site
F24_all_site_ests <- rowSums(F24_fm1$N.samples)

# Total number of survey days
survey_days <-  max(F24_wtd_cams$day_of_year) - min(F24_wtd_cams$day_of_year)
tot_survey_days <- survey_days * 27

# Area surveyed = area surveyed by camera * number of cameras
area_surveyed = offset_acre * 27

# Total surveyed area = total area by camera * total days surveyed * total hours surveyed
tot_surveyed_area = area_surveyed * tot_survey_days * 10

# mean(F24_fm1$N.samples) / area_surveyed * 2710

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
F24_dens_vec <- F24_all_site_ests / tot_surveyed_area

# Correcting desity estimates to total abundance in the area
F24_abund_vec <- F24_dens_vec * 2710

# Compute summary statistics
F24_abund_summary <- data.frame(Model = "Cam Nmix",
                                Season = "Fall 2024",
                                Data = "Camera",
                                Season_Model = "F24 Cam Nmix",
                                N = mean(F24_abund_vec, na.rm = TRUE),
                                LCI = as.numeric(quantile(F24_abund_vec, probs = 0.025, na.rm = TRUE)),
                                UCI = as.numeric(quantile(F24_abund_vec, probs = 0.975, na.rm = TRUE))
)

# Print Abundance Summary
print(F24_abund_summary)

# Export abundance estimates
saveRDS(F24_abund_summary, "./Model_Objects/F24_Cam_Nmix_AbundEst.rds")