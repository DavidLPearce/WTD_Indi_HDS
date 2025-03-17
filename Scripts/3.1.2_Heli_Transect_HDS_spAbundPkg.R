# Author: David L. Pearce
# Description:
#             This script extends the script and vignette written by Jeff Doser
#             and Marc Kéry for hierarcical distance sampling.  Which can be 
#             found here: 
#             https://doserlab.com/files/spabundance-web/articles/distancesampling
#             This script analyses 4 helicopter surveys with two replicates per
#             survey to estimate white-tailed deer abundance.             

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
# install.packages("sp")

# Load library
library(tidyverse)
library(spAbundance)
library(sf)
library(sp)

# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Load survey data
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Formatted_Heli_Transect_Data.csv", row.names = 1)

# Take a look at the data
head(heli_dat, 5)

# Load site covariates data
site_covs <- read.csv("./Data/Survey_Data/Helicopter_Data/Heli_Transect_siteCovs.csv", row.names = 1)

# Take a look at the data
head(site_covs, 5)

# Read in transects
transects <- st_read("./Data/Spatial_Data/Helicopter_Transects/Helicopter_Transects.shp")
# Take a look at the data
head(transects, 5)


# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# ----------------------
# Transect Effort
# ----------------------

# Data frame for transect ID, length and the viewshed
transect_effort <- data.frame(Transect_ID = 1:12,
                              Transect_Length_km = c(2.94, 4.99, 4.99, 4.99, 4.68, 4.74,
                                                     4.79, 4.84, 1.25, 1.25, 1.25, 1.25),
                              ViewShed_m = 100 
  
)

# Convert Transect Length (km to meters) and calculate total area covered
transect_effort <- transect_effort %>%
  mutate(
    Transect_Length_m = Transect_Length_km * 1000,  # Convert km to meters
    Area_m2 = Transect_Length_m * (2 * ViewShed_m),  # Compute area in m²
    Area_ha = Area_m2 / 10000,  # Convert to hectares
    Area_ac = Area_m2 / 4046.86  # Convert to acres
  )

# Print result
print(transect_effort)

# ----------------------
# Adding a Survey Code
# ----------------------

# Convert Survey_Time to 1 = evening, and 2 = morning
heli_dat$Survey_Code <- ifelse(heli_dat$Survey_Time == "Evening", 1, 
                               ifelse(heli_dat$Survey_Time == "Morning", 2, NA))

# ----------------------
# Subsetting by survey
# ----------------------

# Subsetting by survey season
fall23_dat <- heli_dat[which(heli_dat$Date == "9/8/2023" | heli_dat$Date == "9/9/2023"),]
win24_dat <- heli_dat[which(heli_dat$Date ==  "2/9/2024" | heli_dat$Date == "2/10/2024"),]
fall24_dat <- heli_dat[which(heli_dat$Date ==  "9/6/2024" | heli_dat$Date == "9/7/2024"),]
win25_dat <- heli_dat[which(heli_dat$Date ==  "2/7/2025" | heli_dat$Date == "2/8/2025"),]

# Adding a new Transect name that is Transect_Survey for stacking data
# spAbundance doesn't have a replicate function so data needs to be stacked and site ID
# will be used to account for pseudoreplication
# Create Transect_Survey column by pasting Transect_ID and Survey_Code
fall23_dat$Transect_Survey <- paste(fall23_dat$Transect_ID, fall23_dat$Survey_Code, sep = "_")
win24_dat$Transect_Survey <- paste(win24_dat$Transect_ID, win24_dat$Survey_Code, sep = "_")
fall24_dat$Transect_Survey <- paste(fall24_dat$Transect_ID, fall24_dat$Survey_Code, sep = "_")
win25_dat$Transect_Survey <- paste(win25_dat$Transect_ID, win25_dat$Survey_Code, sep = "_")

# ----------------------
# Frequency Plots 
# ----------------------

# # Looking at the frequency of detections in relation to distance
# hist(heli_dat$Perpendicular_Distance,  xlab="Distance (m)", ylab="Frequency", main = "") 
# 
# # Frequency of detections by survey
# hist(fall23_dat$Perpendicular_Distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '23") 
# hist(win24_dat$Perpendicular_Distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '24") 
# hist(fall24_dat$Perpendicular_Distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '24") 
# hist(win25_dat$Perpendicular_Distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '25") 


# Going to use the same distance bins as DS sampling 
# which were Bins of 0-20, 20-40, 40-60, 60-100 


# ----------------------
# Formatting Survey Data
# ----------------------

# spAbundance requires data to be formatted in site by distance bin

# Fall 2023
fall23_dat$Distance_Bins <- cut(fall23_dat$Perpendicular_Distance,
                                breaks = c(0, 20, 40, 60, 100), 
                                labels = c("bin1", "bin2", "bin3", "bin4"),
                                include.lowest = TRUE)

# Winter 2024
win24_dat$Distance_Bins <- cut(win24_dat$Perpendicular_Distance,
                                breaks = c(0, 20, 40, 60, 100), 
                                labels = c("bin1", "bin2", "bin3", "bin4"),
                                include.lowest = TRUE)

# Fall 2024
fall24_dat$Distance_Bins <- cut(fall24_dat$Perpendicular_Distance,
                                breaks = c(0, 20, 40, 60, 100), 
                                labels = c("bin1", "bin2", "bin3", "bin4"),
                                include.lowest = TRUE)

# Winter 2025
win25_dat$Distance_Bins <- cut(win25_dat$Perpendicular_Distance,
                                breaks = c(0, 20, 40, 60, 100), 
                                labels = c("bin1", "bin2", "bin3", "bin4"),
                                include.lowest = TRUE)

# Setting bins as characters for looping
fall23_dat$Distance_Bins <- as.character(fall23_dat$Distance_Bins)
win24_dat$Distance_Bins <- as.character(win24_dat$Distance_Bins)
fall24_dat$Distance_Bins <- as.character(fall24_dat$Distance_Bins)
win25_dat$Distance_Bins <- as.character(win25_dat$Distance_Bins)

# Initializing a matrix for 12 transects x 2 surveys and 4 distance bins for each survey
fall23_mat <- matrix(0, nrow = 24, ncol = 4)
win24_mat <- matrix(0, nrow = 24, ncol = 4)
fall24_mat <- matrix(0, nrow = 24, ncol = 4)
win25_mat <- matrix(0, nrow = 24, ncol = 4)

# Define Transect IDs (1 to 12) and Survey Codes (1 for Evening, 2 for Morning)
transect_ids <- 1:12   # Each transect appears twice
survey_codes <- rep(1:2, times = 12)  # Alternates between 1 (Evening) and 2 (Morning)

# Create the new row names
T_S_rownames <- c(paste(transect_ids, 1, sep = "_"),  # First add all _1
                   paste(transect_ids, 2, sep = "_"))  # Then add all _2

# Name rows in matrix
rownames(fall23_mat) <- T_S_rownames
rownames(win24_mat) <- T_S_rownames
rownames(fall24_mat) <- T_S_rownames
rownames(win25_mat) <- T_S_rownames

# Name column in matrix
colnames(fall23_mat) <- c("bin1", "bin2", "bin3", "bin4")
colnames(win24_mat) <- c("bin1", "bin2", "bin3", "bin4")
colnames(fall24_mat) <- c("bin1", "bin2", "bin3", "bin4")
colnames(win25_mat) <- c("bin1", "bin2", "bin3", "bin4")

##  Fill in the matrix with group sizes

# Fall 23
for (row in 1:nrow(fall23_dat)) { 
  
  # Subsetting row data
  transect_dat <- fall23_dat[row,]
  
  # Subsetting bin data
  bin_dat <- transect_dat[,"Distance_Bins"]
  
  # Subsetting Transect_Survey data
  TS_dat <- transect_dat[,"Transect_Survey"]
  
  # Subsetting Group Size data
  gs_dat <- transect_dat[,"Group_size"]

  # Ensure the bin exists in column names (avoids errors)
  if (bin_dat %in% colnames(fall23_mat)) {
    # Update the matrix: add Group Size to the respective cell
    fall23_mat[TS_dat, bin_dat] <- fall23_mat[TS_dat, bin_dat] + gs_dat
  } # end if
} # end loop

# Winter 24
for (row in 1:nrow(win24_dat)) { 
  
  # Subsetting row data
  transect_dat <- win24_dat[row,]
  
  # Subsetting bin data
  bin_dat <- transect_dat[,"Distance_Bins"]
  
  # Subsetting Transect_Survey data
  TS_dat <- transect_dat[,"Transect_Survey"]
  
  # Subsetting Group Size data
  gs_dat <- transect_dat[,"Group_size"]
  
  # Ensure the bin exists in column names (avoids errors)
  if (bin_dat %in% colnames(win24_mat)) {
    # Update the matrix: add Group Size to the respective cell
    win24_mat[TS_dat, bin_dat] <- win24_mat[TS_dat, bin_dat] + gs_dat
  } # end if
} # end loop

# Fall 24
for (row in 1:nrow(fall24_dat)) { 
  
  # Subsetting row data
  transect_dat <- fall24_dat[row,]
  
  # Subsetting bin data
  bin_dat <- transect_dat[,"Distance_Bins"]
  
  # Subsetting Transect_Survey data
  TS_dat <- transect_dat[,"Transect_Survey"]
  
  # Subsetting Group Size data
  gs_dat <- transect_dat[,"Group_size"]
  
  # Ensure the bin exists in column names (avoids errors)
  if (bin_dat %in% colnames(fall24_mat)) {
    # Update the matrix: add Group Size to the respective cell
    fall24_mat[TS_dat, bin_dat] <- fall24_mat[TS_dat, bin_dat] + gs_dat
  } # end if
} # end loop


# Winter 25
for (row in 1:nrow(win25_dat)) { 
  
  # Subsetting row data
  transect_dat <- win25_dat[row,]
  
  # Subsetting bin data
  bin_dat <- transect_dat[,"Distance_Bins"]
  
  # Subsetting Transect_Survey data
  TS_dat <- transect_dat[,"Transect_Survey"]
  
  # Subsetting Group Size data
  gs_dat <- transect_dat[,"Group_size"]
  
  # Ensure the bin exists in column names (avoids errors)
  if (bin_dat %in% colnames(win25_mat)) {
    # Update the matrix: add Group Size to the respective cell
    win25_mat[TS_dat, bin_dat] <- win25_mat[TS_dat, bin_dat] + gs_dat
  } # end if
} # end loop


# Take a look
print(fall23_mat)
print(win24_mat)
print(fall24_mat)
print(win25_mat)

# Detections by survey
sum(fall23_mat)
sum(win24_mat)
sum(fall24_mat)
sum(win25_mat)

# ----------------------
# Formatting Site Covs
# ----------------------

# Since spAbundance is indexed by site, creating an object for matching
# To format covariates
all_transects <- expand.grid(Transect = 1:12, Survey = 1:2) %>%
                      mutate(Transect_Survey = paste(Transect, Survey, sep = "_")) %>%
                      select(Transect_Survey)

# Create a new dataframe with SiteID extracted from Transect_Survey
SiteID_df <- all_transects %>%
                         mutate(SiteID = as.numeric(sub("_.*", "", Transect_Survey))) 

# Making a categorical covariate of survey time 1 = evening, 2 = morning
SiteID_df[1:12, "SurveyTime"] <- 1
SiteID_df[12:24, "SurveyTime"] <- 2

# Adding in site_covs to spA_site_covs
spA_site_covs <- SiteID_df %>%
                   left_join(site_covs, by = c("SiteID" = "ID"))


# Since spAbundance is indexed by site, Group size needs to be a summary by site
 

# Fall 23
# Calculate mean group size per Transect_Survey
F23_mnGS <- fall23_dat %>%
  group_by(Transect_Survey) %>%
  summarise(mnGS = mean(Group_size, na.rm = TRUE), .groups = "drop")  

# Merge with all possible Transect_Survey
F23_mnGS_full <- all_transects %>%
  left_join(F23_mnGS, by = "Transect_Survey") %>%
  mutate(mnGS = ifelse(is.na(mnGS), 0, mnGS),  
         mnGS = round(mnGS, 2),  
         Transect = as.numeric(sub("_.*", "", Transect_Survey)), 
         Survey = as.numeric(sub(".*_", "", Transect_Survey))) %>%  
  arrange(Survey, Transect) %>%   
  select(Transect_Survey, mnGS)  

# Print to check
print(F23_mnGS_full)

# Winter 24
W24_mnGS <- win24_dat %>%
  group_by(Transect_Survey) %>%
  summarise(mnGS = mean(Group_size, na.rm = TRUE), .groups = "drop")  
W24_mnGS_full <- all_transects %>%
  left_join(W24_mnGS, by = "Transect_Survey") %>%
  mutate(mnGS = ifelse(is.na(mnGS), 0, mnGS),  
         mnGS = round(mnGS, 2),  
         Transect = as.numeric(sub("_.*", "", Transect_Survey)), 
         Survey = as.numeric(sub(".*_", "", Transect_Survey))) %>%  
  arrange(Survey, Transect) %>%   
  select(Transect_Survey, mnGS)  

# Print to check
print(W24_mnGS_full)

# Fall 24
F24_mnGS <- fall24_dat %>%
  group_by(Transect_Survey) %>%
  summarise(mnGS = mean(Group_size, na.rm = TRUE), .groups = "drop")  
F24_mnGS_full <- all_transects %>%
  left_join(F24_mnGS, by = "Transect_Survey") %>%
  mutate(mnGS = ifelse(is.na(mnGS), 0, mnGS),  
         mnGS = round(mnGS, 2),  
         Transect = as.numeric(sub("_.*", "", Transect_Survey)), 
         Survey = as.numeric(sub(".*_", "", Transect_Survey))) %>%  
  arrange(Survey, Transect) %>%   
  select(Transect_Survey, mnGS)  

# Print to check
print(F24_mnGS_full)

# Winter 25
W25_mnGS <- win25_dat %>%
  group_by(Transect_Survey) %>%
  summarise(mnGS = mean(Group_size, na.rm = TRUE), .groups = "drop")  
W25_mnGS_full <- all_transects %>%
  left_join(W25_mnGS, by = "Transect_Survey") %>%
  mutate(mnGS = ifelse(is.na(mnGS), 0, mnGS),  
         mnGS = round(mnGS, 2),  
         Transect = as.numeric(sub("_.*", "", Transect_Survey)), 
         Survey = as.numeric(sub(".*_", "", Transect_Survey))) %>%  
  arrange(Survey, Transect) %>%   
  select(Transect_Survey, mnGS)  

# Print to check
print(W25_mnGS_full)

# Combine each season mean group size with the rest of the covariates

# Fall 23
F23spA_site_covs <- spA_site_covs %>%
  mutate(Transect_Survey = paste(SiteID, SurveyTime, sep = "_")) %>% 
  left_join(F23_mnGS_full, by = "Transect_Survey") 

# Winter 24
W24spA_site_covs <- spA_site_covs %>%
  mutate(Transect_Survey = paste(SiteID, SurveyTime, sep = "_")) %>% 
  left_join(W24_mnGS_full, by = "Transect_Survey") 

# Fall 24
F24spA_site_covs <- spA_site_covs %>%
  mutate(Transect_Survey = paste(SiteID, SurveyTime, sep = "_")) %>% 
  left_join(F24_mnGS_full, by = "Transect_Survey") 

# Winter 25
W25spA_site_covs <- spA_site_covs %>%
  mutate(Transect_Survey = paste(SiteID, SurveyTime, sep = "_")) %>% 
  left_join(W25_mnGS_full, by = "Transect_Survey") 



# ----------------------
# Formatting Transects
# ----------------------

# Need to calculate the midpoint of the transect since package can only accept
# one set of GPS coordinates. The midpoint seems to be the most resonable.

# Calculate Midpoints
trans_midpt <- st_line_sample(transects, sample = 0.5) %>%
  st_cast("POINT")  # Convert sample to POINT geometry

# Extract X and Y coords
trans_midpt_df <- st_coordinates(trans_midpt)

# Add into transects sf
transects$MidptX <- trans_midpt_df[, 1]  # X coordinate (Easting)
transects$MidptY <- trans_midpt_df[, 2]  # Y coordinate (Northing)

# View the result
print(transects)



# spDS does not allow duplicate site coordinates
# since observation data has replicates and they are
# stacked. A set of the transects coordinates needs to 
# change slightly

# Apply shift function to each geometry in the sf object
transects_shifted <- transects %>%
  mutate(MidptX = MidptX + 0.1,  # Shift X by 1 m
         MidptY = MidptY + 0.1)  # Shift Y by 1 m

# Modify IDs for original transects
transects <- transects %>%
  mutate(ID = paste0(ID, "_1"),
         Shifted = "No")  

# Modify IDs for shifted transects
transects_shifted <- transects_shifted %>%
  mutate(ID = paste0(ID, "_2"),
         Shifted = "Yes")  

# Combine both into a single sf object
transects_combined <- bind_rows(transects, transects_shifted)

# Extracting transect coordinates
transect_coords <- transects_combined %>%
  select(MidptX, MidptY)

# Remove geometry
transect_coords <- as.data.frame(transect_coords)
transect_coords <- transect_coords[,-3]

# Take a look
print(transect_coords)

# ----------------------
# Offset
# ----------------------

# Since there are two replicates each transect needs to be represented twice
offset <- c(transect_effort[,'Area_ac'], transect_effort[,'Area_ac'])


# ----------------------
# spAbundance Format
# ----------------------

# spAbundance use long formatting
F23_spA_dat <- list(y = fall23_mat, 
                   covs = F23spA_site_covs, 
                   coords = transect_coords[,c('MidptX', 'MidptY')],
                   dist.breaks = c(0, 20, 40, 60, 100),
                   offset = offset
) 

W24_spA_dat <- list(y = win24_mat, 
                   covs = W24spA_site_covs,
                   coords = transect_coords[,c('MidptX', 'MidptY')],
                   dist.breaks = c(0, 20, 40, 60, 100),
                   offset = offset
)

F24_spA_dat <- list(y = fall24_mat, 
                   covs = F24spA_site_covs,
                   coords = transect_coords[,c('MidptX', 'MidptY')],
                   dist.breaks = c(0, 20, 40, 60, 100),
                   offset = offset
) 

W25_spA_dat <- list(y = win25_mat, 
                   covs = W25spA_site_covs, 
                   coords = transect_coords[,c('MidptX', 'MidptY')],
                   dist.breaks = c(0, 20, 40, 60, 100),
                   offset = offset
)  
 
# Take a look
str(F23_spA_dat)
str(W24_spA_dat)
str(F24_spA_dat)
str(W25_spA_dat)

 
# ------------------------------------------------------------------------------
#
#                     Hierarcical Distance Sampling Models
#
# ------------------------------------------------------------------------------

# These specifications are for all models

# ----------------------
# MCMC Specifications
# ----------------------
batch.length <- 25
n.batch <- 10000
batch.length * n.batch # Total number of MCMC samples per chain
n.burn <- 60000
n.thin <- 10
n.chains <- 3
n.report <- 5000
n.omp.threads <- 6

# ----------------------
# Site Distance
# ----------------------

# Pair-wise distances between all sites
dist_mat <- dist(transect_coords[,c('MidptX', 'MidptY')])

# ----------------------
# Set Priors
# ----------------------
priors <- list(alpha.normal = list(mean = 0, var = 10),  
               beta.normal = list(mean = 0, var = 10),
               kappa.unif = c(0, 100),
               sigma.sq.p.ig = list(mean = 0, var = 2.72),
               phi.unif = c(3 / max(dist_mat), 3 / min(dist_mat))
)

# ----------------------
# Tuning
# ----------------------
tuning <- list(beta = 0.5, 
               alpha = 0.5,
               kappa = 0.5, 
               beta.star = 0.5,
               alpha.star = 0.5,
               w = 0.5, 
               phi = 0.5
)

# ----------------------
# Initial values
# ----------------------

# Initial values are by data set


# -------------------------------------------------------
#                     Fall 2023
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F23_inits <- list(beta = 0, 
                  alpha = 0, 
                  kappa = 1,
                  sigma.sq.p = 0.1,
                  sigma.sq = 1, phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(F23_spA_dat$y)),
                  N = apply(F23_spA_dat$y, 1, sum)) 

# ----------------------
# Fit Model
# ----------------------
F23_fm1 <- spDS(abund.formula = ~ scale(herb_Pdens),
                det.formula = ~ mnGS + as.factor(SurveyTime) + scale(woody_AggInx) + (1|SiteID), 
                data = F23_spA_dat,
                family = 'Poisson',
                det.func = 'halfnormal',
                transect = 'line',
                cov.model <- 'exponential',
                NNGP <- TRUE,
                n.neighbors <- 15,
                search.type <- 'cb',
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
                n.report = n.report
)


# ----------------------
# Checking convergence 
# ----------------------

# Rhat values of 1.0 to 1.1 indicate good mixing
F23_fm1$rhat 

# # Trace Plots
# plot(F23_fm1, 'beta', density = TRUE)       # Abundance
# plot(F23_fm1, 'alpha', density = TRUE)      # Detection
# plot(F23_fm1, 'sigma.sq.p', density = TRUE) # Random effect

# # Clear plots
# dev.off() 

# # Mean of spatial random effects
# w.means <- apply(F23_fm1$w.samples, 2, mean)
# hist(w.means)

# Check fit
F23_bm_ppc <- ppcAbund(F23_fm1, fit.stat = "chi-squared", group = 1)
summary(F23_bm_ppc)

# ----------------------
# Abundance Estimates 
# ----------------------

# F23_fm1$N.samples is abundance estimate per transect
print(F23_fm1$N.samples)

# Summarizing estimates by transect
F23_all_site_ests <- rowSums(F23_fm1$N.samples)

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
F23_dens_vec <-  (F23_all_site_ests / (sum(transect_effort$Area_ac)))

# Correcting desity estimates to total abundance in the area
F23_abund_vec <- F23_dens_vec * 2710

# Compute summary statistics
F23_abund_summary <- data.frame(Model = "Heli HDS", 
                                Season = "Fall 2023",
                                Data = "Helicopter",
                                Season_Model = "F23 Heli HDS",
                                N = mean(F23_abund_vec, na.rm = TRUE),  
                                LCI = as.numeric(quantile(F23_abund_vec, probs = 0.025, na.rm = TRUE)), 
                                UCI = as.numeric(quantile(F23_abund_vec, probs = 0.975, na.rm = TRUE)) 
)

# Print Abundance Summary
print(F23_abund_summary)

# Export abundance estimates
saveRDS(F23_abund_summary, "./Model_Objects/F23_Heli_HDS_AbundEst.rds")


# -------------------------------------------------------
#                     Winter 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W24_inits <- list(beta = 0, 
                  alpha = 0, 
                  kappa = 1,
                  sigma.sq.p = 0.1,
                  sigma.sq = 1, phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(W24_spA_dat$y)),
                  N = apply(W24_spA_dat$y, 1, sum)) 

# ----------------------
# Fit Model
# ----------------------
W24_fm1 <- spDS(abund.formula = ~ scale(herb_Pdens),
                det.formula = ~ mnGS + as.factor(SurveyTime) + scale(woody_AggInx) + (1|SiteID), 
                data = W24_spA_dat,
                family = 'Poisson',
                det.func = 'halfnormal',
                transect = 'line',
                cov.model <- 'exponential',
                NNGP <- TRUE,
                n.neighbors <- 15,
                search.type <- 'cb',
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
                n.report = n.report
)


# ----------------------
# Checking convergence 
# ----------------------

# Rhat values of 1.0 to 1.1 indicate good mixing
W24_fm1$rhat 

# # Trace Plots
# plot(W24_fm1, 'beta', density = TRUE)       # Abundance
# plot(W24_fm1, 'alpha', density = TRUE)      # Detection
# plot(W24_fm1, 'sigma.sq.p', density = TRUE) # Random effect

# # Clear plots
# dev.off() 

# # Mean of spatial random effects
# w.means <- apply(W24_fm1$w.samples, 2, mean)
# hist(w.means)

# Check fit
W24_bm_ppc <- ppcAbund(W24_fm1, fit.stat = "chi-squared", group = 1)
summary(W24_bm_ppc)

# ----------------------
# Abundance Estimates 
# ----------------------

# W24_fm1$N.samples is abundance estimate per transect
print(W24_fm1$N.samples)

# Summarizing estimates by transect
W24_all_site_ests <- rowSums(W24_fm1$N.samples)

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
W24_dens_vec <-  (W24_all_site_ests / (sum(transect_effort$Area_ac)))

# Correcting desity estimates to total abundance in the area
W24_abund_vec <- W24_dens_vec * 2710

# Compute summary statistics
W24_abund_summary <- data.frame(Model = "Heli HDS", 
                                Season = "Winter 2024",
                                Data = "Helicopter",
                                Season_Model = "W24 Heli HDS",
                                N = mean(W24_abund_vec, na.rm = TRUE),  
                                LCI = as.numeric(quantile(W24_abund_vec, probs = 0.025, na.rm = TRUE)), 
                                UCI = as.numeric(quantile(W24_abund_vec, probs = 0.975, na.rm = TRUE)) 
)

# Print Abundance Summary
print(W24_abund_summary)

# Export abundance estimates
saveRDS(W24_abund_summary, "./Model_Objects/W24_Heli_HDS_AbundEst.rds")


# -------------------------------------------------------
#                     Fall 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F24_inits <- list(beta = 0, 
                  alpha = 0, 
                  kappa = 1,
                  sigma.sq.p = 0.1,
                  sigma.sq = 1, phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(F24_spA_dat$y)),
                  N = apply(F24_spA_dat$y, 1, sum)) 

# ----------------------
# Fit Model
# ----------------------
F24_fm1 <- spDS(abund.formula = ~ scale(herb_Pdens),
                det.formula = ~ mnGS + as.factor(SurveyTime) + scale(woody_AggInx) + (1|SiteID), 
                data = F24_spA_dat,
                family = 'Poisson',
                det.func = 'halfnormal',
                transect = 'line',
                cov.model <- 'exponential',
                NNGP <- TRUE,
                n.neighbors <- 15,
                search.type <- 'cb',
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
                n.report = n.report
)


# ----------------------
# Checking convergence 
# ----------------------

# Rhat values of 1.0 to 1.1 indicate good mixing
F24_fm1$rhat 

# # Trace Plots
# plot(F24_fm1, 'beta', density = TRUE)       # Abundance
# plot(F24_fm1, 'alpha', density = TRUE)      # Detection
# plot(F24_fm1, 'sigma.sq.p', density = TRUE) # Random effect

# # Clear plots
# dev.off() 

# # Mean of spatial random effects
# w.means <- apply(F24_fm1$w.samples, 2, mean)
# hist(w.means)

# Check fit
F24_bm_ppc <- ppcAbund(F24_fm1, fit.stat = "chi-squared", group = 1)
summary(F24_bm_ppc)

# ----------------------
# Abundance Estimates 
# ----------------------

# F24_fm1$N.samples is abundance estimate per transect
print(F24_fm1$N.samples)

# Summarizing estimates by transect
F24_all_site_ests <- rowSums(F24_fm1$N.samples)

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
F24_dens_vec <-  (F24_all_site_ests / (sum(transect_effort$Area_ac)))

# Correcting desity estimates to total abundance in the area
F24_abund_vec <- F24_dens_vec * 2710

# Compute summary statistics
F24_abund_summary <- data.frame(Model = "Heli HDS", 
                                Season = "Fall 2024",
                                Data = "Helicopter",
                                Season_Model = "F24 Heli HDS",
                                N = mean(F24_abund_vec, na.rm = TRUE),  
                                LCI = as.numeric(quantile(F24_abund_vec, probs = 0.025, na.rm = TRUE)), 
                                UCI = as.numeric(quantile(F24_abund_vec, probs = 0.975, na.rm = TRUE)) 
)

# Print Abundance Summary
print(F24_abund_summary)

# Export abundance estimates
saveRDS(F24_abund_summary, "./Model_Objects/F24_Heli_HDS_AbundEst.rds")


# -------------------------------------------------------
#                     Winter 2025
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W25_inits <- list(beta = 0, 
                  alpha = 0, 
                  kappa = 1,
                  sigma.sq.p = 0.1,
                  sigma.sq = 1, phi = 3 / mean(dist_mat),
                  w = rep(0, nrow(W25_spA_dat$y)),
                  N = apply(W25_spA_dat$y, 1, sum)) 

# ----------------------
# Fit Model
# ----------------------
W25_fm1 <- spDS(abund.formula = ~ scale(herb_Pdens),
                det.formula = ~ mnGS + as.factor(SurveyTime) + scale(woody_AggInx) + (1|SiteID), 
                data = W25_spA_dat,
                family = 'Poisson',
                det.func = 'halfnormal',
                transect = 'line',
                cov.model <- 'exponential',
                NNGP <- TRUE,
                n.neighbors <- 15,
                search.type <- 'cb',
                inits = W25_inits,
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
                n.report = n.report
)


# ----------------------
# Checking convergence 
# ----------------------

# Rhat values of 1.0 to 1.1 indicate good mixing
W25_fm1$rhat 

# # Trace Plots
# plot(W25_fm1, 'beta', density = TRUE)       # Abundance
# plot(W25_fm1, 'alpha', density = TRUE)      # Detection
# plot(W25_fm1, 'sigma.sq.p', density = TRUE) # Random effect
# 
# # Clear plots
# dev.off() 

# # Mean of spatial random effects
# w.means <- apply(W25_fm1$w.samples, 2, mean)
# hist(w.means)

# Check fit
W25_bm_ppc <- ppcAbund(W25_fm1, fit.stat = "chi-squared", group = 1)
summary(W25_bm_ppc)


# ----------------------
# Abundance Estimates 
# ----------------------

# W25_fm1$N.samples is abundance estimate per transect
print(W25_fm1$N.samples)

# Summarizing estimates by transect
W25_all_site_ests <- rowSums(W25_fm1$N.samples)

# Create a density matrix which is the latent abundance across sites divided by the area surveyed
W25_dens_vec <-  (W25_all_site_ests / (sum(transect_effort$Area_ac)))

# Correcting desity estimates to total abundance in the area
W25_abund_vec <- W25_dens_vec * 2710

# Compute summary statistics
W25_abund_summary <- data.frame(Model = "Heli HDS", 
                                Season = "Winter 2025",
                                Season_Model = "W25 Heli HDS",
                                Data = "Helicopter",
                                N = mean(W25_abund_vec, na.rm = TRUE),  
                                LCI = as.numeric(quantile(W25_abund_vec, probs = 0.025, na.rm = TRUE)), 
                                UCI = as.numeric(quantile(W25_abund_vec, probs = 0.975, na.rm = TRUE)) 
)

# Print Abundance Summary
print(W25_abund_summary)

# Export abundance estimates
saveRDS(W25_abund_summary, "./Model_Objects/W25_Heli_HDS_AbundEst.rds")



# ----------------------------- End of Script -----------------------------