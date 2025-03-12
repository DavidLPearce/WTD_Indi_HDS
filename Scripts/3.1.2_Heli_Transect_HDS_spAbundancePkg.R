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

# Load library
library(tidyverse)
library(spAbundance)

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

# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# ----------------------
# Addign a Survey Code
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
# spAbundance Format
# ----------------------

# spAbundance use long formatting
F23spA_dat <- list(y = fall23_mat, covs = F23spA_site_covs, dist.breaks = c(0, 20, 40, 60, 100)) 
W24spA_dat <- list(y = win24_mat, covs = W24spA_site_covs, dist.breaks = c(0, 20, 40, 60, 100))  
F24spA_dat <- list(y = fall24_mat, covs = F24spA_site_covs, dist.breaks = c(0, 20, 40, 60, 100))  
W25spA_dat <- list(y = win25_mat, covs = W25spA_site_covs, dist.breaks = c(0, 20, 40, 60, 100))  
 
# Take a look
str(F23spA_dat)
str(W24spA_dat)
str(F24spA_dat)
str(W25spA_dat)

 
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
  alpha.star = 0.25
) 


# Initial values are by data set


# -------------------------------------------------------
#                     Fall 2023
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F23inits <- list(alpha = 0.1,               
                 beta = 0.1,                
                 sigma.sq.p = 0.1,
                 N = apply(F23spA_dat$y, 1, sum)
) 
 
# ----------------------
# Detection Function
# ----------------------

# Half-normal  
F23_hn_fm0 <- DS(abund.formula = ~ 1 ,
            det.formula = ~ (1|SiteID), 
            data = F23spA_dat,
            family = 'Poisson',
            det.func = 'halfnormal',
            transect = 'line',
            inits = F23inits,
            priors = priors,
            tuning = tuning,
            accept.rate = 0.43,
            n.batch = n.batch,
            batch.length = batch.length,
            n.burn = n.burn,
            n.thin = n.thin,
            n.chains = n.chains,
            verbose = FALSE)


# Negative Exponential  
F23_ne_fm0 <- DS(abund.formula = ~ 1 ,
            det.formula = ~ (1|SiteID), 
            data = F23spA_dat,
            family = 'Poisson',
            det.func = 'negexp',
            transect = 'line',
            inits = F23inits,
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
# Checking convergence 
# ----------------------

# Half-normal detection function
plot(F23_hn_fm0, 'beta', density = TRUE) # Abundance parameters
plot(F23_hn_fm0, 'alpha', density = TRUE) # Detection parameters
plot(F23_hn_fm0, 'sigma.sq.p', density = TRUE) # Random effect
F23_hn_fm0$rhat # Rhat values of 1.0 to 1.1 indicate good mixing
dev.off() # clear plots


# Negative exponential detection function
plot(F23_ne_fm0, 'beta', density = TRUE)  
plot(F23_ne_fm0, 'alpha', density = TRUE)
plot(F23_ne_fm0, 'sigma.sq.p', density = TRUE) 
F23_ne_fm0$rhat 
dev.off()  

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
F23_hn_fm0_waic <- waicAbund(F23_hn_fm0)
F23_ne_fm0_waic <- waicAbund(F23_ne_fm0)

# Extract the WAIC values for each model
F23detfun_waic_values <- c(F23_hn_fm0_waic["WAIC"],
                           F23_ne_fm0_waic["WAIC"])

# Create a named vector with model names
F23detfun_names <- c("Half-normal", 
                      "Negative Exponential")

# Combine model names and WAIC values into a data frame for ranking
F23detfun_waic_df <- data.frame(Model = F23detfun_names, 
                              WAIC = F23detfun_waic_values)

# Rank models based on WAIC (lower WAIC is better)
F23detfun_waic_df <- F23detfun_waic_df[order(F23detfun_waic_df$WAIC), ]

# Print the ranked models
print(F23detfun_waic_df)

# Negative Exponential was the better model

# ----------------------
# Detection Covariates
# ----------------------

# Using largest patch index on abundance since WTD typically hang out around woody areas
# with brief excursions out in the open 

# Group size
F23_ne_fm1 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ mnGS + (1|SiteID), 
                 data = F23spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F23inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# SurveyTime
F23_ne_fm2 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ as.factor(SurveyTime) + (1|SiteID), 
                 data = F23spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F23inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Aggregation Index
F23_ne_fm3 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_AggInx) + (1|SiteID), 
                 data = F23spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F23inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Largest Patch Index
F23_ne_fm4 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) + (1|SiteID), 
                 data = F23spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F23inits,
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
# Checking convergence 
# ----------------------

# # Group size
# plot(F23_ne_fm1, 'beta', density = TRUE)  
# plot(F23_ne_fm1, 'alpha', density = TRUE)  
# plot(F23_ne_fm1, 'sigma.sq.p', density = TRUE)  
# F23_ne_fm1$rhat  
# dev.off()  
# 
# 
# # SurveyTime
# plot(F23_ne_fm2, 'beta', density = TRUE)  
# plot(F23_ne_fm2, 'alpha', density = TRUE)
# plot(F23_ne_fm2, 'sigma.sq.p', density = TRUE) 
# F23_ne_fm2$rhat 
# dev.off() 
# 
# # Woody Aggregation Index
# plot(F23_ne_fm3, 'beta', density = TRUE)  
# plot(F23_ne_fm3, 'alpha', density = TRUE)
# plot(F23_ne_fm3, 'sigma.sq.p', density = TRUE) 
# F23_ne_fm3$rhat 
# dev.off()
# 
# # Woody Largest Patch Index
# plot(F23_ne_fm4, 'beta', density = TRUE)  
# plot(F23_ne_fm4, 'alpha', density = TRUE)
# plot(F23_ne_fm4, 'sigma.sq.p', density = TRUE) 
# F23_ne_fm4$rhat 
# dev.off()

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
F23_ne_fm0_waic <- waicAbund(F23_ne_fm0)
F23_ne_fm1_waic <- waicAbund(F23_ne_fm1)
F23_ne_fm2_waic <- waicAbund(F23_ne_fm2)
F23_ne_fm3_waic <- waicAbund(F23_ne_fm3)
F23_ne_fm4_waic <- waicAbund(F23_ne_fm4)

# Extract the WAIC values for each model
F23_waic_values <- c(F23_ne_fm0_waic["WAIC"],
                     F23_ne_fm1_waic["WAIC"],
                     F23_ne_fm2_waic["WAIC"],
                     F23_ne_fm3_waic["WAIC"],
                     F23_ne_fm4_waic["WAIC"]
)

# Create a named vector with model names
F23_names <- c("fm0", 
               "fm1",
               "fm2",
               "fm3",
               "fm4"
)

# Combine model names and WAIC values into a data frame for ranking
F23_waic_df <- data.frame(Model = F23_names, 
                                WAIC = F23_waic_values)

# Rank models based on WAIC (lower WAIC is better)
F23_waic_df <- F23_waic_df[order(F23_waic_df$WAIC), ]

# Print the ranked models
print(F23_waic_df)

# Best model is fm2 by ~5 WAIC

# Check fit
F23_bm_ppc <- ppcAbund(F23_ne_fm2, fit.stat = "chi-squared", group = 1)
summary(F23_bm_ppc)

# Fit is not the best

# Export best model
saveRDS(F23_ne_fm2, "./Model_Objects/F23_Heli_HDS_BestModel.rds")

# -------------------------------------------------------
#                     Winter 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W24inits <- list(alpha = 0.1,               
                 beta = 0.1,                
                 sigma.sq.p = 0.1,
                 N = apply(W24spA_dat$y, 1, sum)
) 

# ----------------------
# Detection Function
# ----------------------

# Half-normal  
W24_hn_fm0 <- DS(abund.formula = ~ 1 ,
                 det.formula = ~ (1|SiteID), 
                 data = W24spA_dat,
                 family = 'Poisson',
                 det.func = 'halfnormal',
                 transect = 'line',
                 inits = W24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# Negative Exponential  
W24_ne_fm0 <- DS(abund.formula = ~ 1 ,
                 det.formula = ~ (1|SiteID), 
                 data = W24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W24inits,
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
# Checking convergence 
# ----------------------

# # Half-normal detection function
# plot(W24_hn_fm0, 'beta', density = TRUE) # Abundance parameters
# plot(W24_hn_fm0, 'alpha', density = TRUE) # Detection parameters
# plot(W24_hn_fm0, 'sigma.sq.p', density = TRUE) # Random effect
# W24_hn_fm0$rhat # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off() # clear plots
# 
# 
# # Negative exponential detection function
# plot(W24_ne_fm0, 'beta', density = TRUE)  
# plot(W24_ne_fm0, 'alpha', density = TRUE)
# plot(W24_ne_fm0, 'sigma.sq.p', density = TRUE) 
# W24_ne_fm0$rhat 
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
W24_hn_fm0_waic <- waicAbund(W24_hn_fm0)
W24_ne_fm0_waic <- waicAbund(W24_ne_fm0)

# Extract the WAIC values for each model
W24_detfun_waic_values <- c(W24_hn_fm0_waic["WAIC"],
                            W24_ne_fm0_waic["WAIC"])

# Create a named vector with model names
W24_detfun_names <- c("Half-normal", 
                      "Negative Exponential")

# Combine model names and WAIC values into a data frame for ranking
W24_detfun_waic_df <- data.frame(Model = W24_detfun_names, 
                                 WAIC = W24_detfun_waic_values)

# Rank models based on WAIC (lower WAIC is better)
W24_detfun_waic_df <- W24_detfun_waic_df[order(W24_detfun_waic_df$WAIC), ]

# Print the ranked models
print(W24_detfun_waic_df)

# Negative Exponential was the better model

# ----------------------
# Detection Covariates
# ----------------------

# Using largest patch index on abundance since WTD typically hang out around woody areas
# with brief excursions out in the open 

# Group size
W24_ne_fm1 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ mnGS + (1|SiteID), 
                 data = W24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# SurveyTime
W24_ne_fm2 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ as.factor(SurveyTime) + (1|SiteID), 
                 data = W24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Aggregation Index
W24_ne_fm3 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_AggInx) + (1|SiteID), 
                 data = W24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Largest Patch Index
W24_ne_fm4 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) + (1|SiteID), 
                 data = W24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W24inits,
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
# Checking convergence 
# ----------------------

# # Group size
# plot(W24_ne_fm1, 'beta', density = TRUE)  
# plot(W24_ne_fm1, 'alpha', density = TRUE)  
# plot(W24_ne_fm1, 'sigma.sq.p', density = TRUE)  
# W24_ne_fm1$rhat  
# dev.off()  
# 
# 
# # SurveyTime
# plot(W24_ne_fm2, 'beta', density = TRUE)  
# plot(W24_ne_fm2, 'alpha', density = TRUE)
# plot(W24_ne_fm2, 'sigma.sq.p', density = TRUE) 
# W24_ne_fm2$rhat 
# dev.off() 
# 
# # Woody Aggregation Index
# plot(W24_ne_fm3, 'beta', density = TRUE)  
# plot(W24_ne_fm3, 'alpha', density = TRUE)
# plot(W24_ne_fm3, 'sigma.sq.p', density = TRUE) 
# W24_ne_fm3$rhat 
# dev.off()
# 
# # Woody Largest Patch Index
# plot(W24_ne_fm4, 'beta', density = TRUE)  
# plot(W24_ne_fm4, 'alpha', density = TRUE)
# plot(W24_ne_fm4, 'sigma.sq.p', density = TRUE) 
# W24_ne_fm4$rhat 
# dev.off()

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
W24_ne_fm0_waic <- waicAbund(W24_ne_fm0)
W24_ne_fm1_waic <- waicAbund(W24_ne_fm1)
W24_ne_fm2_waic <- waicAbund(W24_ne_fm2)
W24_ne_fm3_waic <- waicAbund(W24_ne_fm3)
W24_ne_fm4_waic <- waicAbund(W24_ne_fm4)

# Extract the WAIC values for each model
W24_waic_values <- c(W24_ne_fm0_waic["WAIC"],
                     W24_ne_fm1_waic["WAIC"],
                     W24_ne_fm2_waic["WAIC"],
                     W24_ne_fm3_waic["WAIC"],
                     W24_ne_fm4_waic["WAIC"]
)

# Create a named vector with model names
W24_names <- c("fm0", 
               "fm1",
               "fm2",
               "fm3",
               "fm4"
)

# Combine model names and WAIC values into a data frame for ranking
W24_waic_df <- data.frame(Model = W24_names, 
                          WAIC = W24_waic_values)

# Rank models based on WAIC (lower WAIC is better)
W24_waic_df <- W24_waic_df[order(W24_waic_df$WAIC), ]

# Print the ranked models
print(W24_waic_df)

# Best model is fm2 by ~5 WAIC

# Check fit
W24_bm_ppc <- ppcAbund(W24_ne_fm2, fit.stat = "chi-squared", group = 1)
summary(W24_bm_ppc)

# Fit is not the best

# Export best model
saveRDS(W24_ne_fm2, "./Model_Objects/W24_Heli_HDS_BestModel.rds")


# -------------------------------------------------------
#                     Fall 2024
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
F24inits <- list(alpha = 0.1,               
                 beta = 0.1,                
                 sigma.sq.p = 0.1,
                 N = apply(F24spA_dat$y, 1, sum)
) 

# ----------------------
# Detection Function
# ----------------------

# Half-normal  
F24_hn_fm0 <- DS(abund.formula = ~ 1 ,
                 det.formula = ~ (1|SiteID), 
                 data = F24spA_dat,
                 family = 'Poisson',
                 det.func = 'halfnormal',
                 transect = 'line',
                 inits = F24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# Negative Exponential  
F24_ne_fm0 <- DS(abund.formula = ~ 1 ,
                 det.formula = ~ (1|SiteID), 
                 data = F24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F24inits,
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
# Checking convergence 
# ----------------------

# # Half-normal detection function
# plot(F24_hn_fm0, 'beta', density = TRUE) # Abundance parameters
# plot(F24_hn_fm0, 'alpha', density = TRUE) # Detection parameters
# plot(F24_hn_fm0, 'sigma.sq.p', density = TRUE) # Random effect
# F24_hn_fm0$rhat # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off() # clear plots
# 
# 
# # Negative exponential detection function
# plot(F24_ne_fm0, 'beta', density = TRUE)  
# plot(F24_ne_fm0, 'alpha', density = TRUE)
# plot(F24_ne_fm0, 'sigma.sq.p', density = TRUE) 
# F24_ne_fm0$rhat 
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
F24_hn_fm0_waic <- waicAbund(F24_hn_fm0)
F24_ne_fm0_waic <- waicAbund(F24_ne_fm0)

# Extract the WAIC values for each model
F24_detfun_waic_values <- c(F24_hn_fm0_waic["WAIC"],
                            F24_ne_fm0_waic["WAIC"])

# Create a named vector with model names
F24_detfun_names <- c("Half-normal", 
                      "Negative Exponential")

# Combine model names and WAIC values into a data frame for ranking
F24_detfun_waic_df <- data.frame(Model = F24_detfun_names, 
                                 WAIC = F24_detfun_waic_values)

# Rank models based on WAIC (lower WAIC is better)
F24_detfun_waic_df <- F24_detfun_waic_df[order(F24_detfun_waic_df$WAIC), ]

# Print the ranked models
print(F24_detfun_waic_df)

# Negative Exponential was the better model

# ----------------------
# Detection Covariates
# ----------------------

# Using largest patch index on abundance since WTD typically hang out around woody areas
# with brief excursions out in the open 

# Group size
F24_ne_fm1 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ mnGS + (1|SiteID), 
                 data = F24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# SurveyTime
F24_ne_fm2 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ as.factor(SurveyTime) + (1|SiteID), 
                 data = F24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Aggregation Index
F24_ne_fm3 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_AggInx) + (1|SiteID), 
                 data = F24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F24inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Largest Patch Index
F24_ne_fm4 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) + (1|SiteID), 
                 data = F24spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = F24inits,
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
# Checking convergence 
# ----------------------

# # Group size
# plot(F24_ne_fm1, 'beta', density = TRUE)  
# plot(F24_ne_fm1, 'alpha', density = TRUE)  
# plot(F24_ne_fm1, 'sigma.sq.p', density = TRUE)  
# F24_ne_fm1$rhat  
# dev.off()  
# 
# 
# # SurveyTime
# plot(F24_ne_fm2, 'beta', density = TRUE)  
# plot(F24_ne_fm2, 'alpha', density = TRUE)
# plot(F24_ne_fm2, 'sigma.sq.p', density = TRUE) 
# F24_ne_fm2$rhat 
# dev.off() 
# 
# # Woody Aggregation Index
# plot(F24_ne_fm3, 'beta', density = TRUE)  
# plot(F24_ne_fm3, 'alpha', density = TRUE)
# plot(F24_ne_fm3, 'sigma.sq.p', density = TRUE) 
# F24_ne_fm3$rhat 
# dev.off()
# 
# # Woody Largest Patch Index
# plot(F24_ne_fm4, 'beta', density = TRUE)  
# plot(F24_ne_fm4, 'alpha', density = TRUE)
# plot(F24_ne_fm4, 'sigma.sq.p', density = TRUE) 
# F24_ne_fm4$rhat 
# dev.off()

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
F24_ne_fm0_waic <- waicAbund(F24_ne_fm0)
F24_ne_fm1_waic <- waicAbund(F24_ne_fm1)
F24_ne_fm2_waic <- waicAbund(F24_ne_fm2)
F24_ne_fm3_waic <- waicAbund(F24_ne_fm3)
F24_ne_fm4_waic <- waicAbund(F24_ne_fm4)

# Extract the WAIC values for each model
F24_waic_values <- c(F24_ne_fm0_waic["WAIC"],
                     F24_ne_fm1_waic["WAIC"],
                     F24_ne_fm2_waic["WAIC"],
                     F24_ne_fm3_waic["WAIC"],
                     F24_ne_fm4_waic["WAIC"]
)

# Create a named vector with model names
F24_names <- c("fm0", 
               "fm1",
               "fm2",
               "fm3",
               "fm4"
)

# Combine model names and WAIC values into a data frame for ranking
F24_waic_df <- data.frame(Model = F24_names, 
                          WAIC = F24_waic_values)

# Rank models based on WAIC (lower WAIC is better)
F24_bm_ppc <- F24_waic_df[order(F24_waic_df$WAIC), ]

# Print the ranked models
print(F24_bm_ppc)

# Best model is fm2 by ~5 WAIC

# Check fit
F24_bm_ppc <- ppcAbund(F24_ne_fm2, fit.stat = "chi-squared", group = 1)
summary(F24_bm_ppc)

# Export best model
saveRDS(F24_ne_fm2, "./Model_Objects/F24_Heli_HDS_BestModel.rds")


# -------------------------------------------------------
#                     Winter 2025
# -------------------------------------------------------

# ----------------------
# Initial values
# ----------------------
W25inits <- list(alpha = 0.1,               
                 beta = 0.1,                
                 sigma.sq.p = 0.1,
                 N = apply(W25spA_dat$y, 1, sum)
) 

# ----------------------
# Detection Function
# ----------------------

# Half-normal  
W25_hn_fm0 <- DS(abund.formula = ~ 1 ,
                 det.formula = ~ (1|SiteID), 
                 data = W25spA_dat,
                 family = 'Poisson',
                 det.func = 'halfnormal',
                 transect = 'line',
                 inits = W25inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# Negative Exponential  
W25_ne_fm0 <- DS(abund.formula = ~ 1 ,
                 det.formula = ~ (1|SiteID), 
                 data = W25spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W25inits,
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
# Checking convergence 
# ----------------------

# # Half-normal detection function
# plot(W25_hn_fm0, 'beta', density = TRUE) # Abundance parameters
# plot(W25_hn_fm0, 'alpha', density = TRUE) # Detection parameters
# plot(W25_hn_fm0, 'sigma.sq.p', density = TRUE) # Random effect
# W25_hn_fm0$rhat # Rhat values of 1.0 to 1.1 indicate good mixing
# dev.off() # clear plots
# 
# 
# # Negative exponential detection function
# plot(W25_ne_fm0, 'beta', density = TRUE)  
# plot(W25_ne_fm0, 'alpha', density = TRUE)
# plot(W25_ne_fm0, 'sigma.sq.p', density = TRUE) 
# W25_ne_fm0$rhat 
# dev.off()  

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
W25_hn_fm0_waic <- waicAbund(W25_hn_fm0)
W25_ne_fm0_waic <- waicAbund(W25_ne_fm0)

# Extract the WAIC values for each model
W25_detfun_waic_values <- c(W25_hn_fm0_waic["WAIC"],
                            W25_ne_fm0_waic["WAIC"])

# Create a named vector with model names
W25_detfun_names <- c("Half-normal", 
                      "Negative Exponential")

# Combine model names and WAIC values into a data frame for ranking
W25_detfun_waic_df <- data.frame(Model = W25_detfun_names, 
                                 WAIC = W25_detfun_waic_values)

# Rank models based on WAIC (lower WAIC is better)
W25_detfun_waic_df <- W25_detfun_waic_df[order(W25_detfun_waic_df$WAIC), ]

# Print the ranked models
print(W25_detfun_waic_df)

# Negative Exponential was the better model

# ----------------------
# Detection Covariates
# ----------------------

# Using largest patch index on abundance since WTD typically hang out around woody areas
# with brief excursions out in the open 

# Group size
W25_ne_fm1 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ mnGS + (1|SiteID), 
                 data = W25spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W25inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)


# SurveyTime
W25_ne_fm2 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ as.factor(SurveyTime) + (1|SiteID), 
                 data = W25spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W25inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Aggregation Index
W25_ne_fm3 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_AggInx) + (1|SiteID), 
                 data = W25spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W25inits,
                 priors = priors,
                 tuning = tuning,
                 accept.rate = 0.43,
                 n.batch = n.batch,
                 batch.length = batch.length,
                 n.burn = n.burn,
                 n.thin = n.thin,
                 n.chains = n.chains,
                 verbose = FALSE)

# Woody Largest Patch Index
W25_ne_fm4 <- DS(abund.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) ,
                 det.formula = ~ scale(woody_lrgPInx) + scale(herb_ClmIdx) + (1|SiteID), 
                 data = W25spA_dat,
                 family = 'Poisson',
                 det.func = 'negexp',
                 transect = 'line',
                 inits = W25inits,
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
# Checking convergence 
# ----------------------

# # Group size
# plot(W25_ne_fm1, 'beta', density = TRUE)  
# plot(W25_ne_fm1, 'alpha', density = TRUE)  
# plot(W25_ne_fm1, 'sigma.sq.p', density = TRUE)  
# W25_ne_fm1$rhat  
# dev.off()  
# 
# 
# # SurveyTime
# plot(W25_ne_fm2, 'beta', density = TRUE)  
# plot(W25_ne_fm2, 'alpha', density = TRUE)
# plot(W25_ne_fm2, 'sigma.sq.p', density = TRUE) 
# W25_ne_fm2$rhat 
# dev.off() 
# 
# # Woody Aggregation Index
# plot(W25_ne_fm3, 'beta', density = TRUE)  
# plot(W25_ne_fm3, 'alpha', density = TRUE)
# plot(W25_ne_fm3, 'sigma.sq.p', density = TRUE) 
# W25_ne_fm3$rhat 
# dev.off()
# 
# # Woody Largest Patch Index
# plot(W25_ne_fm4, 'beta', density = TRUE)  
# plot(W25_ne_fm4, 'alpha', density = TRUE)
# plot(W25_ne_fm4, 'sigma.sq.p', density = TRUE) 
# W25_ne_fm4$rhat 
# dev.off()

# ----------------------
# Rank models 
# ----------------------

# Calculating WAIC
W25_ne_fm0_waic <- waicAbund(W25_ne_fm0)
W25_ne_fm1_waic <- waicAbund(W25_ne_fm1)
W25_ne_fm2_waic <- waicAbund(W25_ne_fm2)
W25_ne_fm3_waic <- waicAbund(W25_ne_fm3)
W25_ne_fm4_waic <- waicAbund(W25_ne_fm4)

# Extract the WAIC values for each model
W25_waic_values <- c(W25_ne_fm0_waic["WAIC"],
                     W25_ne_fm1_waic["WAIC"],
                     W25_ne_fm2_waic["WAIC"],
                     W25_ne_fm3_waic["WAIC"],
                     W25_ne_fm4_waic["WAIC"]
)

# Create a named vector with model names
W25_names <- c("fm0", 
               "fm1",
               "fm2",
               "fm3",
               "fm4"
)

# Combine model names and WAIC values into a data frame for ranking
W25_waic_df <- data.frame(Model = W25_names, 
                          WAIC = W25_waic_values)

# Rank models based on WAIC (lower WAIC is better)
W25_waic_df <- W25_waic_df[order(W25_waic_df$WAIC), ]

# Print the ranked models
print(W25_waic_df)

# Best model is fm2 by ~5 WAIC

# Check fit
W25_bm_ppc <- ppcAbund(W25_ne_fm2, fit.stat = "chi-squared", group = 1)
summary(W25_bm_ppc)

# Export best model
saveRDS(W25_ne_fm2, "./Model_Objects/W25_Heli_HDS_BestModel.rds")



# ----------------------------- End of Script -----------------------------