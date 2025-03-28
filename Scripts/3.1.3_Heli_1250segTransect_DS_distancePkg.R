# Author: David L. Pearce
# Description:
#             This script extends the script and vignette written by Eric Rexstad
#             for line transect distance sampling. Which can be found here: 
#             https://distancesampling.org/Distance/articles/lines-distill.html
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
# install.packages("Distance")

# Load packages
library(Distance)

# Set seed, scientific notation, and workplace
set.seed(123)
options(scipen = 9999)
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Load survey data
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Formatted_Heli_1250segTransect_Data.csv", row.names = 1)

# Take a look at the data
head(heli_dat, 5)

# Load site covariates data
site_covs <- read.csv("./Data/Survey_Data/Helicopter_Data/Heli_1250segTransect_siteCovs.csv", row.names = 1)

# Take a look at the data
head(site_covs, 5)


# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# ----------------------
# Site Covariates 
# ----------------------

# Changing Transect ID column name for matching with site covs
names(heli_dat)[3] <- "segID"

# Match with site covs
heli_dat <- merge(x = heli_dat, y = site_covs,  by ="segID")


# ----------------------
# Naming Scheme
# ----------------------

# The distance package requires specific naming of columns
names(heli_dat)[2] <- "Study.Area"        # Study Area
names(heli_dat)[3] <- "Area"              # Area in hectares
names(heli_dat)[1] <- "Sample.Label"      # Transect ID 
names(heli_dat)[4] <- "Effort"            # Transect Length in km
names(heli_dat)[17] <- "replicate"        # Replicate
names(heli_dat)[19] <- "distance"         # Distance
names(heli_dat)[20] <- "size"             # Group size

# Adding a region label
heli_dat$Region.Label <- "Region1"  

# Take a look 
head(heli_dat, 5)

# ----------------------
# Subsetting by survey 
# ----------------------

# Subsetting by survey season
F23_dat <- heli_dat[which(heli_dat$Date == "9/8/2023" | heli_dat$Date == "9/9/2023"),]
W24_dat <- heli_dat[which(heli_dat$Date ==  "2/9/2024" | heli_dat$Date == "2/10/2024"),]
F24_dat <- heli_dat[which(heli_dat$Date ==  "9/6/2024" | heli_dat$Date == "9/7/2024"),]
W25_dat <- heli_dat[which(heli_dat$Date ==  "2/7/2025" | heli_dat$Date == "2/8/2025"),]

# Adding an object column for unique ID - Distance Package naming scheme
F23_dat$object <- rownames(F23_dat)         
W24_dat$object <- rownames(W24_dat)         
F24_dat$object <- rownames(F24_dat)         
W25_dat$object <- rownames(W25_dat)   

# ----------------------
# Frequency Plots 
# ----------------------

# Looking at the frequency of detections in relation to distance
hist(heli_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "") 

# Frequency of detections by survey
hist(F23_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '23") 
hist(W24_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '24") 
hist(F24_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '24") 
hist(W25_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '25") 



# ----------------------
# Distance Bins 
# ----------------------

# Bins of 0-20, 20-40, 40-60, 60-100
F23_dat <- create_bins(F23_dat, c(0, 20, 40, 60, 100))
W24_dat <- create_bins(W24_dat, c(0, 20, 40, 60, 100))
F24_dat <- create_bins(F24_dat, c(0, 20, 40, 60, 100))
W25_dat <- create_bins(W25_dat, c(0, 20, 40, 60, 100))

# Remove Distance column now that distances are binned
F23_dat <- F23_dat[,-19]
W24_dat <- W24_dat[,-19]
F24_dat <- F24_dat[,-19]
W25_dat <- W25_dat[,-19]

# ----------------------
# Units of Measure 
# ----------------------

# Object for converting units
conversion.factor <- convert_units("meter",    # Distance
                                   "meter",# Transect Length
                                   "hectare")  # Study area





# ------------------------------------------------------------------------------
#
#                        Distance Sampling Models
#
# ------------------------------------------------------------------------------

# -------------------------------------------------------
#                     Fall 2023
# -------------------------------------------------------

# ----------------------
# Fit Model
# ----------------------
F23_fit1 <- ds(data = F23_dat, 
               formula = ~ size + factor(replicate) + scale(woody_AggInx), 
               transect = "line",  
               key = "hn", 
               dht_group = FALSE, 
               adjustment = NULL, 
               convert_units = conversion.factor,
)



# Plot detection probabililty by distance bin
plot(F23_fit1, breaks= c(0, 20, 40, 60, 100), main = "Fall '23")

# Goodness of fit
gof_ds(F23_fit1) 


# Getting estimates of abundance
F23_abund <-  dht2(ddf = F23_fit1$ddf,
                   flatfile = F23_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)

# Organize into a dataframe
F23_DS_est <- data.frame(Model = "Heli 1250seg DS",
                         Season = "Fall 2023",
                         Season_Model = "F23 Heli 1250seg DS",
                         Data = "Helicopter",
                         N = F23_abund$Abundance[3],
                         LCI = F23_abund$LCI[3],
                         UCI = F23_abund$UCI[3]
)

# Take a look
print(F23_DS_est)

# -------------------------------------------------------
#                     Winter 2024
# -------------------------------------------------------

# ----------------------
# Fit Model
# ----------------------
W24_fit1 <- ds(data = W24_dat, 
               formula = ~ size + factor(replicate) + scale(woody_AggInx), 
               transect = "line",  
               key = "hn", 
               dht_group = FALSE, 
               adjustment = NULL, 
               convert_units = conversion.factor,
)

# Plot detection probability by distance bin
plot(W24_fit1, breaks= c(0,20,40,60,100), main = "Winter '24")

# Goodness of fit
gof_ds(W24_fit1) 


# Getting estimates of abundance
W24_abund <-  dht2(ddf = W24_fit1$ddf,
                   flatfile = W24_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)

# Organize into a dataframe
W24_DS_est <- data.frame(Model = "Heli 1250seg DS",
                         Season = "Winter 2024",
                         Season_Model = "W24 Heli 1250seg DS",
                         Data = "Helicopter",
                         N = W24_abund$Abundance[3],
                         LCI = W24_abund$LCI[3],
                         UCI = W24_abund$UCI[3]
)

# Take a look
print(W24_DS_est)                 

# -------------------------------------------------------
#                     Fall 2024
# -------------------------------------------------------

# ----------------------
# Fit Model
# ----------------------
F24_fit1 <- ds(data = F24_dat, 
               formula = ~ size + factor(replicate) + scale(woody_AggInx), 
               transect = "line",  
               key = "hn", 
               dht_group = FALSE, 
               adjustment = NULL, 
               convert_units = conversion.factor,
)


# Plot detection probability by distance bin
plot(F24_fit1, breaks= c(0, 20, 40, 60, 100), main = "Fall '24")

# Goodness of fit
gof_ds(F24_fit1) # Chi-square shows a pretty good fit


# Getting estimates of abundance
F24_abund <-  dht2(ddf = F24_fit1$ddf,
                   flatfile = F24_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)


# Organize into a dataframe
F24_DS_est <- data.frame(Model = "Heli 1250seg DS",
                         Season = "Fall 2024",
                         Season_Model = "F24 Heli 1250seg DS",
                         Data = "Helicopter",
                         N = F24_abund$Abundance[3],
                         LCI = F24_abund$LCI[3],
                         UCI = F24_abund$UCI[3]
)

# Take a look
print(F24_DS_est)                           

# -------------------------------------------------------
#                     Winter 2025
# -------------------------------------------------------


# Group size
W25_fit1 <- ds(data = W25_dat, 
               formula = ~ size + factor(replicate) + scale(woody_AggInx), 
               transect = "line",  
               key = "hn", 
               dht_group = FALSE, 
               adjustment = NULL, 
               convert_units = conversion.factor,
)

# Plot detection probability by distance bin
plot(W25_fit1, breaks= c(0, 20, 40, 60, 100), main = "Winter '25")

# Goodness of fit
gof_ds(W25_fit1) # Chi-square shows a pretty good fit


# Getting estimates of abundance
W25_abund <-  dht2(ddf = W25_fit1$ddf,
                   flatfile = W25_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)

# Organize into a dataframe
W25_DS_est <- data.frame(Model = "Heli 1250seg DS",
                         Season = "Winter 2025",
                         Season_Model = "W25 Heli 1250seg DS",
                         Data = "Helicopter",
                         N = W25_abund$Abundance[3],
                         LCI = W25_abund$LCI[3],
                         UCI = W25_abund$UCI[3]
)

# Take a look
print(W25_DS_est)   

# -------------------------------------------------------
# Combine Estimates for Exporting
# -------------------------------------------------------

# Combine all the estimates
All_DS <- rbind(F23_DS_est, W24_DS_est, F24_DS_est, W25_DS_est)
print(All_DS)

# Export
saveRDS(All_DS, "./Model_Objects/Heli_1250segDS_AbundEst.rds")


# ----------------------------- End of Script -----------------------------
