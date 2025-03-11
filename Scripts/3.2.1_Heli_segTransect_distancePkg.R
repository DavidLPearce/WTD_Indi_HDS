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
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Formatted_Heli_segTransect_Data.csv", row.names = 1)

# Take a look at the data
head(heli_dat, 5)

# Load site covariates data
site_covs <- read.csv("./Data/Survey_Data/Helicopter_Data/Heli_segTransect_siteCovs.csv", row.names = 1)

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
head(heli_dat, 5)

# ----------------------
# Naming Scheme
# ----------------------

# The distance package requires specific naming of columns
names(heli_dat)[1] <- "Sample.Label"      # Transect ID 
names(heli_dat)[2] <- "Study.Area"        # Study Area
names(heli_dat)[3] <- "Area"              # Area in hectares
names(heli_dat)[4] <- "Effort"            # Transect Length in km
names(heli_dat)[17] <- "replicate"        # Replicate
names(heli_dat)[19] <- "distance"         # Distance
names(heli_dat)[20] <- "size"             # Group size

# Adding a region label
heli_dat$Region.Label <- "Region1"  

# Check
head(heli_dat, 5)

# ----------------------
# Subsetting by survey 
# ----------------------

# Subsetting by survey season
fall23_dat <- heli_dat[which(heli_dat$Date == "9/8/2023" | heli_dat$Date == "9/9/2023"),]
win24_dat <- heli_dat[which(heli_dat$Date ==  "2/9/2024" | heli_dat$Date == "2/10/2024"),]
fall24_dat <- heli_dat[which(heli_dat$Date ==  "9/6/2024" | heli_dat$Date == "9/7/2024"),]
win25_dat <- heli_dat[which(heli_dat$Date ==  "2/7/2025" | heli_dat$Date == "2/8/2025"),]

# Adding an object column for unique ID - Distance Package naming scheme
fall23_dat$object <- rownames(fall23_dat)         
win24_dat$object <- rownames(win24_dat)         
fall24_dat$object <- rownames(fall24_dat)         
win25_dat$object <- rownames(win25_dat) 

# ----------------------
# Frequency Plots 
# ----------------------

# Looking at the frequency of detections in relation to distance
hist(heli_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "") 

# Frequency of detections by survey
hist(fall23_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '23") 
hist(win24_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '24") 
hist(fall24_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '24") 
hist(win25_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '25") 

# ----------------------
# Distance Bins 
# ----------------------

# Bins of 0-20, 20-40, 40-60, 60-100
fall23_dat <- create_bins(fall23_dat, c(0, 20, 40, 60, 100))
win24_dat <- create_bins(win24_dat, c(0, 20, 40, 60, 100))
fall24_dat <- create_bins(fall24_dat, c(0, 20, 40, 60, 100))
win25_dat <- create_bins(win25_dat, c(0, 20, 40, 60, 100))

# Remove Distance column now that distances are binned
fall23_dat <- fall23_dat[,-19]
win24_dat <- win24_dat[,-19]
fall24_dat <- fall24_dat[,-19]
win25_dat <- win25_dat[,-19]

# Check
head(fall23_dat, 5)

# ----------------------
# Units of Measure 
# ----------------------

# Object for converting units
conversion.factor <- convert_units("meter",    # Distance
                                   "kilometer",# Transect Length
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
# Detection Function
# ----------------------

# Half normal detection function model
F23_hn_null <- ds(data = fall23_dat,
                  formula = ~ 1,     # Detection formula
                  transect = "line", # Point or line 
                  key = "hn",        # Detection function: Half-normal (hn), hazard-rate (hr), uniform (unif)
                  dht_group = FALSE, # Consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                  adjustment = NULL, # Adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                  convert_units = conversion.factor,
)

# Half normal detection function model
F23_hr_null <- ds(data = fall23_dat,
                  formula = ~ 1,
                  transect = "line",  
                  key = "hr",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Model comparison table
model_AIC <- AIC(F23_hn_null, F23_hr_null) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)


# Can see hazard rate is the better model

# Goodness of fit
gof_ds(F23_hr_null) 

# ----------------------
# Covariates
# ----------------------

# Group size
F23_hr_fit1 <- ds(data = fall23_dat, 
                  formula = ~ size, 
                  transect = "line",  
                  key = "hr", 
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Replicate (Evening or Morning) 
F23_hr_fit2 <- ds(data = fall23_dat, 
                  formula = ~ factor(replicate) ,
                  transect = "line", 
                  key = "hr",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,  
)


# Woody Aggregation Index
F23_hr_fit3 <- ds(data = fall23_dat, 
                  formula = ~ scale(woody_AggInx),
                  transect = "line", 
                  key = "hr",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)

# Woody Largest Patch Index
F23_hr_fit4 <- ds(data = fall23_dat, 
                  formula = ~ scale(woody_lrgPInx),
                  transect = "line", 
                  key = "hr",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)




# Model comparison table
model_AIC <- AIC(F23_hr_null, F23_hr_fit1, F23_hr_fit2, F23_hr_fit3, F23_hr_fit4) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)

# No support for covariates. Best model is just the hazard rate detection function

# Plot detection probabililty by distance bin
plot(F23_hr_fit3, breaks= c(0,20,40,60,100), main="Fall '23")

# Goodness of fit
gof_ds(F23_hr_fit1) 


# Getting estimates of abundance
F23_abund <-  dht2(ddf = F23_hr_fit1$ddf,
                   flatfile = fall23_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)


print(F23_abund, report = "density")   # Density of ~ 0.0025 deer per hectare                  
print(F23_abund, report = "abundance") # Total abundance of 94.77 on property                       


# -------------------------------------------------------
#                     Winter 2024
# -------------------------------------------------------

# ----------------------
# Detection Function
# ----------------------

# Half normal detection function model
W24_hn_null <- ds(data = win24_dat,
                  formula = ~ 1,      
                  transect = "line",  
                  key = "hn",         
                  dht_group = FALSE, 
                  adjustment = NULL,  
                  convert_units = conversion.factor,
)

# Half normal detection function model
W24_hr_null <- ds(data = win24_dat,
                  formula = ~ 1,
                  transect = "line",  
                  key = "hr",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Model comparison table
model_AIC <- AIC(W24_hn_null, W24_hr_null) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)


# Can see Half-normal is the better model

# Goodness of fit
gof_ds(W24_hn_null) 

# ----------------------
# Covariates
# ----------------------

# Group size
W24_hn_fit1 <- ds(data = win24_dat, 
                  formula = ~ size, 
                  transect = "line",  
                  key = "hn", 
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Replicate (Evening or Morning) 
W24_hn_fit2 <- ds(data = win24_dat, 
                  formula = ~ factor(replicate) ,
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,  
)


# Woody Aggregation Index
W24_hn_fit3 <- ds(data = win24_dat, 
                  formula = ~ scale(woody_AggInx),
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)

# Woody Largest Patch Index
W24_hn_fit4 <- ds(data = win24_dat, 
                  formula = ~ scale(woody_lrgPInx),
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)




# Model comparison table
model_AIC <- AIC(W24_hn_null, W24_hn_fit1, W24_hn_fit2, W24_hn_fit3, W24_hn_fit4) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)

# Best model was Fit1: Group size

# Plot detection probability by distance bin
plot(W24_hn_fit1, breaks= c(0,20,40,60,100), main = "Winter '24")

# Goodness of fit
gof_ds(W24_hn_fit1) 


# Getting estimates of abundance
W24_abund <-  dht2(ddf = W24_hn_fit1$ddf,
                   flatfile = win24_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)


print(W24_abund, report = "density")   # Density of ~ 0.0012 deer per hectare                  
print(W24_abund, report = "abundance") # Total abundance of 67.58 on property                       



# -------------------------------------------------------
#                     Fall 2024
# -------------------------------------------------------

# ----------------------
# Detection Function
# ----------------------

# Half normal detection function model
F24_hn_null <- ds(data = fall24_dat,
                  formula = ~ 1,      
                  transect = "line",  
                  key = "hn",       
                  dht_group = FALSE,   
                  adjustment = NULL,  
                  convert_units = conversion.factor,
)

# Half normal detection function model
F24_hr_null <- ds(data = fall24_dat,
                  formula = ~ 1,
                  transect = "line",  
                  key = "hr",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Model comparison table
model_AIC <- AIC(F24_hn_null, F24_hr_null) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)


# Can see Half-Normal is the better model

# Goodness of fit
gof_ds(F24_hn_null)  

# ----------------------
# Covariates
# ----------------------

# Group size
F24_hn_fit1 <- ds(data = fall24_dat, 
                  formula = ~ size, 
                  transect = "line",  
                  key = "hn", 
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Replicate (Evening or Morning) 
F24_hn_fit2 <- ds(data = fall24_dat, 
                  formula = ~ factor(replicate) ,
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,  
)


# Woody Aggregation Index
F24_hn_fit3 <- ds(data = fall24_dat, 
                  formula = ~ scale(woody_AggInx),
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)

# Woody Largest Patch Index
F24_hn_fit4 <- ds(data = fall24_dat, 
                  formula = ~ scale(woody_lrgPInx),
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)




# Model comparison table
model_AIC <- AIC(F24_hn_null, F24_hn_fit1, F24_hn_fit2, F24_hn_fit3, F24_hn_fit4) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)

# Best model was Fit2: Replicate/Survey Time

# Plot detection probability by distance bin
plot(F24_hn_fit2, breaks= c(0, 20, 40, 60, 100), main = "Fall '24")

# Goodness of fit
gof_ds(F24_hn_fit2) # Chi-square shows a pretty good fit


# Getting estimates of abundance
F24_abund <-  dht2(ddf = F24_hn_fit2$ddf,
                   flatfile = fall24_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)


print(F24_abund, report = "density")   # Error - not giving good estimates                   
print(F24_abund, report = "abundance")                         



# -------------------------------------------------------
#                     Winter 2025
# -------------------------------------------------------

# ----------------------
# Detection Function
# ----------------------

# Half normal detection function model
W25_hn_null <- ds(data = win25_dat,
                  formula = ~ 1,      
                  transect = "line",  
                  key = "hn",       
                  dht_group = FALSE,   
                  adjustment = NULL,  
                  convert_units = conversion.factor,
)

# Half normal detection function model
W25_hr_null <- ds(data = win25_dat,
                  formula = ~ 1,
                  transect = "line",  
                  key = "hr",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Model comparison table
model_AIC <- AIC(W25_hn_null, W25_hr_null) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)


# Can see Half-Normal is the better model

# Goodness of fit
gof_ds(W25_hn_null)  

# ----------------------
# Covariates
# ----------------------

# Group size
W25_hn_fit1 <- ds(data = win25_dat, 
                  formula = ~ size, 
                  transect = "line",  
                  key = "hn", 
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor,
)


# Replicate (Evening or Morning) 
W25_hn_fit2 <- ds(data = win25_dat, 
                  formula = ~ factor(replicate) ,
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE,  
                  adjustment = NULL, 
                  convert_units = conversion.factor,  
)


# Woody Aggregation Index
W25_hn_fit3 <- ds(data = win25_dat, 
                  formula = ~ scale(woody_AggInx),
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)

# Woody Largest Patch Index
W25_hn_fit4 <- ds(data = win25_dat, 
                  formula = ~ scale(woody_lrgPInx),
                  transect = "line", 
                  key = "hn",  
                  dht_group = FALSE, 
                  adjustment = NULL, 
                  convert_units = conversion.factor, 
)




# Model comparison table
model_AIC <- AIC(W25_hn_null, W25_hn_fit1, W25_hn_fit2, W25_hn_fit3, W25_hn_fit4) # Extract AIC
model_AIC <- model_AIC[order(model_AIC$AIC), ] # Order 
print(model_AIC)

# Best model was Null: No covariates on detection

# Plot detection probability by distance bin
plot(W25_hn_null, breaks= c(0, 20, 40, 60, 100), main = "Winter '25")

# Goodness of fit
gof_ds(W25_hn_null) # Chi-square shows a pretty good fit


# Getting estimates of abundance
W25_abund <-  dht2(ddf = W25_hn_null$ddf,
                   flatfile = win25_dat,
                   stratification = "replicate",
                   strat_formula = ~ replicate,
                   convert_units = conversion.factor
)


print(W25_abund, report = "density")   # Error - not giving good estimates                
print(W25_abund, report = "abundance")                         



# ----------------------------- End of Script -----------------------------

