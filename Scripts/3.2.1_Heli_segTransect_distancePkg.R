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

# # Load packages
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


