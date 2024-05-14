#--------------------------------------
#
#               Packages
#
#--------------------------------------

# Installing and loading required packages
library(spAbundance)
library(sf)
library(dplyr)
library(coda)
library(stars)
library(ggplot2)
library(unmarked)
set.seed(123)

#--------------------------------------
#
#            Loading data
#
#--------------------------------------

# Read in count data
Heli_winter24_data <- readRDS("./Data/HelicopterSurvey_data/Cleaned_Survey_Data/HSwinter24_data.rds")

# Read in Transects
transects <- st_read("./Data/HelicopterSurvey_data/Helicopter_Transects_Updated/Helicopter_Transects_Updated.shp")

# Frequency of observations by distance
hist(Heli_winter24_data$Distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '24") 

#--------------------------------------
#
#   Formatting data for spAbundance
#
#--------------------------------------


# dist bins: 0-10, <10-30, <30-50, <50-70, <70-100

# Create distance bins
Heli_winter24_data$Distance_Bins <- cut(Heli_winter24_data$Distance,
                                         breaks = c(0, 10, 30, 50, 70, 100), 
                                         labels = c("bin1", "bin2", "bin3", "bin4", "bin5"),
                                         include.lowest = TRUE)

# Setting bins as char for looping
Heli_winter24_data$Distance_Bins <- as.character(Heli_winter24_data$Distance_Bins)

# empty matrix for 12 transects and 5 distance bins
obs_matrix <- matrix(0, nrow = 12, ncol = 5)
rownames(obs_matrix) <- 1:12
colnames(obs_matrix) <- c("bin1", "bin2", "bin3", "bin4", "bin5")

# Fill in the matrix with group sizes
for (transect in unique(Heli_winter24_data$Transect_ID)) {
      # subsetting transect data
      transect_data <- subset(Heli_winter24_data, Transect_ID == transect)

      for (bin in unique(transect_data$Distance_Bins)){
        
          # Subsetting binned distance data
          bin_data <- subset(transect_data, Distance_Bins == bin)
          
          # adding summed detection by bin data to matrix
          obs_matrix[transect, bin] <- sum(bin_data$Group_size)
  }
}


#-----------------------------
# Making a spAbundance object
#-----------------------------

# needs matrix to be 
y <- as.matrix(obs_matrix)

# covariates
# Converting transect length from meters to kilometers
covs <- as.matrix(transects$T_Length) 
colnames(covs) <- c("Transect_Length")/ 1000

# distance bins in kilometers
dist.breaks <- (c(0, 10, 30, 50, 70, 100)) / 1000

# offset
# area of a rectangle = L x W
# Length = Transect length x 0.2km 
offset <-  (as.matrix(transects$T_Length)/1000) / 0.2

# Coordinates
coords <- transects$geometry

# Combine all objects into a dataframe
winter_data <- list(
  y = y,
  covs = covs,
  dist.breaks = dist.breaks,
  offset = offset,
  coords = coords
)


#--------------------------------------
#
#             Poisson HDS 
#
#--------------------------------------

# Starting values
inits.list <- list(beta = 0, alpha = 0, kappa = 1, N = apply(winter_data$y, 1, sum))

prior.list <- list(beta.normal = list(mean = 0, var = 100),
                   alpha.normal = list(mean = 0, var = 100),
                   kappa.unif = c(0, 100))

# accept.rate = 0.43 by default, so we do not specify it.
tuning <- list(beta = 0.5, alpha = 0.5, kappa = 0.5)



# Model
out <- DS(abund.formula = ~ 1,
          det.formula = ~ scale(Transect_Length),
          data = winter_data ,
          n.batch = 2000,
          batch.length = 25,
          inits = inits.list,
          family = 'Poisson',
          det.func = 'halfnormal',
          transect = 'line',
          tuning = tuning,
          priors = prior.list,
          accept.rate = 0.43,
          n.omp.threads = 1,
          verbose = TRUE,
          n.report = 500,
          n.burn = 20000,
          n.thin = 30,
          n.chains = 3)

summary(out)












