## ------------------------------------------------------------------------------
##
##                               Loading R packages
##
## ------------------------------------------------------------------------------

library(spAbundance)
library(sf)
library(dplyr)
library(coda)
library(stars)
library(ggplot2)
library(unmarked)


## ------------------------------------------------------------------------------
##
##                                 Reading in data
##
## ------------------------------------------------------------------------------


# Load Data
heli_dat <- readRDS("./Data/Survey_Data/Helicopter_Data/Heli_Dist_Data.rds")




# Convert Date column to Date format
heli_dat$Date <- as.Date(heli_dat$Date, format = "%Y-%m-%d")

# Add Survey_season column based on Date
heli_dat$Survey_season <- ifelse(heli_dat$Date %in% as.Date(c("2023-09-08", "2023-09-09")), "Fall",
                                 ifelse(heli_dat$Date %in% as.Date(c("2024-02-09", "2024-02-10")), "Winter", NA))


View(heli_dat)




### Subsetting data based on survey season

# Subset dataframe for fall
fall_dat <- subset(heli_dat, Survey_season == "Fall")

# Subset dataframe for winter
winter_dat <- subset(heli_dat, Survey_season == "Winter")



## ------------------------------------------------------------------------------
##
##                      Formatting data for spAbundance
##
## ------------------------------------------------------------------------------


# Taking a look at the distances bins
par(mfrow = c(1, 2))

# Fall
hist(fall_dat$Distance,  xlab="Distance (m)", ylab="Frequency", main = "Fall '23")

# Winter
hist(winter_dat$Distance,  xlab="Distance (m)", ylab="Frequency", main = "Winter '24")

## Looking at the histograms the detection distances will need binned.
## Bins of 0 - 20, 20 - 40, 40 - 60 and 60 - 100 should work for both datasets

# Create distance bins
heli_dat$Distance_Bins <- cut(heli_dat$Distance,
                                        breaks = c(0, 20, 40, 60, 100), 
                                        labels = c("bin1", "bin2", "bin3", "bin4"),
                                        include.lowest = TRUE)


# creating a matrix for detections in distance bins
# empty matrix for 12 transects and 5 distance bins
obs_mat <- matrix(0, nrow = NROW(unique(heli_dat$Unique_ID)), ncol = 4)
#rownames(obs_mat) <- heli_dat$Unique_ID
colnames(obs_mat) <- c("bin1", "bin2", "bin3", "bin4")


# Matching distance bin from heli_dat to distance bin in obs_mat
for (i in 1:NROW(heli_dat)){
    
    row <- heli_dat[i, ]
    
    if (row$Distance_Bins == "bin1"){
      obs_mat[i, 1] <- heli_dat[i, 9]
      obs_mat[i, 2] <- 0
      obs_mat[i, 3] <- 0
      obs_mat[i, 4] <- 0
    }
    if (row$Distance_Bins == "bin2"){
      obs_mat[i, 1] <- 0
      obs_mat[i, 2] <- heli_dat[i, 9]
      obs_mat[i, 3] <- 0
      obs_mat[i, 4] <- 0
    }
    if (row$Distance_Bins == "bin3"){
      obs_mat[i, 1] <- 0
      obs_mat[i, 2] <- 0
      obs_mat[i, 3] <- heli_dat[i, 9]
      obs_mat[i, 4] <- 0
    }
    if (row$Distance_Bins == "bin4"){
      obs_mat[i, 1] <- 0
      obs_mat[i, 2] <- 0
      obs_mat[i, 3] <- 0
      obs_mat[i, 4] <- heli_dat[i, 9]
    }
}


# Take a look
head(obs_mat)
head(heli_dat)


# Everything looks good can now make the covariate matrix
covs  <- heli_dat[, c(7,9:14)]
#rownames(covs) <- heli_dat$Unique_ID

# And we have to change the coordinates from a list to a df

# Extracting the coordinates from the list of points
coords <- do.call(rbind, heli_dat$geometry)
# Creating a dataframe with the coordinates
coords <- data.frame(coords)
# Renaming rows
#rownames(coords) <- heli_dat$Unique_ID
# Renaming columns
colnames(coords) <- c("X", "Y")


# Create the list
sp_dat <- list(
  y = obs_mat,
  covs = covs,
  dist.breaks = (c(0, 20, 40, 60, 100)) / 1000, # Dist breaks in kilometers
  offset = heli_dat[1,'Area_ha'],
  coords = coords
)


# Data is now formatted for spAbundance

#--------------------------------------
#
#         Poisson HDS Parameters
#
#--------------------------------------

# the DS function takes abundance and detection formula

# Null model
abund.formula <- ~ 1
det.formula <- ~ 1

# Starting values
inits.list <- list(beta = 0, alpha = 0, kappa = 1, N = apply(dat.EATO$y, 1, sum))

prior.list <- list(beta.normal = list(mean = 0, var = 100),
                   alpha.normal = list(mean = 0, var = 100),
                   kappa.unif = c(0, 100))

n.batch <- 2000
batch.length <- 25
# Total number of MCMC samples per chain
batch.length * n.batch


# accept.rate = 0.43 by default, so we do not specify it.
tuning <- list(beta = 0.5, alpha = 0.5, kappa = 0.5)

n.burn <- 20000
n.thin <- 30
n.chains <- 3

#--------------------------------------
#
#             Poisson HDS
#
#--------------------------------------

# Null fit
out <- DS(abund.formula = abund.formula,
          det.formula = det.formula,
          data = sp_dat,
          n.batch = n.batch,
          batch.length = batch.length,
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
          n.burn = n.burn,
          n.thin = n.thin,
          n.chains = n.chains)

summary(out)

# Trace plots of covariates
plot(out, param = 'beta', density = FALSE)

# Effect of distance on detectability
det.int.samples <- out$alpha.samples[, 1] 
det.quants <- quantile(exp(out$alpha.samples[, 1]), c(0.025, 0.5, 0.975))

x.vals <- seq(0, .125, length.out = 200)
n.vals <- length(x.vals)
p.plot.df <- data.frame(med = gxhn(x.vals, det.quants[2]), 
                        low = gxhn(x.vals, det.quants[1]), 
                        high = gxhn(x.vals, det.quants[3]),
                        x.val = x.vals * 1000)

ggplot(data = p.plot.df) + 
  geom_ribbon(aes(x = x.val, ymin = low, ymax = high), fill = 'grey', 
              alpha = 0.5) +
  theme_bw(base_size = 14) +
  geom_line(aes(x = x.val, y = med), col = 'black', linewidth = 1.3) + 
  labs(x = 'Distance (m)', y = 'Detection Probability')

# Goodness of fit
ppc.out <- ppcAbund(out, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out)

