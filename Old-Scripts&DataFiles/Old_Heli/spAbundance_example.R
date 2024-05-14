#--------------------------------------------------------------------------
# 
# This example is from 
# https://www.jeffdoser.com/files/spabundance-web/articles/distancesampling
# to fit hierarchical distance sampling models
# 
#--------------------------------------------------------------------------

# Installing and loading required packages
install.packages("spAbundance")
install.packages("coda")
install.packages("stars")
install.packages("ggplot2")
install.packages("unmarked")

library(spAbundance)
library(coda)
library(stars)
library(ggplot2)
library(unmarked)
set.seed(123)


#-------------------
#
# Loading data
#
#-------------------

# Load the data set
data(neonDWP)
?neonDWP
# Get an overview of what's in the data
head(neonDWP)
str(neonDWP)

# Subsetting to Eastern Towhee
sp.names <- dimnames(neonDWP$y)[[1]]
dat.EATO <- neonDWP
dat.EATO$y <- dat.EATO$y[sp.names == "EATO", , ]

# Number of EATO individuals observed at each site
apply(dat.EATO$y, 1, sum)
str(dat.EATO)

#--------------------------------------
#
#         Poisson HDS Parameters
#
#--------------------------------------


# the DS function takes abundance and detection formula
# the detection funciton already includes distance, wind is additional
abund.formula <- ~ scale(forest) + scale(grass)
det.formula <- ~ scale(wind)

# Offset to switch from km2 to ha
radius.km <- .25
pi * radius.km^2 * 100

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



out <- DS(abund.formula = abund.formula,
          det.formula = det.formula,
          data = dat.EATO,
          n.batch = n.batch,
          batch.length = batch.length,
          inits = inits.list,
          family = 'Poisson',
          det.func = 'halfnormal',
          transect = 'point',
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

# same as 



#--------------------------------------
#
#   Viewing Poisson HDS output
#
#--------------------------------------


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


# Model selection
waicAbund(out)

#--------------------------------------
#
#    negative binomial HDS model
#
#--------------------------------------

out.nb <- DS(abund.formula = ~ scale(forest) + scale(grass),
             det.formula = ~ scale(wind),
             data = dat.EATO,
             n.batch = n.batch,
             batch.length = batch.length,
             inits = inits.list,
             family = 'NB',
             det.func = 'halfnormal',
             transect = 'point',
             tuning = tuning,
             priors = prior.list,
             accept.rate = 0.43,
             n.omp.threads = 1,
             verbose = FALSE,
             n.report = 400,
             n.burn = n.burn,
             n.thin = n.thin,
             n.chains = n.chains)

waicAbund(out.nb)
waicAbund(out)
# Similar results between poisson and NB model 
# thus inclusion of additional over dispersion that NB allows
# did not improve the model 


#--------------------------------------
#
#           Prediction
#
#--------------------------------------


data(neonPredData)
str(neonPredData)


# Center and scale covariates by values used to fit model
forest.pred <- (neonPredData$forest - mean(dat.EATO$covs$forest)) / 
  sd(dat.EATO$covs$forest)
grass.pred <- (neonPredData$grass - mean(dat.EATO$covs$grass)) / 
  sd(dat.EATO$covs$grass)


# Design matric and predict abudance/density within each 1ha grid cell
X.0 <- cbind(1, forest.pred, grass.pred)
colnames(X.0) <- c('(Intercept)', 'forest', 'grass')
out.pred <- predict(out, X.0)
str(out.pred)


# Plotting

mu.0.quants <- apply(out.pred$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975))

plot.df <- data.frame(Easting = neonPredData$easting,
                      Northing = neonPredData$northing,
                      mu.0.med = mu.0.quants[2, ],
                      mu.0.ci.width = mu.0.quants[3, ] - mu.0.quants[1, ])

coords.stars <- st_as_stars(plot.df, crs = st_crs(32617))
coords.sf <- st_as_sf(as.data.frame(dat.EATO$coords), coords = c('easting', 'northing'), 
                      crs = st_crs(32617))


# Plot of median estimate
ggplot() +
  geom_stars(data = coords.stars, aes(x = Easting, y = Northing, fill = mu.0.med)) +
  geom_sf(data = coords.sf) +
  scale_fill_viridis_c(na.value = NA) +
  theme_bw(base_size = 12) +
  labs(fill = 'Individuals\nper ha', x = 'Longitude', y = 'Latitude', 
       title = 'Eastern Towhee Median Density')


