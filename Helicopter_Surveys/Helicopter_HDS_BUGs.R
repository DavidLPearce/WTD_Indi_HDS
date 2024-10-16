# ------------------------------------------------------------------------------
#
#                               Load Libraries
#
# ------------------------------------------------------------------------------

# If needed
# install.packages("nimble")
# install.packages("coda")
# install.packages("ggfortify")
# install.packages("ggplot2")
# install.packages("gridExtra")


library(nimble)
library(coda)
library(ggfortify)
library(ggplot2)
library(gridExtra)

# Disable scientific notation
options(scipen = 9999)

# set seed
set.seed(123)

# ------------------------------------------------------------------------------
#
#                           Bundle data for BUGs
#
# ------------------------------------------------------------------------------

# Reading in data
data <- readRDS("./Data/Survey_Data/Helicopter_Data/heli_dat_clean.rds")

# subsetting fall 2023 survey data
fall23 <- data[which(data$Date == "9/8/2023" | data$Date == "9/9/2023" ),]
# |
#                      data$Date == "2/9/2024" | data$Date == "2/10/2024"
# Distance
#hist(fall23$Perpendicular_Distance,  xlab="Distance (m)", ylab="Frequency", main = "") 

# Strip half width
W <- 0.1 # in km 

# number of sites (transects)
nsites <- 12

# Transect length (km), ordered 1 through 12
site_length <- c(2.94, 4.99, 4.99, 4.99, 4.68, 4.74,
                 4.79, 4.84, 1.25, 1.25, 1.25, 1.25)

# area surveyed
offset <- 2 * ((site_length) * W) # effort

# Groupsize
# Subtract 1 due so that the intercept is the value of sigma for a real group size of 1
groupsize <- fall23[,"Group_size"] -1 

# vegetation density
vegdensity <- fall23[,"VegDensity"]

# number of surveys
nsurvey <- 2

# survey
survey <- as.factor(fall23[,"Date"])

# M is the total number of possible groups
# can be thought of as the limit of groups that might be there
# needs to be significantly larger than the number of groups detected

# number of groups detected
nrow(fall23)


# Size of augmented data set is M
M <- 200


# Number of "pseudo-groups" added
nz <- M - nrow(fall23)

# Indicator of capture (== 1 for all obs. groups)
fall23[,"observed"] <- 1 # group observed 

y <- c(fall23[,"observed"], rep(0,nz))


# Number of observed groups
nind <- nrow(fall23)

# Site they belong to is unknown
site <- c(fall23[,"Transect_ID"], rep(NA,nz))

# Their distance data is missing
fall23$Perpendicular_Distance_km <- round(fall23$Perpendicular_Distance) / 1000

d <- c(fall23[,"Perpendicular_Distance_km"], rep(NA, nz))

# groupsize is unknown
groupsize <- c(groupsize, rep(NA,nz))

# density of vegetation they are in is unknown
vegdensity <- c(vegdensity, rep(NA,nz))

# survey is unknown
survey <- c(survey, rep(NA,nz))

# Starting values for data augmentation variable
zst <- y


## Bundle data and produce summary
str(nimb_helidat <- list (y = y, 
                       W = W, 
                       nind = nind, 
                       nsites = nsites,
                       #nsurvey = nsurvey,
                       offset = offset,
                       d = d, 
                       site = site, 
                       nz = nz, 
                       groupsize = groupsize,
                       vegdensity = vegdensity# ,
                       #survey = survey
                       )) # end list

# ------------------------------------------------------------------------------
#
#                           Define Model
#
# ------------------------------------------------------------------------------

## Distributions
# ggdistribution(dunif, seq(-10, 10, 0.001), min = -10, max = 10) 
# ggdistribution(dgamma, seq(0, 10, 0.001), shape = 8, rate = 2) 
# ggdistribution(dgamma, seq(0, 10, 0.001), shape = 6, rate = 2) 


heli_mod <- nimbleCode(
  { 
    ## Prior distributions for model parameters
    alpha0 ~ dnorm(0, 0.01)  # Centered at 0 with low variance
    alpha1 ~ dnorm(0, 0.01)  # Low influence of group size initially
    alpha2 ~ dnorm(0, 0.01)  # Low influence of veg density initially
    
    beta0 ~ dnorm(0, 0.01)   # Centered at 0 with low variance
    
    ## Hyperparameters 
    lambda.group ~ dgamma(0.1, 0.1)
    vegdensity_a ~ dgamma(0.1, 0.1)
    vegdensity_b ~ dgamma(0.1, 0.1)
    
    # Random effect variance for surveys
    # tau_epsilon ~ dgamma(0.1, 0.1)       # Precision (1/variance) for survey-level random effects
    # for(s in 1:nsurvey) {
    #   epsilon[s] ~ dnorm(0, tau_epsilon)       # Random effect for each survey
    # }
    
    ## psi is a derived parameter
    psi <- sum(lambda[1:nsites])/(nind+nz)
    
    ## Individual level model: observations and process
    for(i in 1:(nind+nz)){
      z[i] ~ dbern(psi)                   # Data augmentation variable
      d[i] ~ dunif(0, W)                  # Distance is uniformly distributed
      groupsize[i] ~ dpois(lambda.group)  # Group size is Poisson
      vegdensity[i] ~ dbeta(vegdensity_a, vegdensity_b) # Vegetation density comes from a beta distribution
      
      # modeling covariates on sigma
      log(sigma[i]) <- alpha0 + alpha1 * groupsize[i]  + alpha2 * vegdensity[i] #+ epsilon[survey[i]]
      
      ## half normal detection function
      p[i] <- z[i] * exp(-d[i] * d[i] / (2 * sigma[i] * sigma[i]))  
      
      ## hazard rate detection function
      #p[i] <- z[i] * (1 - exp(-(d[i]/sigma[i])^-W))
      
      y[i] ~ dbern(p[i])
      
      
      site[i] ~ dcat(site.probs[1:nsites]) # Population distribution among sites
      zg[i] <- z[i] * (groupsize[i] + 1)      # Number of individuals in that group
    }
    
    for(s in 1:nsites){
      ## Model for population size of groups
      N[s] ~ dpois(lambda[s] * offset[s]) 
      log(lambda[s]) <- beta0 
      site.probs[s] <- lambda[s] / sum(lambda[1:nsites])
    }
    
    # Derived quantities
    G <- sum(z[1:(nind+nz)])        # Total number of groups
    Ntotal <- sum(zg[1:(nind+nz)])  # Total population size (all groups combined)
  }
) # End Model


# ------------------------------------------------------------------------------
#
#                           Run Model
#
# ------------------------------------------------------------------------------

inits <- list(alpha0 = 0,  
               alpha1 = 0,
               alpha2 = 0,
               beta0 = 0, 
               z = zst,
               lambda.group = 1,
               vegdensity_a = 1,
               vegdensity_b = 1
              )

# Parameters to monitor
params <- c("G",
            "Ntotal",
            "alpha0",
            "alpha1", 
            "alpha2",
            "beta0", 
            "psi", 
            "lambda.group",
            "vegdensity_a",
            "vegdensity_b"
             #"epsilon"
            ) 

# Call NIMBLE, plot posterior distributions
mod_out <- nimbleMCMC(code = heli_mod, 
                   constants = nimb_helidat,
                   inits = inits,
                   monitors = params,
                   niter = 100000,
                   nburnin = 1000,
                   nchains = 3,
                   thin = 5,
                   progressBar = getNimbleOption("MCMCprogressBar"),
                   samplesAsCodaMCMC = TRUE)


# Traceplots
par(mfrow=c(3,4))
coda::traceplot(mod_out)

# Rhat
coda::gelman.diag(mod_out)

# Model Summary
summary(mod_out)

# extract model output
samples_matrix <- as.matrix(mod_out)

# Extract parameter mean estimates
summary_stats <- summary(mod_out)
mean_values <- summary_stats$statistics[, "Mean"]

# List to store plots
plot_list <- list()

# Loop through each parameter and create plots
for (param_name in params) {  # Loop through the defined parameter names directly
  
  if (param_name %in% colnames(samples_matrix)) {
    
    param_data <- data.frame(Value = samples_matrix[, param_name])
    
    p <- ggplot(param_data, aes(x = Value)) +
      geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "lightblue", color = "white") +
      geom_density(color = "darkblue", linewidth = 1) +
            geom_vline(xintercept = mean_values[[param_name]], color = "blue", linetype = "solid", linewidth = 1) +
      theme_minimal() +
      labs(title = paste("Posterior Distribution of", param_name, "\n",
                         "Mean Est (blue) =", round(mean_values[[param_name]], 2)),
           x = "Value", y = "Density")
    
    # Store plot in list
    plot_list[[param_name]] <- p
  }
}

# Arrange plots in a grid
do.call("grid.arrange", c(plot_list, ncol = 2))
