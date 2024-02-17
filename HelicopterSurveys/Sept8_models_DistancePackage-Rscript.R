
library(Distance)


# Subsetting 9/8 data
wtd.binary.dat.sept8 <- wtd.binary.dat[which(wtd.binary.dat$date == "9/8/2023" ),]


# Distance
hist(wtd.binary.dat.sept8$distance,  xlab="Distance (m)", ylab="Frequency", main = "") # will probably have to bin distances

# group size
boxplot(wtd.binary.dat.sept8$distance ~ wtd.binary.dat.sept8$Group.size, xlab="Group Size", ylab="Distance (m)")

# Binning distances 
# For Sept 8 we will bin 0 - 20, 20 - 40, 40 - 100
wtd.binary.dat.sept8<-create_bins(wtd.binary.dat.sept8, c(0,10,20,40,100) )




# half normal detection function model
wtd.hn <- ds(data = wtd.binary.dat.sept8, 
             transect = "line", # point or line 
             key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
             dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
             adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
             convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
              )

# Goodness of fit           
gof_ds(wtd.hn, main = "Half-normal")

# Model summary
summary(wtd.hn)

# Hazard-rate model no adjustments
wtd.hr <- ds(data = wtd.binary.dat.sept8, 
             transect = "line", # point or line 
             key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
             dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
             adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
             convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
              )

# Goodness of fit
gof_ds(wtd.hr, main = "Hazard-rate")

# Model summary
summary(wtd.hr)

# Model comparison tables
AIC(wtd.hn, wtd.hr)
knitr::kable(summarize_ds_models(wtd.hn, wtd.hr),
             caption = "9/8 Model comparison table for WTD Helicopter Surveys")



# ploting detection function
par(mfrow=c(2,2))
cutpoints <- c(0,20,40,100)
plot(wtd.hn, breaks=cutpoints, main="Half normal model, WTD Heli Surveys")
plot(wtd.hr, breaks=cutpoints, main="Hazard-rate model, WTD Heli Surveys")
gof_ds(wtd.hn, main = "Half-normal")
gof_ds(wtd.hr, main = "Hazard-rate")


# ------------------------------------------------------------------------------------------
#
# The hazard rate model has a better AIC and gof, will use that key for modeling covariates
# Test to see if truncating the data leads to more inference
#
# ------------------------------------------------------------------------------------------
dev.off()

# Hazard-rate model no adjustments
wtd.hr.null.sept8 <- ds(data = wtd.binary.dat.sept8, 
                  transect = "line", # point or line 
                  key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                  dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                  adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                  convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
                  )

# Goodness of fit
gof_ds(wtd.hr.null.sept8, main = "Hazard-rate Null")

# Model summary
summary(wtd.hr.null.sept8)


# ploting detection function
par(mfrow=c(2,1))
cutpoints <- c(0,10,20,40,100)
plot(wtd.hr.null.sept8, breaks=cutpoints, main="Hazard-rate model - Null")
gof_ds(wtd.hr.null.sept8, main = "Hazard-rate Null")

of_ds(wtd.hr.null.sept8, main = "Hazard-rate Null")




# Hazard-rate model simple polynomial adjustment
wtd.hr.poly<- ds(data = wtd.binary.dat.sept8, 
                       transect = "line", # point or line 
                       key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                       dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                       adjustment = "poly", # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                       convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
                       )
# Goodness of fit
gof_ds(wtd.hr.poly, main = "Hazard-rate Truncated")

# Model summary
summary(wtd.hr.poly)

# Model comparison table
AIC(wtd.hr.null, wtd.hr.poly)


# ------------------------------------------------------------------------------------------
#
# AIC values are the same with and without an adjustment - wont make any further adjustments
# Model for covariates
#
# ------------------------------------------------------------------------------------------
dev.off()


# Hazard-rate model Truncated 5%, ~ Group.size
wtd.hr.gs <- ds(data = wtd.binary.dat.sept8, 
                transect = "line", # point or line 
                key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                formula = ~ log(Group.size) # fails to converge with out taking the log of group.size 
                )

# Goodness of fit
gof_ds(wtd.hr.gs, main = "Hazard-rate ~ Group Size")

# Model summary
summary(wtd.hr.gs)



# Model Comparisons
AIC(wtd.hr.null, wtd.hr.gs)
knitr::kable(summarize_ds_models(wtd.hr.null, wtd.hr.gs),
             caption = "9/8 Model comparison table for WTD Helicopter Surveys")



# ploting detection function
par(mfrow=c(2,1))
cutpoints <- c(0,10,20,40,100)
plot(wtd.hr.null, breaks=cutpoints, main="Hazard-rate model - Null")
gof_ds(wtd.hr.null, main = "Hazard-rate Null")




plot(wtd.hr.gs, breaks=cutpoints, main="Hazard-rate model ~ Group Size")
                        
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          