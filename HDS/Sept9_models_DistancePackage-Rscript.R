
library(Distance)


# Subsetting 9/8 data
wtd.binary.dat.sept9 <- wtd.binary.dat[which(wtd.binary.dat$date == "9/9/2023" ),]


# Distance
hist(wtd.binary.dat.sept9$distance,  xlab="Distance (m)", ylab="Frequency", main = "") # will probably have to bin distances

# group size
boxplot(wtd.binary.dat.sept9$distance ~ wtd.binary.dat.sept9$Group.size, xlab="Group Size", ylab="Distance (m)")

dev.off()



# half normal detection function model
wtd.hn.sept9 <- ds(data = wtd.binary.dat.sept9, 
             transect = "line", # point or line 
             key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
             dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
             adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
             convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
             )

# Goodness of fit           
gof_ds(wtd.hn.sept9, main = "Half-normal")

# Model summary
summary(wtd.hn.sept9)

# Hazard-rate model no adjustments
wtd.hr.sept9 <- ds(data = wtd.binary.dat.sept9, 
             transect = "line", # point or line 
             key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
             dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
             adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
             convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
             )

# Goodness of fit
gof_ds(wtd.hr.sept9, main = "Hazard-rate")

# Model summary
summary(wtd.hr.sept9)

# Model comparison tables
AIC(wtd.hn.sept9, wtd.hr.sept9)
knitr::kable(summarize_ds_models(wtd.hn.sept9, wtd.hr.sept9),
             caption = "9/9 Model comparison table for WTD Helicopter Surveys")



# ploting detection function
par(mfrow=c(2,2))
cutpoints <- c(0,5,10,15,20,30,40,50,65,80,100)
plot(wtd.hn.sept9, breaks=cutpoints, main="Half normal model, WTD Heli Surveys")
plot(wtd.hr.sept9, breaks=cutpoints, main="Hazard-rate model, WTD Heli Surveys")
gof_ds(wtd.hn.sept9, main = "Half-normal")
gof_ds(wtd.hr.sept9, main = "Hazard-rate")


# ------------------------------------------------------------------------------------------
#
# The hazard rate model has a better AIC and gof, will use that key for modeling covariates
# Test to see if truncating the data leads to more inference
#
# ------------------------------------------------------------------------------------------
dev.off()

# Hazard-rate model no adjustments
wtd.hr.null.sept9 <- ds(data = wtd.binary.dat.sept9, 
                  transect = "line", # point or line 
                  key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                  dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                  adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                  convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
                  )

# Goodness of fit
gof_ds(wtd.hr.null.sept9, main = "Hazard-rate Null")

# Model summary
summary(wtd.hr.null.sept9)

# Hazard-rate model simple polynomial adjustment
wtd.hr.truncated.sept9 <- ds(data = wtd.binary.dat.sept9, 
                             transect = "line", # point or line 
                             key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                             dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                             adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                             convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                             truncation = "5%" # truncating 5% of observations 
                             )

# Goodness of fit
gof_ds(wtd.hr.truncated.sept9, main = "Hazard-rate Truncated")

# Model summary
summary(wtd.hr.truncated.sept9)

# Model comparison table
AIC(wtd.hr.null.sept9, wtd.hr.truncated.sept9)


# ------------------------------------------------------------------------------------------
#
# Truncating increased AIC, further modeling will include 5% truncating
#
# ------------------------------------------------------------------------------------------

# Hazard-rate model Truncated 5%, No Adjustment
wtd.hr.null.sept9 <- ds(data = wtd.binary.dat.sept9, 
                  transect = "line", # point or line 
                  key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                  dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                  adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                  convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                  truncation = "5%" # truncating 5% of observations 
                  )

# Goodness of fit
gof_ds(wtd.hr.null.sept9, main = "Hazard-rate Null")

# Model summary
summary(wtd.hr.null.sept9)

# Hazard-rate model Truncated 5%, Adjustment = simple polynomial
wtd.hr.poly.sept9 <- ds(data = wtd.binary.dat.sept9, 
                  transect = "line", # point or line 
                  key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                  dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                  adjustment = "poly", # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                  convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                  truncation = "5%" # truncating 5% of observations 
                  )

# Goodness of fit
gof_ds(wtd.hr.poly.sept9, main = "Hazard-rate Simply Poly adj")

# Model summary
summary(wtd.hr.poly.sept9)

# AIC 
AIC(wtd.hr.null.sept9, wtd.hr.poly.sept9)

# ploting detection function
par(mfrow=c(2,2))
cutpoints <- c(0,5,10,15,20,30,40,50,65,80,100)
plot(wtd.hr.sept9, breaks=cutpoints, main="Hazard-rate model, WTD Heli Surveys")
plot(wtd.hr.poly.sept9, breaks=cutpoints, main="Hazard-rate model simple poly adj, WTD Heli Surveys")
gof_ds(wtd.hr.sept9, main = "Hazard-rate")
gof_ds(wtd.hr.poly.sept9, main = "Hazard-rate Simply Poly adj")





# ------------------------------------------------------------------------------------------
#
# AIC values are the same with and without an adjustment - wont make any further adjustments
# Model for covariates
#
# ------------------------------------------------------------------------------------------
dev.off()


# Hazard-rate model Truncated 5%, ~ Group.size
wtd.hr.gs.sept9 <- ds(data = wtd.binary.dat.sept9, 
                transect = "line", # point or line 
                key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                formula = ~ log(Group.size), # fails to converge with out taking the log of group.size 
                truncation = "5%" # truncating 5% of observations 
                )

# Goodness of fit
gof_ds(wtd.hr.gs.sept9, main = "Hazard-rate ~ Group Size")

# Model summary
summary(wtd.hr.gs.sept9)



# Model Comparisons
AIC(wtd.hr.null.sept9, wtd.hr.gs.sept9)
knitr::kable(summarize_ds_models(wtd.hr.null.sept9, wtd.hr.gs.sept9),
             caption = "9/9 Model comparison table for WTD Helicopter Surveys")



# ploting detection function
par(mfrow=c(2,2))
cutpoints <- c(0,5,10,15,20,30,40,50,60)
plot(wtd.hr.null.sept9, breaks=cutpoints, main="Hazard-rate model - Null")
plot(wtd.hr.gs.sept9, breaks=cutpoints, main="Hazard-rate model ~ Group Size")
# ploting goodness of fit
gof_ds(wtd.hr.poly.sept9, main = "Hazard-rate Null")
gof_ds(wtd.hr.gs.sept9, main = "Hazard-rate ~ Group Size")
















