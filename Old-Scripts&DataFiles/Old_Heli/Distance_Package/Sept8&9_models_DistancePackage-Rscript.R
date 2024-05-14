



load("C:/Users/davep/OneDrive - Texas A&M University/Rprojects/CH1_WTD_Abundance/Data/HelicopterSurvey_data/Cleaned_Survey_Data/HSwinter24_data.rds")



library(Distance)

dev.off()

# Distance
hist(wtd.binary.dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "") # will probably have to bin distances

# Bin 0 - 10, 10 - 20, 20 - 30, 30 - 50, 50 - 100
wtd.binary.dat.binned<-create_bins(wtd.binary.dat, c(0,10,20,30,50,100) )


# half normal detection function model
wtd.hn.all <- ds(data = wtd.binary.dat.binned, 
                 transect = "line", # point or line 
                 key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                 dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                 adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                 convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                 )


# Model summary
summary(wtd.hn.all)

# Hazard-rate model no adjustments
wtd.hr.all <- ds(data = wtd.binary.dat.binned, 
                 transect = "line", # point or line 
                 key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                 dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                 adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                 convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
                 )

# Model summary
summary(wtd.hr.all)

# Model comparison tables
AIC(wtd.hn.all, wtd.hr.all)
knitr::kable(summarize_ds_models(wtd.hn.all, wtd.hr.all),
             caption = "All Data Model comparison table for WTD Helicopter Surveys")


# ------------------------------------------------------------------------------------------
#
# The hazard rate model has a better AIC and gof, will use that key for modeling covariates
#
# ------------------------------------------------------------------------------------------


# Hazard-rate model no adjustments
wtd.hr.null.all <- ds(data = wtd.binary.dat.binned, 
                  transect = "line", # point or line 
                  key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                  dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                  adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                  convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
                  )

# Model summary
summary(wtd.hr.null.all)


# Hazard-rate model simple polynomial adjustment
wtd.hr.poly.all<- ds(data = wtd.binary.dat.binned, 
                     transect = "line", # point or line 
                     key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = "poly", # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = c("meter", "kilometer", "acre") # conversion for units for abundance estimation. distance, effort and study area
                     )


# Model summary
summary(wtd.hr.poly.all)

# Model comparison tables
AIC(wtd.hr.null.all, wtd.hr.poly.all)


# ------------------------------------------------------------------------------------------
#
# No difference using a key adjustment - further models will not use key adjustment
#
# ------------------------------------------------------------------------------------------



# Hazard-rate model  ~ Group.size
wtd.hr.gs.all <- ds(data = wtd.binary.dat.sept8, 
                    transect = "line", # point or line 
                    key = "hr", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                    dht_group=FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                    adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                    convert_units = c("meter", "kilometer", "acre"), # conversion for units for abundance estimation. distance, effort and study area
                    formula = ~ log(Group.size) # fails to converge with out taking the log of group.size 
                    )


# Model summary
summary(wtd.hr.gs.all)

# Model comparison tables
AIC(wtd.hr.null.all, wtd.hr.gs.all)


# ploting detection function
par(mfrow=c(1,2))
plot(wtd.hr.null.all, breaks=c(0,10,20,30,50,100), main="Hazard-rate model - Null")
plot(wtd.hr.gs.all, breaks=c(0,10,20,30,50,100), main="Hazard-rate model ~ Group Size")





