?ds
# packages
library(Distance)
View(heli_dat)

# Load Data
heli_dat <- readRDS("./Data/Survey_Data/Helicopter_Data/Heli_Dist_Data.rds")
#View(heli_dat)
# The distance package requires specific naming of columns
names(heli_dat)[1] <- "Study.Area"        # Study Area
names(heli_dat)[2] <- "Area"              # Area
#heli_dat$Area <- 10.96698 * 1000          # Area in m2
names(heli_dat)[3] <- "object"            # Unique ID 
names(heli_dat)[4] <- "Sample.Label"      # Transect ID 
names(heli_dat)[5] <- "Effort"            # Transect Length
#heli_dat$Effort <- heli_dat$Effort * 1000 # effort in meters
names(heli_dat)[8] <- "distance"          # Distance
names(heli_dat)[9] <- "size"              # Size
names(heli_dat)[17] <- "replicate"        # Replicate
heli_dat$Region.Label <- "Region1"


# Frequency of distance - all
hist(heli_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "") 


# Subsetting by survey season

heli_fall23_dat <- heli_dat[which(heli_dat$Date == "9/8/2023" | heli_dat$Date == "9/9/2023"),]

heli_win24_dat <- heli_dat[which(heli_dat$Date ==  "2/9/2024" | heli_dat$Date == "2/10/2024"),]

# Frequency of distance - fall 23
hist(heli_fall23_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "") 

# Frequency of distance - winter 24
hist(heli_win24_dat$distance,  xlab="Distance (m)", ylab="Frequency", main = "") 

# Distance bins of 0-20, 20-40, 40-60, 60-100 for both fall '23 and winter '24
heli_fall23_dat<-create_bins(heli_fall23_dat, c(0,20,40,60,100))
heli_win24_dat<-create_bins(heli_win24_dat, c(0,20,40,60,100))

# object for converting units
conversion.factor <- convert_units("meter", "kilometer", "hectare")



# ------------------------------------------------------------------------------
#
#                                    Fall '23
#
# ------------------------------------------------------------------------------ 

# half normal detection function model
fall23_hn_null <- ds(data = heli_fall23_dat, 
                       formula = ~ 1,
                       transect = "line", # point or line 
                       key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                       dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                       adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                       convert_units = conversion.factor,
)


# Goodness of fit
gof_ds(fall23_hn, main = "Hazard-rate Null")

# Model summary
summary(fall23_hn)

# ploting detection function
plot(fall23_hn, breaks= c(0,20,40,60,100), main="Half normal - Fall '23")


# Group size
fall23_hn_fit1 <- ds(data = heli_fall23_dat, 
                     formula = ~ log(size),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor,
                     )


# Survey_Time 
fall23_hn_fit2 <- ds(data = heli_fall23_dat, 
                     formula = ~ factor(Survey_Time) ,
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)


# Group size + Survey_Time
fall23_hn_fit3 <- ds(data = heli_fall23_dat, 
                     formula = ~ log(size) + factor(Survey_Time),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)



# number of crowns
fall23_hn_fit4 <- ds(data = heli_fall23_dat, 
                     formula = ~ log(num_Crowns),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)

# number of crowns + group size
fall23_hn_fit5 <- ds(data = heli_fall23_dat, 
                     formula = ~ log(num_Crowns) + log(size),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)


# number of crowns + survey time
fall23_hn_fit6 <- ds(data = heli_fall23_dat, 
                     formula = ~ log(num_Crowns) + factor(Survey_Time),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)

# number of crowns + survey time + size
fall23_hn_fit7 <- ds(data = heli_fall23_dat, 
                     formula = ~ log(num_Crowns) + factor(Survey_Time) + log(size),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)






# Model comparison table
AIC(fall23_hn_null, fall23_hn_fit1, fall23_hn_fit2, fall23_hn_fit3,fall23_hn_fit4, 
    fall23_hn_fit5,fall23_hn_fit6,fall23_hn_fit7)

knitr::kable(summarize_ds_models(fall23_hn_null, fall23_hn_fit1, fall23_hn_fit2, 
                                 fall23_hn_fit3,fall23_hn_fit4, fall23_hn_fit5,
                                 fall23_hn_fit6, fall23_hn_fit7),
                                  digits=3,
                                  caption="Model comparison table")

# Fit 3 is best
# ploting detection function
plot(fall23_hn_fit1, breaks= c(0,20,40,60,100), main="Half normal + Group Size - Fall '23")


# Getting estimates of abundance
fall23_abund <-  dht2(
                      ddf = fall23_hn_fit1$ddf,
                      flatfile = heli_fall23_dat,
                      stratification = "replicate",
                      strat_formula = ~ replicate,
                      convert_units = conversion.factor
                      
                      )
                       
                        
print(fall23_abund, report="density")                     
print(fall23_abund, report="abundance")                       
                        
                        
                        
                        
heli_dat_new <- heli_dat                     



heli_dat_new$replicate <- ifelse(heli_dat_new$Date == "9/8/2023", 1,
                                   ifelse(heli_dat_new$Date == "9/9/2023", 2, 
                                          ifelse(heli_dat_new$Date == "2/9/2024" , 3, 
                                                 ifelse(heli_dat_new$Date == "2/10/2024" , 4,NA))))


# half normal detection function model
hn_null <- ds(data = heli_dat_new, 
                     formula = ~ 1,
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor,
)


# Group size
hn_fit1 <- ds(data = heli_dat_new, 
                     formula = ~ log(size),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor,
)


# number of crowns
hn_fit2 <- ds(data = heli_dat_new, 
                     formula = ~ log(num_Crowns),
                     transect = "line", # point or line 
                     key = "hn", # detection model: Half-normal (hn), hazard-rate (hr), uniform (unif)
                     dht_group = FALSE, # consider group sizes to be size 1 (TRUE), default is FALSE, abundance of individuals is calculated
                     adjustment = NULL, # adjustment to use cosine (cos) default, hermite polynomial (herm), simple polynomial (poly)
                     convert_units = conversion.factor, # conversion for units for abundance estimation. distance, effort and study area
)

# Getting estimates of abundance
abund <-  dht2(
  ddf = hn_fit1$ddf,
  flatfile = heli_dat_new,
  stratification = "replicate",
  strat_formula = ~ replicate,
  convert_units = conversion.factor
  
)

print(abund, report="density")                     
print(abund, report="abundance")                       
