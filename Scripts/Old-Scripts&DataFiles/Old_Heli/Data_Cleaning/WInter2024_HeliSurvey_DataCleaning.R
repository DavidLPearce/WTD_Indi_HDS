
library(sf)
library(dplyr)

# Read in Transects
transects <- st_read("./Data/HelicopterSurvey_data/Helicopter_Transects_Updated/Helicopter_Transects_Updated.shp")

# Read the CSV file
HSwinter24_dat <- read.csv("./Data/HelicopterSurvey_data/Raw_Survey_Data/WInter2024_HelicopterSurveys.csv")


# Subsetting count, date, lat and long
HSwinter24_dat <- HSwinter24_dat[,c(25, 30:35, 39:41, 43,44)]

# Convert the column to an sf object
HSwinter24_dat <- st_as_sf(HSwinter24_dat, coords = c("x", "y"), crs = st_crs(transects))

# Transform to a projected CRS
HSwinter24_dat <- st_transform(HSwinter24_dat, crs = st_crs(transects))

# Snap points to the nearest points on lines
HSwinter24_dat$geometry_snapped <- st_snap(HSwinter24_dat$geometry, transects, tolerance = 0)

# Extract coordinates
coordinates <- st_coordinates(HSwinter24_dat$geometry_snapped )

# Removing sf coords from data
HSwinter24_dat <- HSwinter24_dat[,-12]

# Create separate columns for latitude and longitude
HSwinter24_dat$Long_snapped <- coordinates[, 1]
HSwinter24_dat$Lat_snapped <- coordinates[, 2]

# Converting date column to just date and no time
HSwinter24_dat$CreationDate <- as.Date(HSwinter24_dat$CreationDate, format = "%m/%d/%Y")

# Exporting to enter transect data using ArcGIS Pro
write.csv(HSwinter24_dat, "Helisurveys_Winter2023_snapped.csv")

# ---------------------------------------------------------------------------------
#
#                             After adding transect data
#
# ---------------------------------------------------------------------------------


# Reading in survey data with transect info
HSwinter24_dat <- read.csv("./Data/HelicopterSurvey_data/Raw_Survey_Data/Winter2024_HelicopterSurveys_snapped_GIS.csv")


# Calculating group sizes
HSwinter24_dat$Group_size <- rowSums(HSwinter24_dat[,c(4:9,12)])

# Summing all males rowwise
HSwinter24_dat$Males <- rowSums(HSwinter24_dat[,c(5:7,12)])


# Renaming Columns
colnames(HSwinter24_dat) <- c("Study_Area","Area", "Date", 
                              "Female", "Mature", "Middle", "Young", "Fawn", "Unknown", 
                              "Direction", "Distance", "Male_Unknown", "Long", "Lat",
                              "Transect_ID", "Flight_Path", "Transect_Length", "Observation_Number", 
                              "Group_size", "Males")

# Reorganizing columns
HSwinter24_dat <- HSwinter24_dat[,c("Observation_Number","Study_Area","Area","Date", "Lat", "Long", 
                                    "Transect_ID", "Transect_Length", "Flight_Path", 
                                    "Female", "Fawn", "Males", "Mature", "Middle", "Young",
                                    "Unknown", "Male_Unknown","Group_size", "Direction", "Distance")]
                                   
                                   
# Converting date column to just date and no time
HSwinter24_dat$Date <- as.Date(HSwinter24_dat$Date, format = "%m/%d/%Y")                                        
                                   

# Adding a column for angle using our clock angle call out using recode from the dplyr package
# in a coordinate plane 0 to 180 degrees are positive. To keep our distance angles positive we have to use 0 to 180 degrees
# for all call outs. Doing this makes 1 o'clock and 11 o'clock have the same angle of 30 degrees
HSwinter24_dat$Angle_Degrees <- dplyr::recode(HSwinter24_dat$Direction,
                                       '12.0' = 0, # use r dist
                                       '12.5' = 15,
                                       '1.0' = 30,
                                       '1.5' = 45,
                                       '2.0' = 60,
                                       '2.5' = 75,
                                       '3.0' = 90,
                                       '3.5' = 105,
                                       '4.0' = 120,
                                       '4.5' = 135,
                                       '5.0' = 150,
                                       '5.5' = 165,
                                       '6.0' = 180, # use r dist
                                       '6.5' = 165,
                                       '7.0' = 150,
                                       '7.5' = 135,
                                       '8.0' = 120,
                                       '8.5' = 105,
                                       '9.0' = 90,
                                       '9.5' = 75,
                                       '10.0' = 60,
                                       '10.5' = 45,
                                       '11.0' = 30,
                                       '11.5' = 15,)

# the sin() function in R uses radians not degrees!
# creating another column for angle in radians
# radians = degree * pi/180
HSwinter24_dat$Angle_Radians <- HSwinter24_dat$Angle_Degrees * pi/180

# Now that we have a angle we can calculate the actual position of the deer from the transect line
# The angle of a right triangle is sin = opposite/hypotenuse.
# For us, we know sin (angle) and the hypotenuse (distance) using algebra we can rearrange the formula to
# hypotenuse * sin = opposite - so its hypotenuse (WTD_Distance) * sin(Angle_Radians) = x

# Note: if we use this formula for the degrees of 0 (12 o'clock) or 180 (6 o'clock)
# we would get a result of 0 since there is no opposite side to calculate
# to get around this we will just use the original distance for actual distance

for (i in 1:NROW(HSwinter24_dat)) {
  # if angle is 0 degrees or 180 degrees use r distance
  if (HSwinter24_dat[i,21] == 0 | HSwinter24_dat[i,21] == 180) { # HSwinter24_dat[row,Angle_Degrees]
    HSwinter24_dat[i,23] <- HSwinter24_dat[i,20] # New Dist = old dist
  }
  else{ # if angle is not 0 degrees or 180 degrees
    # use r * sin(angle) = x
    if (HSwinter24_dat[i,21] != 0 | HSwinter24_dat[i,21] != 180) {
      HSwinter24_dat[i,23] <- HSwinter24_dat[i,20] * sin(HSwinter24_dat[i,22]) # hypotenuse (WTD_Distance) * sin(Angle_Radians) = x
      
    }
  }
  # Renaming column
  names(HSwinter24_dat)[23] <- "Actual_Distance"
}


# Removing original distance keeping actual distance
HSwinter24_dat <- HSwinter24_dat[,-20]

# Renaming Actual_Distance to Distance
colnames(HSwinter24_dat)[22] <- "Distance"


# Subsetting and reorganizing columns
HSwinter24_dat <- HSwinter24_dat[,c("Observation_Number","Study_Area","Area","Date", "Lat", "Long", 
                                    "Transect_ID", "Transect_Length", "Flight_Path", "Males",
                                    "Female", "Fawn", "Unknown","Distance", "Group_size")]

# Save the cleaned data as an RDS file
saveRDS(HSwinter24_dat, file = "./Data/HelicopterSurvey_data/Cleaned_Survey_Data/HSwinter24_data.rds")



# Making a binary dataframe from the binomial dataframe
#install.packages("purrr")
library(purrr) 
library(tidyr) 
library(dplyr)




HSwinter24_binary_dat <- pmap_dfr(HSwinter24_dat, 
                                  function(Observation_Number, # ObservationNumber
                                           Study_Area, # Study area survey took place 
                                           Area, # Size of Study Area
                                           Date, # date of observation
                                           Lat, # Latitude
                                           Long, # Longitude
                                           Transect_ID, # Transect number
                                           Transect_Length, # Length of the transect
                                           Flight_Path, # Direction of flying, North to South or South to North
                                           Males, # Counts of Bucks
                                           Female, # Counts of Does
                                           Fawn, # Counts of Fawns
                                           Unknown, # Counts of unknown deer
                                           Distance, # Parallel distance calculated
                                           Group_size # Size of the group observed 
                                  ){                              
                                    data.frame(Observation_Number = Observation_Number,
                                               Study_Area = Study_Area,
                                               Area = Area,
                                               Lat = Lat,
                                               Long = Long,
                                               Transect_ID = Transect_ID,
                                               Transect_Length = Transect_Length,
                                               Flight_Path  = Flight_Path , 
                                               Date = Date ,
                                               Males = c( rep(1, Males),
                                                          rep(0, Group_size - Males)),
                                               
                                               Female = c( rep(1, Female),
                                                           rep(0, Group_size - Female)),
                                               
                                               Fawn = c( rep(1, Fawn),
                                                         rep(0, Group_size - Fawn)),
                                               
                                               Unknown = c( rep(1, Unknown),
                                                            rep(0, Group_size - Unknown)),
                                               
                                               Distance = Distance,
                                               Group_size = Group_size)})






# adding a column for observation number
HSwinter24_binary_dat$Observation_Number <- 1:NROW(HSwinter24_binary_dat)




#
# Save the cleaned data as an RDS file
saveRDS(HSwinter24_binary_dat, file = "./Data/HelicopterSurvey_data/Cleaned_Survey_Data/HSwinter24_binary_data.rds")

