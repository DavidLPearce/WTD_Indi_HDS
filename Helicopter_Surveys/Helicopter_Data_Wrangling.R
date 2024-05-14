
## ------------------------------------------------------------------------------
##
##                               Loading R packages
##
## ------------------------------------------------------------------------------


library(sf)
library(sp)
library(geosphere)
library(tidyverse)
library(lidR)
library(RCSF)


## ------------------------------------------------------------------------------
##
##                                 Reading in data
##
## ------------------------------------------------------------------------------

# Read in Transects
transects <- st_read("./Data/Spatial_Data/Helicopter_Transects/Helicopter_Transects.shp")

# Read in Boundary
boundary <- st_read("./Data/Spatial_Data/La_Copita_Boundary")

# Read the CSV file
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Helicopter_Survey_Data-Rolling5.13.24.csv")


## ------------------------------------------------------------------------------
##
##                                 Data Wrangling
##
## ------------------------------------------------------------------------------


## -------------------------------------------------
##        Snapping points to transects 
## -------------------------------------------------

# Convert the Lat and Long column to an sf object
heli_dat <- sf::st_as_sf(heli_dat, coords = c("Latitude", "Longitude"), crs = 4326)

# Reproject to UTM zone 14N
transects <- sf::st_transform(transects, crs = 32614)
boundary <- sf::st_transform(boundary, crs = 32614)
heli_dat <- sf::st_transform(heli_dat, crs = 32614)

# Take a look
head(heli_dat)

ggplot()+
  geom_sf(data = boundary, col = 'black')+
  geom_sf(data = transects, col = 'grey')+
  geom_sf(data = heli_dat, col = 'red')

## The points are not on the transect lines will have to correct
## this before getting actual gps position of individuals

## This code has been adapted from jjniev01 and hugh-allan's code on the stackoverflow thread
## https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

# Snapping points to lines if points are within 100m from a transect
# Transects are spaced 250m apart and observations were only taken out to 100m
heli_dat_snap <- heli_dat %>% 
  rowwise() %>%
  mutate(
    # Get the nearest river segment linestring:
    nearest_segment = transects[st_nearest_feature(geometry, transects),],
    # Get the linestrings between each point and the closest segment:
    line_to_point = st_nearest_points(geometry, nearest_segment),
    # Retrieve the point from the line sf that was returned:
    closest_point = st_cast(line_to_point, 'POINT')[2],
    # Calculate the distance between the old and new point:     
    distance = st_distance(geometry, closest_point)[,1],
    # If under our limit of 100m distant, adopt the new geometry, 
    # else keep the original
    snapped_point_coords = st_sfc(ifelse(as.numeric(distance) <= 100, 
                                       st_geometry(closest_point),
                                       geometry), 
                                crs = st_crs(transects)))
# Taking a look
View(heli_dat_snap)

ggplot()+
  geom_sf(data = boundary, col = 'black')+
  geom_sf(data = transects, col = 'grey')+
  geom_sf(data = transects, col = 'black')+
  geom_sf(data = heli_dat, col = 'red')+
  geom_sf(data = heli_dat_snap$snapped_point_coords,col = 'blue')

## -------------------------------------------------
##           Getting actual coordinates 
## -------------------------------------------------       

## The Observed points are now snapped to the transect line, can
## now get the actual location of the individuals detected. 
## To get the actual locations we need to convert our call out angle to degrees.

# Adding a column for degree angle using our clock angle call out using the recode function from 'dplyr'
# Flying south to north (S2N) would make the callout of 12 o'clock (12.0) to be 0 degrees, 3 to be 90,
# and 10 to be 300; however, flying north to south (N2S) would plot the actual coordinates inaccurately because
# the degrees are inverted, i.e, 12.5 is actually 6.5 and 3 is actually 9

heli_dat_snap <- as.data.frame(heli_dat_snap)

for (row in 1:NROW(heli_dat)){
  
  # If flight path is south to north than callout to degrees is normal
  if (isTRUE(heli_dat_snap[row, 5] == "S2N")){
    heli_dat_snap[row, "Direction_Degree"] <- dplyr::recode(heli_dat_snap[row, "Direction_clock"], 
                                                  '12.0' = 0, 
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
                                                  '6.0' = 180, 
                                                  '6.5' = 195,
                                                  '7.0' = 210,
                                                  '7.5' = 225,
                                                  '8.0' = 240,
                                                  '8.5' = 255,
                                                  '9.0' = 270,
                                                  '9.5' = 285,
                                                  '10.0' = 300,
                                                  '10.5' = 315,
                                                  '11.0' = 330,
                                                  '11.5' = 345,)
  }
  # if flight path is north to south than callout to degrees is inverted
  else if (isTRUE(heli_dat_snap[row, 5] == "N2S")){
    heli_dat_snap[row, "Direction_Degree"] <- dplyr::recode(heli_dat_snap[row, "Direction_clock"], 
                                            '12.0' = 180, # 6
                                            '12.5' = 195, # 6.5
                                            '1.0' = 210,  # 7
                                            '1.5' = 225,  # 7.5
                                            '2.0' = 240,  # 8 
                                            '2.5' = 255,  # 8.5 
                                            '3.0' = 270,  # 9 
                                            '3.5' = 285,  # 9.5 
                                            '4.0' = 300,  # 10 
                                            '4.5' = 315,  # 10.5  
                                            '5.0' = 330,  # 11 
                                            '5.5' = 345,  # 11.5 
                                            '6.0' = 0,    # 12 
                                            '6.5' = 15,   # 12.5 
                                            '7.0' = 30,   # 1 
                                            '7.5' = 45,   # 1.5 
                                            '8.0' = 60,   # 2 
                                            '8.5' = 75,   # 2.5 
                                            '9.0' = 90,   # 3 
                                            '9.5' = 105,  # 3.5 
                                            '10.0' = 120, # 4
                                            '10.5' = 135, # 4.5 
                                            '11.0' = 150, # 5 
                                            '11.5' = 165,)# 5.5 
    
  }
}





## The destPoint function from 'geosphere' requires the coordinates (p) to be 
## a vector of 2 numbers, a matrix of 2 columns or a SpatialPoints object in degrees

# Subsetting coordinates
snapped_coords <- heli_dat_snap$snapped_point_coords

# Transforming projection to degrees
snapped_coords <- sf::st_transform(snapped_coords, crs = st_crs("+proj=longlat +datum=WGS84"))

# Changing to a sp object
snapped_coords <- sf::as_Spatial(snapped_coords)

# Calculate the destination point
actual_coords <- geosphere::destPoint(p = snapped_coords, 
               b = heli_dat_snap$Direction_Degree, d = heli_dat_snap$Detection_Distance) 

# Transforming from a matrix to a sf object
actual_coords <- as.data.frame(actual_coords)
actual_coords <- sf::st_as_sf(actual_coords, coords = c("lon", "lat"), crs = 4326)
actual_coords_proj <- sf::st_transform(actual_coords, crs = 32614)


# Take a look
head(snapped_coords)
head(actual_coords)

ggplot()+
  geom_sf(data = boundary, col = 'black')+
  geom_sf(data = transects, col = 'grey')+
  geom_sf(data = transects, col = 'black')+
  geom_sf(data = heli_dat_snap$snapped_point_coords, col = 'red')+
  geom_sf(data = actual_coords,col = 'blue')


## Now that the observations have accurate gps coordinates for their locations
## we can extract covariates.



## -------------------------------------------------
##           Detection Covariates 
## -------------------------------------------------       


## Distance sampling requires the the distance of the observation 
## from the transect. Now that the actual coordinates are known we can 
## get that distance.

# Subsetting data 
heli_dat_clean <- heli_dat_snap[, c(1:16)]
heli_dat_clean$Degree <- heli_dat_snap$Direction_Degree
heli_dat_clean$geometry <- actual_coords_proj


# Getting distance from actual coords to transects
distance <- heli_dat_clean %>% 
  rowwise() %>%
  mutate(
    # Get the nearest river segment linestring:
    nearest_segment = transects[st_nearest_feature(geometry, transects),],
    # Get the linestrings between each point and the closest segment:
    line_to_point = st_nearest_points(geometry, nearest_segment),
    # Retrieve the point from the line sf that was returned:
    closest_point = st_cast(line_to_point, 'POINT')[2],
    # Calculate the distance between the old and new point:     
    distance = st_distance(geometry, closest_point)[,1])


# Adding distance to clean df
heli_dat_clean$Perpendicular_Distance <- distance$distance
heli_dat_clean$Perpendicular_Distance <- as.numeric(heli_dat_clean$Perpendicular_Distance)

# Disable scientific notation
options(scipen = 9999)


# Detection of a individual can be affected by the group size, adding a group size covariate
# Calculating group sizes
heli_dat_clean$Group_size <- rowSums(heli_dat_clean[,c(7:13)])

# Summing all males rowwise
heli_dat_clean$Males <- rowSums(heli_dat_clean[,c(9:12)])


## Detection can also be affected by the habitat that an individual or group is located in.
## Habitat covariates will be pulled from a classified 70cm LiDAR point cloud available at
## TNRIS: https://data.tnris.org/collection/?c=6131ecdd-aa26-433e-9a24-97ac1afda7de. LiDAR grids
## are Ben Bolt NW|SW and Ben Bolt NW|NW


## Read in LiDAR data

# Set directory to las files
las_folder <- "D:/LaCopita_GIS_Data/LIDAR2018_70cm/LaCopita_LiDAR_tiles/LAS"

# Read in las files
lasFiles <- readLAScatalog(las_folder, filter = "-keep_class 2 3 4 5")

# Summary of las files
summary(lasFiles)

# Iterating through all observations and extracting LiDAR data within 30m of observation
for (row in 1:NROW(heli_dat_clean)){
  
  # Extracting LiDAR data
  sub_las <- clip_roi(lasFiles, heli_dat_clean[row, "geometry"], radius = 30)
  
  # Putting extracted data into a table
  LiDAR_table <- table(sub_las$Classification)
  
  # Rename the categories
  names(LiDAR_table) <- c("ground", "low_veg", "med_veg", "high_veg")
  
  # Calculate the total count for each habitat type
  total_count <- sum(LiDAR_table)
  
  # Calculate the proportions of habitat
  proportions <- (LiDAR_table / total_count)
  
  # Adding proportions to dataframe
  heli_dat_clean[row, "Ground"] <- proportions[1] # ground
  heli_dat_clean[row, "Low_Veg"] <- proportions[2] # low_veg
  heli_dat_clean[row, "Med_Veg"] <- proportions[3] # med_veg
  heli_dat_clean[row, "High_veg"] <- proportions[4] # high_veg
}

# Take a look
head(heli_dat_clean)

# and just because...
# Taking a look at the last subbed observation in a 3D model
plot(sub_las, color = "Classification", bg = "white", size = 5)



## Creating a subsetted dataset of just sex and age structuring data 

structure_dat <- heli_dat_clean[, c(1:13)]

# saving as a .rds file for analsis later on
saveRDS(structure_dat, file = "./Data/Survey_Data/Helicopter_Data/Structure_Data.rds")


## Creating a dataframe where each observation of multiple individuals 
## is separated into individual observations with accompaning covariates and a 
## unique observation ID

# subsetting out age and sex data
heli_sub_dat <- heli_dat_clean[, -c(7:13, 21)]

# Take a look
head(heli_sub_dat)


## Analysis can either be done using the sum of counts in a distance bin, which
## looses some power in covariates or it can be done where each observation (individual not group)
## has an observation in a distance bin. This allows for observation level covariates to be retained


################################

library(purrr) 
library(tidyr) 
library(dplyr)


unique(colnames(heli_dat_clean))

heli_sub_dat <- heli_dat_clean[, c(1:4, 6:8,13, 16, 18:25)]

heli_bin_dat <- pmap_dfr(heli_sub_dat, 
                                  function(
                                           Study_Area, # Study area survey took place 
                                           Area_ha, # Size of study area in hectares
                                           Date, # date of observation
                                           Transect_ID, # Transect number
                                           Transect_Length_km, # Length of the transect in kilometers
                                           geometry,  # coordinates of observation
                                           Female,  # Does
                                           Fawn,  # Juveniles
                                           Males , # Bucks
                                           Unknown, # Unknown deer
                                           Perpendicular_Distance, # Perpendicular distance calculated
                                           Group_size, # Size of the group observed 
                                           Survey_Time, # time survey was conducted
                                           Ground, # proportion of ground points
                                           Low_Veg, # proportion of low vegetation points
                                           Med_Veg, # proportion of medium vegetation points
                                           High_veg # proportion of high vegetation points
                                  ){                              
                                    data.frame(
                                               Study_Area = Study_Area,
                                               Area_ha = Area_ha,
                                               Date = Date,
                                               Transect_ID = Transect_ID,
                                               Transect_Length_km = Transect_Length_km,
                                               geometry = geometry,
                                               
                                               Males = c( rep(1, Males),
                                                          rep(0, Group_size - Males)),
                                               
                                               Female = c( rep(1, Female),
                                                           rep(0, Group_size - Female)),
                                               
                                               Fawn = c( rep(1, Fawn),
                                                         rep(0, Group_size - Fawn)),
                                               
                                               Unknown = c( rep(1, Unknown),
                                                            rep(0, Group_size - Unknown)),
                                               
                                               Perpendicular_Distance = Perpendicular_Distance,
                                               Group_size = Group_size)})




























##################################################

# Initialize an empty dataframe to store expanded data
heli_bin_dat <- data.frame(matrix(ncol = 12, nrow = sum(heli_sub_dat$Group_size)))

# Iterate through all lines in new expanded dataframe adding data from heli_sub_dat
for (line in 1:NROW(heli_bin_dat)) {

  # Iterate over unique Transect_IDs
  for (transect_id in unique(heli_sub_dat$Transect_ID)) {
    
    # Subset the data for the current Transect_ID
    subset_data <- heli_sub_dat[heli_sub_dat$Transect_ID == transect_id, ]
    
    # Calculate the maximum number of observations for this Transect_ID
    max_observation_number <- max(sum(subset_data$Group_size))
  
  # Iterate over each row of the subset
  for (i in 1:nrow(subset_data)) {
    
    # Select the current row
    row <- subset_data[i, ]  

    # Formating data to DD.MM.YYYY
    formatted_date <- gsub("/", ".", row$Date)
    
    # Get the Group_size value for the current row
    group_size <- row$Group_size 
    
    # Create a copy of the current row
    new_row <- row 
        
    # Create new rows based on Group_size
    for (j in 1:group_size) {
        
      # Update the Observation_Number
      new_row$Observation_Number <- j + max_observation_number * (i - 1)
      
      # Create Unique_ID
      new_row$Unique_ID <- paste0(formatted_date, "_", row$Transect_ID, ".", new_row$Observation_Number)
      
      # Adding to new data frame
      heli_bin_dat[line, 'Study_Area'] <- new_row[1, 'Study_Area']
      heli_bin_dat[line, 'Area_ha'] <- new_row[1, 'Area_ha']
      heli_bin_dat[line, 'Unique_ID'] <- new_row[1, 'Unique_ID']
      heli_bin_dat[line, 'Transect_Length_km'] <- new_row[1, 'Transect_Length_km']
      heli_bin_dat[line, 'geometry'] <- new_row[1, 'geometry']
      heli_bin_dat[line, 'Date'] <- new_row[1, 'Date']
      heli_bin_dat[line, 'Perpendicular_Distance'] <- new_row[1, 'Perpendicular_Distance']
      heli_bin_dat[line, 'Group_size'] <- new_row[1, 'Group_size']
      heli_bin_dat[line, 'Survey_Time'] <- new_row[1, 'Survey_Time']
      heli_bin_dat[line, "Ground"] <- new_row[1, 'Ground']
      heli_bin_dat[line, 'Med_Veg'] <- new_row[1, 'Med_Veg']
      heli_bin_dat[line, 'High_veg'] <- new_row[1, 'High_veg']

      
    }
  }
}
}   
  


