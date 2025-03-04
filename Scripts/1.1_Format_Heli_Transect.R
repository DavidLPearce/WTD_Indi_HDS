# Author: David L. Pearce
# Description:
#             TBD

# Citation: 
#      TBD


# ------------------------------------------------------------------------------
#
#                               Loading R packages
#
# ------------------------------------------------------------------------------

## Install packages (if needed)
# install.packages("sf")
# install.packages("sp")
# install.packages("geosphere")
# install.packages("tidyverse")
# install.packages("lidR")
# install.packages("RCSF")
# install.packages("terra")

# Load library
library(sf)
library(sp)
library(geosphere)
library(tidyverse)
library(lidR)
library(RCSF)
library(terra)

# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")


# ------------------------------------------------------------------------------
#
#                                 Reading in data
#
# ------------------------------------------------------------------------------

# Read in transects
transects <- st_read("./Data/Spatial_Data/Helicopter_Transects/Helicopter_Transects.shp")

# Read in boundary
boundary <- st_read("./Data/Spatial_Data/La_Copita_Boundary")

# Read the .csv data file
heli_dat <- read.csv("./Data/Survey_Data/Helicopter_Data/Helicopter_Survey_Data_Fall2023-Winter2025.csv")
 
# ------------------------------------------------------------------------------
#
#                                 Data Wrangling
#
# ------------------------------------------------------------------------------


# -------------------------------------------------
# Snapping points to transects 
# -------------------------------------------------

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

# The points are not on the transect lines will have to correct
# this before getting actual gps position of individuals

# This code has been adapted from jjniev01 and hugh-allan's code on the stackoverflow thread
# https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

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
ggplot()+
  geom_sf(data = boundary, col = 'black')+
  geom_sf(data = transects, col = 'grey')+
  geom_sf(data = transects, col = 'black')+
  geom_sf(data = heli_dat, col = 'red')+ # Original locations
  geom_sf(data = heli_dat_snap$snapped_point_coords,col = 'blue') # Snapped points

# -------------------------------------------------
# Getting actual coordinates 
# -------------------------------------------------       

# The Observed points are now snapped to the transect, can
# now get the actual location of the individuals detected. 
# To get the actual locations we need to convert our call out angle to degrees.

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





# The destPoint function from 'geosphere' requires the coordinates (p) to be 
# a vector of 2 numbers, a matrix of 2 columns or a SpatialPoints object in degrees

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
  geom_sf(data = heli_dat_snap$snapped_point_coords, col = 'red')+ # Snapped coords
  geom_sf(data = actual_coords,col = 'blue') # Actual locations


# Now that the observations have accurate gps coordinates for their locations we can extract covariates.

# -------------------------------------------------
# Getting Distance 
# -------------------------------------------------       

# Distance sampling requires the the distance of the observation 
# from the transect. Now that the actual coordinates are known we can 
# get that distance.

# Subsetting data 
heli_dat_clean <- heli_dat_snap[, c(1:16)]
heli_dat_clean$Degree <- heli_dat_snap$Direction_Degree
heli_dat_clean$geometry <- actual_coords_proj


# Getting distance from actual coords to transects
distance <- heli_dat_clean %>% 
  rowwise() %>%
  mutate(
    # Get the nearest segment linestring:
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
heli_dat_clean$Perpendicular_Distance <- round(heli_dat_clean$Perpendicular_Distance)

# Detection of a individual can be affected by the group size, adding a group size covariate
# Calculating group sizes
heli_dat_clean$Group_size <- rowSums(heli_dat_clean[,c(7:13)])

# Summing all males rowwise
heli_dat_clean$Males <- rowSums(heli_dat_clean[,c(9:12)])


# End Script