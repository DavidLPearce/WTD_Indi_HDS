


# 1.0 Data viewing and manipulation ####
# Unzipping Helicopter survey data into its own directory 
unzip("Helicopter_Surveys.zip",exdir= "./HeliSurveyData")

# using the ff package to move the zip file to the new directory
# if not installed: install.packages("ff") 
ff::file.move("./Helicopter_Surveys.zip", "./HeliSurveyData")

# Looking into the HeliSurveyData folder
list.files("./HeliSurveyData")

# Reading in white-tailed deer survey data
wtd.orig <- read.csv("./HeliSurveydata/Survey_WTD_0.csv")
View(wtd.orig) # taking a look at the data

# The data includes the testing data
# We are only interested in the dates of 9/8/2023 and 9/9/2023
# First we have to let R know that the date column is a date and time and not a character string
wtd.orig$date <- as.POSIXct(wtd.orig$CreationDate, format="%m/%d/%Y %H:%M:%OS", tz="UTC") # Transforming string to date and time
wtd.orig$date <- as.Date(wtd.orig$date) # Removing time
wtd.dat <- wtd.orig[wtd.orig$date >= "2023-09-08" & wtd.orig$date <= "2023-09-09", ] # subsetting only 9/8 and 9/9
View(wtd.dat) # viewing data


# Adding a column for angle using our clock angle call out using recode from the dplyr package
# in a coordinate plane 0 to 180 degrees are positive. To keep our distance angles positive we have to use 0 to 180 degrees
# for all call outs. Doing this makes 1 o'clock and 11 o'clock have the same angle of 30 degrees
wtd.dat$angle.degrees <- dplyr::recode(wtd.dat$Direction, 
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
wtd.dat$angle.radians <- wtd.dat$angle.degrees * pi/180

# Now that we have a angle we can calculate the actual position of the deer from the transect line
# Going way back to trigonometry, Soh Cah Toa, we know that the angle of a right triangle is
# sin = opposite/hypotenuse. For us, we know sin (angle) and the hypotenuse (distance)
# using algebra we can rearrange the formula to hypotenuse * sin = opposite
# technically we are in a coordinate plane (a circle) so our formula is r * sin = x
# To calculate actual distance we can use: WTD_Distance (r) * sin(angle.radians) = x
# However, if we use this formula for the degrees of 0 (12 o'clock) or 180 (6 o'clock)
# we would get a result of 0 since there is no opposite side to calculate
# to get around this we will just use the original distance for actual distance
for (i in 1:NROW(wtd.dat)) {
      # if angle is 0 degrees or 180 degrees use r distance
      if (wtd.dat[i,44] == 0 | wtd.dat[i,44] == 180) {
          wtd.dat[i,46] <- wtd.dat[i,40]
      }
      else{ # if angle is not 0 degrees or 180 degrees
            # use r * sin(angle) = x
          if (wtd.dat[i,44] != 0 | wtd.dat[i,44] != 180) {
              wtd.dat[i,46] <- wtd.dat[i,40] * sin(wtd.dat[i,45])

          }
      }
          # Renaming column
          names(wtd.dat)[46] <- "distance"
}

View(wtd.dat) # lets take a look

# Distance calculations look correct when comparing outputs by hand calculations
# Lets create another column for group size, a potentially important covariate

# calculating row sums for group size
rowSums(wtd.dat[,c(30,31,32,33,34,35)]) # all deer

# we can see that there are some errors with data entry i.e., group size of 11 and 60
# lets see which columns are the issue
as.list(wtd.dat[,34]) # fawn column
# we can see that row 34 has 10 fawns entered
as.list(wtd.dat[,30]) # female column
# and we can see that row 74 has 60 females entered
# lets correct that to 1 fawn in row 34 and 6 females in row 74
wtd.dat[34,34] <- 1
wtd.dat[74, 30] <- 6

# Lets check the group sizes again
rowSums(wtd.dat[,c(30,31,32,33,34,35)]) 
# Group sizes now corrected. lets Make a covariate column for group size
wtd.dat$Group.size <- rowSums(wtd.dat[,c(30,31,32,33,34,35)]) 

# Lets make a new column for all male WTD observations as there may be some bias in estimating age on the hoof
wtd.dat$All.Males <- rowSums(wtd.dat[,c(31,32,33)])

# Adding a column for the total survey area 
wtd.dat$Area <- 2710 # area of La Copita Ranch in acres

# The package also requires the length of the transect for effort 
# Adding in a column based on the transect that the detection occurred on and 
# the direction of the flight path (N to S or S to N) and transect length
wtd.dat$Transect.Number <- ""
wtd.dat$Transect.Length <- "" 
wtd.dat$Flight.Path <- ""

# Due to the temps and wind the pilot flew La Copita west to east and flew that first transect N to S (not following the layout) on 9/8
# write a .csv to enter transect number, transect length and flight path in ArcGIS Pro
# write.csv(wtd.dat,"./WTD_HeliSurveys_ArcGIS.csv", row.names = FALSE)

# With transect information added read in .csv
# transects are numbered beginning of flight
# on 9/8 surveys started on the west boundary and worked to the east
# On 9/9 surveys started on the east boundary and worked to the west
wtd.dat.gis <- read.csv("./WTD_HeliSurveys_ArcGIS.csv")
head(wtd.dat.gis)  

# Functions in the distance sampling package are rather particular on the naming of columns
# there are few options that allow for columns to be pointed to, instead we have to rename the columns
View(wtd.dat.gis)

names(wtd.dat.gis)[50] <- "Sample.Label" # Renaming transect number to sample label, from Distance: Sample.Label - point transect identifier
names(wtd.dat.gis)[51] <- "Effort" # renaming transect length to effort, from Distance: survey effort
wtd.dat.gis$Study.Area <- "La Copita Ranch" # adding a column for study area
wtd.dat.gis$ObservationNumber <- 1:NROW(wtd.dat.gis)

View(wtd.dat.gis) 

# subsetting dataset based on surveys 9/8 and 9/9
wtd.dat.sept8 <- wtd.dat.gis[wtd.dat.gis$date == "9/8/2023",] # 9/8
wtd.dat.sept9 <- wtd.dat.gis[wtd.dat.gis$date == "9/9/2023",] # 9/9
View(wtd.dat.sept9)

wtd.dat.sept9.bucks <- wtd.dat.gis[, c(47,48,54)]



test.dat <- wtd.dat.sept9.bucks
View(test.dat)
str(test.dat)












# Making a binary dataframe from the binomial dataframe
install.packages("purrr")
library(purrr) 
library(tidyr) 
library(dplyr)



# subsetting date only using columns interested in
View(wtd.dat.gis)
dat.sub <- wtd.dat.gis[,c(54,53, 52, 51, 50, 49, 48, 47, 46, 10, 11, 43, 30, 34, 35)] 
                          
                 
install.packages("readr")
library(readr)                  


wtd.binary.dat <- pmap_dfr(dat.sub, 
                      function(ObservationNumber, # ObservationNumber
                               Study.Area, # Study area survey took place 
                               Area, # Size of Study Area
                               Latitude, # Latitude
                               Longitude, # Longitude
                               Sample.Label, # Transect number
                               Effort, # Length of the transect
                               Flight.Path, # Direction of flying, North to South or South to North
                               date, # date of observation
                               All.Males, # Counts of Bucks
                               WTD..Female, # Counts of Does
                               WTD..Fawn, # Counts of Fawns
                               WTD..Unknown, # Counts of unknown deer
                               distance, # Parallel distance calculated
                               Group.size # Size of the group observed 
                        ){                              
                        data.frame(ObservationNumber = ObservationNumber,
                                   Study.Area = Study.Area,
                                   Area = Area,
                                   Latitude = Latitude,
                                   Longitude = Longitude,
                                   Sample.Label = Sample.Label,
                                   Effort = Effort,
                                   Flight.Path = Flight.Path, 
                                   date = date,
                                   All.Males = c( rep(1, All.Males),
                                                    rep(0, Group.size - All.Males)),
                                   
                                   WTD..Female = c( rep(1, WTD..Female),
                                                    rep(0, Group.size - WTD..Female)),
                                   
                                   WTD..Fawn = c( rep(1, WTD..Fawn),
                                                  rep(0, Group.size - WTD..Fawn)),
                                   
                                   WTD..Unknown = c( rep(1, WTD..Unknown),
                                                    rep(0, Group.size - WTD..Unknown)),
                                   
                                   distance = distance,
                                   Group.size = Group.size
                                   )}
                      
                          )


wtd.binary.dat



# Snap points to lines

snapPointsToLines(points, lines, maxDist=NA, withAttrs = TRUE, idField=NA)
































