
library(unmarked)

# Distance
hist(HSwinter24_binary_dat$Distance,  xlab="Distance (m)", ylab="Frequency", main = "") # will probably have to bin distances


yDat <- formatDistData(HSwinter24_binary_dat, distCol="Distance", transectNameCol="Transect_ID", 
                       dist.breaks=c(0, 20, 40, 60, 100))  #create distance categories (m). Could change these.

# see
#https://www.montana.edu/screel/teaching/bioe-440r-521/bioe-440-distance-sampling.html

# add in transect 1.1 1.2 for survey dates