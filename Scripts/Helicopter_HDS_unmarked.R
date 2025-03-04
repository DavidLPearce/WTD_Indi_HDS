
# packages
library(unmarked)
library(AICcmodavg)


# Load Data
heli_dat <- readRDS("./Data/Survey_Data/Helicopter_Data/Heli_Dist_Data.rds")



heli_dat$Transect_ID <- as.factor(heli_dat$Transect_ID )



heli_dat <- heli_dat[order(heli_dat$Distance), ]   



yDat <- formatDistData(heli_dat, distCol="Distance", 
                       transectNameCol="Unique_ID", dist.breaks=c(0, 20, 40, 60, 100)) 


covs <- heli_dat[,c(5, 9:13)]
rownames(covs) <- heli_dat$Unique_ID

# Order matrix by row names
covs <- covs[order(rownames(covs)), ]



umf <- unmarkedFrameDS(y = yDat, siteCovs = covs, survey = "line", 
                       dist.breaks = c(0, 20, 40, 60, 100), 
                       tlength = covs$Transect_Length_km,  unitsIn="km")



m.half <- distsamp(~1 ~1, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

m.haz <- distsamp(~1 ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")

m.uni <- distsamp(~1 ~1, umf, keyfun="uniform", output="density", unitsOut="kmsq")





# halfnormal has lowest AIC

# Null
fit1 <- distsamp(~1 ~1, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# Group size
fit2 <- distsamp(~1 ~Group_size, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# Survey period
fit3 <- distsamp(~1 ~ Survey_Time, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# Group size + Survey period 
fit4 <- distsamp(~1 ~ Group_size + Survey_Time, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# LiDAR
fit5 <- distsamp(~1 ~ Ground + Med_Veg + High_veg, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# LiDAR + Group
fit6 <- distsamp(~1 ~Group_size + Ground + Med_Veg + High_veg, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# LiDAR + Survey period
fit7 <- distsamp(~1 ~Survey_Time + Ground + Med_Veg + High_veg, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")

# LiDAR + Group + Survey period (Full)
fit8 <- distsamp(~1 ~Group_size + Survey_Time + Ground + Med_Veg + High_veg, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")



# AIC
fitlist <- list(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8) 

aictab(fitlist)

# best model was fit 8

# goodness of fit
fm <- distsamp(~1 ~Group_size + Survey_Time + Ground + Med_Veg + High_veg, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")


fitstats <- function(fm) {
  
  observed <- getY(fm@data)
  
  expected <- fitted(fm)
  
  resids <- residuals(fm)
  
  sse <- sum(resids^2)
  
  chisq <- sum((observed - expected)^2 / expected)
  
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  
  return(out)
  
}

(pb <- parboot(fm, fitstats, nsim=25, report=1))



































