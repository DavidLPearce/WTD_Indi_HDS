# 
# 
# This example is from https://darinjmcneil.weebly.com/hierarchical-distance-models.html#:~:text=Hierarchical%20distance%20models%20(HDM)%20are,and%20account%20for%20detection%20probability.
# Fitting HDS models in unmarked
# 

# Load required packages
install.packages("unmarked")
install.packages("AICcmodavg")
library(unmarked)
library(AICcmodavg)

# Read in data
data1 <- read.csv("./hdm_data_practice.csv")
head(data1)


observations1<-as.matrix(data1[,9:13]) # detection data
breaks<-c(0,1,2,3,4,5) # this needs to be length = J + 1 (J= no. of distance classes)
transect.length<-70
sitecovs1<-data1[,2:8] # both site covs and detection covs
sitecovs1[,"area"]<-((800*1000)/10000) # adds area in ha surveyed

umf1 <- unmarkedFrameGDS(y = observations1, survey="line", unitsIn="m", siteCovs=sitecovs1,
                         dist.breaks=breaks, tlength=rep(transect.length, nrow(data1)),
                         numPrimary=1)


# Fitting models to data
mod.hazard<-gdistsamp(~1,~1,~1, data=umf1, output="density", mixture="P", keyfun = "hazard")
mod.exp<-gdistsamp(~1,~1,~1, data=umf1, output="density", mixture="P", keyfun = "exp")

# seeing which model is better
modlist1<-list(mod.hazard=mod.hazard, mod.exp=mod.exp)
aictab(modlist1)

# ---------------------------------------------------------------------------
#
#                        Modeling Detection 
#
# ---------------------------------------------------------------------------


# Detection models
p.null<-gdistsamp(~1,~1,~1, data=umf1, output="density", mixture="P", keyfun = "exp")
p.observer<-gdistsamp(~1,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")
p.cloud<-gdistsamp(~1,~1,~cloud, data=umf1, output="density", mixture="P", keyfun = "exp")
p.temp<-gdistsamp(~1,~1,~temp, data=umf1, output="density", mixture="P", keyfun = "exp")

# Model Rankings
modlist2=list(p.null=p.null, p.temp=p.temp, p.cloud=p.cloud, p.observer=p.observer)
aictab(modlist2)

# Observer was the most influencial on detection
# Taking a look to see who was best
newdat1<-cbind.data.frame(data1$site,data1$observer)
colnames(newdat1)<-c("site","observer")
p1<-getP(p.observer)
newdat1<-cbind(newdat1,p1)
colnames(newdat1)<-c("pointid","observer", "bin1", "bin2", "bin3", "bin4", "bin5")

joedata<- subset(newdat1, observer=="joe")
joep<-as.data.frame(c((max(joedata$bin1)),(max(joedata$bin2)),(max(joedata$bin3)),(max(joedata$bin4)),(max(joedata$bin5))))
joep<-cbind.data.frame(joep,"joe",c(1,2,3,4,5))
colnames(joep)<-c("p", "obs", "dist")

stephaniedata<- subset(newdat1, observer=="stephanie")
stephaniep<-as.data.frame(c((max(stephaniedata$bin1)),(max(stephaniedata$bin2)),(max(stephaniedata$bin3)),(max(stephaniedata$bin4)),(max(stephaniedata$bin5))))
stephaniep<-cbind.data.frame(stephaniep,"stephanie",c(1,2,3,4,5))
colnames(stephaniep)<-c("p", "obs", "dist")

beckydata<-subset(newdat1, observer=="becky")
beckyp<-as.data.frame(c((max(beckydata$bin1)),(max(beckydata$bin2)),(max(beckydata$bin3)),(max(beckydata$bin4)),(max(beckydata$bin5))))
beckyp<-cbind.data.frame(beckyp,"becky",c(1,2,3,4,5))
colnames(beckyp)<-c("p", "obs", "dist")

plot(-10,-10, xlim=c(1,5), ylim=c(0,0.2), xlab="Distance from Observer (m)",
     ylab="Detection Probability (p)")
lines(stephaniep$dist,stephaniep$p, lty=1, col="blue")
lines(beckyp$dist,beckyp$p, lty=1, col="red")
lines(joep$dist,joep$p, lty=1, col="green")

text(3.5, 0.175, "Joe", col="green")
text(3.5, 0.160, "Becky", col="red")
text(3.5, 0.145, "Stephanie", col="blue")

# Becky was the best


# ---------------------------------------------------------------------------
#
#                        Modeling Abundance
#
# ---------------------------------------------------------------------------

lambda.null<-gdistsamp(~1,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")
lambda.stone<-gdistsamp(~stone,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")
lambda.moisture<-gdistsamp(~moisture,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")
lambda.mosscover<-gdistsamp(~moss.cover,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")
lambda.earthworms<-gdistsamp(~earthworms,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")

modlistb=list(lambda.null=lambda.null, lambda.stone=lambda.stone, lambda.moisture=lambda.moisture,
              lambda.mosscover=lambda.mosscover, lambda.earthworms=lambda.earthworms)
aictab(modlistb)


# Visualizing the relationship
newdat<-data.frame(moss.cover=seq(min(data1$moss.cover), max(data1$moss.cover), length.out = 100))
expectedN<-predict(lambda.mosscover, type="lambda", newdata=newdat, appendData=TRUE)
print(expectedN)
plot(1, xlim=c(0,100), ylim=c(0,200), type="n", axes=T, xlab="Moss cover (%)", pch=20, ylab="Animal Density (ind./ha)", family="serif", cex.lab=1.25, cex.main=1.75)
lines(expectedN$moss.cover, expectedN$Predicted, col="black", lwd=2)
lines(expectedN$moss.cover, expectedN$lower, lty=2, col="black")
lines(expectedN$moss.cover, expectedN$upper, lty=2, col="black")



?gdistsamp






p.observer<-gdistsamp(~1,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")
lambda.mosscover<-gdistsamp(~moss.cover,~1,~observer, data=umf1, output="density", mixture="P", keyfun = "exp")




