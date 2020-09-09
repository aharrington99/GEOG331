######Question 1####

str(noaa)
noaa$dateF <- as.Date(noaa$DATE, "%Y-%m-%d") #reformats to date format in new column
noaa$year <- as.numeric(format(noaa$dateF,"%Y")) #generates new year column
str(noaa)

######Question 2####
factor_ex<-as.factor(c("a1","b3","c4","d9","z7")) #factor 
char_ex<-as.character(c("a","b","c","d","e")) #character
num_ex<-as.numeric(c(1.37,4.25,5.98,3.26,4.01)) #numeric
int_ex<-as.integer(c(1,2,3,4,5)) #integer

######Hitograms and Descriptive stats#####
levels(noaa$NAME)
mean(noaa$TMAX[noaa$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
noaa$TAVE <- noaa$TMIN + ((noaa$TMAX-noaa$TMIN)/2) #average daily temp
averageTemp <- aggregate(noaa$TAVE, by=list(noaa$NAME), FUN="mean",na.rm=TRUE) #average TAVE by NAME 
averageTemp
colnames(averageTemp) <- c("NAME","MAAT") #change the column names to something useful (Mean Annual Air Temp)
averageTemp
noaa$siteN <- as.numeric(as.factor(noaa$NAME))
noaa$NAME <- as.factor(noaa$NAME)
levels(noaa$N)
####



#Question 4#####
par(mfrow=c(2,2))
hist(noaa$TAVE[noaa$NAME == "ABERDEEN, WA US"],freq=FALSE, 
     main = paste(levels(noaa$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
abline(v = mean(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE), #mean line
       col = "tomato3",
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE) - sd(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE), #one SD below
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE) + sd(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE), #one SD above
       col = "tomato3", 
       lty = 3,
       lwd = 3)
hist(noaa$TAVE[noaa$NAME == "MORRISVILLE 6 SW, NY US"],freq=FALSE, 
     main = paste(levels(noaa$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue",
     border="white")
abline(v = mean(noaa$TAVE[noaa$siteN == 5],na.rm=TRUE), #mean line
       col = "tomato3",
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 5],na.rm=TRUE) - sd(noaa$TAVE[noaa$siteN == 5],na.rm=TRUE), #one SD below
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 5],na.rm=TRUE) + sd(noaa$TAVE[noaa$siteN == 5],na.rm=TRUE), #one SD above
       col = "tomato3", 
       lty = 3,
       lwd = 3)
hist(noaa$TAVE[noaa$NAME == "LIVERMORE, CA US"],freq=FALSE, 
     main = paste(levels(noaa$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="green",
     border="white")
abline(v = mean(noaa$TAVE[noaa$siteN == 2],na.rm=TRUE), #mean line
       col = "tomato3",
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 2],na.rm=TRUE) - sd(noaa$TAVE[noaa$siteN == 2],na.rm=TRUE), #one SD below
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 2],na.rm=TRUE) + sd(noaa$TAVE[noaa$siteN == 2],na.rm=TRUE), #one SD above
       col = "tomato3", 
       lty = 3,
       lwd = 3)
hist(noaa$TAVE[noaa$NAME == "MORMON FLAT, AZ US"],freq=FALSE, 
     main = paste(levels(noaa$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="orange",
     border="white")
abline(v = mean(noaa$TAVE[noaa$siteN == 4],na.rm=TRUE), #mean line
       col = "tomato3",
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 4],na.rm=TRUE) - sd(noaa$TAVE[noaa$siteN == 4],na.rm=TRUE), #one SD below
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(noaa$TAVE[noaa$siteN == 4],na.rm=TRUE) + sd(noaa$TAVE[noaa$siteN == 4],na.rm=TRUE), #one SD above
       col = "tomato3", 
       lty = 3,
       lwd = 3)


##### Normal Distibution####
par(mfrow=c(1,1))
h1<-hist(noaa$TAVE[noaa$NAME == "ABERDEEN, WA US"],freq=FALSE, 
     main = paste(levels(noaa$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#the seq function generates a sequence of numbers that we can use to plot
#the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE),
                 sd(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range
#from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the 
#same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

##### Question 5 ####
#####Question 6####
#How often higher than 18.51026 if mean increases by 4
1-pnorm(18.51026,
        mean(noaa$TAVE[noaa$siteN == 1]+4,na.rm=TRUE),
        sd(noaa$TAVE[noaa$siteN == 1],na.rm=TRUE))

#####Question 7####
hist(noaa$PRCP[noaa$NAME == "ABERDEEN, WA US"],freq=FALSE, 
     main = paste(levels(noaa$NAME)[1]),
     xlab = "Daily precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#####Question 8####
library(dplyr)
sum(noaa$PRCP,na.rm=TRUE)
prcpYR<-aggregate(noaa$PRCP, by=list(noaa$NAME,noaa$year), FUN="sum",na.rm=TRUE)
colnames(prcpYR) <- c("NAME","Year","Annual Precipitation")
prcpYR

hist(prcpYR$`Annual Precipitation`[prcpYR$NAME=="ABERDEEN, WA US"],freq = FALSE,
     main = paste(levels(noaa$NAME)[1]),
     xlab = "Average yearly precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#####Question 9####
levels(prcpYR$NAME)
mean(prcpYR$`Annual Precipitation`[prcpYR$NAME==levels(noaa$NAME)[1]])
myp<-aggregate(prcpYR$`Annual Precipitation`, by=list(prcpYR$NAME), FUN="mean",na.rm=TRUE)
colnames(myp)<-c("Name","Mean Yearly Precip")
myp
a<-plot(averageTemp$MAAT,myp$`Mean Yearly Precip`,type="n",xlab="Mean Annual Air Temp (*C)",
     ylab="Mean Yearly Precipitation (mm)")
text(averageTemp$MAAT,myp$`Mean Yearly Precip`,labels=myp$Name,cex=0.5)
a
View(myp)
     
