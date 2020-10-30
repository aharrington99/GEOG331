#####loading in the spatial packages, loading in ALL data####
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(RColorBrewer)
g1966 <- readOGR("data\\GNPglaciers\\GNPglaciers_1966.shp", stringsAsFactors = T) #Glacier data
g1998 <- readOGR("data\\GNPglaciers\\GNPglaciers_1998.shp", stringsAsFactors = T)
g2005 <- readOGR("data\\GNPglaciers\\GNPglaciers_2005.shp", stringsAsFactors = T)
g2015 <- readOGR("data\\GNPglaciers\\GNPglaciers_2015.shp", stringsAsFactors = T)
redL <- raster("data\\glacier_09_05_14\\l08_red.tif") #RGB data
greenL <- raster("data\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("data\\glacier_09_05_14\\l08_blue.tif")
rgbL <- brick(redL, greenL, blueL)
ndviYear <- seq(2003,2016) #set up years to read in NDVI
NDVIraster <- list() #read all files into a list
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("data\\NDVI\\NDVI_",ndviYear[i],".tif"))
}
NDVIraster[[1]]@crs #CRS


#####Resolving name discrepancies####
g1966@data$GLACNAME
g2015@data$GLACNAME
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))
#####Plotting####
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
plot(g1966, col="tan3", border=NA, add=TRUE) #add polygons to plot
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin") #Zooming into the plot
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)
par(mfrow=c(1,2)) #plotting 2003 NDVI and 1966 glaciers side by side
plot(NDVIraster[[1]])
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
plot(g1966, col="red", border=NA, add=TRUE)

#####reproject the glaciers using the NDVI projection####
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs) #spTransform(file to project, new coordinate system)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)
rm(g1966,g1998,g2005,g2015)
#####plotting 2015 NDVI and 2015 glaciers on the same map####
par(mfrow=c(1,1))
plot(NDVIraster[[13]],axes=F) #1-14 for years 2003-2016 so 13=2015
plot(g2015p, border="black", add=TRUE,axes=F)
#####calculate area for all polygons add directly into data table for each shapefile####
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full") #joining into a seperate data table for analysis
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")
rm(gAllp1,gAllp2) #remove intermediary joins

#####Plot of area change over the years for all glaciers#####
plot(c(1966,1998,2005,2015), #this first plot is just overall glacier area for the years
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){ #this add points for each glacier for each year
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
}   
#####Calculate % change from 1966-2015 and plot the ploygons####
g2015p@data$areaCNHG <- (1-(area(g2015p)/area(g1966p)))*100 #calculate new variable for change 
my.palette <- brewer.pal(n = 7, name = "OrRd") #custom color ramp
spplot(g2015p,"areaCNHG",col.regions = my.palette, cuts = 6, 
       col = "transparent",main="% Change in Glacier Size 1966-2015") #spatial plot using custom ramp
#####Plotting the difference polygon between 1966-2015####
diffPoly <- gDifference(g1966p, g2015p, checkValidity = 2L) #removes overlap areas
plot(diffPoly)
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)#plot with NDVI
plot(diffPoly,col="black", border=NA,add=TRUE)

#####Plot of Glacier with largest % loss####
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full") #redoing earlier joins to get table with names AND % change
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")
rm(gAllp1,gAllp2) #remove intermediary joins
View(gALL)#we see here that the glacier with the largest % loss is Boulder glacier

bg1966<-subset(g1966p,g1966p@data$GLACNAME=="Boulder Glacier") #subsetting to boulder for each year
bg1998<-subset(g1998p,g1966p@data$GLACNAME=="Boulder Glacier")
bg2005<-subset(g2005p,g1966p@data$GLACNAME=="Boulder Glacier")
bg2015<-subset(g2015p,g1966p@data$GLACNAME=="Boulder Glacier")

rgbP<-projectRaster(rgbL,crs=NDVIraster[[1]]@crs) #reprojecting the background raster brick

par(xpd=TRUE) #allows drawing outside of the plot area for legend
par(mai=c(1.5,1.5,1.5,1.5)) #changed the margins to make room for legend
plotRGB(rgbP,ext=extent(bg1966)+1500,stretch="lin", axes=T,main="Boulder Glacier: 85% Loss, 1966-2015")# background
plot(bg1966,border="red", add=TRUE) #add polygons to plot
plot(bg1998, col="blue", add=TRUE, border=NA)
plot(bg2005, col="lightblue", add=TRUE, border=NA)
plot(bg2015, col="white", add=TRUE,border=NA)
legend(x=-77825,y=107948,c("1966","1998","2005","2015"),fill = c("white","blue","lightblue","white"), #custom legend
       border=c("red","black","black","black"),title = "Glacier Extent",bty =F)


rm(bg1966,bg1998,bg2005,bg2015,rgbP) #clear environment when done

#####Glacial Retreat and NDVI####
NDVIdiff <- list() #extract NDVI values
meanDiff <- numeric(0) #loop through all NDVI years

for(i in 1:length(ndviYear)){  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b", #plotting 
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)


NDVIstack <- stack(NDVIraster)#designate that NDVIraster list is a stack

#set up lm function to apply to every cell where x is the value of a cell. need to skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)
plot(g2015p,add=T)#overlay glaciers 

#buffering around the glaciers 
#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units
#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)#raster of just glaciers
glacZones <- buffRaster - glacRaster #subtract buffer from original glacier
plot(glacZones)

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply

#####Mean Change + 2015 glaciers####
g2015p@data$mnCHNG<-meanChange[2:40,2] #this subsetting necessary to get just the NDVI change per uear values
spplot(g2015p,"mnCHNG",col.regions = my.palette, cuts = 6, 
       col = "transparent",main="Mean NDVI Change Around Glacier per Year, 2003-2016") 

avNDVI<-calc(NDVIstack,mean) #mean max NDVI across the years
pal <- colorRampPalette(c("blue", "red"))#color ramp
pal(4)

#what is the average ?
sum(avNDVI@data@values,na.rm=T)/length(avNDVI@data@values[!is.na(avNDVI@data@values)]) ## sum of all non n/a measures divided by number of non n/a measures

min(g2015p@data$mnCHNG) # used min and range to get the values to break ifelse statements in plot color argument
range(g2015p@data$mnCHNG)

#Final plot of mean NDVI change w/in 500m over all years
par(xpd=T) #allows drawing outside of the plot area for legend
par(mai=c(0.5,0.5,0.5,0.5)) #changed the margins to make room for legend
plot(avNDVI,main="Mean Max NDVI Change within 500m of Glaciers, Glacier National Park 2003-2016")
plot(g2015p,col=ifelse(g2015p@data$mnCHNG >-0.00146694 & g2015p@data$mnCHNG <= (-0.001466943+0.001540251),"#0000FF", 
                       ifelse(g2015p@data$mnCHNG > (-0.001466943+0.001540251) & g2015p@data$mnCHNG <= (-0.001466943+(0.001540251*2)),"#5500AA",
                              ifelse(g2015p@data$mnCHNG > (-0.001466943+(0.001540251*2)) & g2015p@data$mnCHNG <= (-0.001466943+(0.001540251*3)),
                                     "#AA0055","#FF0000"))),border=NA,add=TRUE)
legend("topright",c("-0.0014-0.00007","0.00007-0.0016","0.0016-0.0032","0.0032-0.0045"),
       fill=c(pal(4)),title="NDVI Change",cex=0.85,bty ="n")
text(x=14567.507,y=101096.4 ,"NDVI",xpd=T)
