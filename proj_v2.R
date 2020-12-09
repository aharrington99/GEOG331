library(maptools)
library(dismo)
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(DescTools)

ext1<-extent(-170,-40,15,75) #north america

#Bringing the data in from GBIF
lud<-gbif("dryopteris","ludoviciana*",geo=TRUE)
gol<-gbif("dryopteris","goldiana*",geo=TRUE)
cel<-gbif("dryopteris","celsa*",geo=TRUE)
cris<-gbif("dryopteris","cristata*",geo=TRUE,ext = ext1 )
exp<-gbif("dryopteris","expansa*",geo=TRUE,ext = ext1 )
cli<-gbif("dryopteris","clintoniana*",geo=TRUE,ext = ext1 )
int<-gbif("dryopteris","intermedia*",geo=TRUE,ext = ext1 )
cam<-gbif("dryopteris","campyloptera*",geo=TRUE,ext = ext1 )
car<-gbif("dryopteris","carthusiana*",geo=TRUE,ext = ext1 )
fil<-gbif("dryopteris","filix-mas*",geo=TRUE,ext=ext1)
mar<-gbif("dryopteris","marginalis*",geo=TRUE,ext=ext1)

#Coordinate cleaning
ludxy<-subset(lud,select=c(lon,lat)) #subsettung to just xy data
golxy<-subset(gol,select=c(lon,lat))
celxy<-subset(cel,select=c(lon,lat))
crixy<-subset(cris,select=c(lon,lat))
expxy<-subset(exp,select=c(lon,lat))
clixy<-subset(cli,select=c(lon,lat))
intxy<-subset(int,select=c(lon,lat))
camxy<-subset(cam,select=c(lon,lat))
carxy<-subset(car,select=c(lon,lat))
filxy<-subset(fil,select=c(lon,lat))
marxy<-subset(mar,select=c(lon,lat))
rm(lud,cel,gol,cris,exp,int,cli,cam,car,fil,mar)

#Preparing the climate data
r<-crop(getData("worldclim",var="bio",res=2.5),ext1) #world clim data 2.5 is resoultion (units?)
corr<-layerStats(r, 'pearson', na.rm=T)
corr_matrix=corr$'pearson correlation coefficient'
v<-FindCorr(corr_matrix,0.85) #correlation matrix
predictors<-r[[-v]] #removes highly correlated variables
rm(r,corr_matrix,corr)

#extract value, coerce to DF, add species name variable, combine into single DF
lud.dat<-raster::extract(predictors,ludxy) #extract value
lud.dat<-data.frame(lud.dat) #data frame
lud.dat$sp<-rep("ludoviciana",nrow(lud.dat)) #species
lud.dat$id<-row(lud.dat) #unique id #
lud.dat$ploid<-rep("2",nrow(lud.dat))# ploidy level
lud.dat<-na.omit(lud.dat)

gol.dat<-raster::extract(predictors,golxy)
gol.dat<-data.frame(gol.dat)
gol.dat$sp<-rep("goldiana",nrow(gol.dat))
gol.dat$id<-row(gol.dat)
gol.dat$ploid<-rep("2",nrow(gol.dat))
gol.dat<-na.omit(gol.dat)

cel.dat<-raster::extract(predictors,celxy)
cel.dat<-data.frame(cel.dat)
cel.dat$sp<-rep("celsa",nrow(cel.dat))
cel.dat$id<-row(cel.dat)
cel.dat$ploid<-rep("4",nrow(cel.dat))
cel.dat<-na.omit(cel.dat)

car.dat<-raster::extract(predictors,carxy)
car.dat<-data.frame(car.dat) #data frame
car.dat$sp<-rep("carthusiana",nrow(car.dat)) #species
car.dat$id<-row(car.dat) #unique id #
car.dat$ploid<-rep("4",nrow(car.dat))

cri.dat<-raster::extract(predictors,crixy)
cri.dat<-data.frame(cri.dat) #data frame
cri.dat$sp<-rep("cristata",nrow(cri.dat)) #species
cri.dat$id<-row(cri.dat) #unique id #
cri.dat$ploid<-rep("4",nrow(cri.dat))

exp.dat<-raster::extract(predictors,expxy)
exp.dat<-data.frame(exp.dat) #data frame
exp.dat$sp<-rep("expansa",nrow(exp.dat)) #species
exp.dat$id<-row(exp.dat) #unique id #
exp.dat$ploid<-rep("2",nrow(exp.dat))

int.dat<-raster::extract(predictors,intxy)
int.dat<-data.frame(int.dat) #data frame
int.dat$sp<-rep("intermedia",nrow(int.dat)) #species
int.dat$id<-row(int.dat) #unique id #
int.dat$ploid<-rep("2",nrow(int.dat))

cam.dat<-raster::extract(predictors,camxy)
cam.dat<-data.frame(cam.dat) #data frame
cam.dat$sp<-rep("campyloptera",nrow(cam.dat)) #species
cam.dat$id<-row(cam.dat) #unique id #
cam.dat$ploid<-rep("4",nrow(cam.dat))

cli.dat<-raster::extract(predictors,clixy)
cli.dat<-data.frame(cli.dat) #data frame
cli.dat$sp<-rep("clintoniana",nrow(cli.dat)) #species
cli.dat$id<-row(cli.dat) #unique id #
cli.dat$ploid<-rep("4",nrow(cli.dat))

fil.dat<-raster::extract(predictors,filxy)
fil.dat<-data.frame(fil.dat) #data frame
fil.dat$sp<-rep("filix-mas",nrow(fil.dat)) #species
fil.dat$id<-row(fil.dat) #unique id #
fil.dat$ploid<-rep("4",nrow(fil.dat))

mar.dat<-raster::extract(predictors,marxy)
mar.dat<-data.frame(mar.dat) #data frame
mar.dat$sp<-rep("marginalis",nrow(mar.dat)) #species
mar.dat$id<-row(mar.dat) #unique id #
mar.dat$ploid<-rep("2",nrow(mar.dat))

#compiling all data into one object
all.dat<-rbind(cel.dat,gol.dat,lud.dat,car.dat,cri.dat,cli.dat,exp.dat,int.dat,cam.dat,fil.dat,mar.dat)
all.dat<-rbind(cel.dat,gol.dat,lud.dat,cri.dat,cli.dat,exp.dat,int.dat,cam.dat)
names(all.dat)<-c("MnDiRange","ISO","AnTRange","MnTWetQ","PreWetMnth","PreDryMnth","PreSzn","PreWrmQ",
                  "PreCldQ","sp","id","ploid")
all.dat$trio<-ifelse(all.dat$sp=="celsa"|all.dat$sp=="ludoviciana"|all.dat$sp=="goldiana","CLG",
                     ifelse(all.dat$sp=="intermedia"|all.dat$sp=="expansa"|all.dat$sp=="campyloptera","ICE","other"))
all.dat$ploid<-as.factor(all.dat$ploid)

#Subseeting each for training and testing
mar.fold <- kfold(marxy, k=5) #20% training and test split, may need to adjust for smaller datasets
mar.test <- marxy[mar.fold == 1, ] #test set
mar.train <- marxy[mar.fold != 1, ] #training set
rm(mar.fold)

cel.fold <- kfold(celxy, k=2) #50% training and test split, may need to adjust for smaller datasets
cel.test <- celxy[cel.fold == 1, ] #test set
cel.train <- celxy[cel.fold != 1, ] #training set
rm(cel.fold)

gol.fold <- kfold(golxy, k=5) #20% training and test split, may need to adjust for smaller datasets
gol.test <- golxy[gol.fold == 1, ] #test set
gol.train <- golxy[gol.fold != 1, ] #training set
rm(gol.fold)

lud.fold <- kfold(ludxy, k=5) #20% training and test split, may need to adjust for smaller datasets
lud.test <- ludxy[lud.fold == 1, ] #test set
lud.train <- ludxy[lud.fold != 1, ] #training set
rm(lud.fold)

int.fold <- kfold(intxy, k=5) #20% training and test split, may need to adjust for smaller datasets
int.test <- intxy[int.fold == 1, ] #test set
int.train <- intxy[int.fold != 1, ] #training set
rm(int.fold)

exp.fold <- kfold(expxy, k=5) #20% training and test split, may need to adjust for smaller datasets
exp.test <- expxy[exp.fold == 1, ] #test set
exp.train <- expxy[exp.fold != 1, ] #training set
rm(exp.fold)

cam.fold <- kfold(camxy, k=5) #20% training and test split, may need to adjust for smaller datasets
cam.test <- camxy[cam.fold == 1, ] #test set
cam.train <- camxy[cam.fold != 1, ] #training set
rm(cam.fold)

cli.fold <- kfold(clixy, k=5) #20% training and test split, may need to adjust for smaller datasets
cli.test <- clixy[cli.fold == 1, ] #test set
cli.train <- clixy[cli.fold != 1, ] #training set
rm(cli.fold)

cri.fold <- kfold(crixy, k=5) #20% training and test split, may need to adjust for smaller datasets
cri.test <- crixy[cri.fold == 1, ] #test set
cri.train <- crixy[cri.fold != 1, ] #training set
rm(cri.fold)

#Celsa raster
cel.m<-maxent(predictors,cel.train)
celmx <- predict(predictors, cel.m, ext=ext1, progress='') #prediction raster

#Ludo raster
lud.m<-maxent(predictors,lud.train)
ludmx <- predict(predictors, lud.m, ext=ext1, progress='') #prediction raster

#Goldiana raster
gol.m<-maxent(predictors,gol.train)
golmx <- predict(predictors, gol.m, ext=ext1, progress='') #prediction raster

#intermedia raster
int.m<-maxent(predictors,int.train)
intmx <- predict(predictors, int.m, ext=ext1, progress='') #prediction raster

#expansa raster
exp.m<-maxent(predictors,exp.train)
expmx <- predict(predictors, exp.m, ext=ext1, progress='') #prediction raster

#campy raster
cam.m<-maxent(predictors,cam.train)
cammx <- predict(predictors, cam.m, ext=ext1, progress='') #prediction raster

#clintoniana raster
cli.m<-maxent(predictors,cli.train)
climx <- predict(predictors, cli.m, ext=ext1, progress='') #prediction raster

#cristata raster
cri.m<-maxent(predictors,cri.train)
crimx <- predict(predictors, cri.m, ext=ext1, progress='') #prediction raster

celmx<-reclassify(celmx,c(0,0.49,0))
ludmx<-reclassify(ludmx,c(0,0.49,0))
golmx<-reclassify(golmx,c(0,0.49,0))
intmx<-reclassify(intmx,c(0,0.49,0))
expmx<-reclassify(expmx,c(0,0.49,0))
cammx<-reclassify(cammx,c(0,0.49,0))
crimx<-reclassify(crimx,c(0,0.49,0))
climx<-reclassify(climx,c(0,0.49,0))

#Rasters plotted
par(mfrow=c(1,1))
plot(celmx, main='D. celsa',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(ludmx, main='D. ludoviciana',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(golmx, main='D. goldiana',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(intmx, main='D. intermedia',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(expmx, main='D. expansa',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(cammx, main='D. campyloptera',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(crimx, main='D. cristata',col=colorRampPalette(c( "gray","blue", "red"))(255))
plot(climx, main='D. clintoniana',col=colorRampPalette(c( "gray","blue", "red"))(255))
e<-evaluate(mar.test,xm,predictors) #model diagnostics code

#Niche overlap tests to obitan Schoener D statistic
a<-nicheEquivalency(exp.test,cam.test,predictors,n=5,model=maxent,verbose = T) #exp vs cam (trio ICE)
b<-nicheEquivalency(int.test,cam.test,predictors,n=5,model=maxent,verbose = T) #int vs cam
c<-nicheEquivalency(lud.test,cel.test,predictors,n=5,model=maxent,verbose = T) #lud vs cel (trio CLG)
d<-nicheEquivalency(gol.test,cel.test,predictors,n=5,model=maxent,verbose = T) #gol vs cel
e<-nicheEquivalency(gol.test,cli.test,predictors,n=5,model=maxent,verbose = T) #gol vs cli (trio GCC)
f<-nicheEquivalency(cri.test,cli.test,predictors,n=5,model=maxent,verbose = T) #cri vs cli

#t-test of of D estimate vs null D estimate
a#exp vs cam (trio ICE)
t.test(a$null.distribution[,1],mu=a$statistic[1])
b#int vs cam
t.test(b$null.distribution[,1],mu=b$statistic[1])
c#lud vs cel (trio CLG)
t.test(c$null.distribution[,1],mu=c$statistic[1])
d#gol vs cel
t.test(d$null.distribution[,1],mu=d$statistic[1])
e#gol vs cli (trio GCC)
t.test(e$null.distribution[,1],mu=e$statistic[1])
f#cri vs cli
t.test(f$null.distribution[,1],mu=f$statistic[1])

###Principal COMP Analyses below
#PCA FOR CLG
all.dat<-na.omit(all.dat)
clg<-all.dat[all.dat$trio=="CLG",]
clg$sp<-as.factor(clg$sp)
clg.pca<-clg[,-10:-13]
test.pca2 <- PCA(clg.pca, graph = FALSE)
fviz_pca_biplot(test.pca2, label = "var",  habillage = clg$sp,addEllipses = TRUE,col.var = "black",repel = TRUE,title = "PCA: D.goldiana-D.celsa-D.ludoviciana")# Concentration ellipses
summary(test.pca2)
#PCA for ICE
ice<-all.dat[all.dat$trio=="ICE",]
ice$sp<-as.factor(ice$sp)
ice.pca<-ice[,-10:-13]
ice.test<- PCA(ice.pca,graph = FALSE)
fviz_pca_biplot(ice.test, label = "var",  habillage = ice$sp,addEllipses = TRUE,col.var = "black",repel = TRUE,title="D.intermedia-D.campyloptera-D.expansa")
summary(ice.test)

##PCA for GCC here (coded as other?)
gcc<-all.dat[all.dat$sp=="goldiana"|all.dat$sp=="cristata"|all.dat$sp=="clintoniana",]
gcc$sp<-as.factor(gcc$sp)
gcc.pca<-gcc[,-10:-13]
gcc.test<- PCA(gcc.pca,graph = FALSE)
fviz_pca_biplot(gcc.test, label = "var",habillage=gcc$sp, addEllipses = TRUE,col.var = "black",repel = TRUE, title="D.goldiana-D.clintoniana-D.cristata")

#Did not end up including these two below
#PCA for all ploidy
all.dat<-na.omit(all.dat)
all.dat$ploid<-as.factor(all.dat$ploid)
all.pca<-all.dat[,-10:-13]
all.test<-PCA(all.pca, graph = F)
fviz_pca_biplot(all.test, label = "var",  habillage = all.dat$ploid,addEllipses = TRUE,col.var = "black",repel = TRUE)
summary(all.test)

#pca for habitat
all.dat2<-all.dat
all.dat2$hab<-ifelse(all.dat$sp=="cristata"|all.dat$sp=="ludoviciana"|
                       all.dat$sp=="celsa"|all.dat$sp=="intermedia"|all.dat$sp=="goldiana"|
                       all.dat$sp=="carthusiana"|all.dat$sp=="clintoniana","Lowlands & Swamp","Upland Forests & Cliffs")

all.dat2$hab<-as.factor(all.dat2$hab)
all.dat2$sp<-as.factor(all.dat2$sp)
all.dat2<-na.omit(all.dat2)
all.dat2$ploid<-as.factor(all.dat2$ploid)
all.pca2<-all.dat2[,-10:-14]
all.test2<-PCA(all.pca2, graph = F)
fviz_pca_biplot(all.test2, label = "var",  habillage = all.dat2$hab,addEllipses = TRUE,col.var = "black",repel = TRUE)
summary(all.test2)
