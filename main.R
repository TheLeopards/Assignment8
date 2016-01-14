# Author: TheLeopards (Samantha Krawczyk, Georgios Anastasiou)
# 14th January 2016
# Assignment 8: creating linear regression model to predict tree cover

# loading the required library and data
library(raster)

load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")
load("data/trainingPoly.rda")

# getting rid of values above 100 representing classes other than forest
vcfGewata[vcfGewata > 100] <- NA

## building a brick containing all data
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

## extracting all data to a data.frame
df <- as.data.frame(getValues(alldata))

# plotting relationship between the Landsat bands and the VCF tree cover. 
opar <- par(mfrow=c(3,2))
for(i in 1:nlayers(alldata[[1:6]])){
	plot(alldata[[i]], alldata$VCF, pch = ".", col = "darkgreen", main= "Relationship between Landsat bands and VCF")
	warnings()
		}
par(opar)


# model
model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = df)
summary(model)

# prediction using model
predVCF <- predict(alldata, model=model, na.rm=TRUE)

# comparing prediction and original forest cover
opar <- par(mfrow=c(1,2))
plot(predVCF, main="Predicted forest cover", zlim=c(0,100))
plot(vcfGewata, main="Original forest cover")
par(opar)

# calculating RMSE
prepRMSE <- (alldata$VCF-predVCF)^2
meanRMSE <- cellStats(prepRMSE, stat=mean)

RMSE <- sqrt(meanRMSE)
RMSE

# calculating RMSE for trainingpolygons
	# convert SpatialPolygons to raster object to be able to use zonal statistics function
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, alldata, field='Code')
	# applying zonal statistics to RMSE using training polygons
RMSE_classes <- zonal(RMSE, classes, fun=mean, na.rm=TRUE)
	# editing resulting matrix for analysis
RMSE_classes <- cbind(RMSE_classes, c("cropland","forest", "wetland"))
colnames(RMSE_classes) <- c("Zone", "RMSE", "Class")









