



library(raster)


load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")
## band 6 (thermal infra-red) will be excluded from this exercise



## build a brick containing all data
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

## extract all data to a data.frame
df <- as.data.frame(getValues(alldata))

# produce one or more plots that demonstrate the relationship between the Landsat bands and the VCF tree cover. 
# What can you conclude from this/these plot(s)?
opar <- par(mfrow=c(3,2))
for(i in 1:nlayers(alldata[[1:6]])){
	plot(alldata[[i]], alldata$VCF, pch = ".", col = "darkgreen", main= "Relationship between Landsat bands and VCF")
	}
par(opar)

# getting rid of values above 100 which represent clouds etc. (keeping just forest cover)
vcfGewata[vcfGewata > 100] <- NA


# model
model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = df, importance=TRUE)

#summary(alldata$VCF)
summary(model)

# prediction using model
predVCF <- predict(alldata, model=model, na.rm=TRUE)
summary(predVCF)

# plotting prediction and original
opar <- par(mfrow=c(1,2))
plot(predVCF, main="Predicted forest cover", zlim=c(0,100))
plot(vcfGewata, main="Original forest cover")

#RMSE for VCF
RMSE <- sqrt(mean((alldata$VCF-predVCF)^2))
par(opar)
plot(RMSE)

#Using the training polygons from the random forest 
#classification, calculate the RMSE separately for each of the classes and compare.
load("data/trainingPoly.rda")
zonal <- zonal(alldata, trainingPoly, fun=mean)
plot(zonal)
plot(predVCF)
plot(trainingPoly, add=TRUE)
