library(rgdal)
library(raster)
library(ggplot2)
library(classInt)
library(RColorBrewer)
library(classInt)
dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Validations/DairyDensity_Comparisons"
setwd(dir)
wi = readOGR("T:/GIS/Statewide_Coverages/Political_Boundaries", "WI_State_Boundary")
wrb = readOGR("T:/Projects/Wisconsin_River/GIS_Datasets/Watersheds", "WRB_Basin")
cafo = readOGR("T:/GIS/Statewide_Coverages/CAFOs/Facility_Locations", "CAFO_Facilities_November2011")
cafo = cafo[-(92:108),]

cafoDens = raster("CAFO_points_densities_2.img")
cafoPts = readOGR("T:/GIS/Statewide_Coverages/DATCP_Dairy_Producers", "DATCP_MILK_PROD")
dairyProdPt = readOGR("T:/GIS/Statewide_Coverages/DATCP_Dairy_Producers", "DATCP_MILK_PROD")
dairyProdFile = paste(dir, "DairyProducerDensity_Statewide.tif", sep="/")
dairyRastFile = paste(dir, "DairyPixelPoints_Density.img", sep="/")
nondairyRastFile = paste(dir, "NonDairyPixelPoints_Density.img", sep="/")

dairyProd = raster(dairyProdFile)
dairyRast = raster(dairyRastFile)
nonDairy = raster(nondairyRastFile)
dairyProd = mask(dairyProd, wi)
dairyRast = mask(dairyRast, wi)
nonDairy = mask(nonDairy, wi)

dairyProdWrb = mask(dairyProd, wrb)
dairyRastWrb = mask(dairyRast, wrb)
nonDairyWrb = mask(nonDairy, wrb)

dairyMod = lm(getValues(dairyProd) ~ getValues(dairyRast))
nondairyMod = lm(getValues(dairyProd) ~ getValues(nonDairy))

valInd = !is.na(getValues(dairyProd)) & !is.na(getValues(dairyRast))
modTable = data.frame(producers=getValues(dairyProd)[valInd],
                      cdl=getValues(dairyRast)[valInd],
                      nondairy=getValues(nonDairy)[valInd])
lm0 = lm(producers ~ cdl, data=modTable)
lm1 = lm(producers ~ nondairy, data=modTable)
modTable$residual = lm0$residuals

residualMap = dairyProd
residualMap[valInd] = modTable$residual
residualMap = mask(residualMap, wi)

pal = brewer.pal(9, name= "YlOrBr")

png(filename=paste(dir, "plots/cdl_and_datcp_raster_images.png", sep="/"),
    , width=8, height=8, units="in", res=300)
par(mfrow=c(2,2))
plot(dairyProd, main="DATCP dairy producers", col="white", legend=F)
plot(dairyProdPt, add=T, cex=0.1, pch=20)
plot(wi, add=T)
plot(dairyProd, col=pal, main="DATCP dairy producer density", legend=F)
plot(wi, add=T)
plot(dairyRast, col=pal, main="CDL dairy rotation density", legend=F)
plot(wi, add=T)
plot(nonDairy, col=pal, main="CDL non-dairy rotation density", legend=F)
plot(wi, add=T)
dev.off()

png(filename = paste(dir,"plots/dairyProducersVsDairyAndNonDairyCdl.png", sep="/"),
    , width=8, height = 4, units="in", res = 300
)
par(mfrow=c(1,2))
plot(producers ~ cdl, data=modTable,
     xlab="CDL Dairy Pixel Density",
     ylab="Dairy Producer Density",
     pch=20,
     cex=0.5)
abline(lm0, col="red", lwd=2)
plot(producers ~ nondairy, data=modTable,
     xlab="CDL Non-dairy Pixel Density",
     ylab="Dairy Producer Density",
     pch=20,
     cex=0.5)
abline(lm1, col="red", lwd=2)
dev.off()

png(filename = paste(dir,"plots/dairyProducersVsDairy_withResiduals.png", sep="/"),
    , width=4, height = 4, units="in", res = 300
)
plot(producers ~ cdl, data=modTable,
     xlab="CDL Dairy Pixel Density",
     ylab="Dairy Producer Density",
     col=as.character(cut(modTable$residual, 4,
             labels=brewer.pal(4, name= "RdYlBu"))),
     cex=0.5,
     pch=20)
dev.off()

png(filename = paste(dir,"plots/dairyProducersVsDairy_residualMap.png", sep="/"),
    , width=8, height = 8, units="in", res = 300
)
plot(residualMap, col=brewer.pal(9, name= "RdYlBu"), legend=F,
     main="DATCP vs. CDL density residual plot")
plot(cafo, add=T, cex=2, pch=20)
dev.off()

plot(producers ~ cdl, data=modTable,
     xlab="CDL Dairy Pixel Density",
     ylab="Dairy Producer Density",
     pch=20,
     cex=0.5)
abline(lm0, col="red", lwd=2)
plot(producers ~ nondairy, data=modTable,
     xlab="CDL Non-dairy Pixel Density",
     ylab="Dairy Producer Density",
     pch=20,
     cex=0.5)
abline(lm1, col="red", lwd=2)

cafoRes = extract(residualMap, cafo, buffer=50000, fun=mean)
shiftCafo = cafoRes + 20
pnorm(log(20), mean=mean(log(shiftCafo), na.rm=T), sd=sd(log(shiftCafo), na.rm=T), lower.tail=F)

modelDiagnosticFile = paste(dir, "modelDiagnosticsComparingDensities.txt", sep="/")
sink(modelDiagnosticFile, append=T)
dairyModWrb = lm(getValues(dairyProdWrb) ~ getValues(dairyRastWrb))
nondairyModWrb = lm(getValues(dairyProdWrb) ~ getValues(nonDairyWrb))
summary(dairyModWrb)
summary(nondairyModWrb)
unlink(modelDiagnosticFile)
