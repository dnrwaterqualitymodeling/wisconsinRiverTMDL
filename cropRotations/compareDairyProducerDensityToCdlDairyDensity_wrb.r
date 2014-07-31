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
# cafo = readOGR("T:/GIS/Statewide_Coverages/CAFOs/Facility_Locations", "CAFO_Facilities_November2011")
# cafo = cafo[-(92:108),]

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

dairyProdPtWrb = dairyProdPt[!is.na(over(dairyProdPt, wrb)[,1]),]
dairyProdWrb = trim(mask(dairyProd, wrb))
dairyRastWrb = trim(mask(dairyRast, wrb))
nonDairyWrb = trim(mask(nonDairy, wrb))

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

png(filename=paste(dir, "plots/cdl_and_datcp_raster_images_wrb.png", sep="/"),
    , width=8, height=8, units="in", res=300)
par(mfrow=c(2,2))
plot(dairyProdWrb, main="DATCP dairy producers", col="white", legend=F)
plot(dairyProdPtWrb, add=T, cex=0.1, pch=20)
plot(wrb, add=T)
plot(dairyProdWrb, col=pal, main="DATCP dairy producer density", legend=F)
plot(wrb, add=T)
plot(dairyRastWrb, col=pal, main="CDL dairy rotation density", legend=F)
plot(wrb, add=T)
plot(nonDairyWrb, col=pal, main="CDL non-dairy rotation density", legend=F)
plot(wrb, add=T)
dev.off()
