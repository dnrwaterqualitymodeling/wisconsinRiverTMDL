# library(xlsx)
library(rgdal)
library(rgeos)
library(raster)

wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)
lake_volume_data = read.csv("ponds/WRT_07_19_13.csv")
wb = readOGR("ponds/waterbodies.gdb", "lake_pond")
watersheds = readOGR("Watersheds/HUC_Subwatersheds", "WRB_HUC16_WTM")
subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
                    "subbasins_minus_urban_boundaries")
dem = raster("DEM/raw_prj_10_m.img")
demFill = raster("DEM/wrb_fill")

# lake_volume_data = read.xlsx("ponds/WRT_07_19_13.xlsx", sheetName="data")
lake_volume_data$Volume..acre.ft.[lake_volume_data$Volume..acre.ft. == 0] = NA
lake_volume_data$Max.Depth..ft.[lake_volume_data$Max.Depth..ft.] = NA
proj4string(watersheds) = proj4string(wb)
proj4string(subbasins) = proj4string(wb)

wb_vol = merge(wb@data,
    lake_volume_data,
    by.x="WATERBODY_WBIC",
    by.y="WBIC",
    all.x=T,
    all.y=F
)

# fit a regression to predict lake volumes for any record where volume is not equal to zero
# for lakes that ARE NOT in lake_volume_data
area_model = lm(log(Volume..acre.ft.) ~ log(Area..acres.), data=wb_vol)
# for lakes that ARE in lake_volume_data
area_depth_model = lm(log(Volume..acre.ft.) ~ log(Area..acres.) + log(Max.Depth..ft.),
    data=wb_vol)


setwd("C:/Users/ruesca/Documents/wisconsin_river_tmdl/Code/doc/img")

pdf("area_model_observed_predicted.pdf", width=3, height=6)
par(mfrow=c(2,1))
plot(area_model$fitted.values ~ area_model$model[["log(Volume..acre.ft.)"]],
	ylab="Predicted volume log(acre-ft)", xlab="Observed volume log(acre-ft)",
	xlim=c(4,12), ylim=c(4,12), pch=20, main="Area Model")
abline(a=0, b=1, col="red")

plot(area_depth_model$fitted.values ~ area_depth_model$model[["log(Volume..acre.ft.)"]],
	ylab="Predicted volume log(acre-ft)", xlab="Observed volume log(acre-ft)",
	xlim=c(4,12), ylim=c(4,12), pch=20, main="Area/Depth Model")
abline(a=0, b=1, col="red")
dev.off()