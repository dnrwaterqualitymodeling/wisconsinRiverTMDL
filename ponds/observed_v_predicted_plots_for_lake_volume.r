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


setwd("T:/Projects/Wisconsin_River/Presentations/Drafts/midwest_watershed_modeling_users_group_2014/images")

png("area_model_observed_predicted.png", width=8, height=8, units="in", res=600)
plot(area_model$fitted.values ~ area_model$model[["log(Volume..acre.ft.)"]],
	ylab="Predicted volume log(acre-ft)", xlab="Observed volume log(acre-ft)",
	xlim=c(4,12), ylim=c(4,12))
abline(a=0, b=1, col="red")
dev.off()

png("area_depth_model_observed_predicted.png", width=8, height=8, units="in", res=600)
plot(area_depth_model$fitted.values ~ area_depth_model$model[["log(Volume..acre.ft.)"]],
	ylab="Predicted volume log(acre-ft)", xlab="Observed volume log(acre-ft)",
	xlim=c(4,12), ylim=c(4,12))
abline(a=0, b=1, col="red")
dev.off()