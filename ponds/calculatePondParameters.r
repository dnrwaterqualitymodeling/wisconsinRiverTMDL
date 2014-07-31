library(xlsx)
library(rgdal)

wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)

lake_volume_data = read.xlsx("ponds/WRT_07_19_13.xlsx", sheetName="data")
lake_volume_data$Volume..acre.ft.[lake_volume_data$Volume..acre.ft. == 0] = NA
lake_volume_data$Max.Depth..ft.[lake_volume_data$Max.Depth..ft.] = NA
wb = readOGR("ponds/waterbodies.gdb", "lake_pond")
watersheds = readOGR("Watersheds/HUC_Subwatersheds", "WRB_HUC16")

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

area_ind = which(is.na(wb_vol$Name))
area_depth_ind = which(!is.na(wb_vol$Name) & !is.na(wb_vol$Max.Depth..ft.))

wb_vol$Area..acres.[area_ind] = wb_vol$SHAPE_Area[area_ind] / 4046.86

# Predict lake volume where volume is listed as zero and fill in table
wb_vol$Volume..acre.ft.[area_ind] = exp(predict(area_model, wb_vol[area_ind,]))
wb_vol$Volume..acre.ft.[area_depth_ind] = exp(predict(area_depth_model, wb_vol[area_depth_ind,]))


