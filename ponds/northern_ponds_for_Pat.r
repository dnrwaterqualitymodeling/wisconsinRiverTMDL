library(rgdal)
library(rgeos)
library(raster)

out_file = "E:/wboi.csv"

wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)
lake_volume_data = read.csv("ponds/WRT_07_19_13.csv")
#file_wb = paste(wd, "ponds/processing_files/ponds_clipped_to_NLaF_ecoregion.shp", sep="/")
file_wb = paste(wd, "Hydrology/Spatial24k03142013_WRB.gdb/lakes_on_network_northern_ecoregion", sep="/")

td = tempdir()

# Erase urban boundaries from waterbody and watershed layers and convert back to
# spatialPolygonsDataFrame (otherwise saved in spatialPolygons)
wb = readOGR(
	dirname(file_wb),
	strsplit(basename(file_wb), "\\.")[[1]][1])
	
#wb = subset(wb, LANDLOCK_C == 0)

# lake_volume_data = read.xlsx("ponds/WRT_07_19_13.xlsx", sheetName="data")
lake_volume_data$Volume..acre.ft.[lake_volume_data$Volume..acre.ft. == 0] = NA
lake_volume_data$Max.Depth..ft.[lake_volume_data$Max.Depth..ft. == 0] = NA

wb_vol = merge(wb,
    lake_volume_data,
    #by.x="WATERBOD_2",
	by.x="WATERBODY_WBIC",
    by.y="WBIC",
    all.x=T,
    all.y=F
)
wb_vol = wb_vol@data

# fit a regression to predict lake volumes for any record where volume is not equal to zero
# for lakes that ARE NOT in lake_volume_data
area_model = lm(log(Volume..acre.ft.) ~ log(Area..acres.), data=wb_vol)
# for lakes that ARE in lake_volume_data
area_depth_model = lm(log(Volume..acre.ft.) ~ log(Area..acres.) + log(Max.Depth..ft.),
    data=wb_vol)

vol_ind = which(wb_vol$Volume..acre.ft. > 0)
area_ind = which(is.na(wb_vol$Name))
area_depth_ind = which(
	!is.na(wb_vol$Name) &
	!is.na(wb_vol$Max.Depth..ft.)
)
	

wb_vol$Area..acres.[area_ind] = wb_vol$Shape_Area[area_ind] / 4046.86

# Predict lake volume where volume is listed as zero and fill in table
wb_vol$Volume..acre.ft.[area_ind] = exp(predict(area_model, wb_vol[area_ind,]))
wb_vol$Volume..acre.ft.[area_depth_ind] = exp(predict(area_depth_model, wb_vol[area_depth_ind,]))

# fill in volume sources, respectively
wb_vol$Volume.Source = 'Area regression'
wb_vol$Volume.Source[area_depth_ind] = 'Area Volume regression'
wb_vol$Volume.Source[vol_ind] = 'Lakes Book'

wboi = data.frame(wb_vol$HYDROID,
	wb_vol$Shape_Area,
	wb_vol$Area..acres.,
	wb_vol$Max.Depth..ft.,
	wb_vol$Mean.Depth..ft.,
	wb_vol$Volume..acre.ft.,
	wb_vol$WRT.med..days.,
	wb_vol$Volume.Source
)
write.csv(wboi, out_file)