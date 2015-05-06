dir_load = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads/SS_00530"
dir_out = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_loads"
gage_subbasin_lu =
	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
	colClasses=c("character", "character", "character", "integer", "integer", "character"))

gage_subbasin_lu = subset(gage_subbasin_lu, LOAD_ID != "")
