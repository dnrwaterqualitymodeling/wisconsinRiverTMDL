# TODO: Add comment
# 
# Author: ruesca
###############################################################################
library(RCurl)
library(raster)


url_mean_et = 
	"ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/MOD16A3.105_MERRAGMAO/Geotiff/MOD16A3_ET_2000_to_2013_mean.tif"
file_mean_et = 
	"T:/Projects/Wisconsin_River/GIS_Datasets/Water_Budget/MODIS/MOD16A3_ET_2000_to_2013_mean.tif"

if (!file.exists(file_mean_et)) {
	download.file(url_mean_et, file_mean_et)
}

mean_et = raster(file_mean_et)
