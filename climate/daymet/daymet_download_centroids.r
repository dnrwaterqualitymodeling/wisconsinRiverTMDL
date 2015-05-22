#DAYMET!!!!!!

library(rgdal)
library(sp)
library(rgeos)

dir_out = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/daymet"

source("C:/Users/hydrig/Documents/Projects/wisconsinRiverTMDL/climate/daymet/download.daymet.r")

subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro", "subbasins_minus_urban_boundaries")

centroids = gCentroid(subbasins, byid=TRUE, id=subbasins@data$Subbasin)
centroids = spTransform(centroids, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

setwd(dir_out)

for (subbasin in subbasins@data$Subbasin) {
	centroid = centroids[row.names(centroids) == subbasin,]
	download.daymet(
		site=paste("subbasin_", row.names(centroid), sep=""),
		lat=centroid$y,
		lon=centroid$x,
		start_yr=2002,
		end_yr=2013
	)
}
