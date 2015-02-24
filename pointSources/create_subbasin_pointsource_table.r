library(rgdal)
library(rgeos)
library(foreign)

wgs84 = CRS("+proj=longlat +datum=WGS84")
dir_pt_srcs = "T:/Projects/Wisconsin_River/GIS_Datasets/Point Sources"
dir_swat_inputs = "T:/projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/Hydro"
file_swat_ptsrc_table = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/point_sources/subbasin_pt_src_table.dbf"
file_subbasins_minus = "subbasins_minus_urban_boundaries"

subbasins = readOGR(
	dsn=dir_swat_inputs,
	layer=file_subbasins_minus)

surf_pts = gPointOnSurface(subbasins, byid=T, id=)

# equal to 337, number of subbasins
no_of_iscts = sum(rowSums(gIntersects(subbasins, surf_pts, byid=T)))

wtm_coords = coordinates(surf_pts)
geo_coords = coordinates(spTransform(surf_pts, wgs84))

pt_src_sbsn_tbl = cbind(wtm_coords, geo_coords)
pt_src_sbsn_tbl = as.data.frame(pt_src_sbsn_tbl)
pt_src_sbsn_tbl["TYPE"] = "D"
names(pt_src_sbsn_tbl)[1:4] = c("XPR", "YPR", "LONG", "LAT")

surf_pts = SpatialPointsDataFrame(surf_pts, cbind(pt_src_sbsn_tbl, data.frame(SUBBASIN = 1:337)))

write.dbf(
	pt_src_sbsn_tbl,
	file_swat_ptsrc_table)

# writeOGR(
	# obj=surf_pts,
	# dsn=dir_pt_srcs,
	# layer="pt_source_subbasins_check",
	# driver="ESRI Shapefile",
	# overwrite_layer=TRUE)

