library(rgdal)
library(rgeos)
library(raster)

dir.p = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/phosphorus"
dir.mod = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro"

file_background_P = "background_P_from_EPZ.txt"

epz = readOGR(dir.p, "wrb_envPzones")
subbasins = readOGR(dir.mod, "subbasins_minus_urban_boundaries")
templateRas = raster("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/swat_lc_wtm.tif")

epzDis = gUnaryUnion(epz, id = epz@data$PTOT_ZN)
epzDis = SpatialPolygonsDataFrame(epzDis,
	data = data.frame(zone = c(0, 1, 2, 4), 
				ref_p = c(NA, 0.032, 0.042, 0.035),
				row.names = row.names(epzDis)))

epzRas = rasterize(epzDis, templateRas, 'ref_p')
plot(epzRas)
epz_sub_df = extract(epzRas, subbasins, fun = 'mean', na.rm = T, df = T)

write.table(epz_sub_df,
    paste(dir.p, 
        file_background_P, 
        sep = '/'),
    row.names = F,
    sep = '\t')