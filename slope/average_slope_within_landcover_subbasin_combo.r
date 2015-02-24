# library(RODBC)
library(raster)
library(reshape2)

dir_prj = "C:/Users/ruesca/Desktop/WRB"
lc_lu_file = "T:/Projects/Wisconsin_River/GIS_Datasets/slope"
out_mean_slope_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/slope/subbasin_landuse_mean_slope.txt"

lc_lu = read.csv(lc_lu_file, stringsAsFactors=F)
names(lc_lu) = c("lc", "landuse")

subbasins_file = paste(dir_prj, "Watershed", "Shapes", "subs1.shp", sep="/")
slope_file = paste(dir_prj, "RasterStore.mdb", "Slope", sep="/")
lc_file = paste(dir_prj, "Watershed", "Grid", "landuse2", sep="/")

py_script = gsub("\\\\", "/", tempfile("polygon_to_raster_", fileext=".py"))
local_poly = gsub("\\\\", "/", tempfile("subbasins_", fileext=".shp"))
out_subbasins = gsub("\\\\", "/", tempfile("subbasin_raster_", fileext=".tif"))
out_slope = paste(gsub("\\\\", "/", tempdir()), "Slope.tif", sep="/")
ln1 = "import arcpy"
ln2 = "from arcpy import env"
ln3 = paste('env.snapRaster = "', lc_file, '"', sep="")
ln3 = paste('env.extent = "', lc_file, '"', sep="") 
ln4 = paste(
	'arcpy.CopyFeatures_management("',
	subbasins_file,
	'", "',
	local_poly,
	'")',
	sep="")
ln5 = paste(
	'arcpy.PolygonToRaster_conversion("',
	subbasins_file,
	'", "Subbasin", "',
	out_subbasins,
	'", "MAXIMUM_COMBINED_AREA", "Subbasin", 30)',
	sep="")
ln6 = paste(
	'arcpy.RasterToOtherFormat_conversion("',
	slope_file,
	'", "',
	dirname(out_slope),
	'", "TIFF")',
	sep="")

writeLines(c(ln1, ln2, ln3, ln4, ln5, ln6), py_script)

cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", py_script)
system(cmd)

subbasins = raster(out_subbasins)
lc = crop(raster(lc_file), subbasins)
slope = crop(raster(out_slope), subbasins)

subbasins_v = getValues(subbasins)
lc_v = getValues(lc)
slope_v = getValues(slope)

subbasins_lc = paste(subbasins_v, lc_v, sep="_")
subbasins_lc[grep("NA", subbasins_lc)] = NA

mean_slope = aggregate(slope_v, list(subbasins_lc), mean)
out_data = colsplit(mean_slope$Group.1, "_", c("subbasin", "lc"))
out_data$mean_slope = mean_slope$x
out_data = merge(lc_lu, out_data)

out_data = data.frame(
	subbasin=out_data$subbasin,
	landuse=out_data$landuse,
	mean_slope=out_data$mean_slope
)

write.table(out_data, out_mean_slope_file, row.names=F, sep="\t")

