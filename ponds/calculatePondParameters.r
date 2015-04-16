# library(xlsx)
library(rgdal)
library(rgeos)
library(raster)

wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)
lake_volume_data = read.csv("ponds/WRT_07_19_13.csv")
file_wb = paste(wd, "ponds/processing_files/ponds_clipped_to_basin.shp", sep="/")
file_watersheds = paste(wd, "Watersheds/HUC_Subwatersheds/WRB_HUC16_WTM_no_buffer.shp", sep="/")
file_subbasins = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
file_dem = paste(wd, "DEM/raw_prj_10_m.img", sep="/")
file_demFill = paste(wd, "DEM/filled_dem_hydro_burned.img", sep="/")
file_out = "ponds/pond_geometry_8.csv"
subbasin_range = 296:337

td = tempdir()

# Erase urban boundaries from waterbody and watershed layers and convert back to
# spatialPolygonsDataFrame (otherwise saved in spatialPolygons)
subbasins = readOGR(
	dirname(file_subbasins),
	strsplit(basename(file_subbasins), "\\.")[[1]][1])
wb = readOGR(
	dirname(file_wb),
	strsplit(basename(file_wb), "\\.")[[1]][1])

watersheds = readOGR(
	dirname(file_watersheds),
	strsplit(basename(file_watersheds), "\\.")[[1]][1])
dem = raster(file_dem)
demFill = raster(file_demFill)
projection(wb) = projection(dem)
projection(subbasins) = projection(dem)
projection(watersheds) = projection(dem)

# lake_volume_data = read.xlsx("ponds/WRT_07_19_13.xlsx", sheetName="data")
lake_volume_data$Volume..acre.ft.[lake_volume_data$Volume..acre.ft. == 0] = NA
lake_volume_data$Max.Depth..ft.[lake_volume_data$Max.Depth..ft. == 0] = NA

wb_vol = merge(wb,
    lake_volume_data,
    by.x="WATERBOD_2",
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

area_ind = which(is.na(wb_vol$Name))
area_depth_ind = which(!is.na(wb_vol$Name) & !is.na(wb_vol$Max.Depth..ft.))

wb_vol$Area..acres.[area_ind] = wb_vol$SHAPE_Area[area_ind] / 4046.86

# Predict lake volume where volume is listed as zero and fill in table
wb_vol$Volume..acre.ft.[area_ind] = exp(predict(area_model, wb_vol[area_ind,]))
wb_vol$Volume..acre.ft.[area_depth_ind] = exp(predict(area_depth_model, wb_vol[area_depth_ind,]))

wb_ll = subset(wb, LANDLOCK_C == 1)
watersheds_ll = subset(watersheds, CATCHID %in% wb_ll@data$HYDROID)
file_local_wb_ll = tempfile(pattern="wb_ll_", fileext=".shp")
wb_ll@data = data.frame(HYDROID=wb_ll@data$HYDROID)
writeOGR(
	wb_ll,
	td,
	strsplit(basename(file_local_wb_ll),"\\.")[[1]][1],
	"ESRI Shapefile")

catchids_ll = watersheds_ll@data$CATCHID
end = F
while (!end) {
    count = length(catchids_ll)
    upstream_catchids = with(watersheds@data,
		CATCHID[TOCATCHID %in% catchids_ll])
	catchids_ll = c(catchids_ll, upstream_catchids)
	catchids_ll = unique(catchids_ll)
    if (length(catchids_ll) == count) {
        end = T
    }
}
watersheds_ll = subset(watersheds, CATCHID %in% catchids_ll)
if (!file.exists("ponds/landlocked_watersheds.shp")) {
	writeOGR(watersheds_ll,
		"ponds",
		"landlocked_watersheds",
		driver = "ESRI Shapefile")
}
file_local_watershed = tempfile(pattern="watersheds_ll_", fileext=".shp")
writeOGR(watersheds_ll,
	td,
	strsplit(basename(file_local_watershed), "\\.")[[1]][1],
	driver = "ESRI Shapefile")


geometry_table = data.frame()
#for (s in subbasins@data$Subbasin) {
for (s in subbasin_range) {
#    if (s == 3) {break}
    print(paste("Subbasin number", s))
	
	file_py = tempfile(pattern="geoprocessing_", fileext=".py")
    subbasin = subset(subbasins, Subbasin == s)
	file_subbasin = tempfile(pattern="subbasin_", fileext=".shp")
	file_contained_watersheds = tempfile(
		pattern="contained_watersheds_",
		fileext=".shp")
	file_dissolve_watersheds = tempfile(
		pattern="dissolve_watersheds_",
		fileext=".shp"
	)
	writeOGR(subbasin, td,
		strsplit(basename(file_subbasin), "\\.")[[1]][1],
		"ESRI Shapefile")
	ln1 = "import arcpy"
	ln2 = paste(
		"arcpy.Intersect_analysis([r'",
		file_local_watershed,
		"',r'",
		file_subbasin,
		"'],r'",
		file_contained_watersheds,
		"')",
		sep="")
	ln3 = paste(
		"arcpy.Dissolve_management(r'",
		file_contained_watersheds,
		"',r'",
		file_dissolve_watersheds,
		"','GRIDCODE')",
		sep=""
	)
	code = paste(ln1,ln2,ln3,sep="\n")
	writeLines(code,file_py)
	cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", file_py)
	system(cmd)
	
	err.wat = try({
		dissolve_watersheds = readOGR(td,
			strsplit(basename(file_dissolve_watersheds), "\\.")[[1]][1])
	}, silent=T)
	if (class(err.wat) == "try-error"){ 
		print(paste("Skipping", s))
		next 
	}
#	if (length(dissolve_watersheds) == 0) {next}
	
	file_contained_ponds = tempfile(
		pattern="contained_ponds_",
		fileext=".shp")
	
	ln1 = "import arcpy"
	ln2 = paste(
		"arcpy.Intersect_analysis([r'",
		file_dissolve_watersheds,
		"',r'",
		file_local_wb_ll,
		"'],r'",
		file_contained_ponds,
		"')",
		sep="")
	code = paste(ln1,ln2,sep="\n")
	writeLines(code,file_py)
	cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", file_py)
	system(cmd)
	
	err.pnd = try({
		contained_ponds = readOGR(td,
			strsplit(basename(file_contained_ponds), "\\.")[[1]][1])
	},silent=T)
	if (class(err.pnd) == "try-error"){
		print(paste("Skipping", s))
		next 
	}
#	if (length(contained_ponds) == 0) {next}
	
	contained_ponds_df = contained_ponds@data
	contained_ponds_df = merge(
		contained_ponds_df,
		wb_vol)
	
    e = alignExtent(dissolve_watersheds, dem)
    mask_dem = mask(crop(dem, e), dissolve_watersheds)
    mask_dem_fill = mask(crop(demFill, e), dissolve_watersheds)
    fill_height = mask_dem_fill - mask_dem	
	mask_sinks = mask(crop(fill_height, e), dissolve_watersheds)
	
	sink_vals = getValues(mask_sinks)
	sink_vol = sum(sink_vals * 100, na.rm=T)
	sink_vals[sink_vals > 0] = 1
	sink_vals[sink_vals == 0] = NA
	sink_extent = mask_sinks
	sink_extent = setValues(sink_extent, sink_vals)
	
	file_sink_extent = tempfile(
		pattern="sink_extent_",
		fileext=".img")
	file_sink_poly = tempfile(
		pattern="sink_poly_",
		fileext=".shp")
	writeRaster(sink_extent, file_sink_extent, "HFA", datatype="INT2S")
	ln1 = "import arcpy"
	ln2 = paste(
		"arcpy.RasterToPolygon_conversion(r'",
		file_sink_extent,
		"',r'",
		file_sink_poly,
		"', 'NO_SIMPLIFY')",
		sep="")
	code = paste(ln1,ln2,sep="\n")
	writeLines(code,file_py)
	cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", file_py)
	system(cmd)
	
	sink_extent = readOGR(
		td,
		strsplit(basename(file_sink_poly), "\\.")[[1]][1])
	
	contained_ponds = gUnionCascaded(contained_ponds)
	sink_extent = gDifference(sink_extent, contained_ponds, drop_lower_td = T)

    row = data.frame(
        subbasin = subbasin@data$Subbasin,
        PND_FR = gArea(dissolve_watersheds) / gArea(subbasin),
        PND_PSA = sum(contained_ponds_df$Area..acres. * 0.404686, na.rm=T),
        PND_PVOL = sum(contained_ponds_df$Volume..acre.ft., na.rm=T) * 0.123348184,
        PND_ESA = sum(contained_ponds_df$Area..acres. * 0.404686, na.rm=T) +
			(gArea(sink_extent) / 1e4),
        PND_EVOL = (sum(contained_ponds_df$Volume..acre.ft., na.rm=T) * 0.123348184) + 
            (sink_vol / 1e4)
    )
    geometry_table = rbind(geometry_table, row)
}

write.csv(geometry_table, file=file_out, row.names=F)
