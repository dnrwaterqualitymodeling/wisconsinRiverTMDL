# library(xlsx)
library(rgdal)
library(rgeos)
library(raster)

wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)
lake_volume_data = read.csv("ponds/WRT_07_19_13.csv")
file_wb = paste(wd, "ponds/processing_files/ponds_clipped_to_basin.shp", sep="/")
file_watersheds = paste(wd, "ponds/processing_files/huc_16s.shp", sep="/")
file_subbasins = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins_honoring_hucs.shp"
file_dem = paste(wd, "DEM/raw_prj_10_m.img", sep="/")
file_demFill = paste(wd, "DEM/filled_dem_hydro_burned.img", sep="/")

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
lake_volume_data$Max.Depth..ft.[lake_volume_data$Max.Depth..ft.] = NA

wb_vol = merge(wb@data,
    lake_volume_data,
    by.x="WATERBOD_2",
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

wb@data = wb_vol

wb_ll = subset(wb, LANDLOCK_C == 1)
watersheds_ll = subset(watersheds, CATCHID %in% wb_ll@data$HYDROID)

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
writeOGR(watersheds_ll, "ponds", "landlocked_watersheds", driver = "ESRI Shapefile")

geometry_table = data.frame()
for (s in subbasins@data$Subbasin) {
#    if (s == 3) {break}
    print(paste("Subbasin number", s))
    subbasin = subset(subbasins, Subbasin == s)
    # If there are no land-locked watersheds in the subbasin, move onto the next iteration
    # if (any(gContains(subbasin, watersheds_ll, byid=T)[,1]) == F) {next}
    # contained_watersheds = subset(
		# watersheds_ll,
		# gContains(
			# subbasin,
			# watersheds_ll,
			# byid=T)[,1])
	# contained_watersheds_df = watersheds_ll@data[gIntersects(subbasin, watersheds_ll, byid=T),]
	contained_watersheds = gIntersection(
		subbasin,
		watersheds_ll,
		byid=T,
		drop_lower_td=T,
		id=as.character(watersheds_ll@data$CATCHID))
	
	if (length(contained_watersheds) == 0) {next}
    dissolve_watersheds = gUnionCascaded(contained_watersheds)
    # If there are no land-locked WATERBODIES in the landlocked watersheds, move onto the next iteration
    # if (any(gContains(dissolve_watersheds, wb_ll, byid=T)[,1]) == F) {next}
    # contained_ponds = subset(
		# wb_ll,
		# gContains(
			# dissolve_watersheds,
			# wb_ll,
			# byid=T)[,1])
	contained_ponds = gIntersection(
		dissolve_watersheds,
		wb_ll,
		byid=T,
		drop_lower_td=T,
		id=as.character(wb_ll@data$HYDROID))
	contained_ponds_df = merge(data.frame(HYDROID=row.names(contained_ponds)), wb_ll)
	contained_ponds = gUnionCascaded(contained_ponds)
	
	if (length(contained_ponds) == 0) {next}
    e = alignExtent(dissolve_watersheds, dem)
    mask_dem = mask(crop(dem, e), dissolve_watersheds)
    mask_dem_fill = mask(crop(demFill, e), dissolve_watersheds)
    fill_height = mask_dem_fill - mask_dem
    clumps = clump(fill_height > 0, directions=8)
    totalArea = 0
    totalVolumeChange = 0
    for (catchid in row.names(contained_watersheds)) {
        print(paste("Assessing emergency geometry within subbasin", catchid))
        # w = subset(contained_watersheds, row.names(contained_watersheds)==catchid)
		w = contained_watersheds[row.names(contained_watersheds)==catchid]
        e_w = alignExtent(w, dem)
        mask_clumps = mask(crop(clumps, e_w), w)
        if (all(is.na(getValues(mask_clumps)))) { next }
        clumps_poly = rasterToPolygons(mask_clumps, dissolve=T)
		clumps_poly = gDifference(clumps_poly, contained_ponds)
        sumArea = sum(gArea(clumps_poly, byid=T))
        totalArea = totalArea + sumArea # square meters
        mask_fill_height = mask(crop(fill_height, e_w), w)
		heights = mask_fill_height[mask_clumps > 0]
        d_vol = sum(heights * 100, na.rm=T)
        totalVolumeChange = totalVolumeChange + d_vol # cubic meters
    }
    row = data.frame(
        subbasin = subbasin@data$Subbasin,
        PND_FR = gArea(contained_watersheds) / gArea(subbasin),
        PND_PSA = sum(contained_ponds_df$Area..acres. * 0.404686, na.rm=T),
        PND_PVOL = sum(contained_ponds_df$Volume..acre.ft., na.rm=T) * 0.123348184,
        PND_ESA = sum(contained_ponds_df$Area..acres. * 0.404686, na.rm=T) + (totalArea / 1e4),
        PND_EVOL = (sum(contained_ponds_df$Volume..acre.ft., na.rm=T) * 0.123348184) + 
            (totalVolumeChange / 1e4)
    )
    geometry_table = rbind(geometry_table, row)
}

write.csv(geometry_table, file="ponds/pond_geometry.csv", row.names=F)