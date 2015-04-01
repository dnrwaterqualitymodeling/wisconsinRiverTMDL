# library(xlsx)
library(rgdal)
library(rgeos)
library(raster)

wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)
lake_volume_data = read.csv("ponds/WRT_07_19_13.csv")
wb_file = paste(wd, "ponds/lake_pond.shp", sep="/")
wb = readOGR("ponds/waterbodies.gdb", "lake_pond")
watersheds = readOGR("Watersheds/HUC_Subwatersheds", "WRB_HUC16_WTM")
subbasins_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins_honoring_hucs.shp"
dem = raster("DEM/raw_prj_10_m.img")
demFill = raster("DEM/filled_dem_hydro_burned.img")
ogr2ogr = "C:\\OSGeo4W64\\bin\\ogr2ogr.exe"

proj4string(watersheds) = proj4string(wb)
proj4string(subbasins) = proj4string(wb)

# Erase urban boundaries from waterbody and watershed layers and convert back to
# spatialPolygonsDataFrame (otherwise saved in spatialPolygons)
subbasins = readOGR(
	dirname(subbasins_file),
	strsplit(basename(subbasins_file))[[1]][1])

wb_df = wb@data
watersheds_df = watersheds@data

wb_clip = tempfile(pattern="wb_clip_", fileext=".shp")
wb_clip = gsub("\\\\", "/", wb_clip)

system(paste(
	ogr2ogr,
	"-clipsrc",
	subbasins_file,
	wb_clip,
	wb_file
	))

wb = gIntersection(
	wb,
	dissolve_subbasins,
	drop_lower_td=T,
	byid=T)
watersheds = gIntersection(
	watersheds,
	dissolve_subbasins,
	drop_lower_td=T,
	byid=T)
watersheds = SpatialPolygonsDataFrame(
	watersheds,
    data=data.frame(
		watersheds_df,
		row.names=row.names(watersheds)))
wb_df = wb_df[row.names(wb_df) %in% row.names(wb),]
wb = SpatialPolygonsDataFrame(wb, data=data.frame(wb_df, row.names=row.names(wb)))

# lake_volume_data = read.xlsx("ponds/WRT_07_19_13.xlsx", sheetName="data")
lake_volume_data$Volume..acre.ft.[lake_volume_data$Volume..acre.ft. == 0] = NA
lake_volume_data$Max.Depth..ft.[lake_volume_data$Max.Depth..ft.] = NA

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

wb@data = wb_vol

wb_ll = wb_vol[which(wb_vol$LANDLOCK_CODE == 1),]
watersheds_ll = subset(watersheds, CATCHID %in% wb_ll$HYDROID)

catchids_ll = watersheds_ll@data$CATCHID
end = F
while (!end) {
    count = length(catchids_ll)
    upstream_catchids = watersheds_ll@data$TOCATCHID %in% catchids_ll
    catchids_ll = unique(c(catchids_ll, upstream_catchids))
    if (length(catchids_ll == count)) {
        end = T
    }
}
watersheds_ll = subset(watersheds, CATCHID %in% catchids_ll)
dissolve_subbasins = gUnionCascaded(subbasins)
watersheds_ll = subset(watersheds_ll, gContains(dissolve_subbasins, watersheds_ll, byid=T)[,1])
writeOGR(watersheds_ll, "ponds", "landlocked_watersheds", driver = "ESRI Shapefile")

geometry_table = data.frame()
for (s in subbasins@data$Subbasin) {
#     if (s == 3) {break}
    print(paste("Subbasin number", s))
    subbasin = subset(subbasins, Subbasin == s)
    # If there are no land-locked watersheds in the subbasin, move onto the next iteration
    if (any(gContains(subbasin, watersheds_ll, byid=T)[,1]) == F) {next}
    contained_watersheds = subset(
		watersheds_ll,
		gContains(
			subbasin,
			watersheds_ll,
			byid=T)[,1])
    dissolve_watersheds = gUnionCascaded(contained_watersheds)
    # If there are no land-locked WATERBODIES in the landlocked watersheds, move onto the next iteration
    if (any(gContains(dissolve_watersheds, wb, byid=T)[,1]) == F) {next}
    contained_ponds = subset(
		wb,
		gContains(
			dissolve_watersheds,
			wb,
			byid=T)[,1])
    e = alignExtent(dissolve_watersheds, dem)
    mask_dem = mask(crop(dem, e), dissolve_watersheds)
    mask_dem_fill = mask(crop(demFill, e), dissolve_watersheds)
    fill_height = mask_dem_fill - mask_dem
    clumps = clump(fill_height > 0, directions=8)
    totalArea = 0
    totalVolumeChange = 0
    for (catchid in contained_watersheds@data$CATCHID) {
        print(paste("assessing emergency geometry within subbasin", catchid))
        w = subset(contained_watersheds, CATCHID==catchid)
        e_w = alignExtent(w, dem)
        mask_clumps = mask(crop(clumps, e_w), w)
        if (all(is.na(getValues(mask_clumps)))) { next }
        clumps_poly = rasterToPolygons(mask_clumps, dissolve=T)
        maxArea = max(gArea(clumps_poly, byid=T))
        totalArea = totalArea + maxArea # square meters
        maxSizeId = clumps_poly@data$clumps[which(gArea(clumps_poly, byid=T) == maxArea)]
        mask_fill_height = mask(crop(fill_height, e_w), w)
        heights = mask_fill_height[mask_clumps == maxSizeId]
        d_vol = sum(heights * 100, na.rm=T)
        totalVolumeChange = totalVolumeChange + d_vol # cubic meters
    }
    row = data.frame(
        subbasin = subbasin@data$Subbasin,
        PND_FR = gArea(contained_watersheds) / gArea(subbasin),
        PND_PSA = gArea(contained_ponds) / 1e4,
        PND_PVOL = sum(contained_ponds$Volume..acre.ft., na.rm=T) * 8.107132,
        PND_ESA = totalArea / 1e4,
        PND_EVOL = (sum(contained_ponds$Volume..acre.ft., na.rm=T) * 8.107132) + 
            (totalVolumeChange / 1e4)
    )
    geometry_table = rbind(geometry_table, row)
}

write.csv(geometry_table, file="ponds/pond_geometry.csv", row.names=F)