library(rgdal)
library(rgeos)
library(raster)

presumed_depth_of_wetlands = 0.5 # meters

mi_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs"
wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)

optFillExe = "T:\\Projects\\Wisconsin_River\\Code\\general_software/OptimizedPitRemoval.exe"

subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
                    "aggregate_subsheds_V6")
watersheds_ll = readOGR("ponds", "landlocked_watersheds")

if (file.exists("C:/TEMP/dem.grd") == F) {
    dem = raster(paste(mi_dir, "DEM/30m_dem_wrb_2milebuffer.img", sep="/"))
    writeRaster(dem, "C:/TEMP/dem.grd", "raster", overwrite=T)
    dem = raster("C:/TEMP/dem.grd")
} else {
    dem = raster("C:/TEMP/dem.grd")
}
if (file.exists("C:/TEMP/lc_lm.grd") == F) {
    lc_lm = raster(paste(mi_dir, "LandCoverLandManagement/landcoverlandmanagement.img", sep="/"))
    writeRaster(lc_lm, "C:/TEMP/lc_lm.grd", "raster", overwrite=T)
    lc_lm = raster("C:/TEMP/lc_lm.grd")
} else {
    lc_lm = raster("C:/TEMP/lc_lm.grd")
}

proj4string(subbasins) = proj4string(dem)

py_file = gsub("\\\\", "/", tempfile(pattern="watershed_", fileext=".py"))
py_prefix = paste(
    "import arcpy;",
    "from arcpy.sa import *;",
    "from arcpy import env;",
    "env.overwriteOutput = True;",
    "arcpy.CheckOutExtension('Spatial');",
    sep=""
)
dem_asc = gsub("\\\\", "/", tempfile(pattern="dem_", fileext=".asc"))
dem_tif = gsub("\\\\", "/", tempfile(pattern="dem_", fileext=".tif"))
optim_fill_asc = gsub("\\\\", "/", tempfile(pattern="optim_fill_", fileext=".asc"))
optim_fill_tif = gsub("\\\\", "/", tempfile(pattern="optim_fill_", fileext=".tif"))
fill_tif = gsub("\\\\", "/", tempfile(pattern="fill_", fileext=".tif"))
fdr_tif = gsub("\\\\", "/", tempfile(pattern="fdr_", fileext=".tif"))
sd8_tif = gsub("\\\\", "/", tempfile(pattern="sd8_", fileext=".tif"))
seed_tif = gsub("\\\\", "/", tempfile(pattern="seed_", fileext=".tif"))
watershed_tif = gsub("\\\\", "/", tempfile(pattern="watershed_", fileext=".tif"))
# 7 is woody wetlands
# 8 is herbaceous wetlands
# 9 is cranberries

geometry_table = data.frame()
for (s in subbasins@data$Subbasin) {
    subbasin = subset(subbasins, Subbasin == s)
    e = alignExtent(subbasin, lc_lm)
    mask_dem = mask(crop(dem, e), subbasin)
    mask_lc_lm = mask(crop(lc_lm, e), subbasin)
    mask_lc_lm = reclassify(mask_lc_lm, 
        rbind(
            c(-Inf,6.5,NA),
            c(6.5,9.5,1),
            c(9.5,Inf,NA)
        )
    )    
    # write dem to ASCII file
    writeRaster(mask_dem, dem_asc, format="ascii", overwrite=T)
    writeRaster(mask_dem, dem_tif, format="GTiff", options=c("COMPRESS=NONE"), overwrite=T)
    # Create optimized fill file
    optim_fill_cmd = paste(optFillExe, '-z', dem_asc, '-fel', optim_fill_asc, '-mode',
        'bal', '-step', '0.1')
    optim_fill_stdout = system(optim_fill_cmd)
    # Read in optimized fill file and define projection
    optim_fill = raster(optim_fill_asc)
    proj4string(optim_fill) = proj4string(mask_dem)
    # Write optimized fill to tiff for watershed analysis
    writeRaster(optim_fill, optim_fill_tif, format="GTiff", options=c("COMPRESS=NONE"), overwrite=T)
    # Fill pits in the standard way
    fill_cmd = paste("mpiexec -n 4 pitremove -z", dem_tif, "-fel", fill_tif)
    fill_stdout = system(fill_cmd) 
    # D8 flow directions
    fdr_cmd = paste("mpiexec -n 4 D8Flowdir -fel", optim_fill_tif, "-p", fdr_tif, "-sd8", sd8_tif)
    fdr_stdout = system(fdr_cmd)
    fdr = raster(fdr_tif)
    fdr = reclassify(fdr,
        rbind(
            c(-Inf,1.5,1), # 1
            c(1.5,2.5,128), 
            c(2.5,3.5,64), 
            c(3.5,4.5,32),
            c(4.5,5.5,16),
            c(5.5,6.5,8),
            c(6.5,7.5,4),
            c(7.5,Inf,2)
        )
    )
    writeRaster(fdr, fdr_tif, format="GTiff", options=c("COMPRESS=NONE"),
        overwrite=T, datatype='INT1U')
    writeRaster(mask_lc_lm, seed_tif, format="GTiff", options=c("COMPRESS=NONE"),
        overwrite=T,datatype='INT1U')
    py_code = paste(py_prefix,
        "w = Watershed(Raster('",
        fdr_tif,
        "'), Raster('",
        seed_tif,
        "'));w.save('",
        watershed_tif,
        "')",
        sep=""
    )
    write(py_code, py_file)
    py_cmd = paste("python", py_file)
    py_stdout = system(py_cmd)
    watershed = raster(watershed_tif)
    if (any(gContains(subbasin, watersheds_ll, byid=T)[,1])) {
        contained_watersheds_ll = subset(watersheds_ll, gContains(subbasin, watersheds_ll, byid=T)[,1])
        contained_watersheds_ll_rast = rasterize(contained_watersheds_ll, watershed)
        watershed[!is.na(contained_watersheds_ll_rast)] = NA
    }
    wet_fr = (length(which(!is.na(getValues(watershed)))) * 900) / gArea(subbasin)
    wet_nsa = length(which(!is.na(getValues(mask_lc_lm)))) * 0.09
    wet_nvol = wet_nsa * presumed_depth_of_wetlands
    wet_vol = wet_nvol
    fill = raster(fill_tif)
    diff = fill - mask_dem
    diff_bin = diff
    diff_bin[diff_bin > 0] = 1
    diff_bin[diff_bin == 0] = NA
    fill_poly = rasterToPolygons(diff_bin)
    if (any(gContains(subbasin, watersheds_ll, byid=T)[,1])) {
        if (any(gIntersects(contained_watersheds_ll, fill_poly, byid=T)[,1] == F)) {
            fill_poly = subset(fill_poly,
                gIntersects(contained_watersheds_ll, fill_poly, byid=T)[,1] == F)
        }
    }
    wetlands_poly = gUnionCascaded(rasterToPolygons(mask_lc_lm))
    if (any(gIntersects(wetlands_poly, fill_poly, byid=T)[,1])) {
        fill_poly = subset(fill_poly, gIntersects(wetlands_poly, fill_poly, byid=T)[,1])
    }
    wet_mxsa = wet_nsa + gArea(fill_poly) * 0.0001
    fill_intersect = rasterize(fill_poly, fill, "layer")
    depths = getValues(diff)[!is.na(getValues(fill_intersect))]
    vols = depths * 0.09
    wet_mxvol = sum(vols) + wet_nvol
    
    row = data.frame(
        subbasin = subbasin@data$Subbasin,
        WET_FR = wet_fr,
        WET_NSA = wet_nsa,
        WET_NVOL = wet_nvol,
        WET_VOL = wet_vol,
        WET_MXSA = wet_mxsa,
        WET_MXVOL = wet_mxvol
    )
    geometry_table = rbind(geometry_table, row)
#     break
}
write.csv(geometry_table, file="wetlands/wetland_geometry.csv", row.names=F)

