args <- commandArgs(TRUE)
start_i = args[1]
end_i = args[2]
out_raster_dir = args[3]
out_geometry_file = args[4]
  
# start_i = 2
# end_i = 2
# out_raster_dir = "K:/temp/wetlands_rasters"
# out_geometry_file = "T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry_1.csv"

library(rgdal)
library(rgeos)
library(raster)

rasterOptions(tmpdir="K:/temp")
presumed_depth_of_wetlands = 0.5 # meters
perennial_threshold = 200 # pixels

# out_wetland_ca_file = "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands/wetland_ca.tif"
# out_wetland_mxsa_file = "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands/wetland_mxsa.tif"

mi_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs"
wd = "T:/Projects/Wisconsin_River/GIS_Datasets"
setwd(wd)

optFillExe = "T:\\Projects\\Wisconsin_River\\Code\\general_software/OptimizedPitRemoval.exe"

subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
                    "subbasins_minus_urban_boundaries")
watersheds_ll = readOGR("ponds", "landlocked_watersheds")
watersheds_ll_df = watersheds_ll@data
subbasins_dissolve = gUnionCascaded(subbasins)
watersheds_ll = gIntersection(watersheds_ll, subbasins_dissolve, drop_not_poly=T, byid=T)
watersheds_ll = SpatialPolygonsDataFrame(watersheds_ll,
    data=data.frame(watersheds_ll_df, row.names=row.names(watersheds_ll)))

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
ca_tif = gsub("\\\\", "/", tempfile(pattern="ca_", fileext=".tif"))
seed_tif = gsub("\\\\", "/", tempfile(pattern="seed_", fileext=".tif"))
watershed_tif = gsub("\\\\", "/", tempfile(pattern="watershed_", fileext=".tif"))

# 7 is woody wetlands
# 8 is herbaceous wetlands
# 9 is cranberries

geometry_table = data.frame()
for (s in subbasins@data$Subbasin[start_i:end_i]) {
    print("###################")
    print(" ")
    print(paste("Subbasin :", s))
    print(" ")
    print("###################")
    wetland_ca_file = paste(out_raster_dir, "/wetland_ca_", s, "_", ".tif", sep="")
    wetland_mxsa_file = paste(out_raster_dir, "/wetland_mxsa_", s, "_", ".tif", sep="")
    if (file.exists(wetland_ca_file) & file.exists(wetland_mxsa_file)) {next}
    subbasin = subset(subbasins, Subbasin == s)
    subbasin_buffer = gBuffer(subbasin, width = 600)
    e = alignExtent(subbasin, lc_lm)
    e_buffer = alignExtent(subbasin_buffer, lc_lm)
    mask_dem = mask(crop(dem, e_buffer), subbasin_buffer)
    mask_lc_lm = mask(crop(lc_lm, e_buffer), subbasin_buffer)
    mask_lc_lm = reclassify(mask_lc_lm, 
    rbind(
        c(-Inf,6.5,NA),
        c(6.5,9.5,1),
        c(9.5,Inf,NA)
    ))    
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
    # D8 flow directions for watershed delineation
    fdr_cmd = paste("mpiexec -n 4 D8Flowdir -fel", optim_fill_tif, "-p", fdr_tif, "-sd8", sd8_tif)
    fdr_stdout = system(fdr_cmd)
    fdr = raster(fdr_tif)
    # Calculate contributing area to mask out perennial pixels
    ca_cmd = paste("mpiexec -n 8 AreaD8 -p", fdr_tif, "-ad8", ca_tif, "-nc")
    ca_stdout = system(ca_cmd)
    ca = raster(ca_tif)
    # Reclassify D8 flow directions for use in ArcGIS
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
    # Mask out perennial pixels from seeds to discourage propogation of watersheds downstream
    perennial = reclassify(ca,
        rbind(
           c(-Inf,perennial_threshold,1),
           c(perennial_threshold,Inf,NA)
        )
    )
    seeds = mask_lc_lm
    seeds[is.na(perennial)] = NA
    writeRaster(fdr, fdr_tif, format="GTiff", options=c("COMPRESS=NONE"),
                overwrite=T, datatype='INT1U')
    writeRaster(seeds, seed_tif, format="GTiff", options=c("COMPRESS=NONE"),
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
    watershed[mask_lc_lm == 1] = 1
    watershed = mask(crop(watershed, e), subbasin)
    mask_lc_lm = mask(crop(mask_lc_lm, e), subbasin)
    if (any(gContains(subbasin, watersheds_ll, byid=T)[,1])) {
        contained_watersheds_ll = subset(watersheds_ll, gContains(subbasin, watersheds_ll, byid=T)[,1])
        contained_watersheds_ll_rast = rasterize(contained_watersheds_ll, watershed)
        watershed[!is.na(contained_watersheds_ll_rast)] = NA
        mask_lc_lm[!is.na(contained_watersheds_ll_rast)] = NA
    }
    wet_nsa = length(which(!is.na(getValues(mask_lc_lm)))) * 0.09
    wet_nvol = wet_nsa * presumed_depth_of_wetlands
    wet_vol = wet_nvol
    fill = raster(fill_tif)
    diff = fill - mask_dem
    diff_bin = diff
    diff_bin[diff_bin > 0] = 1
    diff_bin[diff_bin == 0] = NA
    diff_bin = mask(crop(diff_bin, e), subbasin)
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
    fill_intersect = rasterize(fill_poly, mask_lc_lm, "layer")
    fill_intersect = mask(fill_intersect, subbasin)
    wetland_mxsa_subbasin = sum(fill_intersect, mask_lc_lm, na.rm=T)
    wetland_mxsa_subbasin[wetland_mxsa_subbasin > 1] = 1
    wetland_mxsa_subbasin[wetland_mxsa_subbasin == 0] = NA
    lost_pixels = is.na(getValues(watershed)) & getValues(fill_intersect) == 1
    watershed[lost_pixels] = 1
    wet_fr = (length(which(!is.na(getValues(watershed)))) * 900) / gArea(subbasin)
    if (any(gContains(subbasin, watersheds_ll, byid=T)[,1])) {
        fill_intersect[!is.na(contained_watersheds_ll_rast)] = NA
    }
    diff = mask(crop(diff, e), wetland_mxsa_subbasin)
    depths = getValues(diff)[!is.na(getValues(wetland_mxsa_subbasin))]
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
    writeRaster(watershed, wetland_ca_file, format="GTiff")
    writeRaster(wetland_mxsa_subbasin, wetland_mxsa_file, format="GTiff")
    if (!file.exists(out_geometry_file)) {
        write.csv(row, file=out_geometry_file, row.names=F)
    } else {
        write.table(row, file=out_geometry_file, row.names=F, col.names=F, append=T, sep=",")
    }
    removeTmpFiles(h=0.01)
}
