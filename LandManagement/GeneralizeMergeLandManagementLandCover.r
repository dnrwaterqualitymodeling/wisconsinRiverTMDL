library(raster)
library(rgdal)
library(foreign)

overlay_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Finalizing_Land_Management/Overlay_Process"
rotation_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations"
local_dir = "C:/TEMP/land_mgt_overlay"
if (!file.exists(local_dir)){
    dir.create(local_dir)
}

# Set local variables

inCDLRasterCLU = "CDLRotations_2mileBuffer_MajorityPerCLU.img"
inCDLRasterPixels = "WRB_2mileBuffer_Rotations.img"
crop_codes = "CropCodes_CountyApproved_grid.img"
inLandCover = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WiRiverTmdlLandCoverDefinition.img"
inCDL2MileBuffer = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Cropland/CLU/CLU_WRB_2mileBuffer.shp"

remap1_txt = "remap_from_county_codes_to_numeric_code.txt"
remap2_txt = "remap_from_numeric_codes_to_swat_codes.txt"

expandFiles = list.files(overlay_dir, "^expand.*(img|rrd)$")
for (fl in expandFiles){    
    if (!file.exists(paste(local_dir, fl, sep = '/'))){
        print("Copying expanded files...")
        file.copy(paste(overlay_dir, fl, sep = '/'),
            local_dir)
    }
}
if (!file.exists(paste(local_dir, basename(inCDL2MileBuffer), sep='/'))){
    print("Copying CLU layer...")
    bsnme = substr(basename(inCDL2MileBuffer), 1, nchar(basename(inCDL2MileBuffer)) - 4)
    anc = list.files(dirname(inCDL2MileBuffer), 
        pattern = bsnme)
    for (fl in anc){
        file.copy(paste(dirname(inCDL2MileBuffer), fl, sep = '/'),
            local_dir)
    }
}
# I don't know where this is used, Tom?
# writeRaster(
# 	raster(paste(rotation_dir, inCDLRasterPixels, sep="/")),
# 	paste(local_dir, inCDLRasterPixels, sep="/"))

file.copy(paste(overlay_dir, remap1_txt, sep="/"), 
    paste(local_dir, remap1_txt, sep="/"))
file.copy(paste(overlay_dir, remap2_txt, sep="/"), 
    paste(local_dir, remap2_txt, sep="/"))

setwd(local_dir)
py_file = tempfile("arcpy_util_", fileext = ".py")
py_file = gsub("\\\\", "/", py_file)
# Define the Remap Values.
# The original crop codes are the county specific codes used to identify the original rotation classification.
# They are coded to reflect the county they came from.
# So, a crop code beginning with "Co" for instance is a rotation in Columbia County,
#	crop code beginning with "CL" is a rotation in Clark County,
#	starting with "R" is Richland County, etc.
# See "remap_from_county_codes_to_numeric_code.txt" for details

remap1 = eval(parse(remap1_txt))
remap1 = data.frame(Crp_Code_2=remap1[,1], gen=as.integer(remap1[,2]))
crop_codes_grid = raster(crop_codes)
# crop_codes_grid = readGDAL(crop_codes)
crop_codes_vat = read.dbf(paste(overlay_dir, "/", crop_codes, ".vat.dbf", sep=""), as.is=T)
reclass_tbl = as.matrix(merge(crop_codes_vat, remap1)[c("Value", "gen")])
crop_codes_grid = reclassify(crop_codes_grid, reclass_tbl)
writeGDAL(as(crop_codes_grid, "SpatialGridDataFrame"),
	"LumpedRotationsRaster.tif",
	drivername = "GTiff",
	type = "UInt16",
	mvFlag=65535)
rm(crop_codes_grid)
# writeRaster(crop_codes_grid, "LumpedRotationsRaster")
#STEP 2
#Expand land management types across entire WRB in individual rasters. 

#THE FIRST RUN IS EXECUTED FOR DAIRY ROTATIONS. 

# Variables for Step 3.
# These are the variables that the Expand toolbox is drawing from.
# The zone values are the generalized crop rotations from the land management raster dataset.
# The number of cells is set to 1000 because it really just needs to extend beyond the furthest distance of NoData cells.
# Note that zone values will have to be changed if land management generalizations are adjusted.

n_cells = c(1000,1000,50)
zone_vals = c(
	"[300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310]",
	"[400, 401, 402]",
	"[111, 121, 131, 141, 152, 171, 190, 195, 250]")
inFiles = c(
	rep("LumpedRotationsRaster.tif",2),
	inLandCover)
outFiles = c(
	"expandDairy.img",
	"expandCashGrain.img",
	"expandAllLandCover.img")
s1 = "import arcpy; from arcpy.sa import *; from arcpy import env;"
s1.5 = "arcpy.CheckOutExtension('Spatial');"
s2 = paste("env.workspace = r'", local_dir, "';", sep="")
for (e in 1:3) {
	print(paste('working on number', e))
	s3 = paste(
		"Expand(Raster('",
		inFiles[e],
		"'),",
		n_cells[e],
		",",
		zone_vals[e],
		").save('",
		outFiles[e],
		"')",
		sep="")
	script_str = paste(s1,s1.5,s2,s3, sep="")
	writeLines(script_str, py_file)
	print(script_str)
	cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", py_file)
	system(cmd)
}

# This step splits the continuous corn CDL analysis pixels into 50% dairy pixels and 50% cash grain pixels using uniform random values

lc = raster(inLandCover)
inCDL = extend(raster(inCDLRasterCLU), lc)
extent(inCDL) = extent(lc)
CDL = getValues(inCDL)
CDL[CDL == 7] = NA

set.seed(112014)
rand = runif(length(CDL))
rand = rand * (CDL > 0)

CDL[which((CDL == 1) & (rand <= 0.5))] = 3
CDL[which((CDL == 1) & (rand > 0.5))] = 2

clpExt = extent(inCDL)
inDairyRaster = raster("expandDairy.img")
dairy_mat = extend(inDairyRaster, clpExt)

inCashGrainRaster = raster("expandCashGrain.img")
cashG_mat = extend(inCashGrainRaster, clpExt)

inNonAgLandCoverRaster = raster("expandAllLandCover.img")
nonAg_mat = crop(inNonAgLandCoverRaster, clpExt)

CDL[which(is.na(CDL) | CDL == 7)] = getValues(nonAg_mat)[which(is.na(CDL))]
CDL[which(CDL == 6)] = 800
CDL[which(CDL == 2)] = getValues(cashG_mat)[which(CDL == 2)]
CDL[which(CDL == 3 | CDL == 4)] = getValues(dairy_mat)[which(CDL == 3 | CDL == 4)]
CDL[which(CDL == 5)] = 500
CDL[which(is.na(CDL))] = getValues(nonAg_mat)[which(is.na(CDL))]

CDL = CDL * 10
# Randomizing some cash grain rotations
split1 = rand <= 0.50
split2 = rand > 0.50

split1_query = ((CDL >= 4010 & CDL < 5000) & split1)
CDL[which(split1_query)] = CDL[which(split1_query)] + 1

split2_query = ((CDL >= 4010 & CDL < 5000) & split2) 
CDL[which(split2_query)] = CDL[which(split2_query)] + 2

rm(split1_query, split2_query)

# randomizing dairy rotations and 1 cash$ rotation
split1 = rand <= 0.33
split2 = (rand > 0.33 & rand <= 0.66)
split3 = rand > 0.66

lc_q = (CDL >= 3000 & CDL <= 4000) | CDL == 5000
split1_query = (lc_q & split1)
CDL[which(split1_query)] = CDL[which(split1_query)] + 1

split2_query = (lc_q & split2) 
CDL[which(split2_query)] = CDL[which(split2_query)] + 2

split3_query = (lc_q & split3) 
CDL[which(split3_query)] = CDL[which(split3_query)] + 3

rm(split1_query, split2_query, split3_query)

writeGDAL(as(setValues(inCDL, CDL), "SpatialGridDataFrame"),
	"cdl_randomized.tif",
	drivername = "GTiff",
	type = "UInt16",
	mvFlag=65535)

# Finally, run zonal statistics for crop rotations to expand them once again to the CLU boundaries,
#	otherwise the fields look like fuzzy television screens since it was randomized by pixel. 
s1 = "import arcpy;from arcpy.sa import *;from arcpy import env;arcpy.CheckOutExtension('Spatial');"
s2 = paste("env.workspace='", local_dir, "';", sep="")
s3 = "env.snapRaster='cdl_randomized.tif';env.extent='cdl_randomized.tif';env.cellSize='cdl_randomized.tif';"
s4 = paste("arcpy.PolygonToRaster_conversion('",
    basename(inCDL2MileBuffer),
    "', 'CLUID', 'cluid.tif', 'CELL_CENTER', '', 30);",
    sep="")
s5 =  "arcpy.BuildRasterAttributeTable_management('cluid.tif', 'Overwrite');"
s6 = "maj = ZonalStatistics('cluid.tif', 'value', 'cdl_randomized.tif', 'MAJORITY', 'NODATA');"
s7 = "maj.save('cdl_randomized_zonal.tif')"
script_str=paste(s1,s2,s3,s4,s5,s6,s7,sep="")
writeLines(script_str, py_file)

cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", py_file)
system(cmd)

remap2 = eval(parse(remap2_txt))
remap2 = apply(remap2, c(1,2), as.integer)

cdl_randomized_zonal = getValues(raster('cdl_randomized_zonal.tif'))
cdl_randomized_zonal[which(cdl_randomized_zonal < 3000)] = NA
cdl_randomized = getValues(raster("cdl_randomized.tif"))       
        
cdl_randomized_zonal[which(is.na(cdl_randomized_zonal))] =
    cdl_randomized[which(is.na(cdl_randomized_zonal))]
swat_lc = raster('cdl_randomized.tif')
swat_lc = setValues(swat_lc, cdl_randomized_zonal)
swat_lc = reclassify(swat_lc, remap2)

writeGDAL(as(swat_lc, "SpatialGridDataFrame"),
    "swat_lc.tif",
    drivername = "GTiff",
    type = "Byte",
    mvFlag=255)

#dunzo















