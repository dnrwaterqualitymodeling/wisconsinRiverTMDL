# SOME of the mapunits in the SSURGO database have two designations for hydrologic soil group, these 
# are A/D, B/D, and C/D, the former assuming the land is drained, and the latter assuming that the 
# soil is not drained. The SWAT soils database assumes that all of the soils are drained and there-
# fore assumes the not D option. 
#     This script looks at each of those mapunits with dual HSGs and examines the amound of cropland
# within the polygon. We (A. Ruesch and D. Evans) that if an area contains 50% or more agriculutral
# land (as defined by CDL) then it can be assumed to be drained. We found that NONE of those sites 
# that have two HSG designations in the WR basin have greater than 50% agland within. Thus all of the
# MUKEYs were updated with the D designation.

library(raster)
library(rgdal)
library(rgeos)
options(stringsAsFactors = F, warn = 1)
net_soil_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"
dual_hsgs = paste(net_soil_dir, 'dual_hsgs_to_single.txt', sep="/")
wrb_mukeys_file = paste(net_soil_dir, "wrb_mukeys.txt", sep="/")
comp_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/component.txt", sep="/")
hrzn_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chorizon.txt", sep="/")
chfr_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chfrags.txt", sep="/")

file_lc = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/swat_lc_wtm.tif"

out_shp = paste(net_soil_dir, "mupolygon_2mile_buffer_wMich_wDrained_2014.shp", sep="/")

#########
# mukeys only within the WRB
wrb_mukeys = unique(read.table(wrb_mukeys_file, header=T, sep=',')[["MUKEY"]])
wrb_mukeys = wrb_mukeys[!is.na(wrb_mukeys)]
# read in gSSURGO tables and process
mupolygon = readOGR(net_soil_dir,
    "MUPOLYGON__2mile_buffer_wMich_2014")

mupolygon = subset(mupolygon, MUKEY %in% wrb_mukeys, select=MUKEY)
##############
dat_cols = c(
    "dbovendry_r",
    "awc_r",
    "ksat_r",
    "cbn",
    "claytotal_r",
    "silttotal_r",
    "sandtotal_r",
    "fragvol_r", 
    "albedodry_r",
    "kwfact",
    "ec_r", 
    "caco3_r", 
    "ph1to1h2o_r",
    "hydgrp")
comp_cols = c("mukey",
    "cokey",
    "compname",
    "comppct_r",
    "hydgrp",
    "albedodry_r")
hrzn_cols = c("cokey",
    "chkey",
    "hzdept_r",
    "hzdepb_r",
    "dbovendry_r",
    "awc_r",
    "om_r",
    "ksat_r",
    "claytotal_r",
    "silttotal_r",
    "sandtotal_r",
    "kwfact",
    "ec_r",
    "caco3_r",
    "ph1to1h2o_r")
chfr_cols = c("chkey", "fragvol_r")
print("Reading component, horizon, and chfrags tables. Please wait...")
comp = read.table(comp_file, header=T, sep="\t")[comp_cols]
hrzn = read.table(hrzn_file, header=T, sep="\t")[hrzn_cols]
chfr = read.table(chfr_file, header=T, sep="\t")[chfr_cols]
comp = subset(comp, mukey %in% wrb_mukeys) # Only mukeys in WRB
comp = merge(comp, hrzn, by="cokey", all.x=T, all.y=T) # Join component with chorizon
chfr = aggregate(fragvol_r ~ chkey, chfr, sum, na.rm=T) # Sum rock volumes by hrzn
comp = merge(comp, chfr, by="chkey", all.x=T) # Join component/horizon with chfrags
comp$fragvol_r[is.na(comp$fragvol_r)] = 0 # Force NA rock fragments to zero
##############

lc = raster(file_lc)
lcMat = getValues(lc)
lcMat[lcMat <= 9 | lcMat >= 53] = NA
lcMat[lcMat > 9 & lcMat < 53] = 1
lc = setValues(lc, lcMat)
temp_lc = writeRaster(lc, paste(tempdir(), "\\ag_land.tif", sep=""), datatype='INT2S')

# Select MUKEYs that have at least one dual HSG component
dual_hsg = subset(comp, hydgrp %in% c("A/D", "B/D", "C/D"))
dual_mukeys <- unique(dual_hsg$mukey)
mupoly_dual <- subset(mupolygon, MUKEY %in% dual_mukeys)

wtm <- proj4string(lc)
mupoly_dual <- spTransform(mupoly_dual, CRS(wtm))

writeOGR(
	obj = mupoly_dual,
	dsn = tempdir(),
	layer = "mupoly_dual",
	driver = "ESRI Shapefile")

tmpf = tempfile("polygonize_and_union_", fileext= ".py")
ln1 = "import arcpy"
ln2 = paste("arcpy.env.workspace = r'", tempdir(), "'", sep="")
ln3 = paste(
	'arcpy.RasterToPolygon_conversion("ag_land.tif", "',
	"wrb_tmdl_agric_landuse.shp",
	'", ',
	'"NO_SIMPLIFY"',
	", ",
	'"Value"',
	")",
	sep='')
ln4 = paste(
	'arcpy.Union_analysis(',
	'"mupoly_dual.shp 1; wrb_tmdl_agric_landuse.shp 2",',
	'"ag_ssurgo_union.shp",',
	'"ALL",',
	'"#",',
	'"GAPS")',
	sep="")
writeLines(
	paste(
		ln1,
		ln2,
		ln3,
		ln4,
		sep='\n'),
	tmpf)
py_cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", tmpf)
system(py_cmd)

ag_ssurgo_union = readOGR(tempdir(), "ag_ssurgo_union")
ag_ssurgo_union = subset(ag_ssurgo_union, MUKEY != "")
split_mukeys = aggregate(GRIDCODE ~ MUKEY, mean, data = ag_ssurgo_union@data)
split_mukeys = subset(split_mukeys, GRIDCODE >= 0.1)
ag_ssurgo_union = subset(ag_ssurgo_union,
	MUKEY %in% split_mukeys$MUKEY,
	select=c(MUKEY, GRIDCODE))
names(ag_ssurgo_union@data)[2] = "DRAINED"
ag_ssurgo_union@data$DRAINED = as.logical(ag_ssurgo_union@data$DRAINED)

mupolygon@data$DRAINED = as.logical(0)

writeOGR(mupolygon,
	tempdir(),
	"mupolygon",
	driver="ESRI Shapefile")
writeOGR(ag_ssurgo_union,
	tempdir(),
	"split_mukeys",
	driver="ESRI Shapefile")

tmpf = tempfile("update_", fileext= ".py")
ln1 = "import arcpy"
ln2 = paste("arcpy.env.workspace = r'", tempdir(), "'", sep="")
ln3 = 'arcpy.Update_analysis("mupolygon.shp", "split_mukeys.shp", "mukey_update.shp")'
ln4 = paste('arcpy.Project_management("mukey_update.shp", "', out_shp, '", arcpy.SpatialReference(3071))', sep="")

writeLines(
	paste(
		ln1,
		ln2,
		ln3,
		ln4,
		sep='\n'),
	tmpf)

py_cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", tmpf)
system(py_cmd)







