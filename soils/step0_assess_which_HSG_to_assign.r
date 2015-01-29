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
options(stringsAsFactors = F, warn = 1)
library(rgeos)
net_soil_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"
dual_hsgs = paste(net_soil_dir, 'dual_hsgs_to_single.txt', sep="/")
wrb_mukeys_file = paste(net_soil_dir, "wrb_mukeys.txt", sep="/")
comp_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/component.txt", sep="/")
hrzn_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chorizon.txt", sep="/")
chfr_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chfrags.txt", sep="/")

file_lc = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/swat_lc_wtm.tif"
#########
# mukeys only within the WRB
wrb_mukeys = unique(read.table(wrb_mukeys_file, header=T, sep=',')[["MUKEY"]])
wrb_mukeys = wrb_mukeys[!is.na(wrb_mukeys)]
# read in gSSURGO tables and process
mupolygon = readOGR(net_soil_dir,
    "MUPOLYGON__2mile_buffer_wMich_2014")

mupolygon = subset(mupolygon, MUKEY %in% wrb_mukeys)
mupolygon@data["objID"] = as.integer(rownames(mupolygon@data))
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
lcMat[lcMat <= 9 | lcMat > 52] = 0
lcMat[lcMat > 9 & lcMat <= 52] = 1
lc = setValues(lc, lcMat)

wtm <- proj4string(lc)
mupolygon <- spTransform(mupolygon, CRS(wtm))

dual_hsg = subset(comp, hydgrp %in% c("A/D", "B/D", "C/D"))
dual_mukeys <- unique(dual_hsg$mukey)
mupoly_dual <- subset(mupolygon, MUKEY %in% dual_mukeys)

# This step takes a long time, the resulting data frame from the extract() function 
#   was written out to a table
mupoly_dual_ex <- extract(lc,
	mupoly_dual, 
	fun=mean, 
	na.rm=T,
	df=T,
	sp=F)
bkp = mupoly_dual_ex
mupoly_dual_ex = subset(mupoly_dual_ex, swat_lc_wtm > 0.1)
write.table(mupoly_dual_ex,
	paste(net_soil_dir, "polygons_greater_than_10perc_ag.txt",sep='/'),
	sep='\t',
	row.names=F)
### Table with those polygons ObJIDs with more than 10% Ag 
# mupoly_dual_ex = read.delim(paste(net_soil_dir, "polygons_greater_than_10perc_ag.txt",sep='/'))

mupoly_ag = subset(mupolygon, objID %in% mupoly_dual_ex$ID)

file_mupoly_ag = "mupolygons_greater_10perc_ag"
writeOGR(
	obj = mupoly_ag,
	dsn = tempdir(),
	layer = file_mupoly_ag,
	driver= "ESRI Shapefile")
### create script to polygonize land use

out_landcover_rast = "wrb_tmdl_agric_landuse.tif"
out_landcover_poly = "wrb_tmdl_agric_landuse.shp"
out_lndcvr_soilpoly_intersect = "agric_lndcvr_dualhsg_intersect.shp"
out_lndcvr_soilpoly_intersect_dissolved = "agric_lndcvr_dualhsg_intersect_dissolved.shp"

input_list = paste(
	'["',
	out_landcover_poly,
	'", "',
	file_mupoly_ag,".shp",
	'"]',
	sep='') 
# in_sql_state = "Value < 56 & Value > 9"


tmpf = tempfile("polygonize_", fileext= ".py")
ln1 = "import arcpy"
checkout_line = 'arcpy.CheckOutExtension("Spatial")'
ln2 = "from arcpy.sa import *"
ln3 = paste("arcpy.env.workspace = r'", tempdir(), "'", sep="")


##### to extract the agg polygons
ln4 = paste(
	'extract_lyr = ExtractByAttributes("',
	file_lc,
	'", ',
	'"Value > 9"',
	')',
	sep='')
ln5 = paste(
	'extract_lyr = ExtractByAttributes(extract_lyr, ',
	'"Value < 56"',
	')',
	sep='')
ln6 = paste(
	'extract_lyr.save("',
	out_landcover_rast,
	'")',
	sep='')

#### raster to poly conversion
ln7 = paste(
	'arcpy.RasterToPolygon_conversion(r"',
	out_landcover_rast,
	'", "',
	out_landcover_poly,
	'", ',
	'"NO_SIMPLIFY"',
	",",
	'"Value"',
	")",
	sep='')

## Intersection 
ln8 = paste(
	"arcpy.Intersect_analysis(",
	input_list,
	', "',
	out_lndcvr_soilpoly_intersect,
	'")',
	sep='')
	
##### Dissolve to remove slivers
ln9 = paste(
	"arcpy.Dissolve_management(",
	'"',
	out_lndcvr_soilpoly_intersect,
	'", ',
	'"',
	out_lndcvr_soilpoly_intersect_dissolved,
	'", ', 
	'"objID")',
	sep='')

writeLines(
	paste(
		ln1,
		checkout_line,
		ln2,
		ln3,
		ln4,
		ln5,
		ln6,
		ln7,
		ln8,
		ln9,
		sep='\n'),
	tmpf)

py_cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", tmpf)
system(py_cmd)







