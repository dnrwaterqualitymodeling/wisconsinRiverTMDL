import os
import subprocess
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

dem_net = "T:/Projects/Wisconsin_River/GIS_Datasets/DEM/raw_prj_10_m.img"
sde = "C:/Users/evansdm/AppData/Roaming/ESRI/Desktop10.1/ArcCatalog/DNR SDE PRODUCTION.sde" # sde path in app data roaming
# sde = "C:/Users/ruesca/AppData/Roaming/ESRI/Desktop10.1/ArcCatalog/dnrSdeReadOnly.sde"
fl_hydro = 'W23324.WD_HYDRO_DATA_24K'
sbbsns = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
flow_lines = "W23324.WD_HYDRO_FLOWLINE_LN_24K"
water_bdys = "W23324.WD_HYDRO_WATERBODY_AR_24K"
flowlines_ras = "flowLines_ras"
llckbodys_ras = "lakes_ras"

tmpdir = os.path.expanduser("~") + "/filled_dem_processing"
tmpGDB = "tempGDB.gdb"
if not os.path.isdir(tmpdir):
	os.mkdir(tmpdir)
env.workspace = tmpdir

file_dem = tmpdir + "/raw_prj_10_m.img"
if not os.path.exists(file_dem):
	arcpy.Copy_management(dem_net, file_dem)
env.snapRaster = file_dem

if not arcpy.Exists(tmpdir+"/"+tmpGDB):
	arcpy.CreateFileGDB_management(tmpdir, tmpGDB)
env.workspace = tmpGDB

hydro_path = sde+"/"+fl_hydro

for fc in ["WATERBODY_AR_24k", "FLOWLINE_LN_24K"]:
	arcpy.FeatureClassToFeatureClass_conversion(hydro_path + "/W23324.WD_HYDRO_" + fc,
		env.workspace,
		fc,
		"LANDLOCK_CODE = 0")
	arcpy.Clip_analysis(fc, sbbsns, fc+ "_clip")
	ExtractByMask(file_dem, fc + "_clip").save(fc+"_ras")
	out_null = IsNull(fc+"_ras")
	out_null.save(fc + "_null_ras")

tobeNA = Raster("WATERBODY_AR_24k_null_ras") + Raster("FLOWLINE_LN_24K_null_ras")
arcpy.Clip_management(tobeNA, "#", "clipped_tobe_NA", sbbsns, -3.40282e+038, "ClippingGeometry")
SetNull("clipped_tobe_NA", "clipped_tobe_NA", 'Value < 2').save("flows_lakes_NA")
zero_or_na = Minus("flows_lakes_NA", 2)
Plus(zero_or_na, file_dem).save(tmpdir + "/burned_dem.tif")

SetNull(tmpdir+"/na_burned_dem_v4.tif", tmpdir+"/na_burned_dem_v4.tif", "Value < 1 ").save(tmpdir+"/burned_dem.tif")
## feed to tau dem
# run tau dem pit removal
mipexec = "mpiexec -n 4 pitremove -z "
in_dem = tmpdir+"/burned_dem.tif"
out_dem = tmpdir + "/filled_dem.tif"

cmd = mipexec + in_dem + " -fel " + out_dem

p = subprocess.Popen(cmd)
p.wait()

print("Dunzo.")
