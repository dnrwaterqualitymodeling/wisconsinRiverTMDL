import os
import subprocess
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")


fl_hydro = 'W23324.WD_HYDRO_DATA_24K'
sbbsns = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"

file_dem = "C:/Users/evansdm/filled_dem_processing/raw_prj_10_m_cpy.tif"

# dir_hydro = "T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology"
# file_hydro_in = dir_hydro +"/WI_River_Hydro_Flowline_24K.shp"
# file_dem = dir_dem + "/wrb_dem.tif"

flowlines_ras = "flowLines_clip_PolylineToRas"
llckbodys_ras = "nonLndLckedLkes_clip_Polygon"

# env.snapRaster = file_dem

 # subprocess.Popen("mpiexec -n 8 pitremove")
tmpdir = os.path.expanduser("~") + "/filled_dem_processing"
sde = "C:/Users/evansdm/AppData/Roaming/ESRI/Desktop10.1/ArcCatalog/DNR SDE PRODUCTION.sde"# sde path in app data roaming
env.workspace = sde

# desc = arcpy.Describe(fl_hydro)

flow_lines = "W23324.WD_HYDRO_FLOWLINE_LN_24K"
water_bdys = "W23324.WD_HYDRO_WATERBODY_AR_24K"

if not os.path.isdir(tmpdir):
	os.mkdir(tmpdir)
env.workspace = tmpdir

tmpGDB = "tempGDB.gdb"
if not arcpy.Exists(tmpdir+"/"+tmpGDB):
	arcpy.CreateFileGDB_management(tmpdir, tmpGDB)
env.workspace = tmpGDB

hydro_path = sde+"/"+fl_hydro

arcpy.MakeFeatureLayer_management(hydro_path+"/"+water_bdys, "nonLndLcked", "LANDLOCK_CODE = 0")
arcpy.CopyFeatures_management("nonLndLcked", "nonLndLcked")

arcpy.MakeFeatureLayer_management(hydro_path+"/"+flow_lines, "flowLines", "LANDLOCK_CODE = 0")
arcpy.CopyFeatures_management("flowLines", "flowLines")

arcpy.Clip_analysis("nonLndLcked", sbbsns, "nonLndLcked_clip")
arcpy.Clip_analysis("flowLines", sbbsns, "flowLines_clip")

arcpy.RepairGeometry_management("flowLines_clip")
arcpy.RepairGeometry_management("nonLndLcked_clip")

arcpy.AddField_management("flowLines_clip", "dumDum", "SHORT")
arcpy.AddField_management("nonLndLcked_clip", "dumDum", "SHORT")

arcpy.CalculateField_management("flowLines_clip", "dumDum", "dumDum = 1")
arcpy.CalculateField_management("nonLndLcked_clip", "dumDum", "dumDum = 1")

arcpy.CopyFeatures_management("flowLines_clip", "C:/Users/evansdm/filled_dem_processing/flowLines_clip.shp")
arcpy.CopyFeatures_management("nonLndLcked_clip", "C:/Users/evansdm/filled_dem_processing/nonLndLckedLkes_clip.shp")

## ... manually converted to raster

dem_na_flow = SetNull(flowlines_ras, Raster(file_dem), "VALUE = 1")
SetNull(llckbodys_ras, dem_na_flow, "VALUE = 1").save("C:/Users/evansdm/filled_dem_processing/na_burned_dem.tif")


## feed to tau dem
# run tau dem pit removal
mipexec = "mpiexec -n 8 pitremove"
inDEM = ""

### rasterizing flow lines
# gdal_rasterize = "C:/Program Files/GDAL/gdal_rasterize.exe"
# no_dat_val = " -9999"#" -3.4028230607370965e+038"
# burn_val = " -1"#" -3.4028230607370965e+038"
# shp_flwlnes = "C:/Users/evansdm/filled_dem_processing/flowLines_clip.shp"
# tif_flwlnes = "C:/Users/evansdm/filled_dem_processing/flowLines_clip.tif"

# lyr_name = os.path.basename(shp_flwlnes).replace(".shp","")
# " -a_nodata" + no_dat_val +\
# " -of HFA" + \
# " -ot Int32" +\	
# " -tr 10 10" + \
# cmd = gdal_rasterize + \
	# " -b 1" \
	# " -at" + \
	# " -i" + \
	# " -burn" + burn_val +\
	# " -l " + lyr_name + " " +\
	# shp_flwlnes + " " +\
	# file_dem
# p = subprocess.Popen(cmd)
# p.wait()



# arcpy.PolylineToRaster_conversion("flowLines_clip", "dumDum", "flowlines_ras", "MAXIMUM_COMBINED_LENGTH", "dumDum", 10)
# arcpy.PolygonToRaster_conversion("nonLndLcked_clip", "dumDum", llckbodys_ras, "MAXIMUM_COMBINED_AREA", "dumDum", 10)



## set landlocked lake and flowline rasters to null
# Con(Raster(flowlines_ras) > 2, 1, 0).save(file_hydro_ras_1m)













hydro_ras = "rasterized_hydro.tif"
file_hydro_ras_1m = "rasterized_hydro_1m.tif"
file_hydro_ras_1m_nas = "rasterized_hydro_1m_wnaos.tif"
dem_less_200m_hydro = "wrb_dem_burned_hydro.tif"
dem_filled_no_nulls = "wrb_filled_no_nulls.tif"

#Rasterize hydro streams and 
arcpy.PolylineToRaster_conversion(file_hydro_in, "ORIG_HRZ_S", hydro_ras, "", "", 10)
rasterize landlocked lakes and on network lakes?

#set hydro layer to 1
Con(Raster(hydro_ras) > 2, 1, 0).save(file_hydro_ras_1m)
SetNull(file_hydro_ras_1m, file_hydro_ras_1m, "VALUE = 1").save(file_hydro_ras_1m_nas)
set lake areas to Null


(dem + file_hydro_ras_1m_nas).save("dem_wNas.tif")
make lake and stream areas na in dem

run tau dem pit removal
fill result

subtract dem from filled

print 'dunzo'

#Subtract from DEM
# (Raster(file_dem) - Raster(file_hydro_ras_200m_zeros)).save(dem_less_200m_hydro)

#Fill
# hydro_null = SetNull(Raster(file_hydro_ras_200m_zeros), Raster(file_hydro_ras_200m_zeros), "Value > 100")
# dem_hydro_null = SetNull(Raster(file_hydro_ras_200m), Raster(file_dem), "Value = 1") 
# Fill(dem_hydro_null).save(dem_filled_no_nulls)
# Fill(dem_less_200m_hydro, 150).save(dem_filled_no_nulls)


# (dem_filled_no_nulls + hydro_null).save("wrb_filled.tif")
# Set DEM_Stream overlay area to NA
# SetNull(dem_filled_no_nulls, dem_filled_no_nulls, "VALUE ").save(file_hydro_ras_200m)

print("Dunzo.")

