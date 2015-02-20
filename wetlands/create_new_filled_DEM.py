import os
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

tmpdir = os.path.expanduser("~") + "/filled_dem_processing"

if not os.path.isdir(tmpdir):
	os.mkdir(tmpdir)

env.workspace = tmpdir

dir_dem = "T:/Projects/Wisconsin_River/GIS_Datasets/DEM"
dir_hydro = "T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology"

file_hydro_in = dir_hydro +"/WI_River_Hydro_Flowline_24K.shp"
file_dem = dir_dem + "/wrb_dem.tif"
env.snapRaster = file_dem

hydro_ras = "rasterized_hydro.tif"
file_hydro_ras_200m = "rasterized_hydro_200m.tif"
file_hydro_ras_200m_zeros = "rasterized_hydro_200m_wzeros.tif"
dem_less_200m_hydro = "wrb_dem_burned_hydro.tif"
dem_filled_no_nulls = "wrb_filled_no_nulls.tif"

#Rasterize hydro
arcpy.PolylineToRaster_conversion(file_hydro_in, "ORIG_HRZ_S", hydro_ras, "", "", 10)

#set hydro layer to 200
Con(Raster(hydro_ras) > 2, 200, 0).save(file_hydro_ras_200m)
Con(IsNull(file_hydro_ras_200m), 0, 200).save(file_hydro_ras_200m_zeros)

#Subtract from DEM
(Raster(file_dem) - Raster(file_hydro_ras_200m_zeros)).save(dem_less_200m_hydro)

#Fill

Fill(dem_less_200m_hydro, 150).save(dem_filled_no_nulls)
hydro_null = SetNull(Raster(file_hydro_ras_200m_zeros), Raster(file_hydro_ras_200m_zeros), "Value > 100")

(dem_filled_no_nulls + hydro_null).save("wrb_filled.tif")
#Set DEM_Stream overlay area to NA
# SetNull(dem_filled_no_nulls, dem_filled_no_nulls, "VALUE ").save(file_hydro_ras_200m)

print("Dunzo.")

