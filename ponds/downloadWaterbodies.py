import arcpy, os, time
from arcpy import env
env.overwriteOutput = 1

schema = 'W23324'
pw = '3hatewinter'
sde = 'dnr_sde.world'

appDir = os.environ['APPDATA']
sdeCon = appDir + '/ESRI/Desktop10.1/ArcCatalog/' + schema + '_' + pw + '_' + sde + '.sde'
wd = "T:/Projects/Wisconsin_River/GIS_Datasets"

arcpy.CreateFileGDB_management(wd + '/ponds', 'waterbodies.gdb')

waterbodiesFile = sdeCon + '/' + schema + '.WD_HYDRO_DATA_24K/WD_HYDRO_WATERBODY_AR_24K'
watershedsFile = wd + "/Watersheds/WRB_Basin_2mile_Buffer.shp"
arcpy.MakeFeatureLayer_management(watershedsFile, "watersheds")
arcpy.MakeFeatureLayer_management(waterbodiesFile, "waterbodies", 'HYDROTYPE = 706')

arcpy.SelectLayerByLocation_management("waterbodies", "", "watersheds", "", "NEW_SELECTION")
arcpy.CopyFeatures_management("waterbodies", wd + '/ponds/waterbodies.gdb/lake_pond')

# Create metadata
metadataFile = wd + "/ponds/readme.txt"
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = '"/code/ponds/downloadWaterbodies.py"'
metadataOut = os.path.basename(wd + '/ponds/waterbodies.gdb/lake_pond') +\
	' was created/changed by Aaron Ruesch on ' + timeNow+ ' using ' +\
	scriptFile + '\n\n'
f = open(metadataFile, 'a+')
f.write(metadataOut)
f.close()
