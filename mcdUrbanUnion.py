import arcpy, os, time
from arcpy import env

# Set up SDE Connection
user = 'ruesca'
homeDir = 'C:/Users/' + user
dbConnDir = homeDir + '/AppData/Roaming/ESRI/Desktop10.0/ArcCatalog/'
sdeName = 'Connection to dnr_sde.sde'
sdeConn = dbConnDir + sdeName
# Define a local workspace for intermediate file storage
scratch = 'G:/temp/temp.gdb'
# Set up TMDL network paths
tmdlRoot = r'\\dnr\gis\WD_Projects\DNR\Water\TMDL\Projects\Wisconsin_River'
urbanDir = tmdlRoot + r'\GIS_Datasets\Urban'
# Script output
outUnion = urbanDir + '\mcdUrbanUnion.shp'
print outUnion
# Define a metadata file
urbanMetadataFile = urbanDir + r'\readme.txt'

mcd = sdeConn + '/SDEDNR.EN_MCD_MULTI_CNTY_AR_VAR'
urbanArea = urbanDir + '/WRB_UrbanArea_2010Census.shp'
mcdClip = scratch + '/EN_MCD_MULTI_CNTY_AR_VAR'
wrbBuffer = tmdlRoot + '/Watersheds/WRB_Basin_2mile_Buffer.shp'
# clip mcd to WI River basin
arcpy.Clip_analysis(mcd, wrbBuffer, mcdClip)
# union mcd and urban areas
arcpy.Union_analysis([mcdClip, urbanArea], outUnion) 
# add two new fields
arcpy.AddField_management(outUnion, 'Urban_AR_3', 'SHORT', 2, '', '', '', 'NULLABLE')
arcpy.AddField_management(outUnion, 'CV', 'SHORT', 2, '', '', '', 'NULLABLE')

unionFile = os.path.basename(outUnion)
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\mcdUrbanUnion.py"'
metadataOut = unionFile + " was created/changed by " + user + " on " + timeNow + ' using ' + scriptFile + '\n'
f = open(urbanMetadataFile, 'a+')
f.write(metadataOut)
f.close()