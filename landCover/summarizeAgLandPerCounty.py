import arcpy, time, os
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
env.overwriteOutput = True

###########################
# User-specific inputs
###########################
# Set up SDE Connection
user = 'ruesca'
homeDir = 'C:/Users/' + user
dbConnDir = homeDir + '/AppData/Roaming/ESRI/Desktop10.0/ArcCatalog'
sdeName = '/Connection to dnr_sde.sde'
sdeConn = dbConnDir + sdeName
# Define a local workspace for intermediate file storage
scratch = 'G:/temp/temp.gdb'
############################

# Set up TMDL network paths
tmdlRoot = r'\\dnr\gis\WD_Projects\DNR\Water\TMDL'
wiRiverTmdlDir = tmdlRoot + r'\Projects\Wisconsin_River'
lcDir = wiRiverTmdlDir + r'\GIS_Datasets\Landcover'
# Input files
wrbFile = wiRiverTmdlDir + r'\GIS_Datasets\Watersheds\WRB_Basin.shp'
countyFile = sdeConn + '/SDEDNR.EN_COUNTY_BOUNDARY_DATA_100K/SDEDNR.EN_COUNTY_BOUNDARY_AR_100K'
lcFile = lcDir + r'\nass2011amededByWwi.tif'
# Intermediate Files
clipLc = scratch + '/lc_WRB'
clipCounty = scratch + '/county_WRB'
# Output files
outTable = lcDir + r'\nass2011amededByWwi_summarizedByCounty.dbf'
metadataFile = lcDir + r'\readme.txt'

#####
# RUN
#####
arcpy.Clip_management(lcFile, '', clipLc, wrbFile, '', True)
arcpy.Clip_analysis(countyFile, wrbFile, clipCounty)
tabArea = TabulateArea(clipCounty, 'COUNTY_NAME', clipLc, 'VALUE', outTable)
#####

# Create metadata
tableFile = os.path.basename(outTable)
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\landCover\summarizeAgLandPerCounty.py"'
metadataOut = tableFile + ' was created/changed by ' + user + " on " + timeNow + ' using '\
	+ scriptFile + '. Data is in square meters.\n\n'
f = open(metadataFile, 'a+')
f.write(metadataOut)
f.close()