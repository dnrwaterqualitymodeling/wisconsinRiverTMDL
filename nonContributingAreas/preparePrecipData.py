import arcpy, os, urllib, zipfile, time
from arcpy import env
arcpy.CheckOutExtension("Spatial")
from arcpy.sa import *
env.overwriteOutput = True

###########################
# User-specific inputs
###########################
user = 'ruesca'
scratch = 'G:/temp/temp.gdb'
###########################
tmdlRoot = '//dnr/gis/WD_Projects/DNR/Water/TMDL'
wiRiverTmdlDir = tmdlRoot + '/Projects/Wisconsin_River'
climateDir = wiRiverTmdlDir + '/GIS_Datasets/Climatological/prcpFreq'
demDir = wiRiverTmdlDir + '/GIS_Datasets/DEM'

# INPUT DATA
# URL for ascii grid of the 10-year 24-hour rainfall event
prcpUrl = 'ftp://hdsc.nws.noaa.gov/pub/hdsc/data/mw/mw10yr24ha.zip'
prjFile = 'C:/Program Files (x86)/ArcGIS/Desktop10.0/Coordinate Systems/Geographic Coordinate Systems/North America/NAD 1983.prj'
transformation = 'NAD_1983_To_HARN_Wisconsin'
downloadsFolder = 'C:/Users/' + user + '/Downloads'
demFile = demFile = demDir + '/WRB_dem'
# Intermediate data
prcpFile = scratch + '/prcp'
prcpPrjFile = scratch + '/prcpPrj'
# Output data
backupAscii = climateDir + '/mw10yr24ha.asc'
prcp10mFile = climateDir + '/prcp_10yr24hr.tif'
metadataFile = climateDir + '/readme.txt'

# Download Prcp data, read data from archive, save backup
asciiArchive = downloadsFolder + '/mw10yr24ha.zip'
asciiFile = downloadsFolder + '/mw10yr24ha.asc'
urllib.urlretrieve(prcpUrl, asciiArchive)
zf = zipfile.ZipFile(asciiArchive, 'r')
asciiData = zf.read('mw10yr24ha.asc')
zf.close()
f = open(backupAscii, 'w')
f.write(asciiData)
f.close()

arcpy.ASCIIToRaster_conversion(backupAscii, prcpFile, 'INTEGER') 
arcpy.DefineProjection_management(prcpFile, prjFile)

env.snapRaster = demFile
env.cellSize = demFile
env.mask = demFile
env.extent = demFile

arcpy.ProjectRaster_management(prcpFile, prcpPrjFile, demFile, 'BILINEAR', demFile, transformation)

dem = Raster(demFile)
prcp = Raster(prcpPrjFile)
prcpClip = Con(dem > 0, prcp)
prcpClip.save(prcp10mFile)

del dem, prcp, prcpClip
arcpy.Delete_management(prcpFile)
arcpy.Delete_management(prcpPrjFile)

# Create metadata
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\nonContributingAreas\preparePrecipData.py"'
metadata = os.path.basename(backupAscii) + ' and ' + os.path.basename(prcp10mFile)\
	+ ' were created/changed by ' + user + " on " + timeNow + ' using '+ scriptFile\
	+ '. Data is in 1000ths of inches.\n\n'
f = open(metadataFile, 'a+')
f.write(metadata)
f.close()
