import arcpy, time
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
tmdlRoot = r'T:\'
wiRiverTmdlDir = tmdlRoot + r'\Projects\Wisconsin_River'
stateWetlandDir = tmdlRoot + r'\GIS\Statewide_Coverages\wetlands\DWI'
lcDir = wiRiverTmdlDir + r'\GIS_Datasets\Landcover'
cdlDir = lcDir + r'\Crop_Data_Layers_2008_2012\WTM_Reprojection'
wiRiverWetlandDir = lcDir + r'\Wetland'
# Input files
nass2011File = cdlDir + r'\wrb_cdl_2011'
nass2011 = Raster(nass2011File)
wetlandFC = sdeConn + r'\W23321.WT_WETLAND_DATA_24K\W23321.WT_WETLAND_AR_24K'
boundaryFC = wiRiverTmdlDir + r'\GIS_Datasets\Watersheds\WRB_Basin_2mile_Buffer.shp'
# Intermediate Files
clipFC = scratch + '/clip_WRB'
wetlandRasterFile = scratch + r'\wwi'
# Output files
wetlandNetworkCopy = wiRiverWetlandDir + r'\wwi.tif'
clipNetworkCopy = wiRiverWetlandDir + r'\wwi.shp'
mergedNetworkCopy = lcDir + r'\nass2011amededByWwi.tif'
wetlandMetadataFile = wiRiverWetlandDir + r'\readme.txt'
mergedMetadataFile = lcDir + r'\readme.txt'
# Other variables
classField = "WETLAND_CLASS_DESC"
rasterValueField = "rasterValue"

#####
# RUN
#####

# Clip WWI to 2-mile buffer of Wisconsin River basin
arcpy.AddMessage("Clipping WWI to WI River basin...")
arcpy.Clip_analysis(wetlandFC, boundaryFC, clipFC)
arcpy.CopyFeatures_management(clipFC, clipNetworkCopy)
arcpy.AddField_management(clipFC, rasterValueField, "SHORT", "", "", "", "", "NULLABLE")

arcpy.AddMessage("Crosswalk WWI classes to NASS classes...")
# Do not include these wetland classes
naClasses = ('Deep water lake', 'Filled/drained wetland, Emergent/wet meadow'\
	, 'Filled/drained wetland, Emergent/wet meadow, Aquatic bed'\
	, 'Filled/drained wetland, Emergent/wet meadow, Open Water'\
	, 'Filled/drained wetland, Flats/unvegetated wet soil'\
	, 'Filled/drained wetland, Forested'\
	, 'Filled/drained wetland, Forested, Emergent/wet meadow'\
	, 'Filled/drained wetland, Forested, Scrub/shrub', 'Filled/drained wetland, Open Water'\
	, 'Filled/drained wetland, Scrub/shrub'\
	, 'Filled/drained wetland, Scrub/shrub, Emergent/wet meadow'\
	, 'Open Water', 'River', 'Road', 'Upland')
# Assign these classes a value of NASS 190
woodyClasses = ['Forested', 'Forested, Aquatic bed', 'Forested, Emergent/wet meadow'\
	, 'Forested, Flats/unvegetated wet soil', 'Forested, Open Water'\
	, 'Forested, Scrub/shrub']
# Assign these classes a value of NASS 195
emergentClasses = ['Aquatic bed', 'Aquatic bed, Emergent/wet meadow'\
	, 'Aquatic bed, Open Water', 'Emergent/wet meadow', 'Emergent/wet meadow, Aquatic bed'\
	, 'Emergent/wet meadow, Flats/unvegetated wet soil', 'Emergent/wet meadow, Open Water'\
	, 'Flats/unvegetated wet soil', 'Flats/unvegetated wet soil, Aquatic bed'\
	, 'Flats/unvegetated wet soil, Open Water', 'Moss', 'Scrub/shrub'\
	, 'Scrub/shrub, Aquatic bed', 'Scrub/shrub, Emergent/wet meadow'\
	, 'Scrub/shrub, Flats/unvegetated wet soil', 'Scrub/shrub, Moss'\
	, 'Scrub/shrub, Open Water', 'Wet']

exp = '"' + classField + '" NOT IN ' + str(naClasses)
fields = classField + '; ' + rasterValueField
rows = arcpy.UpdateCursor(clipFC, exp, "", fields)
for row in rows:
	if row.getValue(classField) in woodyClasses:
		row.setValue(rasterValueField, 190)
	if row.getValue(classField) in emergentClasses:
		row.setValue(rasterValueField, 195)
	rows.updateRow(row)
del row, rows

env.snapRaster = nass2011File
env.cellSize = nass2011File
env.extent = nass2011File

arcpy.AddMessage("Rasterize WWI polygons...")
arcpy.PolygonToRaster_conversion(clipFC, rasterValueField, wetlandRasterFile\
	, 'MAXIMUM_COMBINED_AREA', '', nass2011File)
arcpy.Delete_management(clipFC)
wetlandRaster = Raster(wetlandRasterFile)
wetlandRaster.save(wetlandNetworkCopy)

arcpy.AddMessage("Merge WWI and NASS rasters")
# If NASS pixel is 'developed' class, maintain fidelity, otherwise assign wetland pixel
# from wwi raster
merge = Con((IsNull(wetlandRaster)) | ((nass2011 >= 121) & (nass2011 <= 124))\
	, nass2011, wetlandRaster)
merge.save(mergedNetworkCopy)

del wetlandRaster
arcpy.Delete_management(wetlandRasterFile)

# Create metadata
wetlandFile = os.path.basename(wetlandNetworkCopy)
wetlandShapefile = os.path.basename(clipNetworkCopy)
mergedFile = os.path.basename(mergedNetworkCopy)
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\landCover\mergeWwiWithNass2011.py"'

wetlandMetadataOut = wetlandShapefile + ' and ' + wetlandFile + " were created/changed by " + user\
	+ " on " + timeNow + ' using ' + scriptFile + '\n'
f = open(wetlandMetadataFile, 'a+')
f.write(wetlandMetadataOut)
f.close()

mergedMetadataOut = mergedFile + " was created/changed by " + user + " on " + timeNow\
	+ ' using ' + scriptFile + '\n\n'
f = open(mergedMetadataFile, 'a+')
f.write(mergedMetadataOut)
f.close()



