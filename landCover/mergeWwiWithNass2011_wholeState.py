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
scratch = 'D:/temp/temp.gdb'
############################

# Input files
nass2011File = 'T:/GIS/Statewide_Coverages/Landcover/NASS/NASS_2011_clipHuc8.img'
nass2011 = Raster(nass2011File)
wetlandFC = sdeConn + '/W23321.WT_WETLAND_DATA_24K/W23321.WT_WETLAND_AR_24K'
# Intermediate Files
copyFC = scratch + '/wwi_copy'
nassPrj = scratch + '/nass_prj'
wetlandRasterFile = scratch + r'\wwi'
# Output files
mergedFile = 'T:/GIS/Statewide_Coverages/Landcover/NASS/nass2011amendedByWwi.tif'
mergedMetadataFile = 'T:/GIS/Statewide_Coverages/Landcover/NASS/readme.txt'
# Other variables
classField = "WETLAND_CLASS_DESC"
rasterValueField = "rasterValue"

#####
# RUN
#####

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

arcpy.CopyFeatures_management(wetlandFC, copyFC)
arcpy.AddField_management(copyFC, rasterValueField, 'SHORT', '', '', '', '', 'NULLABLE')
exp = '"' + classField + '" NOT IN ' + str(naClasses)
fields = classField + '; ' + rasterValueField
rows = arcpy.UpdateCursor(copyFC, exp, "", fields)
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
arcpy.PolygonToRaster_conversion(copyFC, rasterValueField, wetlandRasterFile\
	, 'MAXIMUM_COMBINED_AREA', '', nass2011File)
# arcpy.Delete_management(copyFC)
wetlandRaster = Raster(wetlandRasterFile)

arcpy.AddMessage("Merge WWI and NASS rasters")
# If NASS pixel is 'developed' class, maintain fidelity, otherwise assign wetland pixel
# from wwi raster
merge = Con((IsNull(wetlandRaster)) | ((nass2011 >= 121) & (nass2011 <= 124)) | (nass2011 == 250)\
	, nass2011, wetlandRaster)
merge.save(mergedFile)

del wetlandRaster
# arcpy.Delete_management(wetlandRasterFile)

# Create metadata
mergedFile = os.path.basename(mergedFile)
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\landCover\mergeWwiWithNass2011_wholeState.py"'

mergedMetadataOut = mergedFile + " was created/changed by " + user + " on " + timeNow\
	+ ' using ' + scriptFile + '\n\n'
f = open(mergedMetadataFile, 'a+')
f.write(mergedMetadataOut)
f.close()



