import arcpy, time, os

user = 'ruesca'
in24kDir = 'C:/Users/' + user + '/Documents/GIS/24kAttribution'

tmdlRoot = '//dnr/gis/WD_Projects/DNR/Water/TMDL'
wiRiverTmdlDir = tmdlRoot + '/Projects/Wisconsin_River'
hydroDir = wiRiverTmdlDir + '/GIS_Datasets/Hydrology'
inGdb = in24kDir + '/Spatial24k03142013.gdb'
outGdb = hydroDir + '/Spatial24k03142013.gdb'
watershedFile =  wiRiverTmdlDir + '/GIS_Datasets/Watersheds/WRB_Basin_2mile_Buffer.shp'
metadataFile = hydroDir + '/README.txt'

# Create outGdb
arcpy.CreateFileGDB_management(hydroDir, 'Spatial24k03142013.gdb', '10.0')

arcpy.Clip_analysis(inGdb + '/flowlines', watershedFile, outGdb + '/flowlines')
arcpy.Clip_analysis(inGdb + '/nodes', watershedFile, outGdb + '/nodes')
arcpy.Clip_analysis(inGdb + '/waterbodies', watershedFile, outGdb + '/waterbodies')
arcpy.Clip_analysis(inGdb + '/watersheds', watershedFile, outGdb + '/watersheds')
arcpy.Copy_management(inGdb + '/noderelationships', outGdb + '/noderelationships')
arcpy.Copy_management(inGdb + '/relationships', outGdb + '/relationships')
arcpy.Clip_management(inGdb + '/reaches', '', outGdb + '/reaches', watershedFile, True)
arcpy.Clip_management(inGdb + '/riparianBuffers', '', outGdb + '/riparianBuffers'\
	, watershedFile, True)

# Create metadata
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\nonContributingAreas\move24kSpatialDataToTmdlNetworkDrive.py"'
metadata = os.path.basename(outGdb) + ' was created/changed by ' + user + " on "\
	+ timeNow + ' using '+ scriptFile
f = open(metadataFile, 'a+')
f.write(metadata)
f.close()





