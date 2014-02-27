import arcpy, os, time
import numpy as np
from arcpy import env
arcpy.CheckOutExtension("Spatial")
from arcpy.sa import *
env.overwriteOutput = True

tempDir = os.environ['TEMP']
tempGdb = tempDir + '/temp.gdb'
if not os.path.exists(tempGdb):
	arcpy.CreateFileGDB_management(tempDir, "temp.gdb", "CURRENT")

###########################
# Set up SDE Connection
homeDir = 'C:/Users/' + os.environ['USERNAME']
dbConnDir = homeDir + '/AppData/Roaming/ESRI/Desktop10.0/ArcCatalog/'
sdeName = 'Connection to dnr_sde.sde'
sdeConn = dbConnDir + sdeName
# Set up TMDL network paths
tmdlRoot = '//dnr/gis/WD_Projects/DNR/Water/TMDL'
wiRiverTmdlDir = tmdlRoot + '/Projects/Wisconsin_River'
demDir = wiRiverTmdlDir + '/GIS_Datasets/DEM'
nonContribDir = wiRiverTmdlDir + '/GIS_Datasets/Non_Contributing_Areas'
# Input files
demFile = demDir + '/WRB_dem'
fillFile = demDir + '/WRB_fill'
fdrFile = demDir + '/WRB_fdr'
cnFile = tmdlRoot + '/GIS/Statewide_Coverages/Runoff/cn2006_10mWTM.tif'
prcpFile = wiRiverTmdlDir + '/GIS_Datasets/Climatological/prcpFreq/prcp_10yr24hr.tif'
boundaryFC = wiRiverTmdlDir + '/GIS_Datasets/Watersheds/WRB_Basin_2mile_Buffer.shp'
puFC = wiRiverTmdlDir + '/GIS_Datasets/Watersheds/HUC_Subwatersheds/WRB_HUC10.shp'
flowlineFile = wiRiverTmdlDir + '/GIS_Datasets/Hydrology/Spatial24k03142013_WRB.gdb/flowlines'
llTableFile = wiRiverTmdlDir + '/GIS_Datasets/Hydrology/Hydro24k03052013.mdb/BaseAttributes_24k'
# Intermediate Files
pus = tempGdb + '/pu'
# Output files
nonContributingAreasFile = nonContribDir + '/nonContributingAreas.tif'
nonContributingAreasMetadataFile = nonContribDir + '/README.txt'

env.scratchWorkspace = tempGdb
env.workspace = tempGdb
env.snapRaster = demFile
env.extent = demFile
env.cellSize = demFile
env.mask = demFile

# Set up processing units
arcpy.CopyFeatures_management(puFC, pus)
idField = arcpy.Describe(pus).OIDFieldName
rows = arcpy.SearchCursor(pus)
ids = []
for row in rows:
	ids.append(row.getValue(idField))
del row, rows
arcpy.MakeFeatureLayer_management(pus, 'puLayer')

for id in ids:
	print str(id)

	pu = tempGdb + '/pu_' + str(id)
	puBuffer = tempGdb + '/puBuffer_' + str(id)
	demPu = tempGdb + '/demPu_' + str(id)
	fillPu = tempGdb + '/fillPu_' + str(id)
	fdrPu = tempGdb + '/fdrPu_' + str(id)
	prcpPu = tempGdb + '/prcpPu_' + str(id)
	cnPu = tempGdb + '/cnPu_' + str(id)
	flowlinePu = tempGdb + '/flowlinePu_' + str(id)
	ncaPu = tempGdb + '/ncaPu_' + str(id)
	runoffTable = tempGdb + '/runoffTable_' + str(id)
	storageTable = tempGdb + '/storageTable_' + str(id)
	trueSinkTable = tempGdb + '/trueSinkTable_' + str(id)
	ncaRaw = tempGdb + '/ncaRaw_' + str(id)
	ncaFiltered = tempGdb + '/ncaFiltered_' + str(id)
	ncaUngrouped = tempGdb + '/ncaUngrouped_' + str(id)
	nonLlFlowlines = tempGdb + '/nonLlFlowlines_' + str(id)
	ncaPu = tempGdb + '/ncaPu_' + str(id)

	expr = '"' + idField + '"  = ' + str(id)
	arcpy.SelectLayerByAttribute_management('puLayer', 'NEW_SELECTION', expr)
	arcpy.CopyFeatures_management('puLayer', pu)
	arcpy.Buffer_analysis('puLayer', puBuffer, '2 Miles')

	arcpy.Clip_management(demFile, '', demPu, puBuffer, '', True)
	arcpy.Clip_management(fillFile, '', fillPu, puBuffer, '', True)
	arcpy.Clip_management(fdrFile, '', fdrPu, puBuffer, '', True)
	arcpy.Clip_management(prcpFile, '', prcpPu, puBuffer, '', True)
	arcpy.Clip_management(cnFile, '', cnPu, puBuffer, '', True)
	arcpy.Clip_analysis(flowlineFile, puBuffer, flowlinePu)
	env.extent = demPu

	# Identify sinks
	sinkDepth = Raster(fillPu) - Raster(demPu)
	A = float(env.cellSize)**2  # area of a gridcell
	storageVolume = sinkDepth * A
	sinkExtent = Con(sinkDepth > 0, 1)
	sinkGroup = RegionGroup(sinkExtent, "EIGHT")
	maxDepth = ZonalStatistics(sinkGroup, "Value", sinkDepth, "MAXIMUM", "DATA")
	prcpMeters = Raster(prcpPu) * 0.0000254
	meanPrecip = ZonalStatistics(sinkGroup, "Value", prcpMeters, "MEAN", "DATA")
	sinkLarge = Con(maxDepth > meanPrecip, sinkGroup)
	del sinkDepth, sinkExtent, sinkGroup, maxDepth

	# Use curve number approach for estimating runoff
	CN = Raster(cnPu) / 100.
	prcpInches = Raster(prcpPu) / 1000.
	S = (1000.0 / CN) - 10.0
	Ia = 0.2 * S
	runoffDepth = (prcpInches - Ia)**2 / (prcpInches - Ia + S)
	runoffVolume = (runoffDepth * 0.0254) * A
	runoffAcc = FlowAccumulation(fdrPu, runoffVolume, 'FLOAT')
	del CN, S, Ia, runoffDepth

	arcpy.BuildRasterAttributeTable_management(sinkLarge, True)

	ZonalStatisticsAsTable(sinkLarge, "VALUE", runoffAcc, runoffTable, "DATA", "MAXIMUM")
	ZonalStatisticsAsTable(sinkLarge, "VALUE", storageVolume, storageTable, "DATA", "SUM")

	arcpy.JoinField_management(runoffTable, 'VALUE', storageTable, 'VALUE')
	arcpy.TableSelect_analysis(runoffTable, trueSinkTable, '"SUM" > "MAX"')

	trueSinks = []
	rows = arcpy.SearchCursor(trueSinkTable, '', '', 'Value')
	for row in rows:
		trueSinks.append(row.Value)
	del row, rows

	seeds = arcpy.sa.ExtractByAttributes(sinkLarge, 'VALUE IN ' + str(tuple(trueSinks)))
	nca = Watershed(fdrPu, seeds)
	env.extent = pus
	del seeds

	arcpy.RasterToPolygon_conversion(nca, ncaRaw, False, 'Value')
	# pull out LL features
	arcpy.MakeFeatureLayer_management(flowlinePu, 'flowline_layer')
	arcpy.MakeTableView_management(llTableFile, 'llTable_view')
	arcpy.AddJoin_management('flowline_layer', 'REACHID', 'llTable_view', 'REACHID', 'KEEP_COMMON')
	arcpy.SelectLayerByAttribute_management('flowline_layer', 'NEW_SELECTION'\
		, '"BaseAttributes_24k.LANDLOCK" = 0')
	arcpy.CopyFeatures_management('flowline_layer', nonLlFlowlines)
	arcpy.MakeFeatureLayer_management(nonLlFlowlines, 'nonLlFlowlines_layer')
	arcpy.MakeFeatureLayer_management(ncaRaw, 'ncaRaw_layer')
	arcpy.SelectLayerByLocation_management('ncaRaw_layer', 'INTERSECT', pu, ''\
		, 'NEW_SELECTION')
	arcpy.SelectLayerByLocation_management('ncaRaw_layer', 'INTERSECT', 'nonLlFlowlines_layer', ''\
		, 'REMOVE_FROM_SELECTION')
	arcpy.CopyFeatures_management('ncaRaw_layer', ncaFiltered)
	arcpy.PolygonToRaster_conversion(ncaFiltered, 'grid_code', ncaUngrouped, 'CELL_CENTER', ''\
		, demFile)
	noId = Reclassify(ncaUngrouped, "Value", RemapRange([[1,10000000000,1]]))
	grouped = RegionGroup(noId, 'EIGHT')
	arcpy.Clip_management(grouped, '', ncaPu, pu, '', True)

ncaFiles = ''
for id in ids:
	ncaFiles = ncaFiles + 'ncaPu_' + str(id) + ';'
ncaFiles = ncaFiles[:-1]
env.extent = demFile
arcpy.MosaicToNewRaster_management(ncaFiles, tempGdb, 'ncaFinal', demFile, '', demFile, 1)
noId = Reclassify(tempGdb + '/ncaFinal', "Value", RemapRange([[1,10000000000,1]]))
grouped = RegionGroup(noId, 'EIGHT')
grouped.save(nonContributingAreasFile)

# Create metadata
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = r'"\code\nonContributingAreas\identifyNoncontributingAreas.py"'
metadata = os.path.basename(nonContributingAreasFile) + ' was created/changed by ' + user + " on "\
	+ timeNow + ' using '+ scriptFile
f = open(nonContributingAreasMetadataFile, 'a+')
f.write(metadata)
f.close()




