import arcpy, os, time
import numpy as np
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
# Set up SDE Connection
homeDir = 'C:/Users/' + user
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
flowlineFile = wiRiverTmdlDir + '/GIS_Datasets/Hydrology/Spatial24k03142013_WRB.gdb/flowlines'
llTableFile = wiRiverTmdlDir + '/GIS_Datasets/Hydrology/Hydro24k03052013.mdb/BaseAttributes_24k'
# Intermediate Files
clipCn = scratch + '/clipCn'
optimFillFile = scratch + '/optimFill'
runoffTable = scratch + '/runoffTable'
storageTable = scratch + '/storageTable'
trueSinkTable = scratch + '/trueSinkTable'
nonContribRaw = scratch + '/nonContribRaw'
nonContribFiltered = scratch + '/nonContribFiltered'
nonContribUngrouped = scratch + '/nonContribUngrouped'
nonLlFlowlines = scratch + '/nonLlFlowlines'
# Output files
nonContributingAreasFile = nonContribDir + '/nonContributingAreas.tif'
nonContributingAreasMetadataFile = nonContribDir + '/README.txt'

env.scratchWorkspace = scratch
env.workspace = scratch
env.snapRaster = demFile
env.extent = demFile
# env.extent = "G:/temp/lincolnCounty.shp"
env.cellSize = demFile
env.mask = demFile

sinkDepth = Raster(fillFile) - Raster(demFile)
A = float(env.cellSize)**2  # area of a gridcell
storageVolume = sinkDepth * A
sinkExtent = Con(sinkDepth > 0, 1)
sinkGroup = RegionGroup(sinkExtent, "EIGHT")
maxDepth = ZonalStatistics(sinkGroup, "Value", sinkDepth, "MAXIMUM", "DATA")
prcpMeters = Raster(prcpFile) * (1./1000.) * 0.0254
meanPrecip = ZonalStatistics(sinkGroup, "Value", prcpMeters, "MEAN", "DATA")
sinkLarge = Con(maxDepth > meanPrecip, sinkGroup)
del sinkDepth, sinkExtent, sinkGroup, maxDepth

CN = Raster(cnFile) / 100
prcpInches = Raster(prcpFile) * (1./1000.)
S = (1000.0 / CN) - 10.0
Ia = 0.2 * S
runoffDepth = (prcpInches - Ia)**2 / (prcpInches - Ia + S)
runoffVolume = (runoffDepth * 0.0254) * A
fdr = Raster(fdrFile)
runoffAcc = FlowAccumulation(fdr, runoffVolume, 'FLOAT')
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

seeds = ExtractByAttributes(sinkLarge, 'VALUE IN ' + str(tuple(trueSinks)))
nonContributingAreas = Watershed(fdr, seeds)
del seeds, fdr

arcpy.RasterToPolygon_conversion(nonContributingAreasFile, nonContribRaw, False, 'Value')
arcpy.MakeFeatureLayer_management(flowlineFile, 'flowline_layer')
arcpy.MakeTableView_management(llTableFile, 'llTable_view')
arcpy.AddJoin_management('flowline_layer', 'REACHID', 'llTable_view', 'REACHID', 'KEEP_COMMON')
arcpy.SelectLayerByAttribute_management('flowline_layer', 'NEW_SELECTION'\
	, '"BaseAttributes_24k.LANDLOCK" = 0')
arcpy.CopyFeatures_management('flowline_layer', nonLlFlowlines)
arcpy.MakeFeatureLayer_management(nonLlFlowlines, 'nonLlFlowlines_layer')
arcpy.MakeFeatureLayer_management(nonContribRaw, 'nonContribRaw_layer')
arcpy.SelectLayerByLocation_management('nonContribRaw_layer', 'INTERSECT', 'nonLlFlowlines_layer'\
	, '', 'NEW_SELECTION')
arcpy.CopyFeatures_management('nonContribRaw_layer', nonContribFiltered)
arcpy.PolygonToRaster_conversion(nonContribFiltered, 'grid_code'\
	, nonContribUngrouped, 'CELL_CENTER', '', demFile)
noId = Reclassify(nonContribUngrouped, "Value"\
	, RemapRange([[0,1000000000000000,1]]))

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




