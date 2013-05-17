import arcpy, os
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
# Set up TMDL network paths
tmdlRoot = '//dnr/gis/WD_Projects/DNR/Water/TMDL'
wiRiverTmdlDir = tmdlRoot + '/Projects/Wisconsin_River'
demDir = wiRiverTmdlDir + '/GIS_Datasets/DEM'
nonContribDir = wiRiverTmdlDir + '/GIS_Datasets/nonContributingAreas'
# Input files
optFillExe = r"%UserProfile%\Documents\hydroSoftware\OptimPitRemoval\OptimizedPitRemoval.exe"
demFile = demDir + '/WRB_dem'
fillFile = demDir + '/WRB_fill'
fdrFile = demDir + '/WRB_fdr'
cnFile = tmdlRoot + '/GIS/Statewide_Coverages/Runoff/cn2006_10mWTM.tif'
prcpFle = wiRiverTmdlDir + '/GIS_Datasets/Climatological/prcpFreq/prcp_10yr24hr.tif'
boundaryFC = wiRiverTmdlDir + '/GIS_Datasets/Watersheds/WRB_Basin_2mile_Buffer.shp'
# Intermediate Files
clipCn = scratch + '/clipCn'
optimFillFile = scratch + '/optimFill'
runoffTable = scratch + '/runoffTable'
storageTable = scratch + '/storageTable'
trueSinkTable = scratch + '/trueSinkTable'
# Output files
facFile = scratch + '/fac'
nonContributingAreasFile = scratch + '/nonContributingAreas'

env.scratchWorkspace = scratch
env.workspace = scratch
env.snapRaster = demFile
env.extent = demFile
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

CN = Raster(cnFile)
prcpInches = Raster(prcpFile) * (1./1000.)
S = (1000.0 / CN) - 10.0
Ia = 0.2 * S
runoffDepth = (prcpInches - Ia)**2 / (prcpInches - Ia + S)
runoffVolume = (runoffDepth * 0.0254) * A
fdr = Raster(fdrFile)
runoffAcc = FlowAccumulation(fdr, runoffVolume, 'FLOAT')
del CN, S, Ia, runoffDepth

runoffTable = ZonalStatisticsAsTable(sinkLarge, "Value", runoffAcc, runoffTable, "DATA", "MAX")
storageTable = ZonalStatisticsAsTable(sinkLarge, "Value", storageVolume, storageTable, "DATA", "SUM")

arcpy.JoinField_management(runoffTable, 'Value', storageTable, 'Value', 'KEEP_COMMON')
arcpy.TableSelect_analysis(runoffTable, trueSinkTable, '"SUM" > "MAX"')

trueSinks = []
rows = arcpy.SearchCursor(trueSinkTable, '', '', 'Value')
for row in rows:
	trueSinks.append(row.Value)
del row, rows

seeds = ExtractByAttributes(sinkLarge, "VALUE IN " + str(tuple(trueSinks)))
nonContributingAreas = Watershed(fdr, seeds)
nonContributingAreas.save(nonContributingAreasFile)
del seeds, fdr

