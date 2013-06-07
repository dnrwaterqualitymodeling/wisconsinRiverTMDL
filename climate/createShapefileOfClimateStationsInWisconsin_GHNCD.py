import arcpy, urllib, time, os
import numpy as np
from arcpy import env
env.overwriteOutput = True
user = os.environ['USERNAME']

# Input
climateDir = 'T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/stationData'
wrbFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Watersheds/WRB_Basin.shp"
stationLink = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
wgs84file = "C:/Program Files (x86)/ArcGIS/Desktop10.0/Coordinate Systems/Geographic Coordinate Systems/World/WGS 1984.prj"
# Intermediate Files
stationFile = 'C:/Users/' + user + '/Downloads/stationInfo.txt'
cleanedFile = 'C:/Users/' + user + '/Downloads/stationInfo_cleaned.txt'
globalStations = 'C:/TEMP/globalStations.shp'
buffer50Mile = 'C:/TEMP/buffer50Mile.shp'
# Output Files
wisconsinStations = climateDir + '/locations/stationLocations_GHNCD.shp'
urllib.urlretrieve (stationLink, stationFile)
fixedWidths = [12, 9, 10, 7, 3, 31, 4, 4, 5]
dtypes = np.dtype([('ID', 'S12')\
          , ('LAT', float)\
          , ('LON', float)\
          , ('C4', 'S7')\
          , ('STATE', 'S3')\
          , ('NAME', 'S31')\
          , ('GSN', 'S4')\
          , ('HCN', 'S4')\
          , ('C9', 'S5')])
# Use skiprows if version earlier than 1.3, otherwise skip_header
if float(np.version.version[0:3]) >= 1.7:
	stationData = np.genfromtxt (stationFile, dtype = dtypes, skip_header = 22\
		, delimiter = fixedWidths)
else:
	stationData = np.genfromtxt (stationFile, dtype = dtypes, skiprows = 22\
		, delimiter = fixedWidths)
recovery = np.empty_like(stationData)
recovery[:] = stationData
# ------------------------------------------

stationData = np.empty_like(recovery)
stationData[:] = recovery
# stationData = stationData[stationData['LAT'] != -99999.0]
# stationData = stationData[stationData['LAT'] != 0.0]
# stationData = stationData[np.invert(np.isnan(stationData['LAT']))]
# stationData = stationData[stationData['LON'] != -99999.0]
# stationData = stationData[stationData['LON'] != 0.0]
# stationData = stationData[np.invert(np.isnan(stationData['LON']))]
# stationData['LAT'] = stationData['LAT'] / 1000.
# stationData['LON'] = stationData['LON'] / 1000.
stationData = stationData[['ID', 'LAT', 'LON']]
# Strip leading and trailing characters
stationData['ID'] = np.char.rstrip(stationData['ID'], ' ')

f = open(cleanedFile, 'w')
f.write(','.join(stationData.dtype.names) + '\n')
np.savetxt(f, stationData, fmt = '%s,%f,%f')
f.close()
##arcpy.MakeTableView_management(cleanedFile, "stationTable")
arcpy.MakeXYEventLayer_management(cleanedFile, "LON", "LAT", "latLongPoints", wgs84file)
arcpy.CopyFeatures_management("latLongPoints", globalStations)
arcpy.Buffer_analysis(wrbFile, buffer50Mile, "50 miles")
env.outputCoordinateSystem = wrbFile
arcpy.Clip_analysis(globalStations, buffer50Mile, wisconsinStations)
