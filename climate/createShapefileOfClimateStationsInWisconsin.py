#####################################################
#
#  createShapefileOfClimateStationsInWisconsin_tool.py
#
#  Written by:
#  Sarah Kempen, WI DNR and
#  Aaron Ruesch, WI DNR
#
######################################################

import arcpy, ftplib, time, os, traceback
import numpy as np
from ftplib import FTP
from arcpy import env
env.overwriteOutput = True

clipShapefile = arcpy.GetParameterAsText(0)
bufferSize = arcpy.GetParameterAsText(1)
noaaDatabase = arcpy.GetParameterAsText(2)
outputShapefile = arcpy.GetParameterAsText(3)

def getStations(clipShapefile, bufferSize, noaaDatabase, outputShapefile):
	user = os.environ['USERNAME']
	installDir = arcpy.GetInstallInfo("desktop")["InstallDir"]
	installDir = installDir.replace('\\', '/')
	tempDir = os.environ['TEMP']
	tempDir = tempDir.replace('\\','/')
	# Input
	noaaFtpSite = 'ftp.ncdc.noaa.gov'
	if noaaDatabase == 'GHCN (Global Historical Climatology Network)':
		stationLink = 'pub/data/ghcn/daily/ghcnd-stations.txt'
	else:
		stationLink = 'pub/data/inventories/ISH-HISTORY.TXT'
	wgs84file = installDir + r'\Coordinate Systems\Geographic Coordinate Systems\World\WGS 1984.prj'
	if env.scratchWorkspace == None:
		env.scratchWorkspace = tempDir
	# Intermediate Files
	stationFile = tempDir + '/stationInfo.txt'
	cleanedFile = tempDir + '/stationInfo_cleaned.txt'
	globalStations = tempDir + '/globalStations.shp'
	buffer50Mile = tempDir + '/buffer50Mile.shp'
	arcpy.AddMessage("Retrieve station text file (may take awhile depending on server)...")
	tries = 0
	while tries != 1000:
		tries += 1
		try:
			arcpy.AddMessage("    Trying " + noaaFtpSite)
			ftp = FTP(noaaFtpSite)
			ftp.login()
			ftpCall = 'RETR ' + stationLink
			f = open(stationFile, 'w')
			ftp.retrbinary(ftpCall, f.write)
			ftp.quit()
			break 
		except:
			arcpy.AddMessage("    Server error, waiting 5 seconds...")
			arcpy.AddMessage(traceback.print_exc())
			time.sleep(5)
	if tries == 1000:
		raise Error ("Tried server 1000 times...")
	if noaaDatabase == 'GHCN (Global Historical Climatology Network)':
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
	else:
		fixedWidths = [7, 6, 30, 6, 3, 5, 7, 8, 10, 9, 9]
		dtypes = np.dtype([('USAF', 'S7')\
			, ('WBAN', 'S6')\
			, ('STATION NAME', 'S30')\
			, ('CTRY', 'S6')\
			, ('ST', 'S3')\
			, ('CALL', 'S5')\
			, ('LAT', float)\
			, ('LON', float)\
			, ('ELEV', float)\
			, ('BEGIN', 'S9')\
			, ('END', 'S9')])
	# Use skiprows if version earlier than 1.3, otherwise skip_header
	if noaaDatabase == 'GHCN (Global Historical Climatology Network)':
		skiprows = 0
	else:
		skiprows = 22
	arcpy.AddMessage("Reading station text file on local disk...")
	if float(np.version.version[0:3]) >= 1.7:
		stationData = np.genfromtxt (stationFile, dtype = dtypes, skip_header = skiprows\
			, delimiter = fixedWidths)
	else:
		stationData = np.genfromtxt (stationFile, dtype = dtypes, skiprows = skiprows\
			, delimiter = fixedWidths)
	if noaaDatabase == 'GSOD (Global Summary of the Day)':
		stationData = stationData[stationData['LAT'] != -99999.0]
		stationData = stationData[stationData['LAT'] != 0.0]
		stationData = stationData[np.invert(np.isnan(stationData['LAT']))]
		stationData = stationData[stationData['LON'] != -99999.0]
		stationData = stationData[stationData['LON'] != 0.0]
		stationData = stationData[np.invert(np.isnan(stationData['LON']))]
		stationData['LAT'] = stationData['LAT'] / 1000.
		stationData['LON'] = stationData['LON'] / 1000.
	if noaaDatabase == 'GHCN (Global Historical Climatology Network)':
		stationData = stationData[['ID','LAT','LON']]
		stationData['ID'] = np.char.rstrip(stationData['ID'], ' ')
		fmt = '%s,%f,%f'
	else:
		stationData = stationData[['USAF', 'WBAN', 'LAT', 'LON', 'BEGIN', 'END']]
		for col in ['USAF', 'WBAN', 'BEGIN', 'END']:
			stationData[col] = np.char.rstrip(stationData[col], ' ')
			stationData[col] = np.char.lstrip(stationData[col], ' ')
		fmt = '%s,%s,%f,%f,%s,%s'
	arcpy.AddMessage("Writing cleaned station file to local disk...")
	f = open(cleanedFile, 'w')
	f.write(','.join(stationData.dtype.names) + '\n')
	np.savetxt(f, stationData, fmt = fmt)
	f.close()
	arcpy.MakeXYEventLayer_management(cleanedFile, "LON", "LAT", "latLongPoints", wgs84file)
	arcpy.CopyFeatures_management("latLongPoints", globalStations)
	bufferSize = str(bufferSize) + " Miles"
	arcpy.Buffer_analysis(clipShapefile, buffer50Mile, "50 miles")
	env.outputCoordinateSystem = clipShapefile
	arcpy.Clip_analysis(globalStations, buffer50Mile, outputShapefile)
	# Create metadata
	metadataFile = os.path.dirname(outputShapefile) + '/README.txt'
	outputShapefile = os.path.basename(outputShapefile)
	timeNow = time.strftime("%a, %d %b %Y %H:%M:%S", time.gmtime())
	scriptFile = r'"\code\climate\createShapefileOfClimateStationsInWisconsin_tool.py"'
	metadataOut = outputShapefile + " was created/changed by " + user + " on " + timeNow\
		+ ' using ' + scriptFile + '\n\n'
	f = open(metadataFile, 'a+')
	f.write(metadataOut)
	f.close()

if __name__ == '__main__':
	getStations(clipShapefile, bufferSize, noaaDatabase, outputShapefile)
