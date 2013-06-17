import arcpy, os, re, time, struct, calendar, datetime, copy
from dbfpy import dbf
import numpy as np
from ftplib import FTP
from StringIO import StringIO
from arcpy import env
env.overwriteOutput = True
noaaFtpSite = 'ftp.ncdc.noaa.gov'
climateDataLink = 'pub/data/ghcn/daily/all'
stationLocations = arcpy.GetParameterAsText(0)
startYr = int(arcpy.GetParameterAsText(1))
endYr = int(arcpy.GetParameterAsText(2))
ghcndVars = arcpy.GetParameterAsText(3)
outDir = arcpy.GetParameterAsText(4)
tempDir = os.environ['TEMP']
fieldWidths = [11,4,2,4] + [5,1,1,1] * 31
fieldTypes = ['s'] * 4 + ['i', 's', 's', 's'] * 31
fmtString = ''.join('%ds' % f for f in fieldWidths)
valCols = range(4,125,4)
charWidth = 269
ghcndVars = ['PRCP', 'TMAX', 'TMIN']
varMap = {'PRCP' : 'PCP'\
	, 'TMAX' : 'MAX'\
	, 'TMIN' : 'MIN'\
	, 'SNOW' : 'SNW'\
	, 'SNWD' : 'SND'\
	, 'AWND' : 'WND'\
}

def createTimeSeries(startYr, endYr):
	startDate = datetime.date(startYr, 1, 1)
	endDate = datetime.date(endYr, 12, 31)
	nDays = (endDate - startDate).days
	dates = np.empty([nDays,3], int)
	delta = datetime.timedelta(days=1)
	day = copy.deepcopy(startDate)
	for row in range(0,nDays):
		dates[row,0] = day.year
		dates[row,1] = day.month
		dates[row,2] = day.day
		day += delta
	return dates

def readStationInfo(stationLocations):
	rows = arcpy.SearchCursor(stationLocations)
	stationIds = []
	xpr = []
	ypr = []
	for row in rows:
		stationIds.append(row.ID)
		xpr.append(row.X)
		ypr.append(row.Y)
	del row, rows
	stationInfo = {'ID' : stationIds, 'XPR' : xpr, 'YPR' : ypr}
	return stationInfo

def retrieveStationData(ftp, dailyFile, charWidth, ghcndVars, startYr, endYr):
	d = StringIO()
	ftp.retrlines('RETR ' + dailyFile, d.write)
	d = d.getvalue()
	nRecords = len(d) / charWidth
	stationData = np.empty([nRecords, len(fieldTypes)], dtype='S11')
	for i in range(1, nRecords + 1):
		start = (i * charWidth) - charWidth
		end = i * charWidth
		recordData = d[start:end]
		var = recordData[17:21]
		if var not in ghcndVars:
			stationData[i-1,:] = ''
			continue
		year = recordData[11:15]
		if not startYr <= int(year) <= endYr:
			stationData[i-1,:] = ''
			continue
		mo = recordData[15:17]
		parse = struct.Struct(fmtString).unpack_from
		fields = parse(recordData)
		stationData[i-1,:] = list(fields)
	stationData[stationData == '-9999'] = '-990'
	stationData = stationData[stationData[:,3] != '']
	return stationData

def restructureClimateData(stationData, ghcndVar, valCols):
	varData = stationData[stationData[:,3] == ghcndVar]
	if len(varData) == 0:
		return None
	nrow = varData.shape[0]
	nDays = 0
	for row in range(0, nrow):
		year = int(varData[row, 1])
		mo = int(varData[row, 2])
		mDays = calendar.monthrange(year, mo)[1]
		nDays = nDays + mDays
	outData = np.zeros([nDays,4], float)
	i = 0
	for row in range(0, nrow):
		year = int(varData[row, 1])
		mo = int(varData[row, 2])
		mDays = calendar.monthrange(year, mo)[1]
		moVals = varData[row, valCols[0:mDays]]
		moVals = moVals.astype(np.float)
		if ghcndVar in ['PRCP', 'TMAX', 'TMIN']:
			moVals = moVals / 10
		outInd = range(i, i + mDays)
		outData[outInd, 0] = year
		outData[outInd, 1] = mo
		outData[outInd, 2] = range(1, mDays + 1)
		outData[outInd, 3] = moVals
		i = i + mDays
	return outData

def writeClimData(climData, stationId, outDir):
	for ghcndVar in climData.keys():
		if ghcndVar == 'TMP':
			outFile = outDir + '/' + stationId + 'tmp.dbf'
		else:
			outFile = outDir + '/' + stationId + varMap[ghcndVar].lower() + '.dbf'
		db = dbf.Dbf(outFile, new=True)
		if ghcndVar == 'TMP':
			db.addField(
				('DATE', 'D')\
				, ('MAX','N', 5, 1)\
				, ('MIN','N', 5, 1)\
			)
		else:
			db.addField(
				("DATE", "D")\
				, (varMap[ghcndVar],'N', 5, 1)
			)
		varData = climData[ghcndVar]
		for row in range(0,varData.shape[0]):
			rec = db.newRecord()
			rec["DATE"] = (int(varData[row,0]), int(varData[row,1]), int(varData[row,2]))
			if ghcndVar == 'TMP':
				rec['MAX'] = varData[row,3]
				rec['MIN'] = varData[row,4]
			else:
				rec[varMap[ghcndVar]] = varData[row,3]
			rec.store()
		db.close()
		del db

def populateTimeSeries(dates, climData):
	ghcndVars = climData.keys()
	timeSeries = np.zeros([dates.shape[0], len(climData.keys()) + 3], float)
	timeSeries[:,[0,1,2]] = dates
	for row in range(0,dates.shape[0]):
		year = timeSeries[row,0]
		month = timeSeries[row,1]
		day = timeSeries[row,2]
		for i,ghcndVar in enumerate(ghcndVars):
			yearBool = climData[ghcndVar][:,0] == year
			monthBool = climData[ghcndVar][:,1] == month
			dayBool = climData[ghcndVar][:,2] == day
			# ind = yearBool & monthBool & dayBool
			val = climData[ghcndVar][yearBool & monthBool & dayBool,3]
			if len(val) == 0:
				val = -99.0
			col = i + 3
			timeSeries[row,col] = val
	climData = {}
	for i,ghcndVar in enumerate(ghcndVars):
		climData[ghcndVar] = timeSeries[:,([0,1,2] + [i + 3])]
	return climData

def writeStationInfo(stationInfo, outDir):
	filename = outDir + '/wgnstations.dbf'
	nrow = len(stationInfo['ID'])
	db = dbf.Dbf(filename, new=True)
	db.addField(
		('ID', 'N', 5, 0)\
		, ('NAME','C', 30)\
		, ('XPR','N', 15, 6)\
		, ('YPR','N', 15, 6)\
	)
	for row in range(0,nrow):
		rec = db.newRecord()
		rec['ID'] = row + 1
		rec['NAME'] = stationInfo['ID'][row]
		rec['XPR'] = stationInfo['XPR'][row]
		rec['YPR'] = stationInfo['YPR'][row]
		rec.store()
	db.close()
	del db

def mainFunction(noaaFtpSite, stationLocations, startYr, endYr, climateDataLink, charWidth\
	, ghcndVars, valCols, outDir):
	ftp = FTP(noaaFtpSite)
	ftp.login()
	stationInfo = readStationInfo(stationLocations)
	dates = createTimeSeries(startYr, endYr)
	for stationId in stationInfo['ID']:
		arcpy.AddMessage("Downloading and formatting station ID " + stationId + '...'
		dailyFile = climateDataLink + '/' + stationId + '.dly'
		stationData = retrieveStationData(ftp, dailyFile, charWidth, ghcndVars, startYr, endYr)
		if len(stationData) == 0:
			continue
		climData = {}
		for ghcndVar in ghcndVars:
			climData[ghcndVar] = restructureClimateData(stationData, ghcndVar, valCols)
			if climData[ghcndVar] == None:
				del climData[ghcndVar]
		climData = populateTimeSeries(dates, climData)
		if 'TMAX' in climData.keys() and 'TMIN' in climData.keys():
			maxData = np.copy(climData['TMAX'])
			shape = (maxData.shape[0], maxData.shape[1] + 1)
			tmpData = np.zeros(shape, float)
			tmpData[:,range(0,4)] = maxData
			tmpData[:,4] = climData['TMIN'][:,3]
			climData['TMP'] = tmpData
			del climData['TMAX'], climData['TMIN']
		writeClimData(climData, stationId, outDir)
	writeStationInfo(stationInfo, outDir)

if __name__ == '__main__':
	mainFunction(noaaFtpSite, stationLocations, startYr, endYr, climateDataLink, charWidth\
		, ghcndVars, valCols, outDir)

