import arcpy, os, re, time, struct, calendar, datetime
from dbfpy import dbf
import numpy as np
from ftplib import FTP
from StringIO import StringIO
from arcpy import env
env.overwriteOutput = True
noaaFtpSite = 'ftp.ncdc.noaa.gov'
climateDataLink = 'pub/data/ghcn/daily/all'
stationLocations = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/stationData/locations/stationLocations_GHCND.shp"
tempDir = os.environ['TEMP']
outDir = tempDir
years = np.arange(1995, 2014)
vars = ['PRCP', 'TMIN', 'TMAX']
fieldWidths = [11,4,2,4] + [5,1,1,1] * 31
fieldTypes = ['s'] * 4 + ['i', 's', 's', 's'] * 31
fmtString = ''.join('%ds' % f for f in fieldWidths)
charWidth = 269
valCols = range(4, 126, 4)

rows = arcpy.SearchCursor(stationLocations)
ids = []
for row in rows:
	ids.append(row.ID)
del row, rows

dtypeDays = []
for day in range(1,32):
	dtypeDay = [('val' + str(day), int)\
		, ('M' + str(day), 'S1')\
		, ('Q' + str(day), 'S1')\
		, ('S' + str(day), 'S1')]
	dtypeDays = dtypeDays + dtypeDay

for id in ids:
	ftp = FTP(noaaFtpSite)
	ftp.login()
	idFilename = climateDataLink + '/' + id + '.dly'
	d = StringIO()
	ftp.retrlines('RETR ' + idFilename, d.write)
	d = d.getvalue()
	nRecords = len(d) / charWidth
	dtypes = np.dtype([('ID', 'S11')\
		, ('YEAR', int)\
		, ('MO', int)\
		, ('VAR', 'S4')]\
		+ dtypeDays)
	stationData = np.empty([nRecords, len(fieldTypes)], dtype='S11')
	for i in range(1, nRecords + 1):
		start = (i * charWidth) - charWidth
		end = i * charWidth
		recordData = d[start:end]
		var = recordData[17:21]
		# if var not in vars:
			# continue
		year = recordData[11:15]
		mo = recordData[15:17]
		parse = struct.Struct(fmtString).unpack_from
		fields = parse(recordData)
		stationData[i-1,:] = list(fields)
	stationData[stationData == '-9999'] = '-990'
	# Pull out dates between start and end
	# for var in vars:
		# filename = outDir + '/' + var + '.dbf'
		# tableNames = 'DATE D; PCP N(5,1)'
		# table = dbf.Table(filename, tableNames)
		# table.open()
		# varData = stationData[stationData[:,3] == var]
		# nrow = varData.shape[0]
		# for row in range(0, nrow):
			# year = int(stationData[row, 1])
			# mo = int(stationData[row, 2])
			# nDays = calendar.monthrange(year, mo)[1]
			# days = range(1, nDays + 1)
			# moVals = varData[row, valCols[0:(nDays + 1)]]
			# for day in days:
				# dayData = float(moVals[day - 1]) / 10
				# date = datetime.date(year, mo, day)
				# table.append((date, dayData))
		# table.close()
	for var in vars:
		filename = outDir + '/' + var + '.dbf'
		tableNames = 'DATE D; PCP N(5,1)'
		db = dbf.Dbf(filename, new=True)
		db.addField(
			("DATE", "D"),
			("PRCP", "N", 5, 1)
		)
		varData = stationData[stationData[:,3] == var]
		nrow = varData.shape[0]
		for row in range(0, nrow):
			year = int(stationData[row, 1])
			mo = int(stationData[row, 2])
			nDays = calendar.monthrange(year, mo)[1]
			days = range(1, nDays + 1)
			moVals = varData[row, valCols[0:(nDays + 1)]]
			for day in days:
				dayData = float(moVals[day - 1]) / 10
				rec = db.newRecord()
				rec["DATE"] = (year, mo, day)
				rec[var] = dayData
				rec.store()
		db.close()
