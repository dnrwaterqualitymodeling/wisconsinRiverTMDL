import arcpy, os
from datetime import date, timedelta

#get the folder which all SLAMM output .csv files should be in
#SLAMM_folder = arcpy.GetParameterAsText(0)
SLAMM_folder = 

#time series table name
TimeTable = "FullTimeSeries13.dbf"

#temporary scratch workspace
TempScratch = 'C:\Users\hydrig\Documents\Projects\Wisconsin TMDL Local Files'
arcpy.env.workspace = TempScratch

#set start and end dates
StDate = date(2002, 1, 1)
EnDate = date(2013, 12, 31)
numDays = 4383

#generate full time series table
arcpy.CreateTable_management (TempScratch, TimeTable)
arcpy.AddField_management (TimeTable, "Date", "DATE")
arcpy.AddField_management (TimeTable, "Precip", "TEXT")
arcpy.AddField_management (TimeTable, "Soil_Type", "TEXT")

#populate full time series
cursor = arcpy.da.InsertCursor(TimeTable, "Date")
for x in xrange(0,numDays):
	cursor.insertRow((StDate,))
	StDate = StDate + timedelta(days=1)

#generate list of SLAMM .csv files to be loaded
arcpy.env.workspace = SLAMM_folder
SLAMM_files = arcpy.ListFiles("*.csv")

# # loop through .csv files
 for csv in SLAMM_files:
	# Use splitext to set the output table name
    TableName = os.path.splitext(csv)[0] 
	
	# convert .csv file to geodatabase table
	arcpy.TableToTable_conversion(csv, TempScratch, TableName)
	
	# clean summaries off bottom of tables
	getRows = arcpy.GetCount_management (TableName)
	numRows = int(getRows.getOutput(0))
	cursor = arcpy.da.UpdateCursor(TableName, "*")
	r = 0
	for row in cursor:
		if numRows - r <= 13:			#summaries are last 13 rows	
			cursor.deleteRow()
		else:
			cursor.
		r = r + 1	
	# write something in this loop to have the cursor update precip_file and soil_type




