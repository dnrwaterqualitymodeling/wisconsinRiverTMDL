import arcpy, os
from datetime import date, timedelta


#declare variables -----------------


#the folder which all SLAMM output .csv files should be in
SLAMM_folder = "T:\Projects\Wisconsin_River\Model_Inputs\WinSLAMM_Inputs\WinSLAMM"
#SLAMM_folder = "C:\Users\hydrig\Documents\Projects\WinSLAMM"

soils = "T:\Projects\Wisconsin_River\GIS_Datasets\Urban\SLAMM Model Area\SLAMM_Soil_Texture_Draft_1.shp"
UABs = "T:\Projects\Wisconsin_River\GIS_Datasets\Urban\Urban Area Boundaries\SWAT_Urban_Areas.gdb\SWAT_Urban_Areas6"
subbasins = "T:\Projects\Wisconsin_River\Model_Inputs\SWAT_Inputs\hydro\subbasins.shp"
pcpLookup = "T:\Projects\Wisconsin_River\GIS_Datasets\Urban\SLAMM Model Area\MS4_Subbasins\PCP_export.xlsx\'Guy Lookup$'"

#temporary scratch workspace
TempScratch = "C:\Users\hydrig\Documents\Projects\Wisconsin TMDL Local Files\Try3.gdb"
arcpy.env.workspace = TempScratch

#permitted sewersheds
PerMS4s = [
r'Permitted Reachshed\SB_Weston4',
r'Permitted Reachshed\SB_Wausau2',
r'Permitted Reachshed\SB_Schofield2',
r'Permitted Reachshed\SB_Rothschild3',
r'Permitted Reachshed\SB_RibMtn',
r'Permitted Reachshed\SB_Rapids',
r'Permitted Reachshed\SB_Mosinee',
r'Permitted Reachshed\SB_Merrill',
r'Permitted Reachshed\SB_Marshfield',
r'Permitted Reachshed\SB_Kronen2',
r'Permitted Reachshed\SB_Baraboo'
]

#full time series output file
TimeTable = "FullTimeSeries2"


#define functions--------------------


#generate full time series table
def genTimeSeries(stDate, numDays):
	arcpy.CreateTable_management (TempScratch, TimeTable)
	arcpy.AddField_management (TimeTable, "Date", "DATE")
	arcpy.AddField_management (TimeTable, "Subbasin", "TEXT")
	arcpy.AddField_management (TimeTable, "Sed_load", "DOUBLE")
	arcpy.AddField_management (TimeTable, "Min_P_Load", "DOUBLE")
	arcpy.AddField_management (TimeTable, "Org_P_Load", "DOUBLE")
	cursor = arcpy.da.InsertCursor(TimeTable, ("Date"))
	for x in xrange(0,numDays):
		cursor.insertRow((stDate,))
		stDate = stDate + timedelta(days=1)
	del cursor
	

#generate list of SLAMM .csv files to be loaded
#Ann said the .csv files were the authoritative outputs
def getSlammOutputs():
	SLAMM_files = []
	arcpy.env.workspace = SLAMM_folder
	wksps = arcpy.ListWorkspaces()
	for w in wksps:
		arcpy.env.workspace = w
		csv_files = arcpy.ListFiles("*.csv")
		if csv_files == "None":
			continue
		for csv in csv_files:
			print csv
			TableName = os.path.splitext(csv)[0]
			TableName = TableName.replace('-Output', '')
			TableName = TableName.rstrip()
			SLAMM_files.append(TableName)
			arcpy.TableToTable_conversion(csv, TempScratch, TableName)
			arcpy.AddField_management(TempScratch + '\\' + TableName, "Join_Date", "DATE")
			
			# clean summaries off bottom of tables
			getRows = arcpy.GetCount_management(TempScratch + '\\' + TableName)
			numRows = int(getRows.getOutput(0))
			cursor = arcpy.da.UpdateCursor(TempScratch + '\\' + TableName, ["Rain_Start_Date", "Join_Date"])
			r = 0
			for row in cursor:
				if numRows - r <= 13:			#summaries are last 13 rows	
					cursor.deleteRow()
				else:
					row[1] = row[0]
					cursor.updateRow(row)
				r = r + 1
			del cursor
			
	#reset workspace
	arcpy.env.workspace = TempScratch
	#clean up file naming convention (CL, SA, SL) by hand (in the raw files from Ann)


#create soil types by area
def getSoilAreaRatios():
	arcpy.FeatureClassToFeatureClass_conversion(soils, TempScratch, "Soils")
	arcpy.FeatureClassToFeatureClass_conversion(UABs, TempScratch, "UABs")
	arcpy.Union_analysis([UABs, soils], "soil_UABs_union")
	arcpy.AddField_management("soil_UABs_union", "TexturePerc", "FLOAT")
	arcpy.Dissolve_management('soil_UABs_union', 'soil_UABs_diss', ["MCD_NAME","LAST_SLA_2"], [["MCD_AREA","FIRST"],["Shape_Area","SUM"]])
	arcpy.CalculateField_management("soil_UABs_diss", "TexturePerc", "!Shape_Area! / !FIRST_MCD_AREA! * 100", "PYTHON")
	#export table with AOI name, soil type, percent area
	
	
def createReachsheds():
	arcpy.Merge_management(PerMS4s,'C:\Users\hydrig\Documents\Projects\Wisconsin TMDL Local Files\TimeSeriesScratch.gdb\Permitted_reachsheds')
	#arcpy.Update_analysis("subbasins","Permitted_reachsheds","test2")
	#arcpy.Dissolve_management("test2","test2_diss","Subbasin")
	#arcpy.SpatialJoin_analysis("ST_UAB_DA_Union","FinalMunis","C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/TimeSeriesScratch.gdb/ST_UAB_DA_Union_Muni2","JOIN_ONE_TO_ONE","KEEP_ALL","""LAST_SLA_2 "LAST_SLA_2" true true false 6 Text 0 0 ,First,#,C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/TimeSeriesScratch.gdb/ST_UAB_DA_Union,LAST_SLA_2,-1,-1;Subbasin "Subbasin" true true false 4 Long 0 0 ,First,#,C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/TimeSeriesScratch.gdb/ST_UAB_DA_Union,Subbasin,-1,-1;MCD_NAME "MCD_NAME" true true false 100 Text 0 0 ,First,#,T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb/FinalMunis,MCD_NAME,-1,-1""","INTERSECT","#","#")
	#arcpy.Clip_analysis("ST_UAB_DA_Union_Muni2","SWAT_Urban_Areas6","FinalOverlay")
	#arcpy.Dissolve_management("FinalOverlay", "FinalOverlayForReal", ["Subbasin","MCD_NAME","LAST_SLA_2"])
	
#def prepPcpLookup():
#	arcpy.TableToTable_conversion(pcpLookup, TempScratch, "PCP_lookup")
#I converted Ann's table to a GDB table, then used field calc to clean up the name field
#Permanently joined PCP field onto Final Overlay


def noNull(inVal):
	if inVal is None:
		return 0.0
	else:
		return float(inVal)

#do this shit
def calcDailyReachLoads():
	soil_key = {'CLAY': 'CL', 'SILT': 'SL', 'SAND': 'SA'}
	
	overlayFOIs = [
	'Shape_Area',
	'LAST_SLA_2',
	'Best_PCP2',
	'Subbasin',
	'MCD_NAME'
	]
	
	precipFOIs = [
	'Total_Solids__lbs_',
	'Particulate_Phosphorus__lbs_',
	'Filterable_Phosphorus__lbs_',
	'Join_Date',
	]
	
	masterFOIs = [
	'Date',
	'Subbasin',
	'Sed_load',
	'Min_P_Load',
	'Org_P_Load',
	'Muni'
	]
		
	with arcpy.da.SearchCursor('FinalOverlayForReal', overlayFOIs) as cursor:
		for row in cursor:
			if row[1] in soil_key:
				pre_file = row[2] + '_' + soil_key[row[1]]
				print pre_file
				
				acreage = row[0] * 0.0247105381 #area in sqm converted to 100 acres
				subbasin = row[3]
				muni = row[4]
				all_loads = []
				
				with arcpy.da.SearchCursor(pre_file, precipFOIs) as cursor2:
					for row2 in cursor2:
						#convert units to tons and kg, respectively
						#try:
						d_load = (
							row2[3],
							subbasin,
							noNull(row2[0]) * acreage * 0.00045359237,
							noNull(row2[2]) * acreage * 0.453592,
							noNull(row2[1]) * acreage * 0.453592,
							muni
							)
						all_loads.append(d_load)
						#except:
						#	raise Exception(row2)
				del cursor2
				
				with arcpy.da.InsertCursor("MasterLoadsA", masterFOIs) as cursor3:
					for d in all_loads:
						cursor3.insertRow(d)
				all_loads = []

		del cursor3
		
	del cursor
	
	arcpy.Statistics_analysis("MasterLoadsA", "FinalOutput", [["Min_P_Load","SUM"],["Org_P_Load","SUM"],["Sed_load","SUM"]], ["Date","Subbasin","Muni"])
	
	
def outputToTxt(outFile):
	filepath = "C:/Users/hydrig/Desktop/"
	f = open(filepath + outFile, "w")
	#write headers
	headers = ["OID", "Date", "Subbasin", "Muni", "Min_P", "Org_P", "Total_Sed"]
	f.write("\t".join(headers) + "\n")
	with arcpy.da.SearchCursor("FinalOutput", ["OBJECTID", "Date", "Subbasin", "Muni", "SUM_Min_P_Load", "SUM_Org_P_Load", "SUM_Sed_load"]) as cursor:
		for row in cursor:
			f.write("\t".join(map(str, row)) + "\n")
	del cursor
	f.close()

#execute script-----------------------------

genTimeSeries(date(2002, 1, 1), 4383)

getSlammOutputs()

getSoilAreaRatios()

createReachsheds()

calcDailyReachLoads()

outputToTxt('S2S_Output.txt')