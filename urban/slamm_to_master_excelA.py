#-------------------------------------#
# SLAMM output to SWAT input script   #
# Created by Guy Hydrick Feb-Mar 2015 #
#-------------------------------------#

import arcpy, os, subprocess
from datetime import date, timedelta

arcpy.env.overwriteOutput = True

#declare variables -----------------


soils = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/SLAMM Model Area/SLAMM_Soil_Texture_Draft_1.shp"
UABs = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb/SWAT_Urban_Areas6"
subbasins = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
FinalMunis = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb/FinalMunis"
pcpLookup = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/SLAMM Model Area/MS4_Subbasins/PCP_export.xlsx/'Guy Lookup$'"
out_master_table = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads.txt"

#temporary local workspace
TempGDB = "C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/Try4.gdb"
arcpy.env.workspace = TempGDB

#the folder which all SLAMM output .csv files should be in
SLAMM_folder = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/WinSLAMM"
#SLAMM_folder = "C:/Users/hydrig/Documents/Projects/WinSLAMM"
#folder for cleaned up CSV files
cleanCSV = "C:/Users/hydrig/Documents/Projects/wisconsinRiverTMDL/urban/clean_csv.r"
#Location of R -- You’ll want to check your version here, you’re maybe 3.3.1…
rscript = os.path.expanduser("~")+"\\Documents\\R\\R-3.1.3\\bin\\Rscript.exe"

#location of permitted sewersheds
PerMS4_folder = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/SLAMM Model Area/Urban Subbasins/To Kurt"

#permitted sewersheds
PerMS4s = [
'SB_Weston4.shp',
'SB_Wausau2.shp',
'SB_Schofield2.shp',
'SB_Rothschild3.shp',
'SB_RibMtn.shp',
'SB_Rapids.shp',
'SB_Mosinee.shp',
'SB_Merrill.shp',
'SB_Marshfield.shp',
'SB_Kronen2.shp',
'SB_Baraboo.shp'
]

#Soil type from field to .csv file naming convention key
soil_key = {'CLAY': 'CL', 'SILT': 'SL', 'SAND': 'SA'}
	
#FOIs = Fields Of Interest
	
overlayFOIs = [
'Shape_Area',
'LAST_SLA_2',
'Best_PCP2',
'Subbasin',
'MCD_NAME'
]

precipFOIs_clean = [
"Runoff Volume (cf)",
"Total Solids (lbs)",
'Particulate Phosphorus (lbs)',
'Filterable Phosphorus (lbs)',
"Rain Start Date",
]

# masterFOIs = [
# 'Date',
# 'Subbasin',
# 'Sed_load',
# 'Min_P_Load',
# 'Org_P_Load',
# 'Muni'
# ]


#define functions--------------------

#Dave created an R script to work around the read-only problem that was sporadically messing up this process in the next function; call that
def cleanCSVs():
	cmd = rscript + " " + cleanCSV
	p = subprocess.Popen(cmd)
	p.wait()

# # generate list of SLAMM .csv files to be loaded
# # Ann said the .csv files were the authoritative outputs
# # cleaned up file naming convention (CL, SA, SL) by hand (in the raw files from Ann)
# def getSlammOutputs():
	# # arcpy.env.workspace = SLAMM_folder
	# arcpy.env.workspace = SLAMM_folder + "\\cleanedCsvs"
	# # wksps = arcpy.ListWorkspaces()
	# # for w in wksps:
		# # arcpy.env.workspace = w
	# csv_files = arcpy.ListFiles("*.csv")
		# # if csv_files == "None":
			# # continue
	# for csv in csv_files:
		# print csv
			# clean up .csv names and convert to GDB tables
			# # TableName = os.path.splitext(csv)[0]
			# # TableName = TableName.replace('-Output', '')
			# # TableName = TableName.rstrip()
		# arcpy.TableToTable_conversion(csv, TempGDB, csv.replace(".csv", ""))		#convert to GDB tables so I can use cursors later - cursors on .csv files give strange results
			
			# # date field is not really a date format field, so add one
		# # arcpy.AddField_management(TempGDB + '\\' + csv, "Join_Date", "DATE")						#is this used anymore, with no joins? Can calc later?
			
			# clean summaries off bottom of tables
			# # getRows = arcpy.GetCount_management(TempGDB + '\\' + TableName)
			# # numRows = int(getRows.getOutput(0))
			# # with arcpy.da.UpdateCursor(TempGDB + '\\' + TableName, ["Rain_Start_Date", "Join_Date"]) as cursor:
				# # for r, row in enumerate(cursor):
					# # if numRows - r <= 13:			#summaries are last 13 rows	
						# # cursor.deleteRow()
					# # else:
						# # row[1] = row[0]				#calc date field
						# # cursor.updateRow(row)
			# # del cursor
			
	# # reset workspace
	# arcpy.env.workspace = TempGDB

#create sewer/reach sheds by unique muni, subbasin, soil type, and add best precip file
def createReachsheds():
	#create an feature class of all the modified permitted sewersheds
	arcpy.env.workspace = PerMS4_folder
	arcpy.Merge_management(PerMS4s, TempGDB + '\Permitted_reachsheds')
	
	#update subbasins with permitted sewershed changes
	arcpy.Update_analysis(subbasins,"Permitted_reachsheds","test2")
	arcpy.Dissolve_management("test2","test2_diss","Subbasin")
	
	#reset workspace
	arcpy.env.workspace = TempGDB
	
	#overlay Urban Area Boundaries with soils data
	arcpy.FeatureClassToFeatureClass_conversion(soils, TempGDB, "Soils")
	arcpy.FeatureClassToFeatureClass_conversion(UABs, TempGDB, "UABs")
	arcpy.Union_analysis([UABs, soils], "soil_UABs_union")
	
	#overlay with municipalities and finalize
	arcpy.SpatialJoin_analysis("ST_UAB_DA_Union","FinalMunis","C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/TimeSeriesScratch.gdb/ST_UAB_DA_Union_Muni2","JOIN_ONE_TO_ONE","KEEP_ALL","""LAST_SLA_2 "LAST_SLA_2" true true false 6 Text 0 0 ,First,#,C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/TimeSeriesScratch.gdb/ST_UAB_DA_Union,LAST_SLA_2,-1,-1;Subbasin "Subbasin" true true false 4 Long 0 0 ,First,#,C:/Users/hydrig/Documents/Projects/Wisconsin TMDL Local Files/TimeSeriesScratch.gdb/ST_UAB_DA_Union,Subbasin,-1,-1;MCD_NAME "MCD_NAME" true true false 100 Text 0 0 ,First,#,T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb/FinalMunis,MCD_NAME,-1,-1""","INTERSECT","#","#")
	arcpy.Clip_analysis("ST_UAB_DA_Union_Muni2","SWAT_Urban_Areas6","FinalOverlay")
	arcpy.Dissolve_management("FinalOverlay", "FinalOverlayForReal", ["Subbasin","MCD_NAME","LAST_SLA_2"])
	
	#'join' precip file information by muni from pcpLookup
	arcpy.AddField_management("FinalOverlayForReal", "BEST_PCP2", "TEXT")
	arcpy.TableToTable_conversion(pcpLookup, TempGDB, "pcpLookup")
	arcpy.CalculateField_management("pcpLookup","NAMELSAD10", "!NAMELSAD10!.rpartition(' ')[0]", "PYTHON")
	arcpy.JoinField_management("FinalOverlayForReal", "MCD_NAME", "pcpLookup", "NAMELSAD10", ["Best_PCP2"])

#replace any nulls with zero to avoid errors with calculations
def noNull(inVal):
	if inVal is None:
		return 0.0
	else:
		if type(inVal) in [str, unicode]:
			inVal = inVal.replace(",", "")
		return float(inVal)

#calculate the final output
def calcDailyReachLoads():
	#get muni, subbasin, and area from each reachshed polygon
	f = open(out_master_table, "w")
	i = 0
	with arcpy.da.SearchCursor('FinalOverlayForReal', overlayFOIs) as cursor:
		for row in cursor:
			i += 1
			if row[1] in soil_key:
				pre_file = row[2] + '_' + soil_key[row[1]]
				print pre_file
				acreage = row[0] * 0.0247105381 #area in sqm converted to 100 acres
				subbasin = row[3]
				muni = row[4]
				all_loads = []
				
				#grab and calc precip events from precip file
				csv_clean = SLAMM_folder + "/cleanedCsvs/" + pre_file + ".csv"
				with arcpy.da.SearchCursor(csv_clean, precipFOIs_clean) as cursor2:
					for row2 in cursor2:
						#convert units to tons and kg, respectively
						#try:
						d_load = (
							row2[4],
							subbasin,
							noNull(row2[0]) * acreage * 0.0283168,
							noNull(row2[1]) * acreage * 0.00045359237,
							noNull(row2[3]) * acreage * 0.453592,
							noNull(row2[2]) * acreage * 0.453592,
							muni
							)
						all_loads.append(d_load)
						# print d_load
						#except:
						#	raise Exception(row2)
				del cursor2

				
				#write headers
				headers = ["date", "subbasin", "flow_m3", "TSS_tons", "P_filt_kg", "P_part_kg", "muni"]
				if i == 1:
					f.write("\t".join(headers) + "\n")
				for row3 in range(0, len(all_loads)):
					out_row = list(all_loads[row3])
					out_row[0] = out_row[0].strftime("%Y-%m-%d")
					f.write("\t".join(map(str, out_row)) + "\n")
				
				#write precip events to master loads table
				# with arcpy.da.InsertCursor("MasterLoadsA", masterFOIs) as cursor3:
					# for d in all_loads:
						# cursor3.insertRow(d)
	del cursor
	f.close()
	#aggregate by data, subbasin, and muni
	# arcpy.Statistics_analysis("MasterLoadsA", "FinalOutput", [["Min_P_Load","SUM"],["Org_P_Load","SUM"],["Sed_load","SUM"]], ["Date","Subbasin","Muni"])
	
	
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

cleanCSVs()

getSlammOutputs()

createReachsheds()

calcDailyReachLoads()

outputToTxt('S2S_OutputA.txt')
