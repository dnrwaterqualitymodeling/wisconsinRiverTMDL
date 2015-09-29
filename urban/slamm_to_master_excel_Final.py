#-------------------------------------#
# SLAMM output to SWAT input script   #
# Created by Guy Hydrick Feb-Mar 2015 #
#-------------------------------------#

import arcpy, os, subprocess

arcpy.env.overwriteOutput = True


#declare variables -----------------


soils = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/SLAMM Model Area/SLAMM_Soil_Texture_Draft_1.shp"
UABs = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb/SWAT_Urban_Areas7"
subbasins = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
FinalMunis = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb/FinalMunis"
pcpLookup = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/SLAMM Model Area/MS4_Subbasins/PCP_export.xlsx/'Guy Lookup$'"
out_master_table = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads_2.txt"

#temporary local workspace
TempGDB = "C:/TEMP/temp.gdb"
arcpy.env.workspace = TempGDB

#the folder which all SLAMM output .csv files should be in
SLAMM_folder = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/WinSLAMM_Daymet"

#folder for cleaned up CSV files
cleanCSV = "C:/Users/ruesca/Documents/wisconsinRiverTMDL/urban/clean_csv.r"
#Location of R -- You'll want to check your version here, you're maybe 3.3.1
rscript = os.path.expanduser("~") + "\\Documents\\R\\R-3.1.2\\bin\\Rscript.exe"

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
'SB_Baraboo.shp',
'SB_Portage.shp',
'SB_Stettin.shp'
]

#Soil type from field to .csv file naming convention key
soil_key = {'CLAY': 'CL', 'SILT': 'SL', 'SAND': 'SA'}
	
#FOIs = Fields Of Interest
	
overlayFOIs = [
'Shape_Area',
'LAST_SLA_2',
# 'Best_PCP2',
'Subbasin',
'MCD_NAME'
]

precipFOIs_clean = [
"Runoff Volume (cf)",
"Suspended Solids Mass (lbs)",
'Particulate Phosphorus (lbs)',
'Filterable Phosphorus (lbs)',
"Rain Start Date",
]


#define functions--------------------


#Dave created an R script to work around the read-only problem that was sporadically messing up this process in arcpy; call this instead
def cleanCSVs():
	cmd = rscript + " " + cleanCSV
	p = subprocess.Popen(cmd)
	p.wait()

#create sewer/reach sheds by unique muni, subbasin, soil type, and add best precip file
#****** we are now using Daymet - so how should we go about the precip???***
def createReachsheds():
	#create an feature class of all the modified permitted sewersheds
	arcpy.env.workspace = PerMS4_folder
	arcpy.Merge_management(PerMS4s, TempGDB + '\Permitted_reachsheds')
	
	#reset workspace
	arcpy.env.workspace = TempGDB

	#update subbasins with permitted sewershed changes
	arcpy.Update_analysis(subbasins,"Permitted_reachsheds","test2")
	arcpy.Dissolve_management("test2","test2_diss","Subbasin")
		
	#overlay Urban Area Boundaries with soils data
	arcpy.Intersect_analysis([UABs, soils], "soil_UABs_isect")
	
	#overlay that with subbasins
	arcpy.Intersect_analysis(["soil_UABs_isect",'test2_diss'],"Soil_UAB_DA_Isect")
	
	#overlay that with municipalities and finalize
	arcpy.Intersect_analysis([FinalMunis, 'Soil_UAB_DA_Isect'], "AlmostFinalOverlay")
	arcpy.Dissolve_management("AlmostFinalOverlay", "FinalOverlayForReal", ["Subbasin","MCD_NAME","LAST_SLA_2"])
	

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
	headers = ["date", "subbasin", "flow_m3", "TSS_tons", "P_filt_kg", "P_part_kg", "muni", "soil_type"]
	f.write("\t".join(headers) + "\n")
	
	with arcpy.da.SearchCursor('FinalOverlayForReal', overlayFOIs) as cursor:
		for row in cursor:
			if row[1] in soil_key:
				pre_file = row[3].replace(" ", "_") + '_' + soil_key[row[1]]
				print pre_file
				#acreage = row[0] * 0.0247105381 	#area in sqm converted to 100 acres
				acreage = row[0] / 404686 	#area in sqm converted to 100 acres
				subbasin = row[2]
				muni = row[3]
				all_loads = []
				
				#grab precip events from precip file and calc loads
				csv_clean = SLAMM_folder + "/cleanedCsvs/" + pre_file + ".csv"
				with arcpy.da.SearchCursor(csv_clean, precipFOIs_clean) as cursor2:
					for row2 in cursor2:
						d_load = (
							row2[4],
							subbasin,
							noNull(row2[0]) * acreage * 0.0283168 * 1000,		#cubic feet to cubic meters
							noNull(row2[1]) * acreage * 0.00045359237,	#pounds to metric tonnes
							noNull(row2[3]) * acreage * 0.453592,		#pounds to kilograms
							noNull(row2[2]) * acreage * 0.453592,		#pounds to kilograms
							muni,
							row[1]
							)
						all_loads.append(d_load)

				del cursor2

				#write output to tab delimited txt file
				for row3 in range(0, len(all_loads)):
					out_row = list(all_loads[row3])
					out_row[0] = out_row[0].strftime("%Y-%m-%d")
					f.write("\t".join(map(str, out_row)) + "\n")
				
	del cursor
	f.close()
	
	
#execute script-----------------------------


cleanCSVs()

createReachsheds()

calcDailyReachLoads()