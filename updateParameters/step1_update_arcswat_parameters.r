### Before running this script ensure that:
###	Reservoirs are added and associated with a daily time series
###	Point sources are added and associated with a daily time series
###	Wetlands and ponds are summarized by the correct subbasin boundaries
###	Mean slope per LULC/subbasin are summarized by the correct subbasin boundaries.
###
### NOTE: this script is meant to be run BEFORE re-write swat input files
library(RODBC)
library(stringr)
library(foreign)
library(rgeos)
library(rgdal)
library(dplyr)
options(stringsAsFactors=F)
options(warn=1)
# CHANGE THESE ACCORDING TO SWAT PROJECT
projectDir = "C:/Users/ruesca/Desktop/WRB"
#projectDir = "E:/WRB"
mean_slope_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/slope/subbasin_landuse_mean_slope.txt"

#pond_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"
reservoir_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/hydrology/dams_parameters.csv"
gw_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/alphaBflowSubbasin_lookup.csv"
op_db_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/OpSchedules.mdb"
file_bio_e = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/crop/Bio_E_Calibration_Report.csv"
lu_op_xwalk_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/landuse_operation_crosswalk.csv"
background_p_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/phosphorus/background_P_from_EPZ.txt"
soil_p_file = "T:/Projects/Wisconsin_River/GIS_Datasets/Soil_Phosphorus/soil_phosphorus_by_subbasin.txt"
hydro_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro"
subbasins_file = "subbasins_minus_urban_boundaries"
lulc_manN_lu = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/lulc_manN_lu.csv"
SWAT_manID_lu = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/SWAT_manID_lu.csv"

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")

natural_lu = c(
	"WATR",
	"URML",
	"PAST",
	"WETN",
	"ONIO",
	"FRSD",
	"FRSE",
	"FRST",
	"CRRT"
)

# UPDATE IRRIGATION
# 0 is off, 1 from reach, 3 from shallow aquifer
irr_sca = 3
# If CNOP = T set CNOPs, else revert to CN2
CNOP = T

# UPDATE SOIL TEXTURES
con = odbcConnectAccess(inDb)
txtr_query = "UPDATE sol SET TEXTURE = HYDGRP;"
stdout = sqlQuery(con, txtr_query)
close(con)

# UPDATE CANMX
con = odbcConnectAccess(inDb)
# Wu and Johnston, 2008
# Hydrologic comparison between a forested and a wetland/lake dominated watershed using SWAT
canmx_query = "UPDATE hru SET CANMX = 2.0 WHERE LANDUSE = 'FRSD';"
stdout = sqlQuery(con, canmx_query)
canmx_query = "UPDATE hru SET CANMX = 4.3 WHERE LANDUSE = 'FRST';"
stdout = sqlQuery(con, canmx_query)
canmx_query = "UPDATE hru SET CANMX = 6.6 WHERE LANDUSE = 'FRSE';"
stdout = sqlQuery(con, canmx_query)
# Zinke, 1967, Forest interception studies in the United States
canmx_query = "UPDATE hru SET CANMX = 1.25 WHERE LANDUSE NOT IN ('FRSD','FRST','FRSE','WATR','URML');"
stdout = sqlQuery(con, canmx_query)
close(con)

# UPDATE SLOPE AND SLOPE LENGTH BASED ON RECCS IN BAUMGART, 2005
mean_slope = read.table(mean_slope_file, header=T)

con = odbcConnectAccess(inDb)
hru_data = sqlQuery(con, "SELECT * from hru")

for (row in 1:nrow(hru_data)) {
	new_slope = with(mean_slope,
		mean_slope[
			subbasin == hru_data$SUBBASIN[row] &
			landuse == hru_data$LANDUSE[row]
		]
	)	
	query = paste(
		"UPDATE hru ",
		"SET HRU_SLP = ", new_slope / 100, " ",
		"WHERE SUBBASIN = ", hru_data$SUBBASIN[row],
		" AND HRU = ", hru_data$HRU[row], ";",
		sep=""
	)
	stdout = sqlQuery(con, query)
	sl = 91.4 / (new_slope + 1)^0.4 # Baumgart, 2005
	query = paste(
		"UPDATE hru ",
		"SET SLSUBBSN = ", sl, " ",
		"WHERE SUBBASIN = ", hru_data$SUBBASIN[row],
		" AND HRU = ", hru_data$HRU[row], ";",
		sep=""
	)
	stdout = sqlQuery(con, query)
}
close(con)

# UPDATE MANNINGS N BASED ON RECCS IN BAUMGART, 2005

con = odbcConnectAccess(inDb)
query = "UPDATE hru SET OV_N = 0.1;"
stdout = sqlQuery(con, query)
query = "UPDATE sub SET CH_N1 = 0.065;"
# Average of Eau Claire and Little Rib FEMA 2010 Marathon County Flood Insurance Study
stdout = sqlQuery(con, query)
query = "UPDATE rte SET CH_N2 = 0.043;"
stdout = sqlQuery(con, query)
close(con)

# UPDATE MANNINGS N OVERLAND FLOW
# OV_N values: natural LULCs - McCuen 1998, moldboard - Engman 1986, all else - Engman 1983
con = odbcConnectAccess(inDb)

N_by_lulc = read.csv(lulc_manN_lu)
SWAT_lulc_by_manID = read.csv(SWAT_manID_lu)

for (row in 1:nrow(N_by_lulc)) {
	temp = SWAT_lulc_by_manID$Man_ID == row
	lulc = toString(paste(
		"'",
		SWAT_lulc_by_manID$SWAT_LULC[temp],
		"'", sep = ""))
	query = paste(
		"UPDATE hru SET OV_N = ",
		N_by_lulc$ManningsN[row],
		" WHERE LANDUSE IN (", lulc, ");",
		sep = "")
	stdout = sqlQuery(con, query)
}
close(con)


# SET EVAPOTRANSPIRATION EQUATION TO PENMAN MONTEITH

con = odbcConnectAccess(inDb)
query = "UPDATE bsn SET IPET = 1;"
stdout = sqlQuery(con, query)
close(con)


#UPDATE SWAT SOIL PHOSPHORUS PARAMETER
soil_p = read.table(soil_p_file, header = T)# UPDATE SWAT RESERVOIR PARAMETERS 
reservoir_parameters = read.csv(reservoir_parameter_file)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

resData = sqlQuery(con, "SELECT * FROM res")

for (row in 1:nrow(reservoir_parameters)) {
	query = paste(
		"UPDATE res ",
		"SET RES_ESA = ", reservoir_parameters$res_psa[row] * 1.5, ",",
		"RES_EVOL = ", reservoir_parameters$res_evol[row], ",",
		"RES_PSA = ", reservoir_parameters$res_psa[row], ",",
		"RES_PVOL = ", reservoir_parameters$res_pvol[row], ",",
		"RES_VOL = ", reservoir_parameters$res_pvol[row], ",",
		"NDTARGR=15, IFLOOD1R=7, IFLOOD2R=2 ",
		"WHERE SUBBASIN = ", reservoir_parameters$Subbasin[row], ";",
		sep = ""
	)
	stdout = sqlQuery(con, query)
	for (m in 1:12) {
		query = paste(
			"UPDATE res SET STARG",
			m, "=", reservoir_parameters$res_pvol[row],
			" WHERE SUBBASIN = ",
			reservoir_parameters$Subbasin[row], ";",
			sep="")
		stdout = sqlQuery(con, query)
	}
}
close(con)


inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

soilp_query = paste("UPDATE chm SET SOL_LABP1 = 5, SOL_ORGP1 = 0")
stdout = sqlQuery(con, soilp_query)
for (sb in soil_p$Subbasin){
	soilp_query = paste(
		"UPDATE chm",
		"SET SOL_LABP1 = ", soil_p$SOLP[which(soil_p$Subbasin == sb)], ",",
		"SOL_ORGP1 =", soil_p$ORGP[which(soil_p$Subbasin == sb)],
		"WHERE SUBBASIN =", sb, "AND LANDUSE NOT IN",
		paste("('", paste(natural_lu, collapse="','"), "');", sep=""),
	)   
	stdout = sqlQuery(con, soilp_query)
}
close(con)

#UPDATE SWAT GROUNDWATER PHOSPHORUS PARAMETER
background_p = read.table(background_p_file, header = T)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

for (rw in background_p$ID){
	gwp_query = paste(
		"UPDATE gw ",
		"SET GWSOLP = ", background_p$layer[which(background_p$ID == rw)],
		" WHERE SUBBASIN = ", rw, ";",
		sep = ''
		)   
	stdout = sqlQuery(con, gwp_query)
}
close(con)

#UPDATE ALPHA_BF 

gw_parameters = read.csv(gw_parameter_file)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

# resData = sqlQuery(con, "SELECT * FROM res")

for (row in 1:nrow(gw_parameters)) {
	query = paste(
		"UPDATE gw ",
		"SET ALPHA_BF = ", gw_parameters$alphaBflow_Preds_mod3[row], ' ',
		"WHERE SUBBASIN = ", gw_parameters$Subbasin[row], ";",
		sep = ""
	)
	stdout = sqlQuery(con, query)
}
close(con)

#UPDATE MANAGEMENT OPERATIONS

insert_fert = TRUE

prjDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
swatDb = paste(projectDir, "SWAT2012.mdb", sep="/")

# Read in all necessary tables

crosswalk = read.csv(lu_op_xwalk_file)			 # for defaults:
												# OpSchedules_fert.mbd
con_updates = odbcConnectAccess(op_db_file)

opSched = sqlFetch(con_updates, "OpSchedules")
if (irr_sca == 0) {
	opSched = subset(opSched, MGT_OP != 10)
}
fert = sqlFetch(con_updates, "fert")
close(con_updates)

con_fert = odbcConnectAccess(swatDb)
fert_query = paste("INSERT INTO fert (IFNUM,FERTNM,FMINN,FMINP,FORGN,FORGP,FNH3N,",
	"BACTPDB,BACTLPDB,BACTKDDB,FERTNAME,MANURE) VALUES (55,'20-10-18',0.200,0.044,",
	"0.000,0.000,0.00,0,0,0,'Starter WRB',0);", sep="")
fert_row_count = sqlQuery(con_fert, "SELECT COUNT(OBJECTID) FROM fert;")[[1]]
if (fert_row_count < 55) {
	sqlQuery(con_fert, fert_query)
}
close(con_fert)

con_mgt1 = odbcConnectAccess(inDb)#prjDb
mgt1 = sqlFetch(con_mgt1, "mgt1")
close(con_mgt1)

con_mgt2 = odbcConnectAccess(prjDb)
sqlQuery(con_mgt2, "SELECT * INTO mgt2_backup FROM mgt2;")
sqlQuery(con_mgt2, "DROP TABLE mgt2")
sqlQuery(con_mgt2, "Select * INTO mgt2 From mgt2_backup Where 1 = 2")
close(con_mgt2)

# for irrigation
pot_veggie_landuses = c("SGBT", "POTA", "SPOT")
## Note:
##	IRR_SC=3 for irrigating from shallow aquifer
##	IRR_NO=the subbasin number from which the water comes

py_file = tempfile(fileext=".py")
write(paste("import arcpy; arcpy.Compact_management('", inDb, "')", sep=""), py_file)

con_mgt2 = odbcConnectAccess(prjDb)
con_swat2012 = odbcConnectAccess(swatDb)

sol = sqlQuery(con_mgt2, "SELECT SUBBASIN, HRU, LANDUSE, SLOPE_CD, SNAM FROM sol")
sol_drained = subset(
	sol,
	SLOPE_CD %in% c("0-0.5","0.5-1.5") &
		grepl("^E", SNAM) &
		!(LANDUSE %in% natural_lu)
)

for (row in 1:nrow(mgt1)) {
    row_data = mgt1[row,]
	sb = as.character(row_data$SUBBASIN)
	hru = as.character(row_data$HRU)
	lu = as.character(row_data$LANDUSE)
#	if (lu == "CRRT") {break} else {next}
    print(paste('Subbasin:',sb,'hru:',hru,'lu:',lu))
    opCode = unique(as.character(crosswalk$OPCODE[crosswalk$LANDUSE == lu]))
    if (substr(opCode, 1, 1) == "3" & substr(opCode, 4, 4) == "c") {
        igro_query = paste("UPDATE mgt1 SET IGRO = 1, PLANT_ID = 52, NROT = 0 WHERE SUBBASIN = ",
            as.character(row_data$SUBBASIN),
            " AND HRU = ",
            as.character(row_data$HRU),
            ";",
            sep=""
        )
        sqlQuery(con_mgt2, igro_query)
    }
	# UPDATE TILE DRAIN PARAMETERS
#	if (sb %in% sol_drained$SUBBASIN & hru %in% sol_drained$HRU) {
	if (paste(sb, hru) %in% paste(sol_drained$SUBBASIN, sol_drained$HRU)) {
		tile_query = paste(
			"UPDATE mgt1 SET DDRAIN = 900, TDRAIN = 48, GDRAIN = 20 WHERE ",
			"SUBBASIN = ", sb, " AND HRU = ", hru, ";",
			sep=""
		)
		stdout = sqlQuery(con_mgt2, tile_query)
	} else {
		tile_query = paste(			
			"UPDATE mgt1 SET DDRAIN = 0, TDRAIN = 0, GDRAIN = 0 WHERE ",
			"SUBBASIN = ", sb, " AND HRU = ", hru, ";",
			sep=""
		)
		stdout = sqlQuery(con_mgt2, tile_query)
	}
	# UPDATE IRRIGATION PARAMETERS
	# 	opschedules.mdb currently has place holders for irrigation
	#	these lines set the necessary parameters, later they get turned on.
	if (irr_sca > 0) {
		irrno = row_data$SUBBASIN
	} else {
		irrno = 0
	}
	if (lu %in% pot_veggie_landuses){
		irri_mgt1_query = paste(
			"UPDATE mgt1 SET IRRSC =",
			irr_sca,
			", IRRNO = ",
			irrno,
			" WHERE SUBBASIN = ",
			as.character(row_data$SUBBASIN),
			" AND LANDUSE = '",
			lu,
			"';",
			sep='')
			out = sqlQuery(con_mgt2, irri_mgt1_query)
	}
	
	operation = opSched[gsub(" " , "", as.character(opSched$SID)) == opCode,]
	operation$SUBBASIN = as.character(row_data$SUBBASIN)
	operation$HRU = as.character(row_data$HRU)
	operation$LANDUSE = as.character(row_data$LANDUSE)
	operation$SOIL = as.character(row_data$SOIL)
	operation$SLOPE_CD = as.character(row_data$SLOPE_CD)
	if (row_data$LANDUSE %in% pot_veggie_landuses){
		operation$IRR_NOA = as.character(row_data$SUBBASIN)
	}
	if (!(lu %in% natural_lu)) {
		operation = operation %>%
			arrange(YEAR, MONTH, DAY, MGT_OP)
		operation$OP_NUM = 1:nrow(operation)
	}
	formatTempFile = tempfile()
	write.csv(operation[,2:ncol(operation)], formatTempFile, row.names=F, quote=T)
	colNames = readLines(formatTempFile, 1)
	colNames = gsub("\"", "", colNames)
	for (opRow in 1:nrow(operation)) {
		values = readLines(formatTempFile, opRow + 1)[opRow + 1]
		values = gsub("\"", "'", values)
		values = gsub("NA", "NULL", values)
		insertQuery = paste(
			"INSERT INTO mgt2 (",
			colNames,
			") VALUES (",
			values,
			");",
			sep=""
		)
		sqlQuery(con_mgt2, insertQuery)
	}
	if (!(opCode %in% natural_lu)){ 
#		if (opCode == "CRRT") {
#			nrot = 1
#			iscrop = 0
#		} else {
			nrot = 6
			iscrop = 1
#		}
		husc_query = paste(
			"UPDATE mgt1 SET HUSC = 1, NROT = ",
			nrot,
			", ISCROP = ",
			iscrop,
			" WHERE SUBBASIN = ",
			as.character(row_data$SUBBASIN),
			" AND HRU = ",
			as.character(row_data$HRU),
			";",
			sep=""
		)
		sqlQuery(con_mgt2, husc_query)
	} else {
		husc_query = paste("UPDATE mgt1 SET HUSC = 0, ISCROP = 0 WHERE SUBBASIN = ",
			as.character(row_data$SUBBASIN),
			" AND HRU = ",
			as.character(row_data$HRU),
			";",
			sep=""
		)
		sqlQuery(con_mgt2, husc_query)
	}
	if (row %% 1000 == 0) {
		close(con_mgt2)
		print("Compacting database. Please wait...")
		system(paste("C:\\Python27\\ArcGIS10.1\\python.exe", py_file))
		con_mgt2 = odbcConnectAccess(prjDb) 
	}

}
### IRRIGATION PARAMETERS
# already have a slot in wrb.mdb (from opschedules) for every year in potato veggie ops
if (irr_sca > 0) {
	wstrs_id = 1
	auto_wstrs = 1
	irr_eff = 90
	irr_mx = 90
	irr_asq = 0.02
} else {
	wstrs_id = 1
	auto_wstrs = 0
	irr_eff = 0
	irr_mx = 0
	irr_asq = 0
}
#### for (lu in pot_veggie_landuses){
irri_query = paste(
		"UPDATE mgt2 SET WSTRS_ID = ",
		wstrs_id,
		", AUTO_WSTRS = ",
		auto_wstrs,
		", IRR_EFF = ",
		irr_eff,
		", IRR_MX = ",
		irr_mx,
		", IRR_ASQ = ",
		irr_asq,
		", IRR_SCA = ",
		irr_sca,
		" WHERE MGT_OP = 10;",
		sep=''
)


# CNOP
if (CNOP) {
	hydgrp_lu = unique(sqlQuery(con_mgt2, "SELECT SOIL, HYDGRP from sol"))
	crop_cn_lu = unique(sqlQuery(con_swat2012, "SELECT ICNUM, CN2A, CN2B, CN2C, CN2D from crop"))
	
	# Update CNOP for planting operations
	for (hydgrp in LETTERS[1:4]) {
		for (crop in c(19, 20, 21, 52, 56, 70, 84)) {
			hydgrp_col = paste("CN2", hydgrp, sep="")
			cnop = crop_cn_lu[crop_cn_lu$ICNUM == crop, hydgrp_col]
			soils = hydgrp_lu[hydgrp_lu$HYDGRP == hydgrp, "SOIL"]
			soils = paste("('", paste(soils, collapse="','"), "')", sep="")
			query = paste(
				"UPDATE mgt2 SET CNOP = ",
				cnop,
				" WHERE SOIL IN ",
				soils,
				" AND PLANT_ID = ",
				crop,
				";",
				sep=""
			)
			stdout = sqlQuery(con_mgt2, query)
		}
	}
	
	till_tr55 = list(
		bare_soil = c(77,86,91,94),
		poor_residue = c(76,85,90,93),
		good_residue = c(74,83,88,90)
	)
	# We assume that 1-EFTMIX is equal to crop residue cover
	# Crop residue cover <20% is considered "good" according to TR-55
	# Moldboard leaves so little residue, we chose to give it bare soil properties.
	till_codes = list(
		c('2', "good_residue"), # Generic spring plowing
		c('6', "good_residue"), # Field cultivator
		c('35', "good_residue"), # Cultivator 1 row
		c('58', "good_residue"), # Chisel plow
		c('61', "poor_residue"), # Disk plow
		c('64', "bare_soil") # Moldboard plow
	) 
	
	# Upate CNOP for tillage operations
	for (hydgrp in LETTERS[1:4]) {
		for (till in till_codes) {
			soils = hydgrp_lu[hydgrp_lu$HYDGRP == hydgrp, "SOIL"]
			soils = paste("('", paste(soils, collapse="','"), "')", sep="")
			cnop = till_tr55[[till[2]]][LETTERS == hydgrp]
			query = paste(
				"UPDATE mgt2 SET CNOP = ",
				cnop,
				" WHERE TILLAGE_ID = ",
				till[1],
				" AND SOIL IN ",
				soils,
				";",
				sep=""
			)
			stdout = sqlQuery(con_mgt2, query)
		}
	}
	# Set CNOP as CN2 for non-ag land cover so that CNOP is the only calibrated parameter
	for (hydgrp in LETTERS[1:4]) {
		soils = hydgrp_lu[hydgrp_lu$HYDGRP == hydgrp, "SOIL"]
		soils = paste("('", paste(soils, collapse="','"), "')", sep="")
		for (lc in natural_lu) {
			query = paste(
				"SELECT CN2 FROM mgt1 WHERE LANDUSE = '",
				lc, 
				"' AND SOIL IN ",
				soils,
				";",
				sep=""
			)
			cn2 = sqlQuery(con_mgt2, query)
			if (length(unique(cn2)) == 1) { # They should all be the same
				cn2 = cn2[1,1]
				query = paste(
					"UPDATE mgt2 SET CNOP = ",
					cn2,
					" WHERE LANDUSE = '",
					lc,
					"' AND SOIL IN ",
					soils,
					" AND MGT_OP = 1;",
					sep=""
				)
				stdout = sqlQuery(con_mgt2, query)
			} else {
				print("Why did one of these curve numbers get a different assignment than the others?")
			}
		}
	}
} else {
	sqlQuery(con_mgt2, "UPDATE mgt2 SET CNOP=0;")
}
	
#### UPDATE PLANT.DAT: 
######## 	insert calibrated BIO E and correct issue with Forest, re: PB's suggestion
pth_plant.dat = paste(projectDir, "/plant.dat",sep='')
df_bio_e = read.csv(file_bio_e)

for (crop in c("CORN", "CSIL", "SOYB", "ALFA")) {
	if (crop == "CSIL") {
		bio_e <- df_bio_e[which(df_bio_e$Crop == "CORN"), 'FittedBioE']
	} else {
		bio_e <- df_bio_e[which(df_bio_e$Crop == crop), 'FittedBioE']
	}
	query = paste("UPDATE crop SET BIO_E = ", bio_e, " WHERE CPNM = '", crop, "';", sep="")
	print(query)
	stdout = sqlQuery(con_swat2012, query)
}

#queries = c(
#	"UPDATE crop SET BLAI = 6.5 WHERE CPNM IN ('CORN','CSIL);",
#	"UPDATE crop SET BLAI = 6 WHERE CPNM = 'SCRN';",
#	"UPDATE crop SET BLAI = 5.5 WHERE CPNM = 'SOYB';",
#	"UPDATE crop SET BLAI = 5 WHERE CPNM IN ('GRBN','POTA');",
#	"UPDATE crop SET BIO_E = 7.8 WHERE CPNM = 'ALFA';",
#	"UPDATE crop SET BIO_E = 35 WHERE CPNM IN ('CORN','CSIL);",
#	"UPDATE crop SET BIO_E = 43 WHERE CPNM = 'SOYB';",
#	"UPDATE crop SET BIO_E = 50 WHERE CPNM = 'GRBN';",
#	"UPDATE crop SET BIO_E = 27 WHERE CPNM = 'POTA';",
#	"UPDATE crop SET BIO_E = 39 WHERE CPNM = 'SCRN';",
#	"UPDATE crop SET HVSTI = 6.5 WHERE CPNM IN ('GRBN','SCRN');",
#	"UPDATE crop SET CHTMX = 1 WHERE CPNM = 'GRBN';",
#	"UPDATE crop SET T_OPT = 25 WHERE CPNM = 'SCRN';",
#	"UPDATE crop SET T_BASE = 8 WHERE CPNM = 'SCRN';",
#	"UPDATE crop SET USLE_C = 0.1 WHERE CPNM IN ('ALFA','CORN','CSIL','SCRN','SOYB','GRBN','POTA');"
#)
#for (q in queries) {
#	stdout = sqlQuery(con_swat2012, q)
#	print(stdout)
#}

# fixing forest ALAI_MIN parameter PB's suggestion

# stdout = sqlQuery(con_swat2012, "UPDATE crop SET ALAI_MIN = 0 WHERE CPNM = 'FRST';")

# Set RNGE values to TIMO values *not used in favor of PAST (bermudagrass, set elsewhere in script)

# del_query = "DELETE FROM crop WHERE CPNM='RNGE'"
# sqlQuery(con_swat2012, del_query)

# cpy_query = "SELECT * FROM crop WHERE CPNM='PAST'"
# timo_dat = sqlQuery(con_swat2012, cpy_query)

# timo_dat$ICNUM = 15
# timo_dat$CROPNAME = "'Range-Grasses'"
# timo_dat$CPNM = "'RNGE'"
# timo_dat$OpSchedule = "'PAST'"

# colNames = paste(names(timo_dat[2:length(timo_dat)]), collapse=",")
# vals = paste(timo_dat[,2:length(timo_dat)], collapse=",")
# pst_query = paste(
			# "INSERT INTO crop (",
			# colNames,
			# ") VALUES (",
			# vals,
			# ");",
			# sep=""
		# )
# sqlQuery(con_swat2012, pst_query)

# UPDATE SURLAG
inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

#read areas in from subbasins shapefile - make sure geometry hasn't changed, as shapefiles do not auto update areas!
subbasins = readOGR(dsn=hydro_dir, layer=subbasins_file)
#set max and min
maxAr = max(gArea(subbasins, byid=T))
#convert to 0-1 ratio
surlag_vals = gArea(subbasins, byid=T) / maxAr
surlag_vals = (1 - surlag_vals) + 1

#update query using this data in r
for (row in subbasins@data$Subbasin) {
	query = paste(
		"UPDATE hru SET SURLAG = ", 
		surlag_vals[row], 
		" WHERE SUBBASIN = ", row, ";", sep="")
	stdout = sqlQuery(con, query)
}
close(con)


odbcCloseAll()
