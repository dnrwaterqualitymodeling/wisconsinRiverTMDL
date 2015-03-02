# Before running this script ensure that:
#	Reservoirs are added and associated with a daily time series
#	Point sources are added and associated with a daily time series
#	Wetlands and ponds are summarized by the correct subbasin boundaries
#	Mean slope per LULC/subbasin are summarized by the correct subbasin boundaries.

library(RODBC)
library(stringr)
library(foreign)
options(stringsAsFactors=F)
options(warn=1)
# CHANGE THESE ACCORDING TO SWAT PROJECT
mean_slope_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/slope/subbasin_landuse_mean_slope.txt"
wetland_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands/wetland_geometry.csv"
pond_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"
reservoir_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/hydrology/dams_parameters.csv"
gw_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/alphaBflowSubbasin_lookup.csv"
op_db_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/OpSchedules.mdb"
# ps_files = list.files(
	# "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/point_sources",
	# "^recday_[0-9]+\\.txt$",
	# full.names=T
# )
# should be swat_lookup.csv?
lu_op_xwalk_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/landuse_operation_crosswalk.csv"
background_p_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/phosphorus/background_P_from_EPZ.txt"
soil_p_file = "T:/Projects/Wisconsin_River/GIS_Datasets/Soil_Phosphorus/soil_phosphorus_by_subbasin.txt"

projectDir = "C:/Users/ruesca/Desktop/WRB"
# projectDir = "H:/WRB"

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")

## for irrigation
## 0 is off, 1 from reach, 3 from shallow aquifer
irr_sca = 3 

#############################################
# The point source block below does not work because ArcSWAT cannot handle the size of the TimeSeries table it creates
# If there is a workaround, it's possible the code below could be useful.
#############################################

# UPDATE POINT SOURCES

# py_file = tempfile(fileext=".py")
# write(paste("import arcpy; arcpy.Compact_management('", inDb, "')", sep=""), py_file)

# UPDATE PP TABLE
# wipe_TimeSeries = TRUE
# con = odbcConnectAccess(inDb)
# if (wipe_TimeSeries){
	# sqlQuery(con, "DELETE FROM TimeSeries WHERE TSTypeID > 0")
# }
# close(con)

# sb_hydroid_lu = read.dbf(paste(projectDir, "Watershed", "Shapes", "monitoring_points1.dbf", sep="/"), as.is=T)
# TSTypes = seq(1,35,2)

# dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), "day")
# dates = format(dates, "%m/%d/%Y")

# sb_count = 0
# for (ps_file in ps_files) {
	# ps_file = gsub("/", "\\\\", ps_file)
	# ps_data = read.csv(ps_file)
	
	# sb = str_extract(basename(ps_file), "[0-9]+")
	# if (all(cbind(ps_data[c("Floday","Sedday","Minpday")]) == 0)) {
		# print(paste("Skipping subbasin", sb))
		# next}
	# sb_count = sb_count + 1
	# print(paste("Subbasin:",sb))
	# query = paste(
		# "UPDATE pp SET DAILYREC = '",
		# ps_file,
		# "', TYPE = 10 WHERE SUBBASIN = ",
		# sb,
		# sep="")
	# stdout = sqlQuery(con, query)
	# hydroid = subset(sb_hydroid_lu, Subbasin == sb & Type == "P")$HydroID
	# strt_time = proc.time()[3]
	# i = 0
	# pb = txtProgressBar(0,1)
	# for (dt in dates) {
		# i = i + 1
		# for (TSType in TSTypes) {
			# setTxtProgressBar(pb, i/nrow(ps_data))
			# if (TSType == 1) {
				# v = ps_data$Floday[i]
			# } else if (TSType == 3) {
				# v = ps_data$Sedday[i]
			# } else if (TSType == 15) {
				# v = ps_data$Minpday[i]
			# } else {
				# v = 0
			# }
			# query = paste(
				# "INSERT INTO TimeSeries (FeatureID,TSTypeID,TSDateTime,TSValue) VALUES (",
				# hydroid,
				# ",",
				# TSType,
				# ",'",
				# dt,
				# "',",
				# v,
				# ");",
				# sep=""
			# )
			# stdout = sqlQuery(con, query)
		# }
		
	# }
	# close(pb)
	# print(paste("Elapsed time: ", proc.time()[3]-strt_time))
# }

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
query = "UPDATE rte SET CH_N2 = 0.065;"
stdout = sqlQuery(con, query)
close(con)

# SET EVAPOTRANSPIRATION EQUATION TO PENMAN MONTEITH

con = odbcConnectAccess(inDb)
query = "UPDATE bsn SET IPET = 1;"
stdout = sqlQuery(con, query)
close(con)

# UPDATE SWAT RESERVOIR PARAMETERS 
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
		"RES_VOL = ", reservoir_parameters$res_pvol[row], " ",
		"WHERE SUBBASIN = ", reservoir_parameters$Subbasin[row], ";",
		sep = ""
	)
	stdout = sqlQuery(con, query)
}
close(con)

#UPDATE SWAT SOIL PHOSPHORUS PARAMETER
soil_p = read.table(soil_p_file, header = T)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

for (sb in soil_p$Subbasin){   
	soilp_query = paste(
		"UPDATE chm",
		"SET SOL_LABP1 = ", soil_p$SOLP[which(soil_p$Subbasin == sb)], ",",
		"SOL_ORGP1 =", soil_p$ORGP[which(soil_p$Subbasin == sb)],
		"WHERE SUBBASIN =", sb, "AND LANDUSE NOT IN",
		"('BARR','FRSD', 'WATR', 'URML', 'RNGB','RNGE','WETF', 'WETN','HAY');"
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

#UPDATE SWAT POND PARAMETERS

pond_geometry = read.csv(pond_geometry_file)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

for (row in 1:nrow(pond_geometry)) {
	query = paste(
		"UPDATE pnd ",
		"SET PND_FR = ", pond_geometry$PND_FR[row], ",",
		"PND_PSA = ", pond_geometry$PND_PSA[row], ",",
		"PND_PVOL = ", pond_geometry$PND_PVOL[row], ",",
		"PND_ESA = ", pond_geometry$PND_ESA[row], ",",
		"PND_EVOL = ", pond_geometry$PND_EVOL[row], ",",
		"NDTARG = 15, IFLOD1 = 4, IFLOD2 = 6 ",
		"WHERE SUBBASIN = ", pond_geometry$subbasin[row], ";",
		sep = ""
	)
	stdout = sqlQuery(con, query)
}
close(con)

#UPDATE SWAT WETLAND PARAMETERS

wetland_geometry = read.csv(wetland_geometry_file)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

wetlandData = sqlQuery(con, "SELECT * FROM pnd")

for (row in 1:nrow(wetland_geometry)) {
	query = paste(
		"UPDATE pnd ",
		"SET WET_FR = ", wetland_geometry$WET_FR[row], ",",
		"WET_NSA = ", wetland_geometry$WET_NSA[row], ",",
		"WET_NVOL = ", wetland_geometry$WET_NVOL[row], ",",
		"WET_VOL = ", wetland_geometry$WET_VOL[row], ",",
		"WET_MXSA = ", wetland_geometry$WET_MXSA[row],
		" WHERE SUBBASIN = ", wetland_geometry$subbasin[row], ";",
		sep = ""
	)
	stdout = sqlQuery(con, query)
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
sqlQuery(con_mgt2, "Select * Into mgt2 From mgt2_backup Where 1 = 2")
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

oidStart = 1
for (row in 1:nrow(mgt1)) {
	
    row_data = mgt1[row,]
    print(paste('Subbasin:',as.character(row_data$SUBBASIN),'hru:', as.character(row_data$HRU)))
    lu = as.character(row_data$LANDUSE)
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
	# UPDATE IRRIGATION PARAMETERS
	# 	opschedules.mdb currently has place holders for irrigation
	#	these lines set the necessary parameters, later they get turned on.
	if (lu %in% pot_veggie_landuses){
		irri_mgt1_query = paste(
			"UPDATE mgt1 SET IRRSC = 3, IRRNO = ",
			row_data$SUBBASIN,
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
	if (!(opCode %in% c('BARR','FRSD', 'WATR', 'URML', 'RNGB','RNGE','WETF', 'WETN','HAY'))){ 
		husc_query = paste("UPDATE mgt1 SET HUSC = 1, NROT = 6, ISCROP = 1 WHERE SUBBASIN = ",
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



wstrs_id = 1
auto_wstrs = 1
irr_eff = 90
irr_mx = 90
irr_asq = 0.02

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
stout = sqlQuery(con_mgt2, irri_query)
# }


# CNOP
hydgrp_lu = unique(sqlQuery(con_mgt2, "SELECT SOIL, HYDGRP from sol")) # for CNOP
crop_cn_lu = unique(sqlQuery(con_swat2012, "SELECT ICNUM, CN2A, CN2B, CN2C, CN2D from crop")) # for CNOP

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
	for (lc in c("WATR", "URML", "FRSD", "WETN", "RNGE", "ONIO", "CRRT")) {
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


odbcCloseAll()


