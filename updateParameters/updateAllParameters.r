library(RODBC)

# CHANGE THESE ACCORDING TO SWAT PROJECT
projectDir = "H:/WRB"
wetland_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands/wetland_geometry_v3.csv"
pond_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"
reservoir_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/hydrology/dams_parameters.csv"
gw_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/alphaBflowSubbasin_lookup.csv"
op_db_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/OpSchedules_fert_3Cuts_later.mdb"
lu_op_xwalk_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/landuse_operation_crosswalk.csv"
background_p_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/phosphorus/background_P_from_EPZ.txt"
soil_p_file = "T:/Projects/Wisconsin_River/GIS_Datasets/Soil_Phosphorus/soil_phosphorus_by_subbasin.txt"

#UPDATE SWAT RESERVOIR PARAMETERS 
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
options(warn=2)
insert_fert = TRUE

prjDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
swatDb = paste(projectDir, "SWAT2012.mdb", sep="/")

# Read in all necessary tables

crosswalk = read.csv(lu_op_xwalk_file)             # for defaults:
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

con_mgt1 = odbcConnectAccess(prjDb)
mgt1 = sqlFetch(con_mgt1, "mgt1")
close(con_mgt1)

con_mgt2 = odbcConnectAccess(prjDb)
sqlQuery(con_mgt2, "SELECT * INTO mgt2_backup FROM mgt2;")
sqlQuery(con_mgt2, "DROP TABLE mgt2")
sqlQuery(con_mgt2, "Select * Into mgt2 From mgt2_backup Where 1 = 2")
close(con_mgt2)

py_file = tempfile(fileext=".py")
write(paste("import arcpy; arcpy.Compact_management('", prjDb, "')", sep=""), py_file)

con_mgt2 = odbcConnectAccess(prjDb)

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
    operation = opSched[gsub(" " , "", as.character(opSched$SID)) == opCode,]
    operation$SUBBASIN = as.character(row_data$SUBBASIN)
    operation$HRU = as.character(row_data$HRU)
    operation$LANDUSE = as.character(row_data$LANDUSE)
    operation$SOIL = as.character(row_data$SOIL)
    operation$SLOPE_CD = as.character(row_data$SLOPE_CD)
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
    }                                       # testing to see if nrot = 1 is better than 6
    
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
odbcCloseAll()
