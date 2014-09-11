library(RODBC)

# CHANGE THESE ACCORDING TO SWAT PROJECT
projectDir = "C:/SWAT/Burnt_rollways"
wetland_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands/wetland_geometry.csv"
pond_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"
reservoir_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/hydrology/dams_parameters.csv"

#UPDATE SWAT RESERVOIR PARAMETERS 

reservoir_parameters = read.csv(reservoir_parameter_file)

inDb = paste(projectDir, "Burnt_Rollways.mdb", sep="/")
con = odbcConnectAccess(inDb)

resData = sqlQuery(con, "SELECT * FROM res")

for (row in 1:nrow(reservoir_parameters)) {
	query = paste(
		"UPDATE res ",
		"SET RES_ESA = ", reservoir_parameters$MaxStorage[row], ",",
		"RES_EVOL = ", reservoir_parameters$MaxS_10to4[row], ",",
		"RES_PSA = ", reservoir_parameters$NormalStor[row], ",",
		"RES_PVOL = ", reservoir_parameters$NormS_10to[row], ",",
		"RES_VOL = ", reservoir_parameters$NormS_10to[row], " ",
		"WHERE SUBBASIN = ", reservoir_parameters$Subbasin[row], ";",
		sep = ""
	)
    stdout = sqlQuery(con, query)
}

close(con)

#UPDATE SWAT POND PARAMETERS

pond_geometry = read.csv(pond_geometry_file)

inDb = paste(projectDir, "Burnt_Rollways.mdb", sep="/")
con = odbcConnectAccess(inDb)

pndData = sqlQuery(con, "SELECT * FROM pnd")

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

inDb = paste(projectDir, "Burnt_Rollways.mdb", sep="/")
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

#UPDATE MANAGEMENT OPERATIONS

insert_fert = TRUE

prjDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
swatDb = paste(projectDir, "SWAT2012.mdb", sep="/")
netDir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement"
crosswalk_file = paste(netDir, "Landuse_Lookup.csv", sep="/")

# Read in all necessary tables

crosswalk = read.csv(crosswalk_file)

con_updates = odbcConnectAccess(paste(netDir, "OpSchedules_fert.mdb", sep="/"))
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
    print(paste(as.character(row_data$SUBBASIN), as.character(row_data$HRU)))
    lu = as.character(row_data$LANDUSE)
    opCode = unique(as.character(crosswalk$KEY[crosswalk$LANDUSE == lu]))
    if (opCode =="800") {opCode = "HAY"}
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
    }
    if (row %% 500 == 0) {
        close(con_mgt2)
        print("Compacting database. Please wait...")
        system(paste("C:\\Python27\\ArcGIS10.1\\python.exe", py_file))
        con_mgt2 = odbcConnectAccess(prjDb) 
    }
}

