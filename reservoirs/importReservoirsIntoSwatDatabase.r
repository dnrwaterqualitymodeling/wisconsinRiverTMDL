library(RODBC)

# CHANGE THESE ###########
# SWAT project
projectDir = "H:/WRB"
reservoir_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/hydrology/dams_parameters.csv"

reservoir_parameters = read.csv(reservoir_parameter_file)

inDb = paste(projectDir, "Reservoirs_2.mdb", sep="/")
con = odbcConnectAccess(inDb)

# resData = sqlQuery(con, "SELECT * FROM res")

for (row in 1:nrow(reservoir_parameters)) {
	query = paste(
		"UPDATE res ",
		"SET RES_ESA = ", reservoir_parameters$MaxStorage[row] * 1.1, ",",
		"RES_EVOL = ", reservoir_parameters$MaxS_10to4[row], ",",
		"RES_PSA = ", reservoir_parameters$res_psa[row], ",",
		"RES_PVOL = ", reservoir_parameters$NormS_10to[row], ",",
		"RES_VOL = ", reservoir_parameters$NormS_10to[row], " ",
		"WHERE SUBBASIN = ", reservoir_parameters$Subbasin[row], ";",
		sep = ""
	)
    stdout = sqlQuery(con, query)
}

close(con)


