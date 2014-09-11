library(RODBC)

# CHANGE THESE ###########
# SWAT project
projectDir = "C:/SWAT/Wetlands_2"
gw_parameter_file = "T:/Projects/Wisconsin_River/GIS_Datasets/groundWater/alphaBflowSubbasin_lookup.csv"

gw_parameters = read.csv(gw_parameter_file)

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
con = odbcConnectAccess(inDb)

# resData = sqlQuery(con, "SELECT * FROM res")

for (row in 1:nrow(gw_parameters)) {
	query = paste(
		"UPDATE gw ",
		"SET alphaBflow_Preds_mod3 = ", gw_parameters$areaWtdAvg[row], ' ',
		"WHERE SUBBASIN = ", gw_parameters$Subbasin[row], ";",
		sep = ""
	)
    stdout = sqlQuery(con, query)
}

close(con)


