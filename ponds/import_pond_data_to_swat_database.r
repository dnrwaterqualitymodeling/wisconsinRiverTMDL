library(RODBC)

# CHANGE THESE ###########
# SWAT project
projectDir = "C:/SWAT/Reservoirs_2"
pond_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"

pond_geometry = read.csv(pond_geometry_file)

inDb = paste(projectDir, "Reservoirs_2.mdb", sep="/")
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


