library(RODBC)

# CHANGE THESE ###########
# SWAT project
projectDir = "C:/SWAT/Reservoirs_2"
wetland_geometry_file = "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands/wetland_geometry.csv"

wetland_geometry = read.csv(wetland_geometry_file)

inDb = paste(projectDir, "Reservoirs_2.mdb", sep="/")
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


