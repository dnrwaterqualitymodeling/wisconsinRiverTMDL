library(RODBC)

file_alfa_Scheds = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/OpSchedules_alfalfa_kill_fix.txt"
opschedules = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/OpSchedules.mdb"
alfa_scheds = read.delim(file_alfa_Scheds)

drop_cols = c("SUBBASIN", "HRU", "LANDUSE", "SOIL", "SLOPE_CD", "CROP")
sel_bool = !(names(alfa_scheds) %in% drop_cols)
alfa_scheds  = alfa_scheds[,sel_bool]
cls = names(alfa_scheds)

alfa_con = odbcConnectAccess(opschedules)

for (rw in 1:nrow(alfa_scheds)){
	
	vals = alfa_scheds[rw,]
	alfa_qury = paste(
		"INSERT INTO OpSchedules (",
		paste(cls, collapse=","),
		") VALUES ('",
		vals[1],
		"',",
		paste(vals[2:length(vals)], collapse = ","),
		");",
		sep=""
	)
	print(alfa_qury)
	stdout = sqlQuery(alfa_con, alfa_qury)
	print(paste("\t", stdout))
}

odbcCloseAll()