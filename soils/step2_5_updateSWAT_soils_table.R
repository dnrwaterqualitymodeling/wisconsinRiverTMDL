library(RODBC)
options(stringsAsFactors=F)
agg_soil_data = read.delim("T:/Projects/Wisconsin_River/GIS_Datasets/Soils/aggregated_soil_units.txt")
f_swat_soils_db = "C:/SWAT/ArcSWAT/Databases/SWAT_US_SSURGO_Soils.mdb"
con = odbcConnectAccess(f_swat_soils_db)

cleared = sqlQuery(con, "DELETE FROM SSURGO_Soils")

colmns = names(agg_soil_data)
tbl_cols = paste("(", 
	paste(colmns, collapse=','),
	")",
	sep='')

qry_bgning = paste("INSERT INTO SSURGO_Soils ",
	tbl_cols,
	" VALUES ",
	sep='')

for (rw in 1:nrow(agg_soil_data)){
	rw_vals = paste("('", 
		paste(agg_soil_data[rw,], collapse="','"),
	"')",
	sep='')
	# print(paste(rw_vals))
	qry = paste(qry_bgning,
		rw_vals)
	
	rslt = sqlQuery(con, qry)
}
odbcCloseAll()
