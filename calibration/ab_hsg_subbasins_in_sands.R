library(RODBC)

con = odbcConnectAccess("C:/Users/ruesca/Desktop/WRB/WRB.mdb")
hru = sqlQuery(
	con,
	"SELECT sol.SUBBASIN,hru.HRU_FR,sol.HYDGRP
	FROM hru
	LEFT JOIN sol
	ON hru.SUBBASIN = sol.SUBBASIN AND hru.HRU = sol.HRU;"
)
odbcClose(con)

eco_sub = read.table(
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/SB_by_ECO_lookup_ecological_landscapes.txt",
	header=T
)

d = merge(hru, eco_sub, by.x="SUBBASIN", by.y="subbasin")
d = subset(d, eco_land %in% c("Central Sand Plains", "Forest Transition"))
maj_hsg = NULL
for (s in unique(d$SUBBASIN, sort=T)) {
	d_s = subset(d, SUBBASIN==s)
	row = data.frame(
		subbasin=s,
		maj_hsg=d_s$HYDGRP[which.max(d_s$HRU_FR)]
	)
	maj_hsg = rbind(maj_hsg, row)
}

write.table(
	maj_hsg,
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/ab_hsg_subbasins_in_sands.txt",
	row.names=F)