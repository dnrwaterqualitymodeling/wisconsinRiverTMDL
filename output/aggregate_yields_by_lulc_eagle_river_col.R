dir = "C:/Users/ruesca/Desktop/WRB.Sufi2.SwatCup"
out_tbl = "T:/Projects/Wisconsin_River/Model_Outputs/eagle_river_swat/eagle_river_yields.txt"

hrudata = read.table(
	paste(dir, "/output.hru", sep=""),
	skip = 9,
	header = F
)
sub_codes = c(116:123, 136, 209, 223:225, 286, 315:317, 320)

hrudata$Area = substr(hrudata$V6, 5, 14)
hrudata$V6 = substr(hrudata$V6, 1, 4)
colnames(hrudata) = c("LULC", "HRU", "GIS", "SUB", "MGT", "YEAR", "PRCP", "WYLD", "SYLD", "ORGP", "SEDP", "SOLP", "AREA")

eglrvrsub = subset(
	hrudata, SUB %in% sub_codes,
	select=c(LULC, YEAR, HRU, SUB, WYLD, SYLD, ORGP, SEDP, SOLP))

eglrvrsub=subset(eglrvrsub, YEAR %in% 2009:2013)

eglrvrsub$LULC=sub("ALFA|CSIL", "AGRL", eglrvrsub$LULC)

d = aggregate(cbind(WYLD,SYLD,ORGP,SEDP,SOLP) ~ LULC, eglrvrsub, FUN=mean, na.rm=T)

colnames(d) = c("LULC_SWAT", "water_yield_mm", "sediment_yield_metric_t_ha", "organic_p_kg_ha", "sediment_p_kg_ha ", "soluble_p_kg_ha")

d$LULC_SWAT[d$LULC_SWAT == "BERM"] = "URML"
d$LULC_SWAT[d$LULC_SWAT == "FESC"] = "PAST"

write.table(d, out_tbl, col.names = T, sep="\t", row.names=F)
