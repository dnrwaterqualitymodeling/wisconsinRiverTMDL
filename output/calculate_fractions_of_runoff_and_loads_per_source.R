library(rgdal)
library(RODBC)

dir_swat_prj = "C:/TEMP/WRB.Sufi2.SwatCup"
file_swat_db = "K:/WRB/WRB.mdb"
file_lu_lkp = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_Update.csv"
file_of = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/FINAL_WASTEWATER_DATASET/WRB_Permitted_Outfall_Data_FINAL_May2015.txt"
file_muni = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads.txt"
file_muni_shp = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/SWAT_Urban_Areas7.shp"
out_main_table = "T:/Projects/Wisconsin_River/Model_Outputs/tables/runoff_load_breakdown.txt"

of_data = read.delim(file_of)
muni_data = read.table(file_muni, header=T, sep="\t")

file_output.hru = paste(dir_swat_prj, "/output.hru", sep="")

w = c(4,5,10,5,5,5,rep(10,67))
col.names = read.fwf(file_output.hru, widths=w, skip=8, n=1, as.is=T, strip.white=T)
output.hru = read.fwf(file_output.hru, widths=w, skip=9)
names(output.hru) = col.names[1,]
#hru_ann_ave = subset(output.hru, MON == 12)

dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by="1 day")
dates = data.frame(
	YEAR = as.integer(format(dates, "%Y")),
	MON = as.integer(format(dates, "%m")),
	DAY = as.integer(format(dates, "%j"))
)
days_per_mo = ts(
	diff(seq(as.Date("2002-01-01"), as.Date("2014-01-01"), by="month")),
	start = c(2002, 01),
	freq = 12
)
days_per_mo = as.integer(days_per_mo)
year = as.integer(format(
	seq(as.Date("2002-01-01"), as.Date("2013-12-01"), by="month"),
	"%Y"
))
mo = as.integer(format(
	seq(as.Date("2002-01-01"), as.Date("2013-12-01"), by="month"),
	"%m"
))
days_per_mo = data.frame(
	year=year,
	mo=mo,
	days_per_mo=days_per_mo
)

out_table = data.frame()

###################
# Point sources
###################
ofs = unique(of_data$SAMPLE_ID)
sub_of_data = data.frame()
for (of in ofs) {
	print(of)
	pt_data = subset(of_data, SAMPLE_ID == of)
	flow_data = subset(pt_data, STORET_PARM_DESC == "Flow Rate")
	# Convert million gallons per day to cubic meters per day
	mean_flow = flow_data$MODEL_VALUE * 3785.41
	# calculate average daily phosphorous and sediment loads
	p_data = subset(pt_data, STORET_PARM_DESC == "Phosphorus, Total")
	p_conc = p_data$MODEL_VALUE
	# mg/L * m3/day * (1000 L / 1 m3) * (1 kg / 1e6 mg)
	p_load = p_conc * mean_flow * 0.001
	
	sed_data = subset(pt_data, STORET_PARM_DESC == "Suspended Solids, Total")
	units = sed_data$PARM_UNIT_TYPE[1]
	if (tolower(units) == "mg/l" | units == "MGD") { # MGD was a mistake in the database---actually mg/L
		sed_conc = sed_data$MODEL_VALUE
		# mg/L * m3/day * (1000 L / 1 m3) * (1 kg / 1e6 mg)
		sed_load = sed_conc * mean_flow * 0.001
	} else if (units == "lbs/day") {
		sed_conc = sed_data$MODEL_VALUE
		sed_load = sed_conc * 0.454
	} else if (units == "kg/day") {
		sed_conc = sed_data$MODEL_VALUE
		sed_load = sed_conc
	} 
	out_mon = cbind(p_data["YEAR_"], p_data["MONTH_"], mean_flow, sed_load, p_load)
	names(out_mon)[1:2] = c("YEAR", "MON")
	out_day = merge(dates, out_mon, all.x=T, all.y=F)
	sub_of_data = rbind(sub_of_data, out_day)
}
# convert flow from m3 to hectare meters
sub_of_data$mean_flow = sub_of_data$mean_flow / 10000

# Add all loads over the whole simulation period
of_summ = colSums(sub_of_data[c("mean_flow", "sed_load", "p_load")], na.rm=T)
# Divide simulation total by 12 years to calculate the annual average load (kg / yr)
of_summ = of_summ / 12 
of_summ = as.data.frame(t(of_summ))

ps_table = data.frame(
	source = "point sources",
	area_km2 = NA,
	variable = c("Q", "TP", "TSS"),
	load = c(of_summ$mean_flow, of_summ$p_load, of_summ$sed_load),
	unit = c("hectare-meters", "kg", "kg")
)
out_table = rbind(out_table, ps_table)

###################
# Municipalities
###################

muni_data$flow_ha_m = muni_data$flow_m3 / 10000
muni_data$tss_kg = muni_data$TSS_tons * 1000
muni_data$tp_kg = muni_data$P_filt_kg + muni_data$P_part_kg
muni_data$date = as.Date(muni_data$date)

muni_summ = aggregate(
	cbind(flow_ha_m, tp_kg, tss_kg) ~ format(date, "%Y"),
	data=muni_data,
	sum
)
muni_summ = colMeans(muni_summ[c("flow_ha_m", "tp_kg", "tss_kg")], na.rm=T)

muni_shp = readOGR(
	dirname(file_muni_shp),
	strsplit(basename(file_muni_shp), "\\.")[[1]][1]
)
	
ms4_table = data.frame(
	source = "Urban areas",
	area_km2 = sum(muni_shp@data$Shape_Area) / 1000^2,
	variable = c("Q", "TP", "TSS"),
	load = c(muni_summ$flow_ha_m, muni_summ$tp_kg, muni_summ$tss_kg),
	unit = c("hectare-meters", "kg", "kg")
)

out_table = rbind(out_table, ms4_table)

###################
# Ag
###################

con = odbcConnectAccess(file_swat_db)
hru_lkp = sqlQuery(con, "select landuse,hru_gis from hrus")
close(con) 

lu_lkp = read.csv(file_lu_lkp)
ag = lu_lkp$LANDUSE[!(lu_lkp$TYPE %in% c("Non-Ag", "Cranberry"))]
gis = hru_lkp$hru_gis[hru_lkp$landuse %in% ag]
ag_dt = subset(
	output.hru, GIS %in% gis & MON != 12,
	select=c(
		"GIS",
		"MON",
		"AREAkm2",
		"SURQ_GENmm",
		"SYLDt/ha",
		"ORGPkg/ha",
		"SEDPkg/ha",
		"SOLPkg/ha"
	)
)
ag_dt$flow_ha_m =
	(ag_dt$AREAkm2 * 100) * (ag_dt$SURQ_GENmm / 1000)
ag_dt$tss_kg = (ag_dt$AREAkm2 * 100) * ag_dt[["SYLDt/ha"]] * 1000
ag_dt$tp_kg = 
	(ag_dt$AREAkm2 * 100) *
		rowSums(cbind(
			ag_dt[["ORGPkg/ha"]],
			ag_dt[["SEDPkg/ha"]],
			ag_dt[["SOLPkg/ha"]]
		))
ag_dt = aggregate(
	cbind(AREAkm2, flow_ha_m, tss_kg, tp_kg) ~ GIS,
	ag_dt,
	FUN=mean
)
ag_summ = colSums(ag_dt[c("flow_ha_m", "tp_kg", "tss_kg")])
ag_summ = as.data.frame(t(ag_summ))

ag_table = data.frame(
	source = "Agriculture",
	area_km2 = sum(ag_dt$AREAkm2),
	variable = c("Q", "TP", "TSS"),
	load = c(ag_summ$flow_ha_m, ag_summ$tp_kg, ag_summ$tss_kg),
	unit = c("hectare-meters", "kg", "kg")
)

out_table = rbind(out_table, ag_table)

###################
# Other land uses
###################

source_names = data.frame(
	lu_code = c("URML", "PAST", "WETN", "FRST"),
	desc = c(
		"Non-point developed",
		"Grassland",
		"Wetland",
		"Forest"
	)
)

for (lu_code in c("URML", "PAST", "WETN", "FRST")) {
	if (lu_code == "FRST") {
		lu_code = c("FRSD", "FRSE", "FRST")
	}
	gis = hru_lkp$hru_gis[hru_lkp$landuse %in% lu_code]
	non_pt_dt = subset(
		output.hru, GIS %in% gis & MON != 12,
		select=c(
			"GIS",
			"AREAkm2",
			"SURQ_GENmm",
			"SYLDt/ha",
			"ORGPkg/ha",
			"SEDPkg/ha",
			"SOLPkg/ha"
		)
	)
	non_pt_dt$flow_ha_m =
		(non_pt_dt$AREAkm2 * 100) * (non_pt_dt$SURQ_GENmm / 1000)
	non_pt_dt$tss_kg = (non_pt_dt$AREAkm2 * 100) * non_pt_dt[["SYLDt/ha"]] * 1000
	non_pt_dt$tp_kg = 
		(non_pt_dt$AREAkm2 * 100) *
		rowSums(cbind(
				non_pt_dt[["ORGPkg/ha"]],
				non_pt_dt[["SEDPkg/ha"]],
				non_pt_dt[["SOLPkg/ha"]]
			))
	non_pt_dt = aggregate(
		cbind(AREAkm2, flow_ha_m, tss_kg, tp_kg) ~ GIS,
		non_pt_dt,
		FUN=mean
	)
	non_pt_summ = colSums(non_pt_dt[c("flow_ha_m", "tp_kg", "tss_kg")])
	non_pt_summ = as.data.frame(t(non_pt_summ))
	
	non_pt_table = data.frame(
		source = source_names$desc[source_names$lu_code %in% lu_code],
		area_km2 = sum(non_pt_dt$AREAkm2),
		variable = c("Q", "TP", "TSS"),
		load = c(non_pt_summ$flow_ha_m, non_pt_summ$tp_kg, non_pt_summ$tss_kg),
		unit = c("hectare-meters", "kg", "kg")
	)
	
	out_table = rbind(out_table, non_pt_table)
}
#
#out_table = data.frame(
#	source = out_table$source,
#	area_km2 = out_table$area_km2,
#	variable = out_table$variable,
#	load = out_table$load
#)
#
#out_tabl

write.table(out_table, file=out_main_table, sep="\t", row.names=F)

