library(tidyr)
library(rgdal)

dir_swat_prj = "C:/TEMP/WRB.Sufi2.SwatCup"
file_swat_db = "K:/WRB/WRB.mdb"
file_lu_lkp = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_Update.csv"
file_of = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/FINAL_WASTEWATER_DATASET/WRB_Permitted_Outfall_Data_FINAL_May2015.txt"
file_muni = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads_tss_fix.txt"
file_muni_shp = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/SWAT_Urban_Areas7.shp"
out_main_table = "T:/Projects/Wisconsin_River/Model_Outputs/tables/runoff_load_breakdown.txt"

of_data = read.delim(file_of)
muni_data = read.delim(file_muni)

file_output.hru = paste(dir_swat_prj, "/output.hru", sep="")

w = c(4,5,10,5,5,5,rep(10,67))
col.names = read.fwf(file_output.hru, widths=w, skip=8, n=1, as.is=T, strip.white=T)
output.hru = read.fwf(file_output.hru, widths=w, skip=9)
names(output.hru) = col.names[1,]
hru_ann_ave = subset(output.hru, MON == 12)

dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), by="1 day")
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
	mean_flow = (flow_data$MODEL_VALUE)*3785.41
	# calculate average daily phosphorous and sediment loads
	p_data = subset(pt_data, STORET_PARM_DESC == "Phosphorus, Total")
	p_conc = (p_data$MODEL_VALUE)
	p_load = p_conc*mean_flow*0.001
	
	sed_data = subset(pt_data, STORET_PARM_DESC == "Suspended Solids, Total")
	units = sed_data$PARM_UNIT_TYPE[1]
	if(tolower(units) == "mg/l" | units == "MGD") { # MGD was a mistake in the database---actually mg/L
		sed_conc = (sed_data$MODEL_VALUE)
		sed_load = sed_conc*mean_flow*0.000001
	} else if(units == "lbs/day"){
		sed_conc = (sed_data$MODEL_VALUE)
		sed_load = sed_conc*0.000454
	} else if(units == "kg/day"){
		sed_conc = (sed_data$MODEL_VALUE)
		sed_load = sed_conc*.001
	} 
	out_mon = cbind(p_data["YEAR_"], p_data["MONTH_"], mean_flow, sed_load, p_load)
	names(out_mon)[1:2] = c("YEAR", "MON")
	out_day = merge(dates, out_mon, all.x=T, all.y=F)
	out_day$SAMPLE_ID = of
	sub_of_data = rbind(sub_of_data, out_day)
}
aggregated_of = aggregate(
	cbind(mean_flow, sed_load, p_load) ~ YEAR + DAY, 
	data=sub_of_data, 
	FUN=sum,
	na.rm=T)
aggregated_of = merge(dates, aggregated_of, all.x=T, all.y=F)
# Seperate P into 58% organic and 42% mineral
aggregated_of$p_org_load = aggregated_of$p_load * 0.58
aggregated_of$p_min_load = aggregated_of$p_load * 0.42
aggregated_of = aggregated_of[,-6]
if (sum(aggregated_of[,4:7], na.rm=T) == 0) {
	ofs_issues = paste(ofs, collapse=",")
	writeLines(paste(ofs_issues, "SAMPLE_PTs have all zero data"), log_file)
}

conc_to_load = 	function (x, days_per_mo) {
	x = as.data.frame(t(x))
	# convert flow to cubic meters per day
	mean_flow = as.numeric(as.character(x$MODEL_VALUE)) * 3785.41
	days = days_per_mo$days_per_mo[
		days_per_mo$year == as.integer(as.character(x$YEAR_)) &
			days_per_mo$mo == as.integer(as.character(x$MONTH_))
	]
	if (x$STORET_PARM_DESC == "Flow Rate") {
		# Convert flow "load" to hectare-meters per day
		load = mean_flow * (1/1000)
	# Calculate pollutant loads in average kilograms per day for both sed and P
	} else if (x$STORET_PARM_DESC == "Suspended Solids, Total") {
		units = x$PARM_UNIT_TYPE[1]
		model_value = as.numeric(as.character(x$MODEL_VALUE))
		if (tolower(units) == "mg/l" | units == "MGD") { # MGD was a mistake in the database---actually mg/L
			load = model_value * mean_flow * 0.001
		} else if (units == "lbs/day") {
			load = model_value * 0.454
		} else if (units == "kg/day") {
			load = model_value
		}
	} else if (x$STORET_PARM_DESC == "Phosphorus, Total") {
		model_value = as.numeric(as.character(x$MODEL_VALUE))
		load = model_value * mean_flow * 0.001
	}
	return(load * days)
}

of_data$load = apply(
	of_data,
	1,
	conc_to_load,
	days_per_mo=days_per_mo
)

of_summ = aggregate(load ~ STORET_PARM_DESC, data=of_data, FUN=sum, na.rm=T)
of_summ$load = of_summ$load / 12
of_summ$unit = "kg"
names(of_summ)[1] = "variable"
of_summ$variable = as.character(of_summ$variable)
of_summ$variable[of_summ$variable == "Flow Rate"] = "Q"
of_summ$unit[of_summ$variable == "Q"] = "hectare-meters"
of_summ$variable[of_summ$variable == "Phosphorus, Total"] = "TP"
of_summ$variable[of_summ$variable == "Suspended Solids, Total"] = "TSS"
ps_table = data.frame(
	source = "point sources",
	area_km2 = NA,
	variable = of_summ$variable,
	load = of_summ$load,
	unit = of_summ$unit
)
out_table = rbind(out_table, ps_table)

###################
# Municipalities
###################

muni_data$flow_ha_m = muni_data$flow_m3 / 10000
muni_data$tss_kg = muni_data$TSS_tons * 1000
muni_data$tp_kg = muni_data$P_filt_kg + muni_data$P_part_kg

muni_summ = colSums(muni_data[c("flow_ha_m", "tp_kg", "tss_kg")])
muni_summ = as.data.frame(t(muni_summ))

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
	hru_ann_ave, GIS %in% gis
)
ag_dt$flow_ha_m =
	(ag_dt$AREAkm2 * 100) * (ag_dt$SURQ_GENmm / 1000) * 365.25
ag_dt$tss_kg = (ag_dt$AREAkm2 * 100) * ag_dt[["SYLDt/ha"]] * 1000 * 365.25
ag_dt$tp_kg = 
	(ag_dt$AREAkm2 * 100) *
		rowSums(cbind(
			ag_dt[["ORGPkg/ha"]],
			ag_dt[["SEDPkg/ha"]],
			ag_dt[["SOLPkg/ha"]]
		)) * 365.25
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
		hru_ann_ave, GIS %in% gis
	)
	non_pt_dt$flow_ha_m =
		(non_pt_dt$AREAkm2 * 100) * (non_pt_dt$SURQ_GENmm / 1000) * 365.25
	non_pt_dt$tss_kg = (non_pt_dt$AREAkm2 * 100) * non_pt_dt[["SYLDt/ha"]] * 1000 * 365.25
	non_pt_dt$tp_kg = 
		(non_pt_dt$AREAkm2 * 100) *
		rowSums(cbind(
				non_pt_dt[["ORGPkg/ha"]],
				non_pt_dt[["SEDPkg/ha"]],
				non_pt_dt[["SOLPkg/ha"]]
			)) * 365.25
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

out_table = data.frame(
	source = out_table$source,
	area_km2 = out_table$area_km2,
	variable = out_table$variable,
	load = out_table$load,
	yield = out_table$load / out_table$area_km2,
)

out_tabl

write.table(out_table, file=out_main_table, sep="\t", row.names=F)

