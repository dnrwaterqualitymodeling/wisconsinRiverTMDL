library(dplyr)
library(tidyr)
library(rgdal)
library(RODBC)
library(xlsx)

dir_swat_prj = "C:/TEMP/WRB.Sufi2.SwatCup"
file_swat_db = "K:/WRB/WRB.mdb"
file_lu_lkp = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_Update.csv"
file_of = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/FINAL_WASTEWATER_DATASET/WRB_Permitted_Outfall_Data_FINAL_NOV2015.txt"
file_muni = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads.txt"
file_muni_shp = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/SWAT_Urban_Areas7.shp"
out_main_table = "T:/Projects/Wisconsin_River/Model_Outputs/tables/runoff_load_breakdown.txt"
out_ag_table = "T:/Projects/Wisconsin_River/Model_Outputs/tables/runoff_load_breakdown_ag.txt"

of_data = read.delim(file_of)[
	c(
		"SAMPLE_ID",
		"YEAR_",
		"MONTH_",
		"STORET_PARM_DESC",
		"PARM_UNIT_TYPE",
		"MODEL_VALUE",
		"Flow.Thru"
	)
]
muni_data = read.table(file_muni, header=T, sep="\t")

dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by="1 day")
dates = data.frame(
	YEAR = as.integer(format(dates, "%Y")),
	MON = as.integer(format(dates, "%m")),
	DAY = as.integer(format(dates, "%j"))
)

out_table = data.frame()

#of_q_data = subset(of_data, STORET_PARM_DESC == "Flow Rate")
#of_tp_data = subset(of_data, STORET_PARM_DESC == "Phosphorus, Total")
#of_tss_data = subset(of_data, STORET_PARM_DESC == "Suspended Solids, Total")
#
#of_wide_data = merge(of_q_data, of_tp_data, by=c("SAMPLE_ID", "YEAR_", "MONTH_"))
#names(of_wide_data) = sub("\\.x", ".Q", names(of_wide_data))
#names(of_wide_data) = sub("\\.y", ".TP", names(of_wide_data))
#of_wide_data = merge(of_wide_data, of_tss_data, by=c("SAMPLE_ID", "YEAR_", "MONTH_"))
#names(of_wide_data) = sub("STORET_PARM_DESC", "STORET_PARM_DESC.TSS", names(of_wide_data))
#names(of_wide_data) = sub("STORET_PARM_DESC", "STORET_PARM_DESC.TSS", names(of_wide_data))

###################
# Point sources
###################
ofs = unique(of_data$SAMPLE_ID)
of_loads = data.frame()
for (of in ofs) {
	print(of)
	pt_data = subset(of_data, SAMPLE_ID == of)
	flow_data = subset(pt_data, STORET_PARM_DESC == "Flow Rate")
	# Convert million gallons per day to cubic meters per day
	mean_flow = flow_data$MODEL_VALUE
	# calculate average daily phosphorous and sediment loads
	p_data = subset(pt_data, STORET_PARM_DESC == "Phosphorus, Total")
	p_conc = p_data$MODEL_VALUE
	# x MG/day * y mg/L * 3785412 L/MG * 1e-6 kg/mg
	p_load = p_conc * mean_flow * 3.785412
	
	sed_data = subset(pt_data, STORET_PARM_DESC == "Suspended Solids, Total")
	units = sed_data$PARM_UNIT_TYPE[1]
	if (tolower(units) == "mg/l" | units == "MGD") { # MGD was a mistake in the database---actually mg/L
		sed_conc = sed_data$MODEL_VALUE
		# x MG/day * y mg/L * 3785412 L/MG * 1e-9 metric tons/mg
		sed_load = sed_conc * mean_flow * .003785412
	} else if (units == "lbs/day") {
		sed_load = sed_data$MODEL_VALUE * 0.000453592
	} else if (units == "kg/day") {
		sed_load = sed_data$MODEL_VALUE  * 0.001
	} 
	mean_flow = mean_flow * !flow_data$Flow.Thru
	out_mon = cbind(
		sample_id=of,
		p_data["YEAR_"],
		p_data["MONTH_"],
		mean_flow=mean_flow * 0.3785412,
		sed_load,
		p_load
	)
	
	names(out_mon)[2:3] = c("YEAR", "MON")
	out_day = merge(dates, out_mon, all.x=T, all.y=F)
	of_loads = rbind(of_loads, out_day)
}

# Add all loads over the whole simulation period
of_summ = of_loads %>%
	group_by(YEAR) %>%
	summarise(
		area_km2=NA,
		Q=sum(mean_flow),
		TP=sum(p_load, na.rm=T),
		TSS=sum(sed_load)
	)
of_summ = of_summ %>%
	group_by(source="point_sources") %>%
	summarise(
		area_km2=mean(area_km2),
		Q_ha_m=mean(Q),
		TP_kg=mean(TP),
		TSS_mt=mean(TSS)
	)
out_table = rbind(out_table, of_summ)

###################
# Municipalities
###################

muni_data$flow_ha_m = muni_data$flow_m3 / 10000
#muni_data$tss_kg = muni_data$TSS_tons
muni_data$tp_kg = muni_data$P_filt_kg + muni_data$P_part_kg
muni_data$date = as.Date(muni_data$date)

muni_shp = readOGR(
	dirname(file_muni_shp),
	strsplit(basename(file_muni_shp), "\\.")[[1]][1]
)

muni_summ = muni_data %>%
	group_by(
		source="ms4",
		area_km2 = sum(muni_shp@data$Shape_Area) / 1000^2,
		year=format(muni_data$date, "%Y")
	) %>%
	summarise(
		Q_ha_m=sum(flow_ha_m),
		TP_kg=sum(tp_kg),
		TSS_mt=sum(TSS_tons)
	)
muni_summ = muni_summ %>%
	group_by(source) %>%
	summarise(
		area_km2=mean(area_km2),
		Q_ha_m=mean(Q_ha_m),
		TP_kg=mean(TP_kg),
		TSS_mt=mean(TSS_mt)
	)

out_table = rbind(out_table, muni_summ)

###################
# Ag
###################

###################################################################
## Much of the credit for the code below belongs to Jim Almendinger.
# Select the variables you actually are interested in
myVars <- c(
	'GIS',
	'MON',
	'AREA',
	'SURQ_GEN',
	'SYLD',
	'USLE',
	'ORGP',
	'SEDP',
	'SOLP',
	'P_GW'
)

# -----Read in output.hru-----
# format of the .hru file (SWAT 2012)
hru_fmt <- list(vars = c('LULC','HRU','GIS','SUB','MGT','MON','AREA','PRECIP',
		'SNOMELT','IRR','PET','ET','SW_INIT','SW_END','PERC','GW_RCHG',
		'SNOFALL','DA_RCHG','REVAP','SA_IRR','DA_IRR','SA_ST','DA_ST',
		'SURQ_GEN','SURQ_CNT','TLOSS','LATQ','GW_Q','WYLD','DAILYCN',
		'TMP_AV','TMP_MX','TMP_MN','SOL_TMP','SOLAR','SYLD','USLE',
		'N_APP','P_APP','NAUTO','PAUTO','NGRZ','PGRZ','NCFRT','PCFRT',
		'NRAIN','NFIX','F-MN','A-MN','A-SN','F-MP','AO-LP','L-AP','A-SP',
		'DNIT','NUP','PUP','ORGN','ORGP','SEDP','NSURQ','NLATQ','NO3L',
		'NO3GW','SOLP','P_GW','W_STRS','TMP_STRS','N_STRS','P_STRS',
		'BIOM','LAI','YLD','BACTP','BACTLP','WTAB','WTABELO','SNO_HRU',
		'CMUP_KGH','CMTOT_KGH','QTILE','TNO3','LNO3','GW_Q_D',
		'LATQ_CNT'),
	col_wds = c(4,5,10,5,5,5,rep(10,67),rep(11,2),rep(10,10)))

# select columns to actually read in, to conserve computer memory
# do this by making column widths for data to ignore negative (* -1)

hru_fmt$col_wds[!(hru_fmt$vars %in% myVars)] =
	hru_fmt$col_wds[!(hru_fmt$vars %in% myVars)] * -1

# read file into dataframe df; ignore SWAT's header line because it doesn't line up well
file_output.hru = paste(dir_swat_prj, "/output.hru", sep="")
output.hru <- read.fwf(
	file=file_output.hru,
	widths=hru_fmt$col_wds,
	head=F,
	skip=9,
	strip.white=TRUE,
	buffersize=20000
)

# set column names to this ordered list of variables
colnames(output.hru) <- myVars
# get rid of rows with HRU-average values over model runs... where MON = # of runYears, << years
output.hru <- output.hru[output.hru$MON >= 1950, ]

# -----Calculate TP and TN yields, and loads for important variables-----
output.hru$SURQ_GEN_ha_m  <- output.hru$SURQ_GEN * output.hru$AREA * 0.1 # mm * km2 * 100 ha/km2 * 0.001 m/mm
output.hru$SED_t  <- output.hru$SYLD * output.hru$AREA * 100    # t/ha * km2 * ha/km2
#output.hru$SED_t  <- output.hru$USLE * output.hru$AREA * 100    # t/ha * km2 * ha/km2
output.hru$TP     <- output.hru$SOLP + output.hru$SEDP + output.hru$ORGP + output.hru$P_GW
output.hru$TP_kg  <- output.hru$TP * output.hru$AREA * 100

con = odbcConnectAccess(file_swat_db)
hru_lkp = sqlQuery(con, "select LANDUSE,HRU_GIS as GIS from hrus")
close(con) 

lu_lkp = read.csv(file_lu_lkp)[c("LANDUSE", "TYPE")]
lu_lkp$TYPE[lu_lkp$TYPE == "Cranberries"] = "Wetland"

hru_lkp = merge(hru_lkp, lu_lkp, all.x=T, all.y=F)[c("GIS", "TYPE")]
output.hru = merge(output.hru, hru_lkp)

non_pt_summ <- output.hru %>%
	group_by(TYPE, MON) %>%
	summarise(
		area_km2=sum(AREA),
		Q=sum(SURQ_GEN_ha_m),
		TP=sum(TP_kg),
		TSS=sum(SED_t)
	)
non_pt_summ <- non_pt_summ %>%
	group_by(source=TYPE) %>%
	summarise(
		area_km2=mean(area_km2),
		Q_ha_m=mean(Q),
		TP_kg=mean(TP),
		TSS_mt=mean(TSS)
	)

out_table = rbind(out_table, non_pt_summ)
out_table = out_table[!(out_table$source == "Water"),]

############################
# Summarize all sources

out_table = out_table %>%
	mutate(
		area_percent = (area_km2 / sum(area_km2, na.rm=T)) * 100,
		Q_mm = (Q_ha_m / area_km2) * 10,
		Q_percent = (Q_ha_m / sum(Q_ha_m, na.rm=T)) * 100,
		TP_kg_ha = (TP_kg / area_km2) * 0.01,
		TP_percent = (TP_kg / sum(TP_kg, na.rm=T)) * 100,
		TSS_mt_ha = (TSS_mt / area_km2) * 0.01,
		TSS_percent = (TSS_mt / sum(TSS_mt, na.rm=T)) * 100
	) %>% select(
		source,
		area_km2,
		area_percent,
		Q_ha_m,Q_mm,
		Q_percent,TP_kg,
		TP_kg_ha,
		TP_percent,
		TSS_mt,
		TSS_mt_ha,
		TSS_percent
	)
write.table(out_table, file=out_main_table, sep="\t", row.names=F, na="")

#############################
# Summarize ag sources

# read file into dataframe df; ignore SWAT's header line because it doesn't line up well
file_output.hru = paste(dir_swat_prj, "/output.hru", sep="")
output.hru <- read.fwf(
	file=file_output.hru,
	widths=hru_fmt$col_wds,
	head=F,
	skip=9,
	strip.white=TRUE,
	buffersize=20000
)

# set column names to this ordered list of variables
colnames(output.hru) <- myVars
# get rid of rows with HRU-average values over model runs... where MON = # of runYears, << years
output.hru <- output.hru[output.hru$MON >= 1950, ]

# -----Calculate TP and TN yields, and loads for important variables-----
output.hru$SURQ_GEN_ha_m  <- output.hru$SURQ_GEN * output.hru$AREA * 0.1 # mm * km2 * 100 ha/km2 * 0.001 m/mm
output.hru$SED_t  <- output.hru$SYLD * output.hru$AREA * 100    # t/ha * km2 * ha/km2
#output.hru$SED_t  <- output.hru$USLE * output.hru$AREA * 100    # t/ha * km2 * ha/km2
output.hru$TP     <- output.hru$SOLP + output.hru$SEDP + output.hru$ORGP + output.hru$P_GW
output.hru$TP_kg  <- output.hru$TP * output.hru$AREA * 100

con = odbcConnectAccess(file_swat_db)
hru_lkp = sqlQuery(con, "select LANDUSE,HRU_GIS as GIS from hrus")
close(con) 

lu_lkp = read.csv(file_lu_lkp)[c("VALUE", "LANDUSE", "GEN_DEF")]
lu_lkp = subset(
	lu_lkp,
	grepl("Dairy|Cash|Potato", GEN_DEF),
	select=c(LANDUSE, GEN_DEF) 
)

hru_lkp = merge(hru_lkp, lu_lkp, all.x=F, all.y=F)[c("GIS", "GEN_DEF")]
output.hru = merge(output.hru, hru_lkp)

ag_summ <- output.hru %>%
	group_by(GEN_DEF, MON) %>%
	summarise(
		area_km2=sum(AREA),
		Q=sum(SURQ_GEN_ha_m),
		TP=sum(TP_kg),
		TSS=sum(SED_t)
	)
ag_summ <- ag_summ %>%
	group_by(source=GEN_DEF) %>%
	summarise(
		area_km2=mean(area_km2),
		Q_ha_m=mean(Q),
		TP_kg=mean(TP),
		TSS_mt=mean(TSS)
	)

write.table(ag_summ, file=out_ag_table, sep="\t", row.names=F, na="")

