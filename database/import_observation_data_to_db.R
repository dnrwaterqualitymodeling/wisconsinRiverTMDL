library(dplyr)
library(xlsx)

file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"
dir_obs = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads"
file_wq_summ = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Summary_All_4_30_15_clean.xls"

db_con = src_sqlite(file_db)
par_dirs = list.files(dir_obs)
d = NULL
for (par_dir in par_dirs) {
	dir_data = paste(dir_obs, par_dir, "Loads", sep="/")
	files_data = list.files(dir_data, full.names=T)
	for (file_data in files_data) {
		print(file_data)
		d_stn = read.table(file_data, header=T, sep="\t")
		if (dim(d_stn)[2] != 10) { next }
		names(d_stn) = c(
			"station_id",
			"date",
			"dflow",
			"censcd",
			"actual",
			"dload",
			"sepdload",
			"cilo",
			"cihi",
			"if_avpredict"
		)
		d_stn$parameter = par_dir
		d_stn$date = as.character(
			format(as.Date(d_stn$date, format="%m/%d/%Y"), "%Y-%m-%d"))
		d_stn$mon = as.integer(format(as.Date(d_stn$date), "%m"))
		d_stn$day = as.integer(format(as.Date(d_stn$date), "%d"))
		d_stn$yr = as.integer(format(as.Date(d_stn$date), "%Y"))
		d_stn = d_stn[,c(
			"station_id",
			"parameter",
			"date",
			"mon",
			"day",
			"yr",
			"dflow",
			"censcd",
			"actual",
			"dload",
			"sepdload",
			"cilo",
			"cihi",
			"if_avpredict")]
		d = rbind(d, d_stn)
	}
}
par_tbl = copy_to(db_con, d, "load_estimates", temporary=FALSE)
rm(db_con)


tbl_lu = rbind(
	c("p00530_SusSed", "SS_00530"),
	c("p00600_TN", "TN_00600"),
	c("p00608_NH4", "NH4_00608"),
	c("p00625_KJ", "KJ_00625"),
	c("p00631_NO3", "NO3_00631"),
	c("p00665_TP", "TP_00665"),
	c("p00671_OP", "OP_00671")
)
summ_tbl_all = NULL
for (tbl_i in 1:nrow(tbl_lu)) {
	tbl = tbl_lu[tbl_i, 1]
	summ_tbl = read.xlsx(file_wq_summ, sheetName=tbl)
	names(summ_tbl) = c(
		"station_id",
		"station_name",
		"area",
		"flow_station_id",
		"flow_Station_area",
		"n_wq_obs",
		"n_wq_censored",
		"wq_start_date",
		"wq_end_date",
		"ave_flow_m3_yr_2011_13",
		"ave_load_kg_yr_2011_13",
		"ave_concentration_mg_L_2011_13",
		"ave_flow_m3_yr_2010_13",
		"ave_load_kg_yr_2010_13",
		"ave_concentration_mg_L_2010_13",
		"mean_df",
		"LOAD_A",
		"SLOAD_A",
		"DSLOAD_A",
		"O_OVER_E",
		"se",
		"yld",
		"b_int",
		"b_ldflow",
		"b_ldflow_2",
		"b_sin",
		"b_cos",
		"b_trend",
		"b_trend_2",
		"p_b_int",
		"p_b_ldflow",
		"p_b_sin",
		"p_b_cos",
		"p_b_trend",
		"nonFAT_Load",
		"p_nonFAT_Load",
		"LOAD_1996",
		"LOAD_1997",
		"LOAD_1998",
		"LOAD_1999",
		"LOAD_2000",
		"LOAD_2001",
		"LOAD_2002",
		"LOAD_2003",
		"LOAD_2004",
		"LOAD_2005",
		"LOAD_2006",
		"LOAD_2007",
		"LOAD_2008",
		"LOAD_2009",
		"LOAD_2010",
		"LOAD_2011",
		"LOAD_2012",
		"LOAD_2013",
		"LOAD_2014",
		"MNDTDF_2010",
		"MNDTDF_2011",
		"MNDTDF_2012",
		"MNDTDF_2013",
		"avg_load_11_13",
		"avg_flow_11_13",
		"ave_flow_m3_yr_2010_13_1",
		"ave_load_kg_yr_2010_13_1",
		"concentration_mg_L_2010_13",
		"flow_m3_yr",
		"concentration_mg_L",
		"special_notes"
	)
	summ_tbl = summ_tbl %>%
		mutate(
			parameter = tbl_lu[tbl_i, 2],
			wq_start_yr=as.integer(format(wq_start_date,"%Y")),
			wq_end_yr=as.integer(format(wq_end_date,"%Y")),
			wq_start_mo=as.integer(format(wq_start_date,"%m")),
			wq_end_mo=as.integer(format(wq_end_date,"%m")),
			wq_start_day=as.integer(format(wq_start_date,"%d")),
			wq_end_day=as.integer(format(wq_end_date,"%d")),
			wq_start_date=format(wq_start_date,"%Y-%m-%d"),
			wq_end_date=format(wq_end_date,"%Y-%m-%d")
		)
	summ_tbl = summ_tbl[,c(
		"station_id",
		"parameter",
		"station_name",
		"area",
		"flow_station_id",
		"flow_Station_area",
		"n_wq_obs",
		"n_wq_censored",
		"wq_start_date",
		"wq_end_date",
		"wq_start_yr",
		"wq_end_yr",
		"wq_start_mo",
		"wq_end_mo",
		"wq_start_day",
		"wq_end_day",
		"ave_flow_m3_yr_2011_13",
		"ave_load_kg_yr_2011_13",
		"ave_concentration_mg_L_2011_13",
		"ave_flow_m3_yr_2010_13",
		"ave_load_kg_yr_2010_13",
		"ave_concentration_mg_L_2010_13",
		"mean_df",
		"LOAD_A",
		"SLOAD_A",
		"DSLOAD_A",
		"O_OVER_E",
		"se",
		"yld",
		"b_int",
		"b_ldflow",
		"b_ldflow_2",
		"b_sin",
		"b_cos",
		"b_trend",
		"b_trend_2",
		"p_b_int",
		"p_b_ldflow",
		"p_b_sin",
		"p_b_cos",
		"p_b_trend",
		"nonFAT_Load",
		"p_nonFAT_Load",
		"LOAD_1996",
		"LOAD_1997",
		"LOAD_1998",
		"LOAD_1999",
		"LOAD_2000",
		"LOAD_2001",
		"LOAD_2002",
		"LOAD_2003",
		"LOAD_2004",
		"LOAD_2005",
		"LOAD_2006",
		"LOAD_2007",
		"LOAD_2008",
		"LOAD_2009",
		"LOAD_2010",
		"LOAD_2011",
		"LOAD_2012",
		"LOAD_2013",
		"LOAD_2014",
		"MNDTDF_2010",
		"MNDTDF_2011",
		"MNDTDF_2012",
		"MNDTDF_2013",
		"special_notes"
	)]
	summ_tbl_all = rbind(summ_tbl_all, summ_tbl)
}
summ_tbl_all = summ_tbl_all %>%
	mutate(
		station_id = as.integer(station_id),
		flow_station_id = as.integer(flow_station_id)
)
db_con = src_sqlite(file_db)
summ_tbl = copy_to(db_con, summ_tbl_all, "load_estimate_summary", temporary=FALSE)
rm(db_con)

