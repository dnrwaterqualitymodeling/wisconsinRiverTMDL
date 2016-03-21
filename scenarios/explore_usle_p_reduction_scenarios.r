library(dplyr)
library(stringr)

dir_swat = "C:/TEMP/WRB.Sufi2.SwatCup"
file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"

swat_files = list.files(dir_swat, full.names=T, include.dirs = FALSE)

td = tempdir()
setwd(td)
file.copy(swat_files, td)
file.copy(file_db, td, overwrite=T)
dir.create("mgt_bkp")

files_mgt = list.files(pattern="\\.mgt$")
file.copy(files_mgt, "mgt_bkp")

scens = seq(0, -1, by=-0.2)
output_scens = NULL
for (scen in scens) {
	for (file_mgt in files_mgt) {
		mgt = readLines(file_mgt)
		ag = str_detect(mgt[29], "6")
		if (ag) {
			print("Applying scenario to *.mgt files...")
			usle_p = as.numeric(str_sub(mgt[12], 9, 16))
			usle_p = usle_p * (1 + scen)
			usle_p = sprintf("%8f", usle_p)
			str_sub(mgt[12], 9, 16) = usle_p
		}
		writeLines(mgt, file_mgt)
	}
	print("Running SWAT...")
	system("swat.exe")
	print("Restoring default *.mgt files...")
	file.copy(list.files("mgt_bkp", full.names=T), ".", overwrite=T)
	
	print("Reading and formatting output.rch...")
	colnames=c("REACH","RCH","GIS","MON","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS",
		"SED_IN","SED_OUT","SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN",
		"ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN","NO2_OUT",
		"MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT",
		"DISOX_IN","DISOX_OUT","SOLPST_IN","SOLPST_OUT","SORPST_IN",
		"SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST",
		"DIFFUSEPST","REACBEDPST","BURYPST","BED_PST","BACTP_OUT",
		"BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P",
		"NO3CONC","WTMP")
	colclasses=c("character","integer","character","character",rep("numeric", 47))
	d = read.table(
		"output.rch",
		skip=9,
		colClasses=colclasses
	)
	names(d) = colnames
	d = d[,-1]
	d = d %>%
		filter(!grepl('(^20)|(^12\\.0)', MON)) %>%
		mutate(MON = as.integer(MON))
	yrs = NULL
	for (y in 2002:2013) {
		yrs = c(yrs, rep(y, dim(d)[1] / 12))
	}
	d = mutate(d, YR=yrs)
	d = d[
		c("RCH","GIS","MON","YR","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS",
			"SED_IN","SED_OUT","SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN",
			"ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN","NO2_OUT",
			"MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT",
			"DISOX_IN","DISOX_OUT","SOLPST_IN","SOLPST_OUT","SORPST_IN",
			"SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST",
			"DIFFUSEPST","REACBEDPST","BURYPST","BED_PST","BACTP_OUT",
			"BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P",
			"NO3CONC","WTMP")
	]
	print("Subtracting point source loads from output.rch...")
	con = src_sqlite("wrb_swat_db.sqlite3")
	stdout = copy_to(con, d, "output_rch_monthly_ag_scen", temporary=FALSE,
		indexes = list(c("RCH", "MON", "YR")))

	output_scen = tbl(con, "output_rch_monthly_ag_scen_no_point_src_ms4") %>%
		left_join(tbl(con, "days_in_mo")) %>%
		mutate(FLOW_VOL_L = FLOW_OUT * 8.64e7 * N_DAYS) %>%
		group_by(RCH) %>%
		summarise(FWMC = (sum(TOT_P) / sum(FLOW_VOL_L)) * 1e9) %>%
		mutate(USLE_P_SCEN = scen) %>%
		select(RCH, USLE_P_SCEN, FWMC) %>%
		collect()
	con$con %>% db_drop_table(table="output_rch_monthly_ag_scen")
	rm(con)
	print("Collecting results. End of scenario processing.")
	output_scens = rbind(output_scens, output_scen)
}

con = src_sqlite(file_db)
output_scens = copy_to(con, output_scens, "USLE_P_scenarios", temporary=F)
