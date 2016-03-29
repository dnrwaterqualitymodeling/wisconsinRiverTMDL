library(dplyr)
library(parallel)
library(RODBC)
library(lhs)

file_lu = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_Update.csv"
file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"
file_funs = "T:/Projects/Wisconsin_River/Code/scenarios/ag_scen_adjustment_functions.r"
file_swat_mdb = "C:/Users/ruesca/Desktop/WRB/WRB.mdb"
dir_swat = "C:/TEMP/WRB.Sufi2.SwatCup"

proc = 8
n_iter = 192

ag_lu = read.csv(file_lu) %>%
	filter(VALUE >= 12, VALUE <= 54) %>%
	select(LANDUSE)
ag_lu = as.character(ag_lu[[1]])

db = odbcConnectAccess(file_swat_mdb)
lu_q = paste("('", paste(ag_lu, collapse="','"), "')", sep="")
qry = paste("SELECT subbasin, hru FROM hru WHERE landuse IN", lu_q)
hru = sqlQuery(db, qry) %>%
	mutate(
		subbasin = sprintf("%05d", subbasin),
		hru = sprintf("%04d", hru),
		fns = paste(subbasin, hru, ".mgt", sep="")
	)
close(db)
mgt_list = list()
for (fn in hru$fns) {
	mgt_list[[fn]] = readLines(paste(dir_swat, fn, sep="/"))
}

adjs = data.frame(
	usle_p = c(0,-0.5),
	filterw = c(0,0.5),
	manure = c(-0.5,0),
	cnop = c(-0.2,0)
)
lh = as.data.frame(optimumLHS(n=n_iter, k=length(adjs)))
i = 0
for (adj in adjs) {
	i = i + 1
	x = lh[,i]
	lh[,i] = (x - min(x)) / max(x - min(x)) * (adj[2] - adj[1]) + adj[1]
}
names(lh) = names(adjs)
grps = rep(1:proc, length.out=n_iter)
lh = split(lh, grps)

cl = makeCluster(proc)
clusterExport(cl, c("dir_swat", "mgt_list", "file_db", "file_funs"))
results = parLapply(
	cl,
	lh,
	function(lh) {
		library(dplyr)
		library(stringr)
		source(file_funs)
		td = tempdir()
		file.copy(list.files(dir_swat, full.names=T), td)
		file.copy(file_db, td, overwrite=T)
		setwd(td)
		dir.create("mgt_bkp")
		file.copy(list.files(pattern="\\.mgt", full.names=T), "mgt_bkp")
		output_scens = NULL
		# remove point sources
		files_ps = list.files(pattern="^[0-9]{1,3}p\\.dat")
		files_ps = file.info(files_ps)
		files_ps$fname = row.names(files_ps)
		reccnst = files_ps %>%
			filter(size < 1e3) %>%
			slice(1)
		reccnst = readLines(reccnst$fname)
		files_ps = files_ps %>%
			filter(size > 1e3)
		for (file_ps in files_ps$fname) {
			writeLines(reccnst, file_ps)
		}
		fig.fig = readLines("fig.fig")
		fig.fig = str_replace(fig.fig, "recday        10", "reccnst       11")
		writeLines(fig.fig, "fig.fig")
		for (r in 1:nrow(lh)) {
			print(r)
			mgt_adj = adj_usle_p(mgt_list, lh[r, "usle_p"])
			mgt_adj = adj_filterw(mgt_adj, lh[r, "filterw"])
			mgt_adj = adj_cnop(mgt_adj, lh[r, "cnop"], c(1,6))
			mgt_adj = adj_manure_amt(mgt_adj, lh[r, "manure"])
			for (file_mgt in names(mgt_adj)) {
				writeLines(mgt_adj[[file_mgt]], file_mgt)
			}
			system("swat.exe")
			file.copy(list.files("mgt_bkp", full.names=T), ".", overwrite=T)
			d = read_output.rch("output.rch")
			con = src_sqlite("wrb_swat_db.sqlite3")
			d = copy_to(con, d, "output_rch_monthly_ag_scen",
				indexes = list(c("RCH", "MON", "YR")))
			
#			output_scen = tbl(con, "output_rch_monthly_ag_scen_no_point_src_ms4") %>%
			output_scen = d %>%
				left_join(tbl(con, "days_in_mo")) %>%
				mutate(FLOW_VOL_L = FLOW_OUT * 8.64e7 * N_DAYS) %>%
				group_by(RCH) %>%
				summarise(FWMC = (sum(TOT_P) / sum(FLOW_VOL_L)) * 1e9) %>%
				mutate(
					USLE_P_SCEN = lh[r, "usle_p"],
					FILTERW_SCEN = lh[r, "filterw"],
					CNOP_SCEN = lh[r, "cnop"],
					MANURE_SCEN = lh[r, "manure"]
				) %>%
				left_join(tbl(con, "rch_criteria_impairment_point_source_upstream")) %>%
				mutate(CRITERIA = criteria) %>%
				select(
					RCH,
					CRITERIA,
					USLE_P_SCEN,
					FILTERW_SCEN,
					CNOP_SCEN,
					MANURE_SCEN,
					FWMC) %>%
				collect()
			con$con %>% db_drop_table(table="output_rch_monthly_ag_scen")
			rm(con)
			print("Collecting results. End of scenario processing.")
			output_scens = rbind(output_scens, output_scen)
		}
		unlink(td, recursive=T, force=T)
		return(output_scens)
	}
)
stopCluster(cl)
results_df = data.frame()
for (i in 1:proc) {
	results_df = rbind(results_df, results[[i]])
}
con = src_sqlite(file_db)
ag_scens = copy_to(con, results_df, "multivariate_ag_scen_fwmc", temporary=F)

