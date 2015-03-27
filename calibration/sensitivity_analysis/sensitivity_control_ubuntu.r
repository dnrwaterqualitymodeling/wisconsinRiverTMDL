#setInternet2(TRUE)
library(RCurl)
options(stringsAsFactors=F)

# unchanging parameters

txtinout = "/media/d/TxtInOut"
dir_out = "/media/d/WRB_sensitivity_sub"
temp_dir = "/media/d/temp"
script_sensitivity = 
	"/media/d/wisconsinRiverTMDL/calibration/sensitivity_analysis/bsn_hru_gw_rte_sol_pnd.r"
iter = 25

# Read parameter table with parameter name, file extension, min, max, method
file_par_table = tempfile()
writeLines(
	getURL(
		paste("https://raw.githubusercontent.com",
			"dnrwaterqualitymodeling",
			"wisconsinRiverTMDL",
			"master",
			"calibration",
			"sensitivity_analysis",
			"basin_sensitivity_parameters.csv",
			sep="/"),
		ssl.verifypeer=0L,
		followlocation=1L),
	file_par_table)
par_inf_tbl = read.csv(file_par_table)
par_inf_tbl = subset(par_inf_tbl, run==1)

ctrl_pid = Sys.getpid()
log_file = tempfile()

# loop on parameter name
for (p.i in 1:nrow(par_inf_tbl)){
	# Create an empty temporary batch file for every 32 parameters
	tmp_sh = tempfile(fileext = ".sh")

	# Grabbing the parameters
	p = par_inf_tbl$param[p.i]
	ext = par_inf_tbl$ext[p.i]
	mn = par_inf_tbl$minVal[p.i]
	mx = par_inf_tbl$maxVal[p.i]
	method = par_inf_tbl$method[p.i]
	# Write a start command on one line of the above batch file, appended
	cmd = paste(
		"Rscript",
		script_sensitivity,
		txtinout,
		dir_out,
		temp_dir,
		p,
		ext,
		mn,
		mx,
		method,
		iter,
		sep = " ")
	writeLines(cmd, tmp_sh)
	system(paste("chmod +x", tmp_sh))
	go.to.next = F
	while (!go.to.next) {
		ps = system('ps -C R', intern=TRUE)
		if (length(ps) <= 9) {
			go.to.next = T
		} else {
			Sys.sleep(1)
			go.to.next = F
		}
	}
	system(tmp_sh, wait=F)
}
