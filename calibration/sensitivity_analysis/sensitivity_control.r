setInternet2(TRUE)
options(stringsAsFactors=F)
# Read parameter table with parameter name, file extension, min, max, method
par_inf_tbl = read.csv("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/calibration/sensitivity_analysis/basin_sensitivity_parameters.csv")
par_inf_tbl = subset(par_inf_tbl, run==1)
# unchanging parameters

txtinout = "D:/TxtInOut"
dir_out = "D:/WRB_sensitivity_sub"
temp_dir = "Y:/temp_directory"
script_sensitivity = "D:/wisconsinRiverTMDL/calibration/sensitivity_analysis/bsn_hru_gw_rte_sol.r"
iter = 25

pat = paste("\\s(", paste(par_inf_tbl$param, collapse="|"), ")\\s", sep="")
# bat_files = NULL
# loop on parameter name
for (p.i in 1:nrow(par_inf_tbl)){
	# Create an empty temporary batch file for every 32 parameters
	tmp_bat = tempfile(fileext = ".bat")

	# Grabbing the parameters
	p = par_inf_tbl$param[p.i]
	ext = par_inf_tbl$ext[p.i]
	mn = par_inf_tbl$minVal[p.i]
	mx = par_inf_tbl$maxVal[p.i]
	method = par_inf_tbl$method[p.i]
	# Write a start command on one line of the above batch file, appended
	cmd = paste(
		"start",
		'"',
		p,
		'"',
		'"C:\\Program Files\\R\\R-3.1.2\\bin\\x64\\Rscript.exe"',
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
	writeLines(cmd, tmp_bat)

	go.to.next = F
	while (!go.to.next) {
		ps = grep(pat, system('tasklist /v', intern=TRUE), value=TRUE)
		if (length(ps) <= 32) {
			go.to.next = T
		} else {
			Sys.sleep(1)
			go.to.next = F
		}
	}
	system(tmp_bat)
}
