setInternet2(TRUE)
options(stringsAsFactors=F)
# Read parameter table with parameter name, file extension, min, max, method
par_inf_tbl = read.csv("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/calibration/sensitivity_analysis/basin_sensitivity_parameters.csv")

# unchanging parameters
txtinout = "D:/TxtInOut"
dir_out = "D:/WRB_sensitivity"
script_sensitivity = "D:/wisconsinRiverTMDL/calibration/sensitivity_analysis/bsn_sub_hru_gw.r"
iter = 25

bat_files = NULL
# loop on parameter name
for (p.i in 1:nrow(par_inf_tbl)){
	# Create an empty temporary batch file for every 32 parameters
	if ((p.i %% 32) == 1){
		tmp_bat = tempfile(fileext = ".bat")
		bat_lines = NULL
		bat_files = c(bat_files, tmp_bat)
	}
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
		p,
		ext,
		mn,
		mx,
		method,
		iter,
		sep = " ")
	bat_lines = rbind(bat_lines, cmd)
	if ((p.i %% 32) == 0 | p.i == nrow(par_inf_tbl)) {
		writeLines(bat_lines, tmp_bat)
	}
}
for (bat_file in bat_files) {
	system(bat_file)
}
