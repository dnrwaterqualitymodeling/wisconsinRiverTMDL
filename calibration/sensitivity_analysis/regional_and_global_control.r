
setwd("D:/WRB_sensitivity")
nc_files = list.files(pattern="\\.nc$")
script_sensitivity = "D:/wisconsinRiverTMDL/calibration/sensitivity_analysis/regional_and_global_sensitivity_analysis.r"
bat_files = NULL
# loop on parameter name
processors = 0
for (nc_file in nc_files) {
	processors = processors + 1
	# Create an empty temporary batch file for every 32 parameters
	if ((processors %% 32) == 1){
		tmp_bat = tempfile(fileext = ".bat")
		bat_lines = NULL
		bat_files = c(bat_files, tmp_bat)
	}
	cmd = paste(
		"start",
		'"',
		sub("\\.nc$", "", basename(nc_file), "\\."),
		'"',
		'"C:\\Program Files\\R\\R-3.1.2\\bin\\x64\\Rscript.exe"',
		script_sensitivity,
		nc_file,
		sep = " ")
	bat_lines = rbind(bat_lines, cmd)
	if ((processors %% 32) == 0 | processors == length(nc_files)) {
		writeLines(bat_lines, tmp_bat)
	}
}
for (bat_file in bat_files) {
	print(bat_file)
	system(bat_file)
}

