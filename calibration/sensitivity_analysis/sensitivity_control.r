setInternet2(TRUE)

# Read parameter table with parameter name, file extension, min, max, method
par_inf_tbl = read.csv("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/calibration/sensitivity_analysis/basin_sensitivity_parameters.csv")

# unchanging parameters
txtinout = "D:/WRB/etc"
dir_out = "D:/WRB_sensitivity" 
iter = 25


# loop on parameter name
for (p in 1:nrow(par_inf_tbl)){
	
	if ((p %% 32) == 1){
		tmp_bat = tempfile(p, fileext = ".bat")
	}
	p = par_inf_tbl$param[p]
	ext = par_inf_tbl$ext[p]
	mn = par_inf_tbl$minVal[p]
	mx = par_inf_tbl$maxVal[p]
	method = par_inf_tbl$method[p]
	
	l1 = "start C:\\R\\R-3.0.1\\bin\\Rscript.exe bsn_sub_hru_gw.r"
	
	cmd = paste(
		l1,
		txtinout,
		dir_out,
		p,
		ext,
		mn,
		mx,
		method,
		iter,
		sep = " ")
	# Create an empty temporary batch file for every 32 parameters
	# Write a start command on one line of the above batch file, appended
	
}


# loop through the number of above batch files
	# system function on batch file, wait=T

	
# txtinout = arguments[1]
# dir_out = arguments[2]
# p = arguments[3]
# ext = arguments[4]
# mn = as.numeric(arguments[5])
# mx = as.numeric(arguments[6])
# method = arguments[7]
# iter = as.integer(arguments[8])