script_sensitivity = "D:/wisconsinRiverTMDL/calibration/sensitivity_analysis/regional_and_global_sensitivity_analysis.r"
dir_nc = "D:/WRB_sensitivity_analysis"
dir_out = paste(dir_nc, "output_data", sep='/')

if (!file.exists(dir_out)){
	dir.create(paste0(dir_out, "/"))
}

nc_files = list.files(dir_nc, pattern="\\.nc$", full.names=T)

pat = paste("(", paste(basename(nc_files), collapse="|"), ")", sep="")
pat = gsub("\\.", "\\\\.", pat)

# loop on parameter name
# processors = 0
for (nc_file in nc_files) {
	# processors = processors + 1
	# Create an empty temporary batch file for every 32 parameters
	tmp_bat = tempfile(fileext = ".bat")
	cmd = paste(
		"start",
		'"',
		basename(nc_file),
		'"',
		'"C:\\Program Files\\R\\R-3.1.2\\bin\\x64\\Rscript.exe"',
		script_sensitivity,
		nc_file,
		dir_out
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

clean.up = F
while (!clean.up) {
	ps = grep(pat, system('tasklist /v', intern=TRUE), value=TRUE)
	if (length(ps) < 1) {
		clean.up = T
	} else {
		Sys.sleep(1)
		clean.up = F
	}
}
output_files = list.files(dir_out, "regional*", full.names=T)
for (dat in c("rch", "sub")){
	out_file_name = paste0(out_dir, "/",dat, "_regional_output.txt")
	out_df = data.frame()
	for (fl in output_files[grepl(dat, output_files)]){
		reg_dat = read.delim(fl)
		out_df = rbind(out_df, reg_dat)
	}
	write.table(out_df, out_file_name, sep='\t',row.names=F)
}
