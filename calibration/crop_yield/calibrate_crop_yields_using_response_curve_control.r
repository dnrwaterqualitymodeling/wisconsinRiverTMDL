txtinout = "D:/TxtInOut"
dir_out = "D:/bio_e_calibration"
td = "Y:/"
r = '"C:\\Program Files\\R\\R-3.1.2\\bin\\x64\\Rscript.exe"'
cal_script = "D:/wisconsinRiverTMDL/calibration/crop_yield/calibrate_crop_yields_using_response_curve.r"
is_bio_e = FALSE

for (crop in c("CORN", "SOYB", "ALFA")) {
	# for (bio_e in seq(10,90,15)) {
	for (bio_e in seq(0.1,1,0.1)) {
		bat = tempfile(fileext=".bat")
		cmd = paste(
			'START "',
			crop, "_", gsub(".", "", bio_e, fixed=T),
			'" ', r, " ",
			cal_script, " ",
			txtinout, " ",
			dir_out, " ",
			td, " ",
			crop, " ",
			bio_e,
			is_bio_e,
			sep=""
		)
		writeLines(cmd, bat)
		print(cmd)
		stdout = system(bat, wait=F)
	}
}

running = T
if (is_bio_e)  {
	pat = "(CORN|SOYB|ALFA)_[1-9]+"
} else {
	pat = "(CORN|SOYB|ALFA)_[0]?[1-9]+"
}
while (running == T) {
	ps = grep(pat, system('tasklist /v',intern=TRUE),value=TRUE)
	if (length(ps) > 0) {
		Sys.sleep(1)
	} else {
		running = F
	}
}

all_d = NULL
for (f in list.files(dir_out, pattern=pat, full.names=T)) {
	d = read.csv(f)
	all_d = rbind(all_d, d)
}
if (is_bio_e) {
	outName = "bio_e_calibration.csv"
} else {
	outName = "blai_calibration.csv"
}
write.csv(all_d, paste(dir_out, outName, sep="/"), row.names=F)

unlink(list.files(dir_out, pattern=pat, full.names=T))

# source("C:/Users/evansdm/Documents/Code/calibration/crop_yield/calibrate_crop_yields_using_response_curve_plotting.r")