txtinout = "D:/TxtInOut"
dir_out = "D:/bio_e_calibration"
td = "Y:\\"
r = '"C:\Program Files\R\R-3.1.2\bin\x64\Rscript.exe"'
cal_script = "D:/wisconsinRiverTMDL/calibration/crop_yield/calibrate_crop_yields_using_response_curve.r"

for (crop in c("CORN", "SOYB", "ALFA")) {
	for (bio_e in seq(10,90,15)) {
		bat = tempfile(fileext=".bat")
		cmd = paste(
			'START "',
			crop, "_", bio_e,
			'" ', r, " ",
			cal_script, " ",
			txtinout, " ",
			dir_out, " ",
			td, " ",
			crop, " ",
			bio_e,
			sep=""
		)
		writeLines(cmd, bat)
		print(cmd)
		stdout = system(bat, wait=F)
	}
}

running = T
while (running == T) {
	ps = grep("(CORN|SOYB|ALFA)_[1-9]+",readLines(textConnection(system('tasklist /v',intern=TRUE))),value=TRUE)
	if (length(ps) > 0) {
		Sys.sleep(1)
	} else {
		running = F
	}
}

all_d = NULL
for (f in list.files(dir_out, full.names=T)) {
	d = read.csv(f)
	all_d = rbind(all_d, d)
}
write.csv(all_d, paste(dir_out, "bio_e_calibration.csv", sep="/"), row.names=F)

unlink(list.files(dir_out, pattern="^(CORN|SOYB|ALFA)_[1-9]+", full.names=T))

# source("C:/Users/evansdm/Documents/Code/calibration/crop_yield/calibrate_crop_yields_using_response_curve_plotting.r")