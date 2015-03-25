library(stringr)
options(stringsAsFactors=T)

cal_dir = 
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/calibration"
out_dir = paste(cal_dir, "spring_10_pct_exc", sep="/")
subset_dir = paste(cal_dir, "/10_pct_exceedance_mam", sep="")
gauge_basin_lu_file =
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"

gauge_basin_lu = read.csv(gauge_basin_lu_file,
	colClasses=c("character", "character", "integer", "integer", "character"))
gauge_basin_lu = subset(gauge_basin_lu, Keep == 1)

obs_files = list.files(cal_dir, pattern="^0[0-9]+\\.txt$", full.names=T)
gauge_ids = str_extract(basename(obs_files), "^0[0-9]+")
obs_files = obs_files[gauge_ids %in% gauge_basin_lu$USGS_ID]

model_period = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")

for (obs_file in obs_files) {
	obsData_raw = read.table(obs_file, skip=2, sep="\t", header=T)
	obsData_raw = obsData_raw[-1,]
	obsData_raw[obsData_raw[,4] == "Ice", 4] = NA
    obsData = obsData_raw[obsData_raw[,5] == "A",]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW=as.numeric(as.character(obsData[,4])))
	# construct boolean for subsetting
	mam_days = model_period[
			as.integer(format(model_period, "%m")) %in% 3:5
	]
	ten_pct_exc = quantile(obsData$FLOW, 0.9)
	bool = as.Date(obsData_raw$datetime) %in% mam_days &
		as.numeric(as.character(obsData_raw[,4])) >= ten_pct_exc
	#
	sub_data = subset(obsData_raw, bool)
	out_file = paste(out_dir, basename(obs_file), sep="/")
	raw_txt = readLines(obs_file)
	header_lines = grep("#", raw_txt)
	header_lines = 1:(max(header_lines) + 2)
	header = raw_txt[header_lines]
	writeLines(header, out_file)
	write.table(sub_data,
		out_file,
		sep="\t",
		append=T,
		row.names=F,
		col.names=F,
		quote=F)
}
		

