library(stringr)
options(stringsAsFactors=T)

usgs_raw_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw"
val_dir = paste(usgs_raw_dir, "/validation", sep="")
cal_dir = paste(usgs_raw_dir, "/calibration", sep="")
gauge_basin_lu_file = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"
fraction_for_validation = 1/4
full_yr_threshold = 1/2
summ_table_file =
		"T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/validation_subset_summary_random_quarter_of_years.txt"

set.seed(6546494)

gauge_basin_lu = read.csv(gauge_basin_lu_file,
	colClasses=c(rep("character", 5), "integer", "integer", "character"))
#gauge_basin_lu = subset(gauge_basin_lu, Keep == 1)

obs_files = list.files(usgs_raw_dir, pattern="^0[0-9]+\\.txt$", full.names=T)
gauge_ids = str_extract(basename(obs_files), "^0[0-9]+")
obs_files = obs_files[gauge_ids %in% gauge_basin_lu$USGS_ID]

model_period = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
# Create a table with the number of days in each month for the model period
n_days_in_yr = as.data.frame(table(format(model_period, "%Y")))
names(n_days_in_yr) = c("YR", "N_DAY")
n_days_in_yr$YR = as.character(n_days_in_yr$YR)

summ_table = NULL
for (obs_file in obs_files) {
	obsData_raw = read.table(obs_file, skip=2, sep="\t", header=T)
	obsData_raw = obsData_raw[-1,]
    obsData = obsData_raw[obsData_raw[,5] == "A",]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW=as.numeric(as.character(obsData[,4])))
	# Create a table with the number of observations in each month
	n_obs_in_yr = as.data.frame(table(format(obsData$DATE, "%Y")))
	names(n_obs_in_yr) = c("YR", "N_OBS")
	n_obs_in_yr$YR = as.character(n_obs_in_yr$YR) 
	full_yrs = subset(
		merge(n_obs_in_yr, n_days_in_yr),
		N_OBS >= N_DAY * full_yr_threshold) # Only use years with 2/3 of days observing flow
	# if there are less than 4 years with 2/3 of days observing flow,
	# 	do not subset.
	obs_full_yrs = subset(
		obsData,
		as.character(format(DATE, "%Y")) %in% full_yrs$YR)
	n_samples = floor(nrow(full_yrs) * fraction_for_validation)
	val_yrs = with(full_yrs, sample(YR, n_samples))

	n_obs_in_yr = cbind(
		data.frame(USGS_ID=str_extract(basename(obs_file), "^0[0-9]+")),
		n_obs_in_yr)
	n_obs_in_yr$VALIDATION[n_obs_in_yr$YR %in% val_yrs] = 1
	n_obs_in_yr$VALIDATION[is.na(n_obs_in_yr$VALIDATION)] = 0	
	summ_table = rbind(summ_table, n_obs_in_yr)
	
	
	# Seperate files into calibration and validation datasets
	for (val_bool in c(0,1)) {
		sub_yrs = full_yrs$YR[n_obs_in_yr$VALIDATION == val_bool]
		sub_bool = with(obsData_raw,
			format(as.Date(datetime), "%Y") %in% sub_yrs
		)
		sub_data = subset(obsData_raw, sub_bool)
		if (val_bool == 0) {out_dir = cal_dir} else {out_dir = val_dir}
		out_file = paste(out_dir, basename(obs_file), sep="/")
		raw_txt = readLines(obs_file)
		header_lines = grep("#", raw_txt)
		header_lines = 1:(max(header_lines) + 2)
		header = raw_txt[header_lines]
		writeLines(header, out_file)
		write.table(sub_data, out_file, sep="\t", append=T, row.names=F, col.names=F, quote=F)
	}
}
write.table(summ_table, summ_table_file, row.names=F, sep="\t", quote=F)
		


