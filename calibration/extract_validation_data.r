library(stringr)
options(stringsAsFactors=T)

usgs_raw_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw"
val_dir = paste(usgs_raw_dir, "/validation", sep="")
cal_dir = paste(usgs_raw_dir, "/calibration", sep="")
gauge_basin_lu_file = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"
fraction_for_validation = 1/3
summ_table_file = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/validation_subset_summary.txt"

gauge_basin_lu = read.csv(gauge_basin_lu_file,
	colClasses=c("character", "character", "integer", "integer", "character"))
gauge_basin_lu = subset(gauge_basin_lu, Keep == 1)

obs_files = list.files(usgs_raw_dir, pattern="^0[0-9]+\\.txt$", full.names=T)
gauge_ids = str_extract(basename(obs_files), "^0[0-9]+")
obs_files = obs_files[gauge_ids %in% gauge_basin_lu$USGS_ID]

model_period = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
# Create a table with the number of days in each month for the model period
n_days_in_mo = as.data.frame(table(
	paste(
		format(model_period, "%Y"),
		format(model_period, "%m"),
	sep="-"
	)
))
names(n_days_in_mo) = c("MON", "N_DAY")
n_days_in_mo$MON = as.character(n_days_in_mo$MON)

summ_table = NULL
for (obs_file in obs_files) {
	obsData_raw = read.table(obs_file, skip=2, sep="\t", header=T)
	obsData_raw = obsData_raw[-1,]
    obsData = obsData_raw[obsData_raw[,5] == "A",]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW=as.numeric(as.character(obsData[,4])))
	# Create a table with the number of observations in each month
	n_obs_in_mo = as.data.frame(table(
		paste(
			format(obsData$DATE, "%Y"),
			format(obsData$DATE, "%m"),
			sep="-"
		)
	))
	names(n_obs_in_mo) = c("MON", "N_OBS")
	n_obs_in_mo$MON = as.character(n_obs_in_mo$MON) 
	full_mos = subset(
		merge(n_obs_in_mo, n_days_in_mo),
		N_OBS == N_DAY)
	obs_full_mos = subset(
		obsData,
		paste(format(DATE, "%Y"), format(DATE, "%m"), sep="-") %in% full_mos$MON)
	obs_full_mos$MON = paste(
		format(obs_full_mos$DATE, "%Y"),
		format(obs_full_mos$DATE, "%m"),
		sep="-")
	mean_flow = aggregate(FLOW ~ MON, data=obs_full_mos, FUN=mean)
	mean_flow$BINS = with(mean_flow,
		cut(
			FLOW,
			quantile(FLOW, c(0,0.25,0.75,1)),
			include.lowest=T
		)
	)
	mean_flow$ORDINAL[as.numeric(mean_flow$BINS) == 1] = "low"
	mean_flow$ORDINAL[as.numeric(mean_flow$BINS) == 2] = "medium"
	mean_flow$ORDINAL[as.numeric(mean_flow$BINS) == 3] = "high"
	summ_rows = NULL
	for (bin in c("low", "medium", "high")) {
		sample_subset = subset(mean_flow, ORDINAL == bin)
		n_samples = floor(nrow(sample_subset) * fraction_for_validation)
		val_bin = with(sample_subset, sample(MON, n_samples))
		sample_subset$VALIDATION[sample_subset$MON %in% val_bin] = 1
		sample_subset$VALIDATION[is.na(sample_subset$VALIDATION)] = 0		
		summ_rows = rbind(summ_rows, sample_subset)	
	}
	summ_rows = cbind(
		data.frame(USGS_ID=str_extract(basename(obs_file), "^0[0-9]+")),
		summ_rows)
	summ_table = rbind(summ_table, summ_rows)
	
	# Seperate files into calibration and validation datasets
	for (val_bool in c(0,1)) {
		sub_mons = summ_rows$MON[summ_rows$VALIDATION == val_bool]
		sub_bool = with(obsData_raw,
			paste(format(as.Date(datetime), "%Y"), format(as.Date(datetime), "%m"), sep="-") %in% sub_mons
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
		

