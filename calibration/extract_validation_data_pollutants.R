## SEDIMENT

dir_load = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads/SS_00530/Loads"
dir_out = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_loads/SS"
val_dir = paste(dir_out, "/validation", sep="")
cal_dir = paste(dir_out, "/calibration", sep="")
gage_subbasin_lu =
	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
	colClasses=c(rep("character", 4), "integer", "integer", "character"))
start_end_lu = read.table("T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/ss_start_end_dates.txt",
		sep="\t", header=T)
fraction_for_validation = 1/4
full_yr_threshold = 13 # half of bi-weekly samples

set.seed(2558698)
	
gage_subbasin_lu = subset(gage_subbasin_lu, LOAD_ID != "" & Keep == 1)
start_end_lu$start = as.Date(start_end_lu$start, format="%m/%d/%Y")
start_end_lu$end = as.Date(start_end_lu$end, format="%m/%d/%Y")

summ_table = NULL
for (id in gage_subbasin_lu$LOAD_ID) {
	file_site = paste(dir_load, "/", id, ".gz", sep="")
	data_site = read.table(file_site, sep="\t", header=T)
	data_site$date = as.Date(data_site$date, format="%m/%d/%Y")
	start_year = format(subset(start_end_lu, station_id == id)$start, "%Y")
	end_year = format(subset(start_end_lu, station_id == id)$end, "%Y")
	data_site = subset(
		data_site,
		date > as.Date(paste(start_year, "-1-1", sep="")) &
			date < as.Date(paste(end_year, "-12-31", sep=""))
	)
	data_site_mo = aggregate(
		dload_00530 ~ format(date, "%Y") + format(date, "%m"),
		data=data_site,
		FUN=sum,
		na.rm=T
	)
	names(data_site_mo) = c("YEAR", "MO", "SS_KG")
	data_site_mo = data_site_mo[order(data_site_mo$YEAR, data_site_mo$MO),]
	file_site_all = paste(dir_out, "/", id, ".txt", sep="")
	write.table(data_site_mo, file_site_all, row.names=F, sep="\t", quote=F)

	n_obs_in_yr = aggregate(
		actual_00530 ~ format(date, "%Y"),
		data=data_site,
		FUN = function(X) {
			sum(!is.na(X))
		}
	)
	names(n_obs_in_yr) = c("YEAR", "N_OBS")
	full_yrs = subset(n_obs_in_yr, N_OBS >= full_yr_threshold)
	n_samples = floor(nrow(full_yrs) * fraction_for_validation)
	val_yrs = with(full_yrs, sample(YEAR, n_samples))
	n_obs_in_yr = cbind(
		data.frame(station_id=id),
		n_obs_in_yr)
	n_obs_in_yr$VALIDATION[n_obs_in_yr$YEAR %in% val_yrs] = 1
	n_obs_in_yr$VALIDATION[is.na(n_obs_in_yr$VALIDATION)] = 0
	summ_table = rbind(summ_table, n_obs_in_yr)
	
	for (val_bool in c(0,1)) {
		sub_yrs = subset(n_obs_in_yr, VALIDATION == val_bool)$YEAR
		sub_bool = with(data_site_mo,
			YEAR %in% sub_yrs
		)
		sub_data = subset(data_site_mo, sub_bool)
		if (val_bool == 0) {dir_sub = cal_dir} else {dir_sub = val_dir}
		file_out = paste(dir_sub, "/", id, ".txt", sep="")
		write.table(sub_data, file_out, sep="\t", row.names=F, quote=F)
	}
}
file_summ_table = paste(dir_out, "/summary_table.txt", sep="")
write.table(summ_table, file_summ_table, sep="\t", row.names=F, quote=F)

## PHOSPHORUS

dir_load = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads/TP_00665/Loads"
dir_out = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_loads/TP"
val_dir = paste(dir_out, "/validation", sep="")
cal_dir = paste(dir_out, "/calibration", sep="")
gage_subbasin_lu =
	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
		colClasses=c(rep("character", 4), "integer", "integer", "character"))
start_end_lu = read.table("T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/tp_start_end_dates.txt",
	sep="\t", header=T)
fraction_for_validation = 1/4
full_yr_threshold = 13 # half of bi-weekly samples

set.seed(2558698)

gage_subbasin_lu = subset(gage_subbasin_lu, LOAD_ID != "" & Keep == 1)
start_end_lu$start = as.Date(start_end_lu$start, format="%m/%d/%Y")
start_end_lu$end = as.Date(start_end_lu$end, format="%m/%d/%Y")

summ_table = NULL
for (id in gage_subbasin_lu$LOAD_ID) {
	file_site = paste(dir_load, "/", id, ".gz", sep="")
	data_site = read.table(file_site, sep="\t", header=T)
	data_site$date = as.Date(data_site$date, format="%m/%d/%Y")
	start_year = format(subset(start_end_lu, station_id == id)$start, "%Y")
	end_year = format(subset(start_end_lu, station_id == id)$end, "%Y")
	data_site = subset(
		data_site,
		date > as.Date(paste(start_year, "-1-1", sep="")) &
			date < as.Date(paste(end_year, "-12-31", sep=""))
	)
	data_site_mo = aggregate(
		dload_00665 ~ format(date, "%Y") + format(date, "%m"),
		data=data_site,
		FUN=sum,
		na.rm=T
	)
	names(data_site_mo) = c("YEAR", "MO", "TP_KG")
	data_site_mo = data_site_mo[order(data_site_mo$YEAR, data_site_mo$MO),]
	file_site_all = paste(dir_out, "/", id, ".txt", sep="")
	write.table(data_site_mo, file_site_all, row.names=F, sep="\t", quote=F)
	
	n_obs_in_yr = aggregate(
		actual_00665 ~ format(date, "%Y"),
		data=data_site,
		FUN = function(X) {
			sum(!is.na(X))
		}
	)
	names(n_obs_in_yr) = c("YEAR", "N_OBS")
	full_yrs = subset(n_obs_in_yr, N_OBS >= full_yr_threshold)
	n_samples = floor(nrow(full_yrs) * fraction_for_validation)
	val_yrs = with(full_yrs, sample(YEAR, n_samples))
	n_obs_in_yr = cbind(
		data.frame(station_id=id),
		n_obs_in_yr)
	n_obs_in_yr$VALIDATION[n_obs_in_yr$YEAR %in% val_yrs] = 1
	n_obs_in_yr$VALIDATION[is.na(n_obs_in_yr$VALIDATION)] = 0
	summ_table = rbind(summ_table, n_obs_in_yr)
	
	for (val_bool in c(0,1)) {
		sub_yrs = subset(n_obs_in_yr, VALIDATION == val_bool)$YEAR
		sub_bool = with(data_site_mo,
			YEAR %in% sub_yrs
		)
		sub_data = subset(data_site_mo, sub_bool)
		if (val_bool == 0) {dir_sub = cal_dir} else {dir_sub = val_dir}
		file_out = paste(dir_sub, "/", id, ".txt", sep="")
		write.table(sub_data, file_out, sep="\t", row.names=F, quote=F)
	}
}
file_summ_table = paste(dir_out, "/summary_table.txt", sep="")
write.table(summ_table, file_summ_table, sep="\t", row.names=F, quote=F)


