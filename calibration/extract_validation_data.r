library(stringr)
options(stringsAsFactors=T)

usgs_raw_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw"
gauge_basin_lu_file = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"

gauge_basin_lu = read.csv(gauge_basin_lu_file,
	colClasses=c("character", "character", "integer", "integer", "character"))
gauge_basin_lu = subset(gauge_basin_lu, Keep == 1)

obs_files = list.files(usgs_raw_dir, pattern="^0[0-9]+\\.txt$", full.names=T)
gauge_ids = str_extract(basename(obs_files), "^0[0-9]+")
obs_files = obs_files[gauge_ids %in% gauge_basin_lu$USGS_ID]

model_period = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
n_days_in_mo = table(paste(format(model_period, "%Y"), format(model_period, "%m")))
n_days_in_mo = data.frame(
	DATE=as.Date(names(n_days_in_mo), format="%Y %m"),
	N=n_days_in_mo)

for (obs_file in obs_files) {
	obsData = read.table(obs_file, skip=2, sep="\t", header=T)
	obsData = obsData[-1,]
    obsData = obsData[obsData[,5] == "A",]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW_OBSERVED=as.numeric(as.character(obsData[,4])))\
	n_obs_in_mo = table(paste(format(obsData$DATE, "%Y"), format(obsData$DATE, "%m")))
	
	names(n_obs_in_mo) == names(n_days_in_mo) & n_obs_in_mo == n_days_in_mo

