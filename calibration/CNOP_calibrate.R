library(ncdf)

file_nc = "C:/Users/ruesca/Desktop/WRB_sensitivity_CNOP/CNOP_mgt.nc"
dir_obs = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/calibration/JJAS_25_pct_exc"
file_fig = "C:/Users/ruesca/Desktop/WRB_sensitivity_CNOP/time_series_by_subbasin.pdf"
gage_subbasin_lu =
	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
		colClasses=c("character", "character", "integer", "integer", "character"))
n_i = 50

nc = open.ncdf(file_nc)

nash_sutcliffe = function(Q_m, Q_o) {
	num = sum((Q_o - Q_m)^2)
	den = sum((Q_o - mean(Q_o))^2)
	ns = 1-(num/den)
	return(ns)
}

i_range = seq(-0.5, 0.5, length.out=n_i)

gage_subbasin_lu = subset(gage_subbasin_lu, Keep == 1)
gage_subbasin_lu = gage_subbasin_lu[c("USGS_ID", "WRB_SubbasinID")]

dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")

data_obs_mod = data.frame()
for (gage in gage_subbasin_lu$USGS_ID) {
	file_gage = paste(dir_obs, "/", gage, ".txt", sep="")
	subbasin = gage_subbasin_lu$WRB_SubbasinID[gage_subbasin_lu$USGS_ID == gage]
	data_obs = read.table(file_gage, skip=2, sep="\t", header=T)
	data_obs = data_obs[-1,]
	data_obs = data_obs[data_obs[,5] == "A",]
	data_obs = data.frame(DATE = as.Date(as.character(data_obs[,3])),
		FLOW_OBSERVED=as.numeric(as.character(data_obs[,4])))
	data_obs$FLOW_OBSERVED = data_obs$FLOW_OBSERVED / 35.3147
	data_obs$SUBBASIN = subbasin
	date_ind = which(dates %in% data_obs$DATE)
	
	data_mod = get.var.ncdf(
		nc,
		"streamflow",
		start=c(1,subbasin,1),
		count=c(-1,1,-1))
	data_mod = data_mod[date_ind,]
	
	data_obs_mod = rbind(data_obs_mod, cbind(data_obs, data_mod))
}
close.ncdf(nc)

ns = apply(
	X = data_obs_mod[as.character(1:n_i)],
	MARGIN=2,
	nash_sutcliffe,
	Q_o = data_obs_mod$FLOW_OBSERVED
)

