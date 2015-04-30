library(rgdal)
library(rgeos)

source("C:/Users/evansdm/Documents/Code/calibration/format_usgs_flow_function.R")
options(stringsAsFactors=F, warn = 1)

dir_raw_usgs = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw"
gauge_basin_lu = read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv")

basin = readOGR(
	dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology",
	layer="wrb_basin")

mod_per = data.frame(DATE = seq(
	as.Date("2002-01-01"),
	as.Date("2012-12-31"),
	by="day"))

yrs =  as.Date(paste(seq(2002, 2012, by=1), "01-01",sep="-"))

pdf(paste("flow_monitoring_stations.pdf",sep=''),
		height=8,
		width=11.5)
for (gage in 1:nrow(gauge_basin_lu)){

	if (gauge_basin_lu[gage, "Keep"] == 0){next}
	sttion = paste("0", gauge_basin_lu[gage, "USGS_ID"],sep='')
	print(sttion)
	rawDataFile = paste(dir_raw_usgs, "/", sttion, ".txt" ,sep='')
	#### Throws a couple warnings, these are the (attempted) 
	#### conversion of characters to numeric
	dat = format_usgs(rawDataFile)
	dat$station = sttion
	dat$flow_name = gauge_basin_lu[gage, "Flow_Name"]
	
	pts = gauge_basin_lu[gage, c("wtm_x", "wtm_y")]
	coordinates(pts) <- ~wtm_x + wtm_y
	
	dat = merge(dat, mod_per, all.y = T)
	par(fig=c(0,1,0,1))
	plot(FLOW_OBSERVED ~ DATE, 
		data = dat,
		type='l',
		main=gauge_basin_lu[gage, "Flow_Name"],
		sub=paste(
			"USGS ID:", gauge_basin_lu[gage, "USGS_ID"],
			"\n",
			"Subbasin:",gauge_basin_lu[gage, "WRB_SubbasinID"]),
		xlab="",
		ylab="cfs")

	abline(v=yrs, col="#BEBEBE80")
	par(fig=c(0,0.5,0.5,1), new=TRUE)
	# layout(matrix(c(1,2,3,4), 2,2,byrow=T))
	plot(basin)
	plot(pts, add=T,col='red')
}
dev.off()

