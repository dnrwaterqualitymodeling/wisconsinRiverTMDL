library(ncdf)
library(rgdal)
library(hydroGOF)
library(reshape)
library(ggplot2)
options(stringsAsFactors=T)
source("~/Code/calibration/format_usgs_flow_function.R")
iter = 7
dir_raw_usgs = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/calibration"
gauge_basin_lu = read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv")
wd = "H:/wrb_calibration_7iters"

# basin = readOGR(
	# dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology",
	# layer="wrb_basin")

mod_per = data.frame(DATE = seq(
	as.Date("2002-01-01"),
	as.Date("2013-12-31"),
	by="day"))

nc_files = list.files(wd, pattern="*.nc")
prm_files = list.files(wd, pattern="*_values.txt")
for (nc_file in nc_files){
	
	nc_file = nc_files[7]
	p = substr(nc_file,1, nchar(nc_file)-7)
	
	nc = open.ncdf(paste(wd, nc_file, sep="/"))
	strmflw = get.var.ncdf(nc, varid="streamflow", start=c(1,1,1), count=c(-1,-1,-1))
	
	param_vals = read.delim(
		paste(wd, 
			paste(p, "_param_values.txt",sep=''),
			sep="/")
	)
	
	param_vals = melt(param_vals, id.vars="Subbasin")
	if (nrow(param_vals) < 20){
		add_rows = TRUE
		all_basins = data.frame()
	} else {
		add_rows = FALSE
	}
	param_vals$nash_sutcliffe = NA
	for (gage in 1:nrow(gauge_basin_lu)){
		
		if (gauge_basin_lu[gage, "Keep"] == 0){next}
		sttion = paste("0", gauge_basin_lu[gage, "USGS_ID"],sep='')
		print(sttion)
		rawDataFile = paste(dir_raw_usgs, "/", sttion, ".txt" ,sep='')
		#### Throws a couple warnings, these are the (attempted) 
		#### conversion of characters to numeric
		dat = format_usgs(rawDataFile)
		# dat$station = sttion
		flow_name = gauge_basin_lu[gage, "Flow_Name"]
		sbID = gauge_basin_lu[gage,"WRB_SubbasinID"]
		dat = merge(dat, mod_per, all.y = T)
		
		for (itr in 1:iter){
			sim.dat = strmflw[,sbID,itr]
			nash_sutcliffe_val = pbias(sim.dat,dat$FLOW_OBSERVED,na.rm=T)
			if (add_rows){
				
			} else {
				indx = which(param_vals$Subbasin == sbID & param_vals$variable == paste("iter", itr,sep=''))
				param_vals[indx, "nash_sutcliffe"] = nash_sutcliffe_val
			}
		}
	}

gplt = ggplot(param_vals, aes(x=value, y=nash_sutcliffe, color=factor(Subbasin)))
gplt + geom_line() + facet_grid(.~Subbasin)
	# pts = gauge_basin_lu[gage, c("wtm_x", "wtm_y")]
	# coordinates(pts) <- ~wtm_x + wtm_y
	# par(fig=c(0,1,0,1))
	# plot(FLOW_OBSERVED ~ DATE, 
		# data = dat,
		# type='l',
		# main=gauge_basin_lu[gage, "Flow_Name"],
		# sub=paste(
			# "USGS ID:", gauge_basin_lu[gage, "USGS_ID"],
			# "\n",
			# "Subbasin:",gauge_basin_lu[gage, "WRB_SubbasinID"]),
		# xlab="",
		# ylab="cfs")

	# abline(v=yrs, col="#BEBEBE80")
	# par(fig=c(0,0.5,0.5,1), new=TRUE)

	# plot(basin)
	# plot(pts, add=T,col='red')