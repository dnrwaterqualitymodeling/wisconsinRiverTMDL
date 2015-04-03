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
param_vals = read.delim("H:/WRB_sensitivity/ALPHA_BF _param_values.txt")
names(param_vals)[2:length(param_vals)] = paste("iter", seq(1,iter),sep="")
param_vals = melt(param_vals, id.vars="Subbasin")

# basin = readOGR(
	# dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology",
	# layer="wrb_basin")

mod_per = data.frame(DATE = seq(
	as.Date("2002-01-01"),
	as.Date("2013-12-31"),
	by="day"))

# vars = c("streamflow", "Annual Average streamflow (cms)")
nc = open.ncdf("H:/WRB_sensitivity/ALPHA_BF_gw.nc")
strmflw = get.var.ncdf(nc, varid="streamflow", start=c(1,1,1), count=c(-1,-1,-1))

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
		indx = which(param_vals$Subbasin == sbID & param_vals$variable == paste("iter", itr,sep=''))
		param_vals[indx, "nash_sutcliffe"] = nash_sutcliffe_val
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