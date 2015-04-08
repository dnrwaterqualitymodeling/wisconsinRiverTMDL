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
all_param_output = data.frame()
pdf("another_look_at_sensitivity.pdf")
for (nc_file in nc_files){
	
	if (grepl("gw", nc_file)){
		sbtrct = 6
	} else {
		sbtrct = 7
	}
	p = substr(nc_file,1, nchar(nc_file)-sbtrct)
	print(paste("Beginning", p))
	nc = open.ncdf(paste(wd, nc_file, sep="/"))
	strmflw = get.var.ncdf(nc, varid="streamflow", start=c(1,1,1), count=c(-1,-1,-1))
	### error handling for those parameters that don't have parameter vals
	err = try({param_vals = read.delim(
			paste(wd, 
				paste(p, "_param_values.txt",sep=''),
				sep="/"))}, silent=T)
	if (class(err) == "try-error"){
		print(paste("No parameter values exist for ", p))
		next
	}
	
	param_vals = melt(param_vals, id.vars="Subbasin")
	param_vals$nash_sutcliffe = NA
	param_vals$pbias = NA
	param_vals$param = gsub(".nc", "", nc_file)
	if (nrow(param_vals) < 10){
		absolute = TRUE
		perf_df = param_vals
		param_dat = data.frame()
	} else {
		absolute = FALSE
	}
	for (gage in 1:nrow(gauge_basin_lu)){
		if (gauge_basin_lu[gage, "Keep"] == 0){next}
		sttion = paste("0", gauge_basin_lu[gage, "USGS_ID"],sep='')
		print(paste("    ",sttion))
		rawDataFile = paste(dir_raw_usgs, "/", sttion, ".txt" ,sep='')
		#### Throws a couple warnings, these are the (attempted) 
		#### conversion of characters to numeric
		dat = format_usgs(rawDataFile)
		# dat$station = sttion
		flow_name = gauge_basin_lu[gage, "Flow_Name"]
		sbID = gauge_basin_lu[gage,"WRB_SubbasinID"]
		dat = merge(dat, mod_per, all.y = T)
		dat$FLOW_OBSERVED = dat$FLOW_OBSERVED * 35.336
		if (absolute){
			perf_df$Subbasin = sbID
		}
		for (itr in 1:iter){
			sim.dat = strmflw[,sbID,itr]
			pbias_val = pbias(sim.dat, dat$FLOW_OBSERVED, na.rm=T)
			nash_sutcliffe_val = NSE(sim.dat, dat$FLOW_OBSERVED, na.rm=T)
			if (absolute){
				indx = which(param_vals$variable == paste("iter", itr,sep=''))
				perf_df[indx, "nash_sutcliffe"] = nash_sutcliffe_val
				perf_df[indx, "pbias"] = pbias_val
			} else {
				indx = which(param_vals$Subbasin == sbID & param_vals$variable == paste("iter", itr,sep=''))
				param_vals[indx, "nash_sutcliffe"] = nash_sutcliffe_val
				param_vals[indx, "pbias"] = pbias_val
			}
		}
		if (absolute){
			param_dat = rbind(param_dat, perf_df)
		} else {
			param_dat = param_vals
		}
		
	}
	xmn = min(param_dat$value)
	xmx = max(param_dat$value)
	ymn = min(param_dat$nash_sutcliffe)
	ymx = max(param_dat$nash_sutcliffe)
	for (sb in 1:length(unique(param_dat$Subbasin))){
		sbst = subset(param_dat, Subbasin==unique(param_dat$Subbasin)[sb])
		if (sb == 1){
			plot(nash_sutcliffe ~ value,
				data=sbst,type='l',
				ylim=c(ymn, ymx),
				xlim=c(xmn, xmx))
				}
	# gplt = ggplot(param_dat, aes(x=value, y=nash_sutcliffe, color=factor(Subbasin)))
	# gplt = gplt + geom_line() + facet_grid(.~Subbasin) + ggtitle(p)
	# plot(gplt)
	all_param_output = rbind(all_param_output, param_dat)
}
dev.off()



#### map best iteration for each gauge station
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