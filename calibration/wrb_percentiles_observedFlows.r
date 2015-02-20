##first step - determine station/year average flows for all flow stations 
##second step - determine percentile annual flows for each station/year
##third step - each year/station combination as "dry", "average", or "wet"
library(reshape)
source("C:/Users/evansdm/Documents/Code/calibration/format_usgs_flow_function.R")
options(stringsAsFactors=F, warn = 1)


##set working directory/files
# drv = "Y:/"
# setwd(paste(drv,"UFWB/SWAT_calibration/"))
# rawDataFile = 'flow_data.csv'
# df = read.csv(rawDataFile, stringsAsFactors=FALSE, header=TRUE)

dir_gis = "T:/Projects/Wisconsin_River/GIS_Datasets/observed"
dir_raw_usgs = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw"
gauge_basin_lu = read.csv(paste(dir_gis, "gauge_basin_lookup.csv", sep = "/"))

df = data.frame()
for (gage in 1:nrow(gauge_basin_lu)){
	if (gauge_basin_lu[gage, "Keep"] == 0){next}
	sttion = paste("0", gauge_basin_lu[gage, "USGS_ID"],sep='')
	print(sttion)
	rawDataFile = paste(dir_raw_usgs, "/", sttion, ".txt" ,sep='')
	#### Throws a couple warnings, these are the (attempted) 
	#### conversion of characters to numeric
	dat = format_usgs(rawDataFile)
	dat$station = sttion
	
	df = rbind(df, dat)
}


##split date into month, day, & year columns
# mdy <- colsplit(as.character(df$DATE), "-", c("month", "day", "year"))  
# sta <- cbind(df, mdy)
df$YEAR = format(df$DATE, "%Y")
##STEP 1
##summarize mean daily flows for each station/year combination
# sta$val <- as.numeric(sta$val)

# yr_mean <- aggregate(FLOW_OBSERVED ~ YEAR + station, data = df, FUN= "mean",na.rm=TRUE)
# names(yr_mean) = c("year","site_id","val")

##STEP2
##determine percentile flows for each station based on 3 quantile criteria
for (site in unique(df$station)){
	site_dat = subset(df, station = site)
	

}


#### ---
temp_quantiles <- tapply(yr_mean$val, yr_mean$site_id, quantile, c(.25, .5, .75))
quants <- data.frame(matrix(unlist(temp_quantiles),nrow=20,byrow=T))
stations <- as.data.frame(names(temp_quantiles))
out_quants <- cbind(quants, stations)
colnames(out_quants) <- c("Dry", "Avg", "Wet", "StationID")
out_quants$Dry <- round(out_quants$Dry, digits = 2)
out_quants$Avg <- round(out_quants$Avg, digits = 2)
out_quants$Wet <- round(out_quants$Wet, digits = 2)

#STEP 3
##flag mean daily flows for each station/year combination as "dry", "avg", or "wet" based on percentiles
stations <- unique(yr_mean$site_id)
for (i in stations){
	wet <- out_quants[out_quants$StationID== i,"Wet"]
	avg <- out_quants[out_quants$StationID== i,"Avg"]
	dry <- out_quants[out_quants$StationID == i,"Dry"]
	yr_mean[yr_mean$site_id == i & yr_mean$val <= dry,"Moisture"] <- "D"
	yr_mean[yr_mean$site_id == i & yr_mean$val < wet & yr_mean$val > dry,"Moisture"] <- "A"
	yr_mean[yr_mean$site_id == i & yr_mean$val >= wet,"Moisture"] <- "W"
}

write.csv(yr_mean, "YearlyFlowRegimes.csv")
