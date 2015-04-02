##first step - determine station/year average flows for all flow stations 
##second step - determine percentile annual flows for each station/year
##third step - each year/station combination as "dry", "average", or "wet"

##set working directory/files
drv = "Y:/"
setwd(paste(drv,"UFWB/SWAT_calibration/"))
rawDataFile = 'flow_data.csv'
df = read.csv(rawDataFile, stringsAsFactors=FALSE, header=TRUE)

##split date into month, day, & year columns
mdy <- colsplit(df$date, "/", c("month", "day", "year"))  
sta <- cbind(df, mdy)

##STEP 1
##summarize mean daily flows for each station/year combination
sta$val <- as.numeric(sta$val)
yr_mean <- aggregate(val ~ site_id + year, data = sta, FUN= "mean",na.rm=TRUE)

##STEP2
##determine percentile flows for each station based on 3 quantile criteria
temp_quantiles <- tapply(yr_mean$val, yr_mean$site_id, quantile, c(.25, .5, .75))
quants <- data.frame(matrix(unlist(temp_quantiles),nrow=43,byrow=T))
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
