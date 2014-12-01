####
# Formatting USGS raw daily flow data
format_usgs <- function(obsDataFile, monthly = F){
    obsData = read.table(obsDataFile, skip=2, sep="\t", header=T)
    obsData = obsData[-1,]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW_OBSERVED=as.numeric(as.character(obsData[,4])))
    months = as.POSIXlt(obsData$DATE)$mo + 1
    years = as.POSIXlt(obsData$DATE)$year + 1900
    date = paste(years, months, "01", sep="-")
    if (monthly) {
    	obsMonthly = aggregate(FLOW_OBSERVED ~ date, data=obsData, mean)
    	obsData = data.frame(DATE=as.Date(obsMonthly[,1]),
    		FLOW_OBSERVED=obsMonthly[,2])
        obsData = obsData[order(obsData$DATE),]
    } else {
      obsData = data.frame(DATE=as.Date(obsData[,1]),
          FLOW_OBSERVED=obsData[,2])
      obsData = obsData[order(obsData$DATE),]
    }
}