# library(RODBC)
source("T:/Projects/Wisconsin_River/Code/validation/functions_query_output.r")


# CHANGE THESE ###########
# SWAT project
projectDir = "H:/WRB"
# Scenario
scenario = "Default"
# Subbasin ID
subbasinID = 137
# Raw USGS data
obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/baraboo/dv.txt"
monthly = F
run = "whatever"
# DON'T CHANGE ANYTHING BELOW HERE #####



plotDir = paste(projectDir, "plots", sep="/")

if (!file.exists(plotDir)) {
	dir.create(plotDir)
}

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


output.rch = paste(projectDir, "Scenarios", scenario, "TxtInOut/output.rch", sep="/")
dat = readLines(output.rch)
dat = dat[10:length(dat)]

select_cols = list(
    cols = c("RCH", "MON", "FLOW_OUT"),
    dtypes = c(as.integer, as.integer, as.numeric)
)
modData = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
modData = as.data.frame(modData)
names(modData) = select_cols$cols
for (row in 1:length(select_cols$cols)) {
    col_name = select_cols$cols[row]
    dtype = select_cols$dtypes[row][[1]]
    vals = query_output.rch(dat, col_name)
    vals = sapply(vals, dtype)
    modData[col_name] = data.frame(vals, stringsAsFactors=F)
}

modData = modData[modData$RCH == subbasinID,]

if (monthly) {
    modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month")
} else {
    modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
}
modData = data.frame("DATE" = modTS, FLOW_MODELED = modData$FLOW_OUT * 35.3)

if (monthly) {
    pdf(paste(plotDir, "/", run, ".pdf", sep=""), width=11, height=8.5)
    d = merge(obsData, modData)
    ylim = c(min(d[,2:3]), max(d[,2:3]))
    plot(FLOW_OBSERVED ~ DATE,
    	data=d,
    	type="l",
    	col="aquamarine",
    	ylab="Flow (cfs)",
    	xlab="Date",
    	ylim=ylim,
    	lwd=3)
    lines(FLOW_MODELED ~ DATE, data=d, col="coral1", lwd=3)
    legend("topright", c("Modeled", "Observed"), fill=c("coral1","aquamarine"))
    plot(FLOW_MODELED ~ FLOW_OBSERVED, data=d, pch=20, ylab="Modeled flow (cfs)", xlab="Observed flow (cfs)")
    dev.off()
} else {
    pdf(paste(plotDir, "/", run, ".pdf", sep=""), width=11, height=8.5)
    d = merge(obsData, modData)
    for (yr in 2002:2013) {
        date_query = d$DATE >= as.Date(paste(yr,"-01-01", sep="")) &
            d$DATE <= as.Date(paste(yr,"-12-31", sep=""))
        d_yr = d[date_query,]
        ylim = c(min(d_yr[,2:3]), max(d_yr[,2:3]))
        plot(FLOW_OBSERVED ~ DATE,
             data=d_yr,
             main=yr,
             type="l",
             col="aquamarine",
             ylab="Flow (cfs)",
             xlab="Date",
             ylim=ylim,
             lwd=3)
        lines(FLOW_MODELED ~ DATE, data=d_yr, col="coral1", lwd=3)
        legend("topright", c("Modeled", "Observed"), fill=c("coral1","aquamarine"))
        plot(FLOW_MODELED ~ FLOW_OBSERVED,
             data=d_yr,
             pch=20,
             main=yr,
             ylab="Modeled flow (cfs)",
             xlab="Observed flow (cfs)")
    }
    dev.off()
}
