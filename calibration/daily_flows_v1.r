library(rgdal)
library(sp)
library(raster)
library(foreign)
library(maptools)
source("T:/Projects/Wisconsin_River/Code/validation/functions_query_output.r")

# SWAT project
projectDir = "H:/WRB"
# Scenario
scenario = "dailyOutput_defaultBioE"

# inputs all data on the front, then queries as need from the function
output.rch = paste(projectDir, "Scenarios", scenario, "TxtInOut/output.rch", sep="/")
dat = readLines(output.rch)
dat = dat[10:length(dat)]

select_cols = list(
    cols = c("RCH", "MON", "FLOW_OUT"),
    dtypes = c(as.integer, as.integer, as.numeric)
)
modData_all = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
modData_all = as.data.frame(modData_all)
names(modData_all) = select_cols$cols
for (row in 1:length(select_cols$cols)) {
    col_name = select_cols$cols[row]
    dtype = select_cols$dtypes[row][[1]]
    vals = query_output.rch(dat, col_name)
    vals = sapply(vals, dtype)
    modData_all[col_name] = data.frame(vals, stringsAsFactors=F)
}

# CHANGE THESE ###########

# Subbasin ID
#   for tomahawk
# subbasinID = 168
# subbasinID = 137

# Raw USGS data
# obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/baraboo/dv.txt"
# obsDir <- "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/"
# monthly = F
# run = "defaults"

plotFlows_ObsSim <- function(subbasinID, 
                    obsDir = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/",
                    monthly = F, scenario = 'dailyOutput_defaultBioE', 
                    run = scenario, projectDir = "H:/WRB"){
    obsFiles <- list.files(obsDir)
    wrbFlows <- read.csv('T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv',
                         colClasses="character")
    obsID <- wrbFlows$USGS_ID[which(wrbFlows$WRB_SubbasinID == as.character(subbasinID))]
    obsName <- wrbFlows$Flow_Name[which(wrbFlows$WRB_SubbasinID == as.character(subbasinID))]
    obsDataFile <- paste(obsDir, obsID,'.txt',sep = '')
                
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
    
    
    modData = modData_all[modData_all$RCH == subbasinID,]
    
    if (monthly) {
        modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month")
    } else {
        modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
    }
    modData = data.frame(DATE = modTS, FLOW_MODELED = modData$FLOW_OUT * 35.3)
    
    if (monthly) {
        pdf(paste(plotDir, "/", run, '_',subbasinID, ".pdf", sep=""), width=11, height=8.5)
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
        pdf(paste(plotDir, "/", run, '_sub_', subbasinID,'_', obsName,".pdf", sep=""), width=11, height=8.5)
        d = merge(obsData, modData)
        for (yr in unique(as.POSIXlt(modData$DATE)$year)+1900) {
            
            date_query = d$DATE >= as.Date(paste(yr,"-01-01", sep="")) &
                d$DATE <= as.Date(paste(yr,"-12-31", sep=""))
            d_yr = d[date_query,]
            if(nrow(d_yr) == 0){next}
            ylim = range(d_yr[,2:3],na.rm = T)
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
                 ylim = ylim,
                 xlim = ylim,
                 ylab="Modeled flow (cfs)",
                 xlab="Observed flow (cfs)")
        }
        dev.off()
    }
}
# subbasins of interest from Pat Oldenburg
subs <- c(169,127,334,167,168,226,135,162,159,158,268,157,155,154,
          152,326,151,150,149,147,146,78,144,142,141,140,199,195,139,138,184,137)
for (sb in subs){
    plotFlows_ObsSim(sb)
}


