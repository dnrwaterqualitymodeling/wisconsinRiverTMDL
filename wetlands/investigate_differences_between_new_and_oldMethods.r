library(ggplot2);library(reshape2)
wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands"
setwd(wd)
source("T:/Projects/Wisconsin_River/Code/validation/functions_query_output.r")

# SWAT project
projectDir = "H:/WRB"
# Scenario
scenario = "daily_wetlandsUpdate_15_oct"

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
modData_newWet <- modData_all
###### for old method
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
modData_oldWet <- modData_all
# 
modData <- cbind(modData_newWet, modData_oldWet[,3])
names(modData) <- c('RCH', "MON", "FLOW_OUT_new", "FLOW_OUT_old")

#########################################
# for observed data
plotFlows_newOldWets <- function(subbasinID, 
                    obsDir = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/",
                    monthly = F, scenario = 'daily_wetlandsUpdate_15_oct', 
                    run = scenario, projectDir = "H:/WRB"){
#     subbasinID <- 137
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
    obsData = data.frame(DATE=as.Date(obsData[,1]),
                FLOW_OBSERVED=obsData[,2])
    obsData = obsData[order(obsData$DATE),]
    #
    modData_sb = modData[modData$RCH == subbasinID,]
    modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
    modData_sb = data.frame(DATE = modTS,
                            FLOW_MODELED_new = modData_sb$FLOW_OUT_new * 35.3,
                            FLOW_MODELED_old = modData_sb$FLOW_OUT_old * 35.3)

    d = merge(obsData, modData_sb)
    pdf(paste(plotDir, "/", run, '_',subbasinID, ".pdf", sep=""),width=13, height=8.5)
    
    for (yr in unique(as.POSIXlt(modData_sb$DATE)$year)+1900) {
#         yr <- 2002
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
        lines(FLOW_MODELED_old ~ DATE, data=d_yr, col="coral1", lwd=1)
        lines(FLOW_MODELED_new ~ DATE, data=d_yr, col="coral4", lwd=1)
        mtext(paste(obsName, ' (Subbasin ID: ',subbasinID,')',sep = ''))
        legend("topright", c("Modeled, old wetlands", "Modeled, new wetlands", "Observed"), 
               fill=c("coral1","coral4","aquamarine"))
    }
    dev.off()
}
plotFlows_newOldWets(137)
plotFlows_newOldWets(159)
plotFlows_newOldWets(184)
plotFlows_newOldWets(199)





##################
nw  <- read.csv('wetland_geometry_v3.csv')
old <- read.csv('old_method/wetland_geometry.csv')

old$Mthd <- 'old'
nw$Mthd <- 'new'
df <- rbind(old, nw)

pdf('NVols.pdf')
p <- 1
while (p < 341){
    strt <- p
    nd <- p + 4
    sb <- subset(df, subbasin >= strt & subbasin < nd)
    plt <- ggplot(data = sb, aes(x = subbasin, y= WET_NVOL,
                    fill = Mthd)) +
            geom_bar(stat = 'identity', colour = 'black',
                     position = position_dodge()) +
            scale_fill_manual(values = c('coral1', 'coral4')) +
            theme_bw()
    plt2 <- plt + aes(x = subbasin, y= WET_MXVOL,
                    fill = Mthd) + geom_bar(stat = 'identity', colour = 'black',
                     position = position_dodge())
    print(plt)
    p <- p +5 
}
dev.off()

pdf('WET_FR.pdf')
p <- 1
while (p < 341){
    strt <- p
    nd <- p + 4
    sb <- subset(df, subbasin >= strt & subbasin < nd)
    plt <- ggplot(data = sb, aes(x = subbasin, y= WET_FR, fill = Mthd)) +
            geom_bar(stat = 'identity', colour = 'black',
                     position = position_dodge()) +
            scale_fill_manual(values = c('coral1', 'coral4')) +
            theme_bw()
    print(plt)
    p <- p +5 
}
dev.off()
       