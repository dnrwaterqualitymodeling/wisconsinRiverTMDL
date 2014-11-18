library(hydroGOF)
options(warn = 1,
    stringsAsFactors = F)
source("C:/Users/evansdm/Documents/Code/validation/format_usgs_flow_function.R")
# SWAT project
projectDir = "H:/WRB"
# Scenario
scenario = "sim_flow"
obs_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/"
obs_files <- list.files(obs_dir)
usgs_lu_file <- "T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"
usgs_lu <- read.csv(usgs_lu_file, colClasses="character")
uncertainty_file <- paste(projectDir, '/', scenario, 'ET_uncertainty.csv',sep='')
sim_files <- list.files(paste("H:", scenario, sep = '/'),
    pattern = "*_rchData.csv")
######
plotDir = paste(projectDir, "plots", sep="/")
    
    if (!file.exists(plotDir)) {
        dir.create(plotDir)
    }
######
uncertainty_df <- data.frame()
######
simDat <- data.frame()
print(paste("importing Data..."))
for (fl in 1:length(sim_files)) {
    fl_nm <- sim_files[fl]
    eq <- strsplit(fl_nm, split = '_')[[1]][1]
    dat <- read.csv(paste("H:", scenario, fl_nm, sep = '/'))
#     dat$EQ <- eq
    if (fl == 1){
        simDat <- rbind(simDat, dat[,c("RCH", "AREA", "MON", "FLOW_OUT")])
    } else {
        simDat <- cbind(simDat, dat[,"FLOW_OUT"])
    }
    names(simDat)[length(simDat)] <- eq
    rm(dat)
}
##############################
for (fl in obs_files){
    
    usgsID <- strsplit(fl, ".", fixed = T)[[1]][1]
    indx <- which(usgs_lu$USGS_ID == usgsID)
    if (usgs_lu$Keep[indx] == 0){
        next
    }
    
    obsDat <- format_usgs(paste(obs_dir, fl, sep = '/'))
    
    subID <- usgs_lu$WRB_SubbasinID[indx]
    gaugeName <- usgs_lu$Flow_Name[indx]
    print(paste("Working on subbasin",subID))
    
    sub_simDat <- subset(simDat, 
        RCH == as.integer(subID))
    
    modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
    sub_simDat$DATE = modTS
    d <- merge(obsDat, sub_simDat, by = 'DATE', all = T)
    
    d[,c("Hargreaves", "Penman", "Priestley")] = 
        d[,c("Hargreaves", "Penman", "Priestley")] * 35.3
    
    # first plotting entire record
    pdf(paste(plotDir, "/_Flows_Subbasin_", subID, ".pdf", sep = ''), width = 12, height = 8)
    ylm = range(d[,c("Hargreaves", "Penman", "Priestley", "FLOW_OBSERVED")],na.rm = T)
    plot(FLOW_OBSERVED ~ DATE,
         data=d,
         main=paste("Flow from subbasin", subID),
         type="l",
         col="aquamarine",
         ylab='Flow (cf/s)',
         xlab="Date",
         ylim=ylm,        
         lwd=3)
    lines(Hargreaves ~ DATE, data=d, col="coral", lwd=3)
    lines(Penman ~ DATE, data=d, col="mediumorchid", lwd=3)
    lines(Priestley ~ DATE, data=d, col="palevioletred", lwd=3)
    mtext(paste(gaugeName, "Entire record."))
        
    legend("topright", 
        c("Observed",
        "Hargreaves",
        "Penman",
        "Priestley"),
        fill=c("aquamarine",
            "coral",
            "mediumorchid",
            "palevioletred"))

    # yearly plots
    for (yr in unique(as.POSIXlt(d$DATE)$year)+1900) {
        date_query = d$DATE >= as.Date(paste(yr,"-01-01", sep="")) &
            d$DATE <= as.Date(paste(yr,"-12-31", sep=""))
        d_yr = d[date_query,]
        if(nrow(d_yr) == 0){next}
        ylm = range(d_yr[,c("Hargreaves", 
            "Penman", 
            "Priestley", 
            "FLOW_OBSERVED")],na.rm = T)
        plot(FLOW_OBSERVED ~ DATE,
             data=d_yr,
             main=yr,
             type="l",
             col="aquamarine",
             ylab="Flow (cf/s)",
             xlab="Date",
             ylim=ylm,
             lwd=3)
        mtext(paste("Subbasin:",subID))
        lines(Hargreaves ~ DATE, data=d_yr, col="coral", lwd=3)
        lines(Penman ~ DATE, data=d_yr, col="mediumorchid", lwd=3)
        lines(Priestley ~ DATE, data=d_yr, col="palevioletred", lwd=3)
        
        legend("topright", 
            c("Observed",
                "Hargreaves",
                "Penman",
                "Priestley"),
            fill=c("aquamarine",
                "coral",
                "mediumorchid",
                "palevioletred"))
        }
    dev.off()
    ### pbias Calcs 
    pb_har <- with(d, pbias(Hargreaves, FLOW_OBSERVED))
    pb_pen <- with(d, pbias(Penman, FLOW_OBSERVED))
    pb_prst <- with(d, pbias(Priestley, FLOW_OBSERVED))
    
    ns_har <- with(d, NSE(Hargreaves, FLOW_OBSERVED))
    ns_pen <- with(d, NSE(Penman, FLOW_OBSERVED))
    ns_prst <- with(d, NSE(Priestley, FLOW_OBSERVED))
    
    rw <- c(subID,
            usgsID,
            pb_har,
            pb_pen,
            pb_prst,
            ns_har,
            ns_pen,
            ns_prst)
    uncertainty_df <- rbind(uncertainty_df, rw)
}
names(uncertainty_df) <- c(
    "SubID",
    "USGS_ID",
    "pbias_harg",
    "pbias_penman",
    "pbias_priestley",
    "nashsut_harg",
    "nashsut_penman",
    "nashsut_priestley")
write.csv(uncertainty_df,
    uncertainty_file,
    row.names = F
)
    
    
 