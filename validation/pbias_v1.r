# library(RODBC)
library(hydroGOF)
# library(plyr)
dst <- 'T:/Projects/Wisconsin_River/Model_Documents/TMDL_report/figures/calibration_validation_figures/'
# CHANGE THESE ###########
# yldPbias <- function(crop, scenario = 'defaultParameters', 
#                      projectDir = 'H:/WRB', basinWide = T){
    
#Parameters
crops <-c('ALFA','CORN',"SOYB")
# scenario = 'defaultParameters'
projectDir = 'H:/WRB'
#     scenario <- 'Default'
subbasinIDs <- read.csv("T:/Projects/Wisconsin_River/Code/validation/subbasins_byWRB_County.txt")
# ----- ----- 
# reading in and process simulated data, for default parameters
#   modData_allSubs_allSubs will contain data regarding LULC, SUB, HRU, Area, Year and Yield
source("T:/Projects/Wisconsin_River/Code/validation/functions_query_output.r")
dat = readLines(paste(projectDir, "Scenarios/defaultParameters/TxtInOut/output.hru", sep="/"))
dat = dat[10:length(dat)]

select_cols = list(
    cols = c("LULC", "SUB", "HRU", "AREA", "MON", "YLD"),
    dtypes = c(as.character, as.integer, as.integer, as.numeric, as.integer, as.numeric)
)
modData_allSubs = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
modData_allSubs = as.data.frame(modData_allSubs)
names(modData_allSubs) = select_cols$cols
for (row in 1:length(select_cols$cols)) {
    col_name = select_cols$cols[row]
    dtype = select_cols$dtypes[row][[1]]
    vals = query_output.hru(dat, col_name)
    vals = sapply(vals, dtype)
    modData_allSubs[col_name] = data.frame(vals, stringsAsFactors=F)
}
modData_default = subset(modData_allSubs, MON > 13)
# ----- ----- 
# reading in and process simulated data, for calibrated parameters
#   modData_allSubs_allSubs will contain data regarding LULC, SUB, HRU, Area, Year and Yield
# 
dat = readLines(paste(projectDir, "Scenarios/bioEcalib/TxtInOut/output.hru", sep="/"))
dat = dat[10:length(dat)]

select_cols = list(
    cols = c("LULC", "SUB", "HRU", "AREA", "MON", "YLD"),
    dtypes = c(as.character, as.integer, as.integer, as.numeric, as.integer, as.numeric)
)
modData_allSubs = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
modData_allSubs = as.data.frame(modData_allSubs)
names(modData_allSubs) = select_cols$cols
for (row in 1:length(select_cols$cols)) {
    col_name = select_cols$cols[row]
    dtype = select_cols$dtypes[row][[1]]
    vals = query_output.hru(dat, col_name)
    vals = sapply(vals, dtype)
    modData_allSubs[col_name] = data.frame(vals, stringsAsFactors=F)
}
modData_calibBio = subset(modData_allSubs, MON > 13)
# ----- ----- 
# reading in and processing Observed Data
obsData_full <- read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/upDated_NASSstats_30092014.csv", stringsAsFactors=F)

obsData_full$DATE <- as.Date(as.character(paste(obsData_full[,'Year'],1,1,sep ='-')))
obsData_full <- obsData_full[obsData_full$DATE>='2002-01-01',]
cropTab <- data.frame( 
    CropKeySWAT = c('CORN', 'ALFA','SOYB'),
    CropKeyNASS = c('CORN, GRAIN - YIELD, MEASURED IN BU / ACRE',
                  'HAY & HAYLAGE, ALFALFA - YIELD, MEASURED IN TONS / ACRE, DRY BASIS',
                  'SOYBEANS - YIELD, MEASURED IN BU / ACRE'),
    stringsAsFactors=F
)
obsData_full$ObsYld <- NA
obsData_full$LULC <- NA
for (crop in c('CORN', 'ALFA','SOYB')){
    cropInd <- which(obsData_full$Data.Item == cropTab[which(cropTab$CropKeySWAT == crop),'CropKeyNASS']) 
    print(paste("There are",length(cropInd),'entries for',crop))
    if (crop == 'CORN'){
        obsData_full$ObsYld[cropInd] <- (obsData_full$Value[cropInd] *56)*(2.471/2205)*(0.845)
        obsData_full$LULC[cropInd] <- crop
    } else if (crop == 'SOYB'){
        obsData_full$ObsYld[cropInd] <- (obsData_full$Value[cropInd] *60)*(2.471/2205)*(0.875)
        obsData_full$LULC[cropInd] <- crop
    } else if (crop == 'ALFA'){
        obsData_full$ObsYld[cropInd] <- (obsData_full$Value[cropInd] * 2.24) *(0.87)
        obsData_full$LULC[cropInd] <- crop
    }
}
obsData_cnty = aggregate(ObsYld ~ County + DATE + LULC, data=obsData_full, mean)        
counties <- unique(obsData_full$County)
all_cnty_data = data.frame()
all_modData = data.frame(hold = rep(NA, 792))
cnt <- 1
for (d in list(modData_default, modData_calibBio)){
#     print(head(d))}
#     d <- modData_calibBio
    print(paste("beginning dataset..."))
    all_cnty_data = data.frame()
    for (cnty in counties){
#             cnty <- 'SAUK'
        print(paste("Starting",cnty,"County"))
        subIDs <- subbasinIDs[toupper(subbasinIDs$CTY_NAME) == cnty, 11]
        cnty_SWAT_data = data.frame()
        # taking just the yearly harvest values
        modData <- d[d$MON > 13,]
        modData <- modData[modData$LULC %in% crops,]
        modData = modData[modData$SUB %in% subIDs,]
        if (nrow(modData) == 0) {next}
        area_wt_yld = merge(aggregate(YLD * AREA ~ MON + LULC, data=modData, sum),
            aggregate(AREA ~ MON + LULC, data=modData, sum))
        area_wt_yld$MEAN = area_wt_yld[['YLD * AREA']] / area_wt_yld[['AREA']]
        cnty_data = data.frame(
            County=cnty,
            DATE=as.Date(paste(area_wt_yld$MON,"01-01", sep="-")),
            simDat=area_wt_yld$MEAN,
            LULC = area_wt_yld$LULC
    #             ObsYld=obsData_cnty$
        )
        area_wt_yld <- NULL
        all_cnty_data <- rbind(all_cnty_data, cnty_data) 
        print(paste(nrow(all_cnty_data), 'rows in all county data'))
    }
    all_modData <- cbind(all_modData, all_cnty_data)
}
all_modData <- all_modData[, c(2,5,3,4,8)]
names(all_modData) <- c('County','LULC', 'DATE', 'Defaults', 'Calib')

allData <- merge(all_modData,obsData_cnty, all.x = T)




for (crop in crops){
#     crop <- 'ALFA'
    print(paste('Beginning',crop,'...'))
    pdf(paste(dst, crop,'_Obs_v_Sim_byCounty.pdf',sep = ''))
    allData_crop <- subset(allData, LULC == crop)
    ylm <- range(c(allData_crop$Defaults, allData_crop$obsYld, allData_crop$Calib))
    for (cnty in counties){
        print(paste('   Plotting',cnty,'...'))
#         cnty <- 'ADAMS'
        allData_crop_cnty <- subset(allData_crop, County == cnty)
        if(nrow(allData_crop_cnty)<= 4){next}
        plot(Defaults ~ DATE,
            data=allData_crop_cnty,
            pch = 20,
            col="coral1",
            type="b",
            ylim=ylm,
            ylab= paste(crop, "Mg/ha"),
            xlab="Date",
            cex = 1.5,
            main=paste(cnty,'County'))
    
        points(Calib ~ DATE, 
               data=allData_crop_cnty,
               type = 'b', pch=20, col="coral4", cex = 1.5)
        
        points(ObsYld ~ DATE, 
               data=allData_crop_cnty,
               type = 'b', pch=20, col="aquamarine", cex = 1.5)
    
        legend('topright', legend = c('Observed','Default', 'Calibrated'),
            fill = c('aquamarine', 'coral1','coral4'))
        
        defltPbias <- with(allData_crop_cnty, pbias(Defaults, ObsYld, na.rm = T))
        calibPbias <- with(allData_crop_cnty, pbias(Calib, ObsYld, na.rm = T))
        mtext(paste('Uncalibrated pbias:',defltPbias,'; Calibrated pbias:', calibPbias))
    }
    dev.off()
}

# for basin wide
obsDataBasin <- aggregate(ObsYld ~ LULC + DATE, data=obsData_full, mean) 

basinSimDat <- data.frame(hold = rep(NA, 36))
for (d in list(modData_default, modData_calibBio)){
    modData <- d[d$MON > 13,]
    modData <- modData[modData$LULC %in% crops,]
    if (nrow(modData) == 0) {next}
    area_wt_yld = merge(aggregate(YLD * AREA ~ MON + LULC, data=modData, sum),
        aggregate(AREA ~ MON + LULC, data=modData, sum))
    area_wt_yld$MEAN = area_wt_yld[['YLD * AREA']] / area_wt_yld[['AREA']]
    basinSimDat <- cbind(basinSimDat, area_wt_yld)
}
basinSimDat <- basinSimDat[,c(2,3,6,11)]
names(basinSimDat) <- c('MON', 'LULC','Defaults','Calib')
basinSimDat$DATE=as.Date(paste(basinSimDat$MON,"01-01", sep="-"))

allData_basin <- merge(basinSimDat, obsDataBasin, all.x = T)

pdf(paste(dst,'Calib_v_Default_BioE_basinWide.pdf',sep = ''))
for (crop in crops){
    allData_basin_crop <- subset(allData_basin, LULC == crop)
    ylm <- range(c(allData_basin_crop$Defaults, 
                   allData_basin_crop$obsYld, 
                   allData_basin_crop$Calib))
    plot(Defaults ~ DATE,
         data=allData_basin_crop,
         pch = 20,
         col="coral1",
         type="b",
         ylim=ylm,
         ylab= paste(crop, "Mg/ha"),
         xlab="Date",
         cex = 1.5,
         main=paste('Basin wide Annual Averages'))
    
    points(Calib ~ DATE, data=allData_basin_crop,
        type = 'b', pch=20, col="coral4", cex = 1.5)
    
    points(ObsYld ~ DATE, data=allData_basin_crop,
        type = 'b', pch=20, col="aquamarine", cex = 1.5)
    
    legend('topright', legend = c('Observed','Default', 'Calibrated'),
        fill = c('aquamarine', 'coral1','coral4'))
        
    defltPbias <- with(allData_basin_crop, pbias(Defaults, ObsYld, na.rm = T))
    calibPbias <- with(allData_basin_crop, pbias(Calib, ObsYld, na.rm = T))
    mtext(paste('Uncalibrated pbias:',defltPbias,'; Calibrated pbias:', calibPbias))
}
dev.off()