# This step aggregates the bflow.exe output data
#   first it process each continguous record, to facilitate evaluation
#   of alpha base flow variation at each site
#   Then aggregates each contiguous record for each site (if more than 1)
#   IF more than 1, the alpha baseflow value is weighted according to the
#   length of record and averaged.
options(warn=0)
wd = "C:/evans/bflow/wi_dailyValues/"
setwd(wd)
# all the flw files for bflow program
flLst <- list.files("C:/evans/bflow/baseflow/", pattern = "*.flw")
# bflow output, complete output with be length 7, incomplete length 4
#       For some reason, bflow.exe would not give full outputs.
bflowOut <- read.fwf("data_files/bflow_OutPut.txt", 
	widths=c(15,13,13,13,7,13,14))
bflowOut[,1] = gsub(" ", "", bflowOut[,1])
# removing those without a complete output
naOut <- bflowOut[which(is.na(bflowOut[,7])),]
bflowOut <- bflowOut[which(!is.na(bflowOut[,7])),]
# site description table
site_file = "data_files/dvSiteDesc.txt"
# with conversion to numeric
sites = read.table(site_file, skip=30, sep="\t", comment.char="",
                   colClasses=c(rep("character", 8), 'numeric'))
sites = sites[sites[,9] > 19.3051 & sites[,9] < 386.102 & !is.na(sites[,9]),]
# # # # # # # # # # # # # # # # ## # # ## # # # # # ## 
recordTable <- NULL
notRun <- NULL; noAlpha <- NULL
siteList <- NULL
for (f in flLst){
    #f <- "5433600_1.flw"
    if(f %in% naOut[,1]){
        print(paste("Skipping", f, "not a complete record.")) 
        next
    }
    if(!(f %in% bflowOut[,1])) {
        notRun <- c(notRun, f)
        print(paste("Skipping", f, "failed in bflow."))
        next
    } 
    recordID <- strsplit(f,'.',fixed = T)[[1]][1]
    recNum <- strsplit(recordID,'_',fixed = T)[[1]][2]
    siteNum <- strsplit(recordID,'_',fixed = T)[[1]][1] 
    recLength <- nrow(read.table(paste("C:/evans/bflow/baseflow/",f, sep = ''), header =T))
    if(!(siteNum %in% sites[,2])) {
        notRun <- c(notRun, f)
        print(paste("Skipping", f, "not in sitetable."))
        next
    }
    siteIndex <- which(sites[,2] == siteNum)
    siteName <- sites[siteIndex,3]
    siteLat <- sites[siteIndex, 5]
    siteLong <- sites[siteIndex, 6]
    siteDrArea <- sites[siteIndex, 9]
     
    if(is.na(bflowOut[which(bflowOut[,1]==f),6])) {
        noAlpha <- c(noAlpha, f)
    }
    
    recIndex <- which(bflowOut[,1] == f)
    alphaBaseFlowFactor <- bflowOut[recIndex, 6]
    thirdPass <- bflowOut[recIndex, 4]
    
    line <- c(siteNum,
              recNum,
              recLength,
              siteName,
              siteLat,
              siteLong,
              siteDrArea,
              thirdPass,
              alphaBaseFlowFactor)
    recordTable <- rbind(recordTable, line) 
    print(paste('Finished', f))
    #print(paste(line))
    #if (siteNum == "04066000_1.flw"){break}
}
recordTable <- data.frame(recordTable)
names(recordTable) <- c("siteNum",
                        "recNum",
                        "recLength",
                        "siteName",
                        "siteLat",
                        "siteLong",
                        "siteDrArea",
                        "thirdPass",
                        "alphaBaseFlowFactor")
write.table(recordTable, "data_files/RecordTable.csv", sep = ',',row.names = F)
######################################################################
recordTable <- read.csv("data_files/RecordTable.csv")
siteTable <- NULL
for (site in unique(recordTable$siteNum)){
    #site <- "4081000"
    print(site)
    
    recInds <- which(recordTable[,1]== site)
    recCount <- length(recInds)
    
    siteName <- as.character(recordTable[recInds[1],4])
    siteLat <- recordTable[recInds[1],5] 
    siteLong <- recordTable[recInds[1],6]
    siteDrArea <- recordTable[recInds[1],7]
    siteTotLength <- sum(recordTable[recInds,3])
    # for weighted average
    if (recCount == 1){
        # note: if only 1 record, baseflow is put into the
        #   wtd avg alpha column
        siteWtdAvgAlpha <- recordTable[recInds,9]
        siteAvgAlpha <- NA
        siteSDAlpha <- NA
        
    } else {
        alphas <- recordTable[recInds,9]
        wts <- recordTable[recInds, 3]/siteTotLength
        siteWtdAvgAlpha <- sum(wts*alphas, na.rm = T)
        siteAvgAlpha <- mean(alphas, na.rm = T)
        siteSDAlpha <- sd(alphas, na.rm = T)
        
    }
    line <- c(site,
              siteName,
              recCount,
              siteLat,
              siteLong,
              siteDrArea,
              siteTotLength,
              siteWtdAvgAlpha,
              siteAvgAlpha,
              siteSDAlpha)
    siteTable <- rbind(siteTable, line)    
}
siteTable <- data.frame(siteTable)
names(siteTable) <- c("siteNumber",
                      'siteName',
                      'NoOfContinuousRecs',
                      'siteLat',
                      'siteLong',
                      'siteDrArea',
                      'siteTotalRecordLength',
                      'siteWeightedtd_Avg_Alpha',
                      'siteAvg_Alpha',
                      'siteSD_Alpha')
write.table(siteTable, "data_files/siteTable.csv", sep = ',',row.names = F)
##################### plotting the site data ######################################
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(classInt)
library(RColorBrewer)

siteTable <- read.csv("data_files/siteTable.csv")
siteTable = siteTable[!is.na(siteTable$siteWeightedtd_Avg_Alpha),]
siteTable.sp <- siteTable
# converting to spatial points dataframe
coordinates(siteTable.sp) <- ~ siteLong + siteLat
proj4string(siteTable.sp) <- CRS("+proj=longlat +datum=NAD83")
siteTable.sp <- spTransform(siteTable.sp, CRS("+init=epsg:3071"))
# writePointsShape(siteTable.sp, "streamFlowSites_v2")
# streamFlowShp <- readOGR(,"streamFlowSites_v2")
streamFlowShp <- readShapePoints("streamFlowSites_v2")

counties = readOGR("T:/GIS/Statewide_Coverages/Political_Boundaries", "WI_Counties")
pal = brewer.pal(5, "YlGnBu")
alpha_bf_class = classIntervals(siteTable$siteWeightedtd_Avg_Alpha,5)
alpha_bf_col = findColours(alpha_bf_class, pal)
pdf('Streamflow Data Sites.pdf')
plot(counties, axes=F)
plot(siteTable.sp, bg=alpha_bf_col, col="black", add=T, pch=21)
# for legend
legTxt <- attr(attr(alpha_bf_col,'table'),'dimnames')[[1]]
newTxt <- NULL
for (rw in 1:length(legTxt)){
    r <- legTxt[rw]
    splt <- strsplit(r, split=',')[[1]]
    opn <- as.numeric(strsplit(splt[1],'[',fixed=T)[[1]][2])
    if (rw == length(legTxt)){
        cls <- as.numeric(strsplit(splt[2],']')[[1]][1])
    } else {
        cls <- as.numeric(strsplit(splt[2],')')[[1]][1])
    }
    print(paste(opn,cls))
    opn <- round(opn, 3)
    cls <- round(cls, 3)
    nw <- paste('[',opn,',',cls,')',sep='')
    newTxt <- c(newTxt, nw)
}
legend('bottomleft', legend =newTxt, bg = 'white', bty = 'n',
       fill = pal, cex=0.75, title = paste("Streamflow \n Data Sites"))
dev.off()