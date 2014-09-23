library(RODBC)
library(hydroGOF)

# CHANGE THESE ###########
# SWAT project
projectDir = "H:/WRB"
# Subbasin IDs in Marathon County
subbasinIDs <- read.csv("T:/Projects/Wisconsin_River/Code/validation/subbasins_byWRB_County.txt")
# Raw NASS data
obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/Avg_cornGrainYlds_byCounty.csv"
run = "default crop parameters"
# DON'T CHANGE ANYTHING BELOW HERE #####
# Grabbing and formating obs for Marathon county
cornGrain <- read.csv(obsDataFile)
cornGrain$DATE <- as.Date(as.character(paste(cornGrain[,1],9,1,sep ='-')))
# to convert from bu/acre to Mg/ha
cornGrain$OBSERVED_YLDbu_acre <- cornGrain$Value..BU.ACRE. #*  .0628

# for modeled data, with default parameters
outDb = paste(projectDir, "Scenarios/defaultParameters/TablesOut/SWATOutput.mdb", sep="/")
con = odbcConnectAccess(outDb)

pbias_df <- data.frame(SubBasin = rep(NA, time = nrow(subbasinIDs)),
                             PBias = rep(NA, time = nrow(subbasinIDs)))
#sb <- 1
cornGrain$OBSERVED_YLDbu_acre <- cornGrain$Value..BU.ACRE. #*  .0628
counties <- unique(cornGrain$County)
# cnty <- 'ADAMS'
subCount <- 1
pdf("crop_yield_validation_default_parameters_ObsPred.pdf", height=7, width=10)
for (cnty in counties){
    print(paste("Working on",cnty,"County"))
    obsData <- cornGrain[cornGrain$County == cnty,]
    obsData$OBSERVED_YLDbu_acre = obsData$OBSERVED_YLDbu_acre * 1.014 # convert to 20% moisture
    subIDs <- subbasinIDs[toupper(subbasinIDs$CTY_NAME) == cnty,11]
    all_mod_data = data.frame()
    for (sb in 1:length(subIDs)){
        print(paste("Working on subbasin",subIDs[sb]))
        query = paste("SELECT YEAR, MON, LULC, BIOMt_ha, YLDt_ha FROM hru WHERE SUB =", subIDs[sb])
        modData = sqlQuery(con, query)        
        #grabbing just corn
        modData <- modData[modData$LULC == 'CORN',]
        if (nrow(modData) == 0) {next}
        modData <- modData[modData$MON == 9,]
        
        # formating time series
        mon = formatC(modData$MON, width = 2, format = "d", flag = "0")
        modTS = as.Date(paste(modData$YEAR, mon, "01", sep="-"))                                          
        modData = data.frame("DATE" = modTS, 
                             MODELED_YLDbu_acre = (modData$YLDt_ha)*(15.92357))
        # This aggregate() will lump the HRUs together 
#         modData <- aggregate(modData$MODELED_YLDbu_acre, list(modData$DATE), mean)
#         names(modData) <- c('DATE', 'MODELED_YLDbu_acre')
        all_mod_data = rbind(all_mod_data, modData)
    }
    # d contains all hrus
    d = merge(obsData, all_mod_data)    
    d2 <- aggregate(d[,6:7], list(d$DATE), mean)
    d2_lm <- lm(MODELED_YLDbu_acre ~ OBSERVED_YLDbu_acre, data = d2)
    ylim = range(c(d2$MODELED_YLDbu_acre, d2$OBSERVED_YLDbu_acre))
    r2 = summary(d2_lm)$adj.r.squared
#     For plotting observed v predicted
#     plot(MODELED_YLDbu_acre ~ OBSERVED_YLDbu_acre, data = d2,
#          ylim = ylim, xlim = ylim, pch = 20,
#          main=paste(cnty,'County'), 
#          ylab = 'Simulated Corn Grain (bu/acre)',
#          xlab = 'Predicted Corn Grain (bu/acre)')
#     abline(a = 0, b = 1, col = 'red')
#     mtext(paste("R2 = ", format(r2, digits=3), sep=""))
    plot(OBSERVED_YLDbu_acre ~ Group.1,
         data=d2,
         pch = 20,
         col="aquamarine",
         type="b",
         ylim=ylim,
         ylab="Corn Grain Yield, bu/acre",
         xlab="Date",
         cex = 1.5,
         main=paste(cnty,'County'))
    mtext(paste("R2 = ", format(r2, digits=3), sep=""))
#     For plotting a point for each HRU
#     points(MODELED_YLDbu_acre ~ DATE, data = d, col = 'coral1', pch = 20)
    points(MODELED_YLDbu_acre ~ Group.1, data=d2,
           type = 'b', pch=20, col="coral4", cex = 1.5)
    legend('topright', legend = c('Observed','Simulated'),
           fill = c('aquamarine', 'coral1'))
}
dev.off()




for (sb in 1:nrow(subbasinIDs)){
    query = paste("SELECT YEAR, MON, LULC, BIOMt_ha, YLDt_ha FROM hru WHERE SUB =", subbasinIDs[sb,3])
    
    modData = sqlQuery(con, query)
    
    #close(con)
    
    #grabbing just corn
    modData <- modData[modData$LULC == 'CORN',]
    modData <- modData[modData$MON == 9,]
    
    # formating time series
    mon = formatC(modData$MON, width = 2, format = "d", flag = "0")
    modTS = as.Date(paste(modData$YEAR, mon, "01", sep="-"))                                          
    modData = data.frame("DATE" = modTS, 
                         MODELED_YLDbu_acre = (modData$YLDt_ha)*(15.92357))
    
    modData <- aggregate(modData$MODELED_YLDbu_acre, list(modData$DATE), mean)
    names(modData) <- c('DATE', 'MODELED_YLDbu_acre')
    d = merge(obsData, modData)
    marathon_pbias$SubBasin[sb] <- subbasinIDs[sb,3]
    marathon_pbias$PBias[sb] <- pbias(d$MODELED_YLDbu_acre, d$OBSERVED_YLDbu_acre)   
}
#modData <- modData[modData$DATE,]
plot(MODELED_YLDbu_acre ~ DATE, data = modData, type = 'l', ylim = c(20, 200))
lines(OBSERVED_YLDbu_acre ~ DATE, data = obsData, type = 'l', col = 'red')

plotDir = paste(projectDir, "plots", sep="/")

if (!file.exists(plotDir)) {
    dir.create(plotDir)
}
d = merge(obsData, modData)
pbs <- paste('Percent bias:', pbias(d$MODELED_YLDbu_acre, d$OBSERVED_YLDbu_acre))

pdf(paste(plotDir, "/", run, ".pdf", sep=""), width=11, height=8.5)

ylim = c(20, 250)
plot(OBSERVED_YLDbu_acre ~ DATE,
    data=d,
	type="l",
	col="aquamarine",
	ylab="Avg Yield bu/acre",
	xlab="Date",
	ylim=ylim,
	lwd=3)
lines(MODELED_YLDbu_acre ~ DATE, data=d, col="coral1", lwd=3)
title(paste('Subbasin', subbasinID, 'Corn Yield'))
legend("topright", c("Modeled", "Observed", pbs), fill=c("coral1","aquamarine", 'white'))
text(as.Date('2004-01-01'), 230, labels = paste(pbs))
dev.off()

diagn_bySubID <- ddply(diagn_df, 'SubBasin', summarise, 
                       avgSubBas_diagn = mean(diagn, na.rm = T))
diagn_byCnty <- ddply(diagn_df, 'County', summarise, 
                       avgCounty_diagn = mean(diagn, na.rm = T),
                      noOfSubBasins = length(diagn))
pdf('diagn.pdf',width = 14, height =6 )
barplot(diagn_byCnty$avgCounty_diagn[1:10], names.arg = diagn_byCnty$County[1:10])
barplot(diagn_byCnty$avgCounty_diagn[11:20], names.arg = diagn_byCnty$County[11:20])
dev.off()