library(RODBC)
library(hydroGOF)
library(plyr)
library(xlsx)

# CHANGE THESE ###########
yldCalib <- function(crop, units, outDb){
    units <- 'metric'
# SWAT project
projectDir = "H:/WRB"
# Subbasin IDs in each County
subbasinIDs <- read.csv("T:/Projects/Wisconsin_River/Code/validation/subbasins_byWRB_County.txt")
# Raw NASS data

obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/NASS_Crop_Yields/NASS_cropyields_original.xlsx"
# Grabbing and formating obs for specific county
yldDat <- read.xlsx2(obsDataFile, 1)
yldDat$DATE <- as.Date(as.character(paste(yldDat[,1],8,1,sep ='-')))
#

run = "default crop parameters"
outDb = paste(projectDir, "Scenarios/defaultParameters/TablesOut/SWATOutput.mdb", sep="/")
con = odbcConnectAccess(outDb)


# to convert from ton/acre to Mg/ha
#yldDat$Value..TONS.ACRE.<- yldDat$Value..TONS.ACRE. * 2.24
counties <- unique(yldDat$COUNTY)
# cnty <- 'RICHLAND'
allDat <- data.frame()
all_cnty_data = data.frame()
pdf('AlfalfaYield_Haylage_t_ha.pdf')
for (cnty in counties){
    print(paste("Working on",cnty,"County"))
	obsData <- yldDat[yldDat$COUNTY == cnty,]
	obsData$YEILD = obsData$YEILD * 2.24170231
    subIDs <- subbasinIDs[toupper(subbasinIDs$CTY_NAME) == cnty, 11]
#     all_mod_data = data.frame()
    cnty_SWAT_data = data.frame()
    for (sb in subIDs){
#          sb <- 2
        print(paste("Working on subbasin",sb))
        query = paste("SELECT HRU, YEAR, MON, AREAkm2, LULC, BIOMt_ha, YLDt_ha FROM hru WHERE SUB =", sb,"AND LULC = 'ALFA'")
        modData = sqlQuery(con, query)        
        if (nrow(modData) == 0) {next}
        maxYlds = data.frame()
        for (hru in unique(modData$HRU)) {
            for (yr in unique(modData$YEAR)) {
                select = modData$YEAR == yr & modData$HRU == hru
                if (length(which(select) > 0)) {
                    maxYld = max(modData[select,"YLDt_ha"], na.rm=T)
                    if (maxYld > 1) {
                        maxYlds = rbind(maxYlds, c(yr, hru, maxYld))
                    }
                }
            }
        }
        names(maxYlds) = c("YEAR", "HRU", "MAX_YLD")
        maxYlds$County <- cnty
        maxYlds$SubBasin <- sb
        cnty_SWAT_data <- rbind(cnty_SWAT_data, maxYlds)   
    }
    avgSub <- aggregate(cnty_SWAT_data['MAX_YLD'], list(cnty_SWAT_data[,'YEAR']), mean)
    names(avgSub) <- c('YEAR', 'MAX_YLD')
    avgSub$County <- cnty
    avgSub$DATE = as.Date(paste(avgSub$YEAR, 8, "01", sep="-"))
    all_cnty_data <- rbind(all_cnty_data, avgSub)
    ylim = c(0,11)#range(c(d2$MODELED_YLDtons_ha, d2$OBSERVED_YLDtons_ha))
    # aggregating by year
    obsData_2 <- aggregate(obsData$YEILD, list(obsData$DATE), max)
    plot(x ~ Group.1,
         data=obsData_2,
         pch = 20,
         col="aquamarine",
         type="b",
         ylim=ylim,
         ylab="Alfalfa Yield, Mg/ha",
         xlab="Date",
         cex = 1.5,
         main=paste(cnty,'County'))
    
#       For plotting a point for each HRU
#     points(MODELED_YLDbu_acre ~ DATE, data = d, col = 'coral1', pch = 20)
    points(MAX_YLD ~ DATE, data=avgSub,
           type = 'b', pch=20, col="coral1", cex = 1.5)
    legend('topright', legend = c('Observed','Simulated'),
           fill = c('aquamarine', 'coral1'))
}
dev.off()    
# }
# ylim = range(c(allDat$MODELED_YLDbu_acre, allDat$OBSERVED_YLDbu_acre))
# avgAnnual <- aggregate(allDat[,2:3],list(allDat$Group.1), mean )
# plot(OBSERVED_YLDbu_acre ~ Group.1, data = avgAnnual,
#      type = 'b', pch = 20, col = 'blue', ylim = ylim)
# points(MODELED_YLDbu_acre ~ Group.1, data = avgAnnual,type = 'b',
#      pch = 20, col = 'coral1', ylim = ylim)
# abline(h = mean(avgAnnual$OBSERVED_YLDbu_acre),col = 'blue')
# abline(h = mean(avgAnnual$MODELED_YLDbu_acre),col = 'coral1')
# legend('topright', legend = c('Observed','Simulated'),
#             fill = c('blue', 'coral1'))
# 
# yldMod <- lm(MODELED_YLDbu_acre ~ OBSERVED_YLDbu_acre,data = avgAnnual)

# dev.off()



  # d contains all hrus
#     d = merge(obsData, all_mod_data)
#     d2 <- aggregate(d[,6:7], list(d$DATE), mean)
#     d2$County <- cnty 
#     allDat <- rbind(allDat, d2)
#     d2_lm <- lm(MODELED_YLDtons_ha ~ OBSERVED_YLDtons_ha, data = d2)
#     ylim = range(c(d2$MODELED_YLDtons_ha, d2$OBSERVED_YLDtons_ha))
#     r2 = summary(d2_lm)$adj.r.squared
    # For plotting observed v predicted


#         avgCounty <- aggregate(
#         names(avgSub) <- c('YEAR','MAX_YLD')
#         avgSub$SubBasin <- sb

        #         for (yld in unique(maxYlds$YEAR)) {
#             mean(maxYlds$MAX_YLD)
#         }

#         meanYldYr = mean(maxYlds)
        # formating time series
#         mon = formatC(modData$MON, width = 2, format = "d", flag = "0")
#         modTS = as.Date(paste(modData$YEAR, mon, "01", sep="-"))                                          
#         modData = data.frame("DATE" = modTS, 
#             MODELED_YLDtons_ha = meanYld)
#         # This aggregate() will lump the HRUs together 
# #         modData <- aggregate(modData$MODELED_YLDbu_acre, list(modData$DATE), mean)
#         names(modData) <- c('DATE', 'MODELED_YLDtons_ha')
#         all_mod_data = rbind(all_mod_data, modData)

# 
# 
# 
# 
# 
# diagn_bySubID <- ddply(diagn_df, 'SubBasin', summarise, 
#                        avgSubBas_diagn = mean(diagn, na.rm = T))
# diagn_byCnty <- ddply(diagn_df, 'County', summarise, 
#                        avgCounty_diagn = mean(diagn, na.rm = T),
#                       noOfSubBasins = length(diagn))
# pdf('diagn.pdf',width = 14, height =6 )
# barplot(diagn_byCnty$avgCounty_diagn[1:10], names.arg = diagn_byCnty$County[1:10])
# barplot(diagn_byCnty$avgCounty_diagn[11:20], names.arg = diagn_byCnty$County[11:20])
# dev.off()
# # for modeled data
# 
# 
# 
# #sb <- 1
# #modData <- modData[modData$DATE,]
# plot(MODELED_YLDbu_acre ~ DATE, data = modData, type = 'l', ylim = c(20, 200))
# lines(OBSERVED_YLDbu_acre ~ DATE, data = obsData, type = 'l', col = 'red')
# 
# plotDir = paste(projectDir, "plots", sep="/")
# 
# if (!file.exists(plotDir)) {
#     dir.create(plotDir)
# }
# d = merge(obsData, modData)
# pbs <- paste('Percent bias:', pbias(d$MODELED_YLDbu_acre, d$OBSERVED_YLDbu_acre))
# 
# pdf(paste(plotDir, "/", run, ".pdf", sep=""), width=11, height=8.5)
# 
# ylim = c(20, 250)
# plot(OBSERVED_YLDbu_acre ~ DATE,
#     data=d,
# 	type="l",
# 	col="aquamarine",
# 	ylab="Avg Yield bu/acre",
# 	xlab="Date",
# 	ylim=ylim,
# 	lwd=3)
# lines(MODELED_YLDbu_acre ~ DATE, data=d, col="coral1", lwd=3)
# title(paste('Subbasin', subbasinID, 'Corn Yield'))
# legend("topright", c("Modeled", "Observed", pbs), fill=c("coral1","aquamarine", 'white'))
# text(as.Date('2004-01-01'), 230, labels = paste(pbs))
# dev.off()
# 
# plot(Value..BU.ACRE. ~ Date, data = marathon, type = 'l')
