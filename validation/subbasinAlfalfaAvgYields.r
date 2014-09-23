library(RODBC)
library(hydroGOF)
library(plyr)

# CHANGE THESE ###########
# SWAT project
projectDir = "G:/WRB"
# Subbasin IDs in each County
subbasinIDs <- read.csv("T:/Projects/Wisconsin_River/Code/validation/subbasins_byWRB_County.txt")
# Raw NASS data
# obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/Avg_cornGrainYlds_byCounty.csv"
obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/Avg_alfalfaYlds_byCounty.csv"
# Grabbing and formating obs for specific county
yldDat <- read.csv(obsDataFile)
yldDat$DATE <- as.Date(as.character(paste(yldDat[,1],9,1,sep ='-')))

run = "default crop parameters"
# DON'T CHANGE ANYTHING BELOW HERE #####
#outDb = paste(projectDir, "Scenarios/Default/TablesOut/SWATOutput.mdb", sep="/")
outDb = paste(projectDir, "Scenarios/defaultParameters/TablesOut/SWATOutput.mdb", sep="/")
con = odbcConnectAccess(outDb)


# to convert from ton/acre to Mg/ha
#yldDat$Value..TONS.ACRE.<- yldDat$Value..TONS.ACRE. * 2.24
counties <- unique(yldDat$County)
# cnty <- 'ADAMS'
allDat <- data.frame()
# pdf("crop_yield_validation_default_parameters_ObsPred.pdf", height=7, width=10)
 # cnty <- 'ADAMS'
for (cnty in counties){
    print(paste("Working on",cnty,"County"))
	obsData <- yldDat[yldDat$County == cnty,]
	obsData$Value..TONS.ACRE. = obsData$Value..TONS.ACRE. * 2.24
    subIDs <- subbasinIDs[toupper(subbasinIDs$CTY_NAME) == cnty,11]
    all_mod_data = data.frame()
    for (sb in subIDs){
        # sb <- 2
        print(paste("Working on subbasin",sb))
        query = paste("SELECT HRU, YEAR, MON, AREAkm2, LULC, BIOMt_ha, YLDt_ha FROM hru WHERE SUB =",
            sb,
            "AND",
            "LULC = 'ALFA'")
        modData = sqlQuery(con, query)        
        if (nrow(modData) == 0) {next}
        maxYlds = NULL
        for (hru in unique(modData$HRU)) {
            for (yr in unique(modData$YEAR)) {
                select = modData$YEAR == yr & modData$HRU == hru
                if (length(which(select) > 0)) {
                    maxYld = max(modData[select,"YLDt_ha"], na.rm=T)
                    if (maxYld > 1) {
                        maxYlds = c(maxYlds, maxYld)
                    }
                }
            }
        }
        meanYld = mean(maxYlds)
        # formating time series
        mon = formatC(modData$MON, width = 2, format = "d", flag = "0")
        modTS = as.Date(paste(modData$YEAR, mon, "01", sep="-"))                                          
        modData = data.frame("DATE" = modTS, 
            MODELED_YLDtons_ha = meanYld)
        # This aggregate() will lump the HRUs together 
#         modData <- aggregate(modData$MODELED_YLDbu_acre, list(modData$DATE), mean)
        names(modData) <- c('DATE', 'MODELED_YLDtons_ha')
        all_mod_data = rbind(all_mod_data, modData)
    }
    # d contains all hrus
    d = merge(obsData, all_mod_data)
    d2 <- aggregate(d[,6:7], list(d$DATE), mean)
    d2$County <- cnty 
    allDat <- rbind(allDat, d2)
    d2_lm <- lm(MODELED_YLDtons_ha ~ OBSERVED_YLDtons_ha, data = d2)
    ylim = range(c(d2$MODELED_YLDtons_ha, d2$OBSERVED_YLDtons_ha))
    r2 = summary(d2_lm)$adj.r.squared
    # For plotting observed v predicted
    plot(MODELED_YLDbu_acre ~ OBSERVED_YLDbu_acre, data = d2,
         ylim = ylim, xlim = ylim, pch = 20,
         main=paste(cnty,'County'), 
         ylab = 'Simulated Alfalfa (tons/ha)',
         xlab = 'Observe Alfalfa (tons/ha)')
    abline(a = 0, b = 1, col = 'red')
    mtext(paste("R2 = ", format(r2, digits=3), sep=""))
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
ylim = range(c(allDat$MODELED_YLDbu_acre, allDat$OBSERVED_YLDbu_acre))
avgAnnual <- aggregate(allDat[,2:3],list(allDat$Group.1), mean )
plot(OBSERVED_YLDbu_acre ~ Group.1, data = avgAnnual,
     type = 'b', pch = 20, col = 'blue', ylim = ylim)
points(MODELED_YLDbu_acre ~ Group.1, data = avgAnnual,type = 'b',
     pch = 20, col = 'coral1', ylim = ylim)
abline(h = mean(avgAnnual$OBSERVED_YLDbu_acre),col = 'blue')
abline(h = mean(avgAnnual$MODELED_YLDbu_acre),col = 'coral1')
legend('topright', legend = c('Observed','Simulated'),
            fill = c('blue', 'coral1'))

yldMod <- lm(MODELED_YLDbu_acre ~ OBSERVED_YLDbu_acre,data = avgAnnual)

# dev.off()











diagn_bySubID <- ddply(diagn_df, 'SubBasin', summarise, 
                       avgSubBas_diagn = mean(diagn, na.rm = T))
diagn_byCnty <- ddply(diagn_df, 'County', summarise, 
                       avgCounty_diagn = mean(diagn, na.rm = T),
                      noOfSubBasins = length(diagn))
pdf('diagn.pdf',width = 14, height =6 )
barplot(diagn_byCnty$avgCounty_diagn[1:10], names.arg = diagn_byCnty$County[1:10])
barplot(diagn_byCnty$avgCounty_diagn[11:20], names.arg = diagn_byCnty$County[11:20])
dev.off()
# for modeled data



#sb <- 1
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

plot(Value..BU.ACRE. ~ Date, data = marathon, type = 'l')
