# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This script assesses the relationship between the alpha bflow factors and 
#   information from the value added watershed database. 
# 
# At line 150, the modeling section begins
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
library(rgdal)
library(sp)
library(raster)
library(foreign)
library(maptools)
library(classInt)
library(RColorBrewer)
# # # # # # # # # # # # # # # # # 
wd = "C:/evans/bflow/wi_dailyValues/"
setwd(wd)
# reach ids were manually added to each flow site, by taking the reachid of the closest
#   stream. This was done in ArcGIS with reachids grabbed from the WD_HYDRO_FLWLN_NTWRK_LN_24K feature class
# contains reachIDS
stream_data <- readShapePoints("streamFlowSites_v2")

# these databases were exported from file geodatabases
#   vaData contains all the data from the WD_HYDRO_VA_WATERSHED_TR_REF table
vaData <- read.dbf("va_watershed_trace.dbf")
#   this just contains data related to ecoregions
ecoregionData = read.dbf("ecoregion.dbf")
#     
########### Based on correlations, pursuing
# soil perm 2006, bedrock type sandstone and carbonate
# CN 2006, Darcy, peat and bogs, sink 1, slope, 
fieldsToJoin <- c("REACHID",
                    #bedrock depths, 1-50,50-100,100-200; %
                    #"TRW_BD_201", "TRW_BD_202",  "TRW_BD_203",
                    #soil perm adjusted for urban areas, 2001,2006,1992; in/hr *100
                    "TRW_APERM0",#"TRW_APERM9","TRW_APER_1",
                    #bedrock depth 200 to 400
                    #"TRW_BD_204",
                    #bedrock depth > 400, 600-800, 800-100
                    #"TRW_BD_205", "TRW_BD_206", "TRW_BD_207",
                    #bedrock depth 1000-1200, 1200-1400, 1400-1600
                    #"TRW_BD_208", "TRW_BD_209", "TRW_BD_210",
                    # sandstone, shale,     carbonate
                    #"TRW_BR_1", #"TRW_BR_3", "TRW_BR_2",
                    # metamorphic, igneous
                    #"TRW_BR_41", "TRW_BR_42",
                    #curve number, 2001, 06, 1992
                    "TRW_CN06", #"TRW_CN01", "TRW_CN92",
                    # groundwater potential
                    "TRW_DARCY",
                    #surf geology
                    #outwash, lacustrine sand and gravel, colluvium
                    #"TRW_QG_1",# "TRW_QG_10", "TRW_QG_11",
                    # alluvium, moraine, drift,
                    #"TRW_QG_12", "TRW_QG_13", "TRW_QG_14",
                    # water, no land: med and coarse, peat and muck
                    #"TRW_QG_16", "TRW_QG_17", "TRW_QG_18", 
                    #  ice contact(CO), loess, dune
                    #"TRW_QG_2", "TRW_QG_24", "TRW_QG_29",
                    # end moraine: fine, med, coarse
                    #"TRW_QG_3", "TRW_QG_4", "TRW_QG_5",
                    # ground moraine: fine, med, coarse
                    #"TRW_QG_6", "TRW_QG_7", "TRW_QG_8",
                    # lacustrine clay and silt
                    #"TRW_QG_9",
                    # intdrain, 1 m, 5m,        mean slope (degrees)
                    "TRW_SLOPE")#"TRW_SINK1",  #"TRW_SINK5",
# note that this merge may disrupt the relationship between feature id and 
#   the attribute table (@data slot)
stream_data <- merge(stream_data, vaData[,fieldsToJoin], by = 'REACHID')
stream_data <- merge(stream_data, ecoregionData, by = 'REACHID')
# removing Dry Run, with alpha base flow > 1
stream_data = subset(stream_data, siteName != "DRY RUN AT 190TH STREET NEAR JEWETT, WI")
#writePointShape(stream_data, "streamFlowSites_v3")
# 
huc16 <- readOGR('D:/hydro_va/wd_hydro_va.gdb', 'WD_HYDRO_VA_CATCHMENT_AR_24K')
huc16@data$REACHID <- huc16@data$CATCHID
huc16 <- merge(huc16, ecoregionData, by = 'REACHID')
huc16 <- merge(huc16, vaData[,fieldsToJoin], by = 'REACHID')
# to clean up
rm(vaData, ecoregionData)
# this rolls smaller ecoregions into largers, and removes 0s
huc16@data$C_ECO[huc16@data$C_ECO == 47] = 51
huc16@data$C_ECO[huc16@data$C_ECO == 54] = 53
huc16@data$C_ECO[huc16@data$C_ECO == 0] = NA
#writePolyShape(huc16, 'data_files/shapefiles/huc16_4_Sept')
#####
# Exploratory data analysis
pdf("Alpha_bflow_Correlations_logAlpha.pdf")
for (f in fieldsToJoin){
    r <- cor(y = stream_data@data$siteWeight,  x = stream_data@data[,f],use = "complete.obs")
    plot(stream_data@data$siteWeight ~ stream_data@data[,f],
         main = paste(f), sub = paste('Pearson\'s r =',round(r,4)))
}
dev.off()
## ## ## ## ## Function for creating correlation matrix # ## ## ## ## ##  
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use="complete.obs")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    #if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2)
}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
pdf('correlation_figure_base10LogofAlphaBaseFlow.pdf', width = 13, height = 11)
pairs(~log10(siteWeight)+TRW_SLOPE+TRW_APERM0+TRW_BR_1+TRW_BR_3 +
          TRW_CN06 + TRW_DARCY + TRW_QG_1 + TRW_SINK1 + siteLong + siteLat,
      upper.panel=panel.cor, data=stream_data@data, cex.labels = 1.25, main = expression('Log'[10]~"of (weighted) alpha"))
dev.off()

pdf("graphs_figs/independentVars_histograms.pdf")
for (f in fieldsToJoin[-1]){
    hist(stream_data@data[,f], main = paste("Histogram of", f))
}
dev.off()
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ## ## For plotting covariates # ## ## ## ## ## ## 
pal = brewer.pal(9, "YlGnBu")
varNames <- c('Ecoregion', 'Permeability',"Curve Number",
              "Darcy", 'Slope')
for (covar in 6:10){
    png(paste(varNames[covar-5],' v3.png',sep =''), width = 24,
        height = 30,units='in', bg="white", res = 300)
    varClass <- classIntervals(huc16@data[,covar], 9, na.rm=T)
    varColr <- findColours(varClass, pal)
    legTxt <- attr(attr(varColr,'table'),'dimnames')[[1]]
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
    plot(huc16, col = varColr, add=F, pch=21, border = NA)
    legend('bottomright', legend =newTxt, bg = 'white',
           fill = pal, cex=2, title = paste(varNames[covar-5]))
    dev.off()
}
# For modeling... ####################################################
# # # # # # # # # # # # # # # # # 
# This section of the script represents the modeling process. 
#   It requires the stream flow
#   sites file with the covariates slope, curve number, darcy, permeability, 
#   and ecoregions joined to it. Assumptions are assessed, and predictions made 
#   for every huc16 in the state, and thus the huc16 layer with data from 
#   each of the above covariates joined to it.
# # # # # # # # # # # # # # # # # 
# stream flow sites, with base flow and catchment attributes
        # _v3 has the covariate data already appended to it
# stream_data <- readShapePoints("streamFlowSites_v3")
stream_data@proj4string <- CRS("+init=epsg:3071")
# tossing little yellow river, barely a year of record,
stream_data <- stream_data[which(stream_data@data$siteNumber!=05403043),]
# # # # # 
# random holdback for validation
set.seed(82014)
trnIndx <- sample(nrow(stream_data@data), nrow(stream_data@data)*0.85)
training <- stream_data[trnIndx,] #n = 158
valid <- stream_data[-trnIndx,]   #n = 28
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   #   #   #   # Model 1 #   #   #   #   # #   #   #   #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
bflw.lm.1 <- lm(log10(siteWeight) ~ log(TRW_SLOPE) + 
                    TRW_CN06 + log(abs(TRW_DARCY)+1), data = training@data)
pdf('Model_1_cn_slope_darcy.pdf')
plot(bflw.lm.1$residuals~bflw.lm.1$fitted, 
     main = "Residual plot, for model with slope and cn and darcy",
     sub = 'Training data')
abline(h=0, col='red')

plot(bflw.lm.1$fitted~log10(training@data$siteWeight[-75]), 
     main = "Model 1: obs vs pred",
     sub = 'Training data')
abline(a=0,b=1, col='red')
#text(training@data$siteWeight[-75]~backTrnsfmFits, 
#     labels=training@data$siteNumber[-75], cex = 0.75)
hist(bflw.lm.1$residuals, main = "Model 1, Hist of residuals",
     sub = 'Training data')
#qqnorm(bflw.lm.1$residuals)
#   #   Validation   #   #   #   #
validPreds.1 <- predict(bflw.lm.1, newdata = valid@data)
plot(validPreds.1~log10(valid@data$siteWeight), xlab = "Observed",
     ylab = "Predicted", main = "Model 1: Observed vs Predicted",
     sub = 'Validation data')

#plot(10^validPreds.1~valid@data$siteWeight, xlab = "Observed",
#     ylab = "Predicted", main = "Model 1: Observed vs Predicted",
#     sub = 'Validation data'))
abline(a=0, b=1, col ='red')
dev.off()
#text(10^validPreds~valid@data$siteWeight, labels=valid@data$siteNumber, cex = 0.75)
#   Kinnicinnic river is an outlier?
#kinnic <- which(valid@data$siteNumber == 4087160)
trnMSE.1 <- mean((residuals(bflw.lm.1))^2)
trnRMSE.1 <- sqrt(trnMSE.1)

valResids.1  <- log10(valid@data$siteWeight) - validPreds.1
valMSE.1 <- mean(valResids.1^2)
valRMSE.1 <- sqrt(valMSE.1)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   #   #   #   # Model 2 #   #   #   #   # #   #   #   #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
bflw.lm.2 <- lm(log10(siteWeight) ~ log(TRW_SLOPE) + log(TRW_APERM0) +
                    log(abs(TRW_DARCY)+1), data = training@data)
pdf('Model_2_perm_slope_darcy.pdf')
plot(bflw.lm.2$residuals~bflw.lm.2$fitted, 
     main = "Model 1: Residual plot, for model with slope and perm and darcy",
     sub = 'Training data')
abline(h=0, col='red')

plot(bflw.lm.2$fitted ~ log10(training@data$siteWeight[-75]), 
     main = "Model 2: obs vs pred, for model with slope and perm and darcy",
     sub = 'Training data')
abline(a=0,b=1, col='red')
#text(training@data$siteWeight[-75]~backTrnsfmFits, 
#     labels=training@data$siteNumber[-75], cex = 0.75)
hist(bflw.lm.2$residuals, main = "Hist of residuals", 
     sub = 'Training data')
#qqnorm(bflw.lm.2$residuals)
#   #   Validation   #   #   #   #
validPreds.2 <- predict(bflw.lm.2, newdata = valid@data)
plot(validPreds.2~log10(valid@data$siteWeight), xlab = "Observed",
     ylab = "Predicted", main = "Model 2, Observed vs Predicted", 
     sub = 'validation data')
abline(a=0, b=1, col ='red')
dev.off()
#plot(10^validPreds.2~valid@data$siteWeight, xlab = "Observed",
#     ylab = "Predicted", main = "Observed vs Predicted")
#abline(a=0, b=1, col ='red')
#text(10^validPreds~valid@data$siteWeight, labels=valid@data$siteNumber, cex = 0.75)
#   Kinnicinnic river is an outlier?
#kinnic <- which(valid@data$siteNumber == 4087160)
trnMSE.2 <- mean((residuals(bflw.lm.2))^2)
trnRMSE.2 <- sqrt(trnMSE.2)

valResids.2  <- log10(valid@data$siteWeight) - validPreds.2
valMSE.2 <- mean(valResids.2^2)
valRMSE.2 <- sqrt(valMSE.2)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   #   #   #   # Model 3 #   #   #   #   # #   #   #   #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
stream_data@data$C_ECO[stream_data@data$C_ECO == 47] = 51
stream_data@data$C_ECO[stream_data@data$C_ECO == 54] = 53
bflw.lm.3 <- lm(log10(siteWeight) ~ log(TRW_SLOPE):factor(C_ECO) + log(TRW_APERM0),
                data = training@data)
pdf('Model_3_perm_slopexEcoregion.pdf')
plot(bflw.lm.3$residuals~bflw.lm.3$fitted, 
     main = "Model 3: Residual plot, for model with slope and perm, with ecoregions",
     sub = 'Training data')
abline(h=0, col='red')

plot(bflw.lm.3$fitted ~ log10(training@data$siteWeight[-75]), 
     main = "Model 3: obs vs pred, for model with slope and perm, with ecoregions",
     sub = 'Training data')
abline(a=0,b=1, col='red')
#text(training@data$siteWeight[-75]~backTrnsfmFits, 
#     labels=training@data$siteNumber[-75], cex = 0.75)
hist(bflw.lm.3$residuals, main = "Hist of residuals", 
     sub = 'Training data')
#qqnorm(bflw.lm.3$residuals)
#   #   Validation   #   #   #   #
validPreds.3 <- predict(bflw.lm.3, newdata = valid@data)
plot(validPreds.3~log10(valid@data$siteWeight), xlab = "Observed",
     ylab = "Predicted", main = "Model 3, Observed vs Predicted", 
     sub = 'validation data')
abline(a=0, b=1, col ='red')
dev.off()
#plot(10^validPreds.3~valid@data$siteWeight, xlab = "Observed",
#     ylab = "Predicted", main = "Observed vs Predicted")
#abline(a=0, b=1, col ='red')
#text(10^validPreds~valid@data$siteWeight, labels=valid@data$siteNumber, cex = 0.75)
#   Kinnicinnic river is an outlier?
#kinnic <- which(valid@data$siteNumber == 4087160)
trnMSE.3 <- mean((residuals(bflw.lm.3))^2)
trnRMSE.3 <- sqrt(trnMSE.3)

valResids.3  <- log10(valid@data$siteWeight) - validPreds.3
valMSE.3 <- mean(valResids.3^2)
valRMSE.3 <- sqrt(valMSE.3)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#huc16 <- shapefile('data_files/shapefiles/huc16_4_Sept')
huc16@data$mod_1_Pred <- predict(bflw.lm.1, newdata = huc16@data)
huc16@data$mod_1_SE <- predict(bflw.lm.1, newdata = huc16@data, se.fit = T)$se.fit
huc16@data$mod_2_Pred <- predict(bflw.lm.2, newdata = huc16@data)
huc16@data$mod_2_SE <- predict(bflw.lm.2, newdata = huc16@data, se.fit = T)$se.fit
huc16@data$mod_3_Pred <- predict(bflw.lm.3, newdata = huc16@data)
huc16@data$mod_3_SE <- predict(bflw.lm.3, newdata = huc16@data, se.fit = T)$se.fit
# for exporting, removing 'Inf' from preds, setting to NA
#   exporting all info from shp file. able to append this table back to the
#   shape file
write.table(huc16@data, "data_files/alpha_baseflow_model_huc16Preds.csv",
            na = "",row.names = F, sep =",")
## ## ## ## ## For plotting # ## ## ## ## ## ## 
#huc16 <- readShapePoly('data_files/shapefiles/huc16_4_Sept_wPreds')
pal = brewer.pal(9, "YlGnBu")
predNames <- c('Model 1 Prediction', "Model 1 Std Error",
               'Model 2 Prediction', "Model 2 Std Error",
               'Model 3 Prediction', "Model 3 Std Error")
for (pred in 11:16){
    png(paste(predNames[pred-10],'.png',sep =''), width = 24,
        height = 30,units='in', bg="white", res = 300)
    varClass <- classIntervals(10^(huc16@data[,pred]), 9, na.rm=T)
    varColr <- findColours(varClass, pal)
    legTxt <- attr(attr(varColr,'table'),'dimnames')[[1]]
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
    plot(huc16, col = varColr, add=F, pch=21, border = NA)
    legend('bottomright', legend =newTxt, bg = 'white',
           fill = pal, cex=2, title = paste(predNames[pred-10]))
    dev.off()
}
