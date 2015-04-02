options(stringsAsFactors = F)
bio_calib <- read.csv("D:/bio_e_calibration/bio_e_calibration.csv")
dir_out = "D:/bio_e_calibration"
# formatting Observed Data
obsData_full <- read.csv("D:/upDated_NASSstats_30092014.csv", stringsAsFactors=F)

setwd(dir_out)
obsData_full$DATE <- as.Date(as.character(paste(obsData_full[,'Year'],1,1,sep ='-')))
obsData_full <- obsData_full[obsData_full$DATE>='2002-01-01',]
cropTab <- data.frame( 
    CropKeySWAT = c('CORN', 'ALFA','SOYB'),
    CropKeyNASS = c('CORN, GRAIN - YIELD, MEASURED IN BU / ACRE',
                  'HAY & HAYLAGE, ALFALFA - YIELD, MEASURED IN TONS / ACRE, DRY BASIS',
                  'SOYBEANS - YIELD, MEASURED IN BU / ACRE'),
    stringsAsFactors=F
)
obsData_full$YLD <- NA
for (crop in c('CORN', 'ALFA','SOYB')){
    cropInd <- which(obsData_full$Data.Item == cropTab[which(cropTab$CropKeySWAT == crop),'CropKeyNASS']) 
    print(paste("There are",length(cropInd),'entries for',crop))
    if (crop == 'CORN'){
        obsData_full$YLD[cropInd] <- (obsData_full$Value[cropInd] *56)*(2.471/2205)*(0.845)
    } else if (crop == 'SOYB'){
        obsData_full$YLD[cropInd] <- (obsData_full$Value[cropInd] *60)*(2.471/2205)*(0.875)
    } else if (crop == 'ALFA'){
        obsData_full$YLD[cropInd] <- (obsData_full$Value[cropInd] * 2.24) * (0.87)
    }
}
        
# crop <- 'ALFA'

cropCalibBioE <- data.frame()
for (crop in unique(bio_calib$crop)){
#     if (crop == 'ALFA'){next}
    cropInd <- which(obsData_full$Data.Item == cropTab[which(cropTab$CropKeySWAT == crop),'CropKeyNASS'])
    simDat <- bio_calib[which(bio_calib$crop == crop),]
    obsDat <- obsData_full[cropInd,]
    
    meanObsYld <- mean(obsDat$YLD)
    
    #Linear model predicting bio e
    bioeMod <- lm(bio_e ~ yld, data = simDat)
    # fitted bio e
    predBioE <- predict(bioeMod, newdata = data.frame(yld = meanObsYld))
    cropCalibBioE <- rbind(cropCalibBioE, c(crop, meanObsYld, predBioE))
    
    pdf(paste(crop, '_Bio_E_Calibration_Curve.pdf'))
    # plotting Calibration curve
    plot(yld ~ bio_e, data = simDat,#main = paste(crop, 'yield'),
         ylab = 'Tons per hectare',
         xlab = 'Bio E Parameter',
         type = 'b', cex = 2,
         pch = 20,
         col = 'coral1')
    title(main = paste(crop,'Bio_E Calibration Curve'))
    abline(h = meanObsYld, cex = 2, col = 'aquamarine')
    
    points(y = meanObsYld, x = predBioE, pch = 20, cex = 1.5)
    text(paste('Best-fit Bio E = ', 
               round(predBioE, digits = 2)), y = meanObsYld, x = predBioE, pos = 4)

    legend('topleft', legend = c('Observed','Simulated'),
        fill = c('aquamarine', 'coral1'))

    dev.off()
}
names(cropCalibBioE) <- c('Crop', 'MeanObsYield','FittedBioE')
write.csv(cropCalibBioE,'Bio_E_Calibration_Report.csv', row.names = F)
