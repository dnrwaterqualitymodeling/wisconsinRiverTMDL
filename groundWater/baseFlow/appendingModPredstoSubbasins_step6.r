library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(foreign)
library(maptools)
library(classInt)
library(RColorBrewer)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This script aggregates the huc16 model predictions up to the subbasin level,
#   making a weighted average for each subbasins.
#   It takes as input the data table containing the model predictions (exported from the analysis
#   and modeling script)as well as the lookup table linking the huc16 to each subbasin
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
wd = "C:/evans/bflow/wi_dailyValues/"
setwd(wd)
#   subbasin layer for plotting
# subbasins <- readOGR('T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro','subbasins')
# huc16 <- readOGR('D:/hydro_va/wd_hydro_va.gdb', 'WD_HYDRO_VA_CATCHMENT_AR_24K')

#   Data table containing shape areas, and model predictions
huc16data <- read.csv("data_files/alpha_baseflow_model_huc16Preds.csv")
#   to append this data to the huc16 layer
# huc16 <- merge(huc16, huc16data, by ='CATCHID')

#   Arc will not know how to deal with Inf values (areas where slope is 0) so these are
#       set to NA
huc16data[which(is.infinite(huc16data$mod_1_Pred)),11] <- NA
huc16data[which(is.nan(huc16data$mod_1_Pred)),11] <- NA
huc16data[which(is.infinite(huc16data$mod_2_Pred)),13] <- NA
huc16data[which(is.nan(huc16data$mod_2_Pred)),13] <- NA
huc16data[which(is.infinite(huc16data$mod_3_Pred)),15] <- NA
huc16data[which(is.nan(huc16data$mod_3_Pred)),15] <- NA
# Lookup table with all the huc16s within the WR basin, and the subbasin IDs
lookup <- read.csv("data_files/subbasin_catchid_lookup.txt")
## merges the subbasin ID to the huc16, removes all nonWi River hucs
WR_huc16data <- merge(huc16data, lookup, by = 'CATCHID')

# for export, only exporting model 3
sub_ExportTable <- data.frame(Subbasin = unique(lookup$SUBBASIN), 
                              alphaBflow_Preds_mod3 = NA)
for (sub in unique(WR_huc16data$SUBBASIN)){
    hucs <- WR_huc16data[which(WR_huc16data$SUBBASIN == sub),]
    totArea <- sum(hucs$SHAPE_Area)
    alphaBflw_wtdAvg <- (10^hucs$mod_3_Pred)*(hucs$SHAPE_Area/totArea)
    alphaBflw_wtdAvg <- sum(alphaBflw_wtdAvg, na.rm = T)
    sub_ExportTable[which(sub_ExportTable$Subbasin == sub),2] <- alphaBflw_wtdAvg
}
write.table(sub_ExportTable, 'data_files/alphaBflowSubbasin_lookup.csv', sep = ',', row.names = F)

##########################
# plotting
subbasins <- merge(subbasins, sub_ExportTable, by = 'Subbasin', sort = F)

pal = brewer.pal(9, "YlGnBu")
varClass <- classIntervals(subbasins@data$alphaBflow_Preds_mod3, 9,na.rm=T)
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
pdf("Wi River TMDL alpha Bflow model 3.pdf", height = 12, width = 10)
plot(subbasins)
dsn <- "T:/GIS/Statewide_Coverages/Political_Boundaries"
counties <- readOGR(dsn, "WI_Counties")
plot(counties, add = T)
plot(subbasins, col = varColr, add=T, pch=21, border = NA)
legend('bottomright', legend =newTxt, bg = 'white',
       fill = pal, cex=0.75, title = 'Alpha B-Flow')
title(main = 'Wi River TMDL Subbasins')
dev.off()







