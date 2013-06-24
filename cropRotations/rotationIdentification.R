writeDomCropTable = T
dominantCropsFigure = F
countUniqueRotations = F
veggiesLumped = T
fiveYrRotBool = F
crosstabFiles = c("C:/Users/benekt/Desktop/WRB_data/R_Code/crosstab_dtrsq/wrb_cdl_2008.csv"
                  , "C:/Users/benekt/Desktop/WRB_data/R_Code/crosstab_dtrsq/wrb_cdl_2009.csv"
                  , "C:/Users/benekt/Desktop/WRB_data/R_Code/crosstab_dtrsq/wrb_cdl_2010.csv"
                  , "C:/Users/benekt/Desktop/WRB_data/R_Code/crosstab_dtrsq/wrb_cdl_2011.csv"
                  , "C:/Users/benekt/Desktop/WRB_data/R_Code/crosstab_dtrsq/wrb_cdl_2012.csv")
rotationTableFile = "C:/Users/benekt/Desktop/WRB_data/R_Code/lumpedrotations_sweetcorn_062413.csv"
legendFile = "C:/Users/benekt/Desktop/WRB_data/R_Code/cdlAttributeTable.csv"
legend = read.csv(legendFile)
legend$CLASS_NAME = as.character(legend$CLASS_NAME)
nonRotCropVals = c(0, 63:180, 182:203)
threshold = 5


referenceCrop = function (x, cropCodes, legend, collapse=T, threshold) {
    if (length(which(x == max(x,na.rm=T))) > 1) {
        if (max(x,na.rm=T) > threshold) {
            cropCode = cropCodes[which(x == max(x,na.rm=T))]
            crop = legend$CLASS_NAME[legend$VALUE %in% cropCode]
            if (collapse) {
                crop = paste(crop, collapse=";")
            }
        } else {
            crop = NA
        }
    } else {
        if (max(x,na.rm=T) > threshold) {
            cropCode = cropCodes[which(x == max(x,na.rm=T))]
            crop = legend$CLASS_NAME[legend$VALUE == cropCode]
        }
    }
    return(crop=as.character(crop))
}



lumpCropTypes = function (crosstab,cropCodes,legend, veggiesLumped) {
    legend = legend[c("VALUE", "CLASS_NAME")]
    corn = 1
    sweetCorn = 12
    alfalfa = c(4, 21, 22, 23, 24, 25, 27, 28, 29, 30, 36, 39, 58, 205)
    pasture = c(37, 62, 181)
    soybeans = 5
    potatoes = 43
    dryBeans = 42
    veggies = c(49,50,53,206,216)
    mainCrops = c(corn,sweetCorn,alfalfa,pasture,soybeans,potatoes,dryBeans,veggies)
    other = cropCodes[-which(cropCodes %in% mainCrops)]
    cropLbl = c("Alfalfa", "Pasture", "Veggies", "Other")
    crops = list(alfalfa, pasture, veggies, other)   
    i = 0
	for (crop in crops) {
		i = i + 1
		cropCols = which(cropCodes %in% crop)
		cropCol = cropCols[1]
		legend$CLASS_NAME[cropCodes[cropCol] + 1] = cropLbl[i]
		if (length(cropCols) > 1) {
			crosstab[,cropCol] = rowSums(crosstab[,cropCols], na.rm=T)
			crosstab = crosstab[,-cropCols[-1]]
			cropCodes = cropCodes[-cropCols[-1]]
		} else if (length(cropCols) == 1) {
			crosstab[,cropCol] = crosstab[,cropCols]
		}
	}
    return(list(crosstab=crosstab,cropCodes=cropCodes,legend=legend))
}



##  Output a table of crop rotations
domCropsAllYrs = NULL
if (writeDomCropTable) {
    for (crosstabFile in crosstabFiles) {
        print(crosstabFile)
        crosstab = read.csv(crosstabFile)
        crosstabMeta = crosstab[,c(1,2,ncol(crosstab))]
        crosstab = crosstab[,-c(1,2,ncol(crosstab))]
        cropCodes = gsub("W_wrb_cdl_[0-9]*_", "", names(crosstab))
        if (length(which(cropCodes == "NoData"))) {
            cropCodes[which(cropCodes == "NoData")] = "255"
        }
        cropCodes = as.integer(cropCodes)
        crosstab = as.matrix(crosstab)  
        cropCols = which(!(cropCodes %in% nonRotCropVals))
        crosstab = crosstab[,cropCols]
        cropCodes = cropCodes[cropCols]
        lumped = lumpCropTypes(crosstab,cropCodes,legend,veggiesLumped=veggiesLumped)
        domCrops = apply(lumped$crosstab, 1
                         , referenceCrop
                         , cropCodes=lumped$cropCodes
                         , legend=lumped$legend
                         , collapse=T
                         , threshold=threshold)
        domCrops = unlist(domCrops)
        print(length(domCrops))
        domCropsAllYrs = cbind(domCropsAllYrs, domCrops)
    }
    mainCrops = c("Corn","Sweet Corn","Alfalfa", "Pasture", "Soybeans","Potatoes"
                  ,"Dry Beans", "Veggies", "Other")
    mainAbbr = c("Co","Sw","Al","Pa","So","Po","Db","Vg","Ot")
    i = 0
    for (crop in mainCrops) {
        i = i + 1
        domCropsAllYrs[which(domCropsAllYrs == crop)] = mainAbbr[i]
    }
    domCropsAllYrs = cbind(crosstabMeta, domCropsAllYrs)
    names(domCropsAllYrs) = c("ID", "cellCount", "naCells", paste("yr", 2008:2012, sep=""))
    write.csv(domCropsAllYrs, file=rotationTableFile, row.names=F)
}






