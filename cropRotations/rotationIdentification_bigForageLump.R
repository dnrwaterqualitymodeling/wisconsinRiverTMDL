writeDomCropTable = T
dominantCropsFigure = F
countUniqueRotations = F
veggiesLumped = T
fiveYrRotBool = F
crosstabFiles = c("D:/NASS/cropRotation/crosstab_dtrsq/wrb_cdl_2008.csv"
                  , "D:/NASS/cropRotation/crosstab_dtrsq/wrb_cdl_2009.csv"
                  , "D:/NASS/cropRotation/crosstab_dtrsq/wrb_cdl_2010.csv"
                  , "D:/NASS/cropRotation/crosstab_dtrsq/wrb_cdl_2011.csv"
                  , "D:/NASS/cropRotation/crosstab_dtrsq/wrb_cdl_2012.csv")
rotationTableFile = "D:/NASS/cropRotation/rotationByQuarterSection_relumped.csv"
domCropsFigureFile = "D:/NASS/cropRotation/dominantCrops_relumped.pdf"
unqSortRotFile = "D:/NASS/cropRotation/uniquefiveYrRotations_relumped.csv"
rotationFrequencyFile = "D:/NASS/cropRotation/rotationFrequencies_relumped.csv"
legendFile = "D:/NASS/cropRotation/cdlAttributeTable.csv"
legend = read.csv(legendFile)
legend$CLASS_NAME = as.character(legend$CLASS_NAME)
nonRotCropVals = c(0, 63:180, 182:203)
referenceCrop = function (x, cropCodes, legend, collapse=T) {
    if (length(which(x == max(x,na.rm=T))) > 1) {
        if (max(x,na.rm=T) > 5) {
            cropCode = cropCodes[which(x == max(x,na.rm=T))]
            crop = legend$CLASS_NAME[legend$VALUE %in% cropCode]
            if (collapse) {
                crop = paste(crop, collapse=";")
            }
        } else {
            crop = NA
        }
    } else {
        cropCode = cropCodes[which(x == max(x,na.rm=T))]
        crop = legend$CLASS_NAME[legend$VALUE == cropCode]
    }
    return(crop=as.character(crop))
}
lumpCropTypes = function (crosstab,cropCodes,legend, veggiesLumped) {
    legend = legend[c("VALUE", "CLASS_NAME")]
    corn = c(1,12,13)
    forage = c(36,37,58,62,181,4,21:25,27:30,205)
    soybeans = 5
    potatoes = 43
    dryBeans = 42
    veggies = c(49,50,53,206,216)
    mainCrops = c(corn,forage,soybeans,potatoes,dryBeans,veggies)
    other = cropCodes[-which(cropCodes %in% mainCrops)]
    cropLbl = c("Corn", "Forage", "Other", "Veggies")
    crops = list(corn,forage,other,veggies)   
    i = 0
    for (crop in crops) {
        i = i + 1
        cropCols = which(cropCodes %in% crop)
        cropCol = cropCols[1]
        legend$CLASS_NAME[cropCodes[cropCol] + 1] = cropLbl[i]
        crosstab[,cropCol] = rowSums(crosstab[,cropCols], na.rm=T)
        crosstab = crosstab[,-cropCols[-1]]
        cropCodes = cropCodes[-cropCols[-1]]
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
                         , collapse=T)
        domCrops = unlist(domCrops)
        print(length(domCrops))
        domCropsAllYrs = cbind(domCropsAllYrs, domCrops)
    }
    if (veggiesLumped) {
        mainCrops = c("Corn","Forage","Soybeans","Potatoes"
                      ,"Dry Beans", "Veggies", "Other")
        mainAbbr = c("Co","Fo","So","Po","Db","Vg","Ot")
    } else {
        mainCrops = c("Corn","Forage","Grain","Soybeans","Dry Beans"
                      , "Potatoes", "Onions", "Cucumbers", "Peas"
                      , "Carrots", "Peppers", "Other")
        mainAbbr = c("Co","Fo","Gr","So","Db","Po","On","Cu","Pe","Ca","Pp","Ot")
    }
    i = 0
    for (crop in mainCrops) {
        i = i + 1
        domCropsAllYrs[which(domCropsAllYrs == crop)] = mainAbbr[i]
    }
    domCropsAllYrs = cbind(crosstabMeta, domCropsAllYrs)
    names(domCropsAllYrs) = c("ID", "cellCount", "naCells", paste("yr", 2008:2012, sep=""))
    if (fiveYrRotBool) {
        rotStr = apply(domCropsAllYrs[paste("yr", 2008:2012, sep="")]
                       , 1, function (x) {paste(x, collapse="-")})
        pat = "^[A-Z][a-z]-[A-Z][a-z]-[A-Z][a-z]-[A-Z][a-z]-[A-Z][a-z]$"
        fiveYrRot = grep(pat,rotStr)
        domCropsAllYrs = domCropsAllYrs[fiveYrRot,]
    }
    write.csv(domCropsAllYrs, file=rotationTableFile, row.names=F)
}
##  Output a frequency table of crops per year
if (dominantCropsFigure) {
    pdf(file=domCropsFigureFile,width=8.5,height=11)
    par(mfrow=c(3,2))
    yrs = 2008:2012
    i=0
    for (crosstabFile in crosstabFiles) {
        i = i + 1
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
                         , collapse=F)
        domCrops = unlist(domCrops)
        freq = sort(table(domCrops), decreasing=TRUE)
        barplot(freq, las=3, col=rainbow(length(freq)), main=paste("Year", yrs[i]))
    }
    dev.off()
}
# Count the number of unique rotations
domCropsAllYrs = NULL
if (countUniqueRotations) {
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
                         , collapse=T)
        domCrops = unlist(domCrops)
        print(length(domCrops))
        domCropsAllYrs = cbind(domCropsAllYrs, domCrops)
    }
    mainCrops = c("Corn","Forage","Soybeans","Potatoes","Dry Beans","Veggies", "Other")
    mainAbbr = c("Co","Fo","So","Po","Db","Vg","Ot")
    i = 0
    for (crop in mainCrops) {
        i = i + 1
        domCropsAllYrs[which(domCropsAllYrs == crop)] = mainAbbr[i]
    }
    domCropsAllYrs = cbind(crosstabMeta, domCropsAllYrs)
    names(domCropsAllYrs) = c("ID", "cellCount", "naCells", paste("yr", 2008:2012, sep=""))
    sortedRotations = apply(domCropsAllYrs[paste("yr", 2008:2012, sep="")]
                            , 1, function(x) {paste(sort(x), collapse="-")})
    rotFreq = sort(table(sortedRotations), decreasing=T)
    write.csv(rotFreq, file=rotationFrequencyFile, row.names=T)
    pat = "^[A-Z][a-z]-[A-Z][a-z]-[A-Z][a-z]-[A-Z][a-z]-[A-Z][a-z]$"
    unqSortRot = sort(unique(sortedRotations))    
    fiveYrRot = grep(pat,unqSortRot)    
    unqSortRot = unqSortRot[fiveYrRot]
    unqSortRot = unqSortRot[!is.na(unqSortRot)]
    write(unqSortRot, file=unqSortRotFile)
}




