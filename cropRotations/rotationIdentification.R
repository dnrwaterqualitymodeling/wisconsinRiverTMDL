writeDomCropTable = T
dominantCropsFigure = T
countUniqueRotations = T
veggiesLumped = T
fiveYrRotBool = T
crosstabFiles = c("G:/NASS/cropRotation/crosstab_dtrsq/2008_30m_cdls.csv"
                  , "G:/NASS/cropRotation/crosstab_dtrsq/2009_56m_cdls.csv"
                  , "G:/NASS/cropRotation/crosstab_dtrsq/2010_30m_cdls.csv"
                  , "G:/NASS/cropRotation/crosstab_dtrsq/2011_30m_cdls.csv"
                  , "G:/NASS/cropRotation/crosstab_dtrsq/2012_30m_cdls.csv")
rotationTableFile = "G:/NASS/cropRotation/rotationByQuarterSection_relumped.csv"
domCropsFigureFile = "G:/NASS/cropRotation/dominantCrops_relumped.pdf"
unqSortRotFile = "G:/NASS/cropRotation/uniquefiveYrRotations_relumped.csv"
rotationFrequencyFile = "G:/NASS/cropRotation/rotationFrequencies_relumped.csv"
legendFile = "G:/NASS/cropRotation/cdlAttributeTable.csv"
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
    corn = c(1,12,13)
    alfalfa = 36
    forage = c(37,58,62,181)
    grain = c(4,21:25,27:30,205)
    soybeans = 5
    potatoes = 43
    dryBeans = 42
    if (veggiesLumped) {
        veggies = c(49,50,53,206,216)
        mainCrops = c(corn,alfalfa,forage,grain,soybeans,potatoes,dryBeans,veggies)
        other = cropCodes[-which(cropCodes %in% mainCrops)]
        cropLbl = c("Corn", "Forage", "Grain", "Other", "Veggies")
        crops = list(corn,forage,grain,other,veggies)
    } else {
        dryBeans = 42
        potatoes = 43
        onions = 49
        cucumbers = 50
        peas = 53
        carrots = 206
        peppers = 216
        mainCrops = c(corn,forage,grain,soybeans,dryBeans,potatoes
                      ,onions,cucumbers,peas,carrots,peppers)
        other = cropCodes[-which(cropCodes %in% mainCrops)]
        cropLbl = c("Corn", "Forage", "Grain", "Other")
        list(corn,forage,grain,other)
    }   
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
        cropCodes = gsub("W_[0-9]*_[0-9]*m_cdls_", "", names(crosstab))
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
        mainCrops = c("Corn","Alfalfa","Forage","Grain","Soybeans","Potatoes"
                      ,"Dry Beans", "Veggies", "Other")
        mainAbbr = c("Co","Al","Fo","Gr","So","Po","Db","Vg","Ot")
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
        cropCodes = gsub("W_[0-9]*_[0-9]*m_cdls_", "", names(crosstab))
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
        cropCodes = gsub("W_[0-9]*_[0-9]*m_cdls_", "", names(crosstab))
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
        mainCrops = c("Corn","Alfalfa","Forage","Grain","Soybeans","Potatoes"
                      ,"Dry Beans","Veggies", "Other")
        mainAbbr = c("Co","Al","Fo","Gr","So","Po","Db","Vg","Ot")
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




