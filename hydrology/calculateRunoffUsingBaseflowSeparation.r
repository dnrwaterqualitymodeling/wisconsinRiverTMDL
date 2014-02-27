wd = "T:/Projects/Wisconsin_River/GIS_Datasets/Baseflow Separation/BFlow_Directory"
setwd(wd)
sites = list.files()

outTable = data.frame()
for (site in sites) {

    bflowFile = list.files(site, pattern="out$", full.names=T)
    id = substr(bflowFile, nchar(dirname(bflowFile)) - 7, nchar(dirname(bflowFile)))
    if (length(bflowFile) == 0) {
        next
    }
    bflowTable = read.fwf(bflowFile, c(4,2,2,13,13,13,13), skip=2)
    yield = 24*60*60*((bflowTable[2:nrow(bflowTable),4] + bflowTable[1:(nrow(bflowTable)-1),4])/2)
    annYield = aggregate(yield, list(bflowTable[1:(nrow(bflowTable)-1),1]), sum)
    meanYield = mean(annYield[,2])
    bfYield = 24*60*60*((bflowTable[2:nrow(bflowTable),7] + bflowTable[1:(nrow(bflowTable)-1),7])/2)
    annBfYield = aggregate(bfYield, list(bflowTable[1:(nrow(bflowTable)-1),1]), sum)
    meanBfYield = mean(annBfYield[,2])
    outRow = data.frame(id=site, waterYield=meanYield, baseflowYield=meanBfYield, runoff=meanYield-meanBfYield)
    outTable = rbind(outTable, outRow)
    
}

write.csv(outTable, "./../runoffResults.csv", row.names=F)
