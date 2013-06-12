# args = commandArgs(trailingOnly = TRUE)
# args = gsub("\\\\", "/", args)
# print(args)
# 
# rasterFolder = args[1]
# zones = args[2]
# outputFolder = args[3]
# tempRDir = args[4]
# runID = args[5]
# zoneType = args[6]
# rasterType=args[7]
rasterFolder = "G:/NASS/cropRotation/resampledNASS_dtrsq"
zones = "G:/NASS/cropRotation/dtrsq.img"
outputFolder = "G:/NASS/cropRotation/crosstab_dtrsq"
tempRDir = "G:/TEMP"
runID = "cr"
zoneType = "W"
rasterType = "img"

allRasters=T     #if you put T, then data for all rasters in rasterFolder will be tabulated
##################################################################################
###################    Testing inputs  ############################################
#An error message for the next few lines of code means that you set one of the folder paths incorrectly

startRaster = 1

library(raster)
library(rgdal)
library(foreign)

setwd(rasterFolder)
rasters=list.files(path = rasterFolder, pattern = paste(rasterType, "$", sep=""))

################################

rasterOptions(tmpdir=tempRDir, timer=FALSE)
#need to set temp area with larger processing area, could also potentially 
#use removeTmpFiles() to get rid of temp files between each run, but then we would have to remake the zone raster each time- 
#which would probably be okay, maybe we should do this to save on memeory

startTime = proc.time()

zoneRaster=raster(zones)
finalData=freq(zoneRaster)
if (length(which(is.na(finalData[,1]))) > 0) {
    finalData=finalData[-which(is.na(finalData[,1])),]
}
colnames(finalData)=c("CATCHID", "cellCount")


errors=FALSE

for (r in startRaster:length(rasters)){
	loopStartTime=proc.time()
	zoneRaster=raster(zones)
	valueRaster=raster(rasters[r])
	valueCropped=crop(valueRaster, zoneRaster)
	zoneCropped=crop(zoneRaster, valueCropped)
	if (all(extent(valueCropped)==extent(zoneCropped)
            , res(valueCropped)==res(zoneCropped)
            , projection(valueCropped)==projection(zoneCropped)))
        {
		tabData=crosstab(zoneCropped, valueCropped, digits=0, long=FALSE)
		colnames(tabData)=paste(zoneType, "_", strsplit(x=rasters[r], "\\.")[[1]][1], "_",colnames(tabData), sep="")
		tabDataNA=cbind(tabData,apply(tabData[, 1:dim(tabData)[2]], 1, sum))
		naCol=paste(zoneType, "_", strsplit(x=rasters[r], "\\.")[[1]][1], "_NoData", sep="")
		colnames(tabDataNA)[ncol(tabDataNA)]=naCol
		CATCHID=as.numeric(rownames(tabDataNA))
		variableData=cbind(CATCHID, tabDataNA)
	    outData=merge(finalData, variableData, by.x ="CATCHID", by.y ="CATCHID" , all.x = TRUE,sort=FALSE)
		outData[,which(colnames(outData)==naCol)]=outData$cellCount-outData[,which(colnames(outData)==naCol)]
		tableName=paste(outputFolder, gsub(rasterType, "csv", basename(rasters[r])), sep="/")
		write.csv(outData, tableName, row.names=FALSE, quote=FALSE, na="NA")
	} else {
		if (errors==FALSE) {
			errorList=data.frame(cbind(rasters[r]
                                       , extent(valueCropped)==extent(zoneCropped)
                                       , (res(valueCropped)==res(zoneCropped))[1]
                                       , projection(valueCropped)==projection(zoneCropped)))
			colnames(errorList)=c("raster", "extent", "resolution", "projection")
			errorList$raster=as.character(errorList$raster)
			errors=TRUE
		} else {
			errorList=rbind(errorList
                            , c(rasters[r]
                                , extent(valueRaster)==extent(zoneRaster)
                                , (res(valueRaster)==res(zoneRaster))[1]
                                , projection(valueRaster)==projection(zoneRaster)))
		}
	}
	removeTmpFiles()	
	loopTime=as.double(((proc.time() - loopStartTime)[3] / 60))
	print (paste(sub(pattern=rasterType, replacement="", x=rasters[r]), "processed in", loopTime, "minutes"))
}

if (errors==TRUE){
  errorTableName=paste(outputFolder, paste("errors", runID, ".txt", sep=""), sep="\\")
  write.csv(errorList, errorTableName, row.names=FALSE, quote=FALSE, na="NA")
  print ("Some rasters not processed due to errors")
  } else {
  print ("All rasters processed")
 }
  
minutesElapsed = as.double(((proc.time() - startTime)[3] / 60))
print (paste("Total process took", minutesElapsed, "minutes"))
