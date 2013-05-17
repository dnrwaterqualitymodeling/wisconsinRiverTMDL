library(foreign)
landCoverDir = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover"
agAreaFile = paste(landCoverDir, "/nass2011amededByWwi_summarizedByCounty.dbf", sep="")
outPercAgFile = paste(landCoverDir, "/nass2011amededByWwi_percAgByCounty.csv", sep="")
metadataFile = paste(landCoverDir, "/readme.txt", sep="")

agArea = read.dbf(agAreaFile)

colNames = names(agArea)
# CDL Values for non-agriculture
nonagVals = c(0,63:65,81:83,87,88,111,112,121:124,131,141:143,152,171,190,195)
nonagCols = paste("VALUE", nonagVals, sep="_")
nonagCols = nonagCols[which(nonagCols %in% colNames)]
agCols = names(agArea)[which(!(names(agArea) %in% nonagCols))]
agCols = agCols[-1]
agCols = agCols[which(agCols %in% colNames)]

nonag = agArea[,nonagCols]
ag = agArea[,agCols]

percAg = rowSums(ag) / (rowSums(nonag) + rowSums(ag))
percAg = percAg * 100

percAgTable = data.frame(county=agArea$COUNTY_NAM, percAg=percAg)
write.csv(percAgTable, file=outPercAgFile, row.names=FALSE)

metadata = paste(basename(outPercAgFile), "was created/changed by Aaron Ruesch on", Sys.time()
	, "using \"code/landCover/aggregateSummariesOfAgLandPerCounty.r\". Data is in percent\n\n"
	, sep=" ")
cat(metadata, file=metadataFile, append=TRUE)

