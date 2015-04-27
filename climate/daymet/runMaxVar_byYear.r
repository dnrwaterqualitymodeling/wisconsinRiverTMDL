args <- commandArgs(TRUE)
# args <- c("F:/daymet", "prcp", 1980, 1980)

library(foreign)
library(raster)

dir <- args[1]
var <- args[2]
yrs <- args[3]:args[4]

source(paste(dir, "/code/climSummaryFunctions_byYear.r", sep=""))
ncDir <- paste(dir, "/data", sep="")
outDir <- paste(dir, "/climSummaries/prcp", sep="")
if (!file.exists(outDir)) { file.create(outDir) }
topologyTableFile <- paste(dir, "/shedTopology11052012_daymetLcc.dbf", sep="")
topologyTable <- read.dbf(topologyTableFile)[c("CATCHID", "TOCATCHID")]
watersheds <- raster(paste(dir, "/shedTopology11052012_daymetLcc.tif", sep=""))
ids <- raster(paste(dir, "/id.tif", sep=""))
load(paste(dir, "/lookupTable.RData", sep=""))
load(paste(dir, "/upstrArea.RData", sep=""))
huc16Ids <- upstrArea$CATCHID[order(upstrArea$UPSTRAREA)]

tileCodes <- c(11925, 11926, 11927, 12104, 12105, 12106, 12107, 12284, 12285, 12286) 
tileExtents <- getTileExtents(tileCodes, ncDir)
shedPx <- getValues(watersheds)
idPx <- getValues(ids)
gridCoords <- coordinates(watersheds)


for (yr in yrs) {
	print(yr)
	yrDir <- paste(outDir, "/", yr, sep="")
	if (!file.exists(yrDir)) { 
		dir.create(yrDir)
	} else {
		outFiles <- paste(yrDir, "/", var, "_", huc16Ids, "_", yr, "_daily.RData", sep="")
		complete <- file.exists(outFiles)
		huc16Ids <- huc16Ids[!complete]
	}
	d <- getNetcdfData(tileCodes, ncDir, yr, var)
	for (huc16Id in huc16Ids) {
		ltm <- proc.time()
		print(paste("HUC:", huc16Id))
		ttm <- proc.time()
		trimmed <- findUpstream(huc16Id, watersheds, shedPx, ids, idPx, lookupTable, gridCoords, topologyTable)
		trimTime <- proc.time() - ttm
		intersections <- getOverlayingTiles(trimmed, tileExtents)
		all <- traceMaxClim(d, trimmed, intersections, tileExtents, var, yr)
		save(all, file = paste(yrDir, "/", var, "_", huc16Id, "_", yr, "_daily.RData", sep=""))
		print(proc.time() - ltm)
	}
}
