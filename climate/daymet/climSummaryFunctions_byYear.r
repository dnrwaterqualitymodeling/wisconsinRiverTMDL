library(ncdf)
library(rgdal)
library(raster)

getTileExtents <- function(tileCodes, ncDir) {
	tileExtents <- list()
	for (tileCode in tileCodes) {
		ncFile <- paste(ncDir, "/prcp_", tileCode, "_1980.nc", sep="")
		nc <- open.ncdf(ncFile)
		res <- abs(nc$dim$x$vals[2] - nc$dim$x$vals[1])
		yRange = range(nc$dim$y$vals, na.rm=TRUE) + c(-res/2, res/2)
		xRange = range(nc$dim$x$vals, na.rm=TRUE) + c(-res/2, res/2)
		close.ncdf(nc)
		e <- extent(c(xRange, yRange))
		tileExtents[as.character(tileCode)] <- e
	}
	return(tileExtents)
}

getNetcdfData <- function(tileCodes, ncDir, yr, var) {
	d <- NULL
	for (i in 1:length(tileCodes)) {
		ncFile <- paste(ncDir, "/", var, "_", tileCodes[i], "_", yr, ".nc", sep="")
		nc <- open.ncdf(ncFile, readunlim=FALSE)
		td <- get.var.ncdf(nc, var)
		td <- aperm(td, c(2,1,3))
		close.ncdf(nc)
		d[[i]] <- td
	}
	return(d)
}


findUpstream <- function(huc16Id, watersheds, shedPx, ids, idPx, lookupTable, gridCoords, topologyTable) {
	cellSize <- res(watersheds)[1]
	upstrSheds <- huc16Id
	tos <- huc16Id
	end <- FALSE
	i <- 0
	while (!end) {
		i <- i + 1
		froms <- topologyTable[topologyTable[,2] %in% tos, 1]
		if (length(froms) > 0) {
			upstrSheds <- c(upstrSheds, froms)
		} else {
			end <- TRUE
		}
		tos <- froms
		if (i > 1000) {
			stop("Circular topology")
		}
	}
	upstrSheds <- upstrSheds[which(!(upstrSheds == 999999))]
	upstrIds <- unique(lookupTable$VALUE[lookupTable$CATCHID %in% upstrSheds])
	
	# Trim watersheds
	pxCoords <- gridCoords[(shedPx %in% upstrSheds) | (idPx %in% upstrIds), ]
	if (length(pxCoords) == 2) {
		xRange <- pxCoords[[1]] + c(-cellSize/2, cellSize/2)
		yRange <- pxCoords[[2]] + c(-cellSize/2, cellSize/2)
	} else {
		xRange <- range(pxCoords[,1], na.rm=TRUE) + c(-cellSize/2, cellSize/2)
		yRange <- range(pxCoords[,2], na.rm=TRUE) + c(-cellSize/2, cellSize/2)
	}
	e <- extent(c(xRange, yRange))
	eWI <- extent(watersheds)

	dxmin <- e@xmin - eWI@xmin
	dymin <- eWI@ymax - e@ymax
	h <- e@ymax - e@ymin
	w <- e@xmax - e@xmin
	
	colMin <- (dxmin / cellSize) + 1
	colMax <- colMin + (w / cellSize) - 1
	rowMin <- (dymin / cellSize) + 1
	rowMax <- rowMin + (h / cellSize) - 1
	# Align geometry
	idPx <- matrix(idPx, nrow=watersheds@nrows, ncol=watersheds@ncols, byrow=TRUE)
	shedPx <- matrix(shedPx, nrow=watersheds@nrows, ncol=watersheds@ncols, byrow=TRUE)
	idPxCrop <- idPx[rowMin:rowMax,colMin:colMax]
	shedPxCrop <- shedPx[rowMin:rowMax,colMin:colMax]
	dim(idPxCrop) <- c((h / cellSize) * (w / cellSize))
	dim(shedPxCrop) <- c((h / cellSize) * (w / cellSize))
	# Mask grids
	shedPxCrop[!(shedPxCrop %in% upstrSheds)] <- NA
	idPxCrop[!(idPxCrop %in% upstrIds)] <- NA
	mask <- shedPxCrop
	mask[!(is.na(idPxCrop) & is.na(shedPxCrop))] <- 1
	mask <- matrix(mask, nrow=(h/cellSize), ncol=(w/cellSize), byrow=FALSE)
	crs = CRS("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
	mask <- raster(mask, xmn=e@xmin, xmx=e@xmax, ymn=e@ymin, ymx=e@ymax, crs=crs)
	return(mask)
}

getOverlayingTiles <- function(trimmed, tileExtents) {
	intersections <- list()
	tileCodes <- names(tileExtents)
	options(warn=-1)
	for (tileCode in tileCodes) {
		intersections[tileCode] <- intersect(tileExtents[tileCode][[1]], trimmed)
	}
	options(warn=0)
	return(intersections)
}

traceMaxClim <- function(d, trimmed, intersections, tileExtents, var, yr) {
	###
	# Build a time template
	ncTemplateFile <- paste(ncDir, "/", var, "_", names(intersections[1]), "_", yr, ".nc", sep="")
	ncTemplate <- open.ncdf(ncTemplateFile, readunlim=FALSE)
	res <- abs(ncTemplate$dim$x$vals[2] - ncTemplate$dim$x$vals[1])
	origin <- strsplit(ncTemplate$dim$time$units, " ")[[1]][3]
	times <- as.POSIXlt(ncTemplate$dim$time$vals * 24 * 60 * 60, tz = "UTC", origin=origin)
	close.ncdf(ncTemplate)
	mo <- times$mo + 1
	##
	mosaic <- NULL
	intTileCodes <- names(intersections)
	for (intTileCode in intTileCodes) {
		# print(intTileCode)
		bbox <- tileExtents[[intTileCode]]
		intrsct <- intersections[[as.character(intTileCode)]]
		
		dxmin <- intrsct@xmin - bbox@xmin
		dymin <- bbox@ymax - intrsct@ymax
		h <- intrsct@ymax - intrsct@ymin
		w <- intrsct@xmax - intrsct@xmin
		
		colMin <- (dxmin / res) + 1
		colMax <- colMin + (w / res) - 1
		rowMin <- (dymin / res) + 1
		rowMax <- rowMin + (h / res) - 1
		# Align geometry
		tileInd <- which(names(tileExtents) == intTileCode)
		td <- d[[tileInd]][rowMin:rowMax,colMin:colMax,]
		dim(td) <- c((h / res) * (w / res), length(times))
		##
		# calculate bounding box
		crs = CRS("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
		cropped <- crop(trimmed, intrsct)
		ncMask <- td[!is.na(cropped@data@values),]
		if (!all(is.na(cropped@data@values))) {
			mosaic <- rbind(mosaic, ncMask)
		}
	}
	shedAvg <- apply(mosaic, MARGIN=2, FUN=mean, na.rm=TRUE)
	shedTable <- data.frame(yr = times$year + 1900
		, mo = times$mo + 1
		, day = times$mday
		, times=times
		, shedAvg = shedAvg
	)
	return(shedTable)
}


aggMaxClim <- function(shedAvg) {
	maxClim <- aggregate(shedAvg["shedAvg"], list(shedAvg$mo, shedAvg$yr), max, na.rm=TRUE)
	sumClim <- aggregate(shedAvg["shedAvg"], list(shedAvg$mo, shedAvg$yr), sum, na.rm=TRUE)
	avgClim <- aggregate(shedAvg["shedAvg"], list(shedAvg$mo, shedAvg$yr), mean, na.rm=TRUE)
	all <- data.frame(yr=maxClim[,2]
		, mo=maxClim[,1]
		, time=as.POSIXct(paste(maxClim[,2], "-", maxClim[,1], "-01", sep=""), tz="UTC")
		, maxClim=maxClim[,3]
		, sumClim=sumClim[,3]
		, avgClim=avgClim[,3]
	)
	return(all)
}


