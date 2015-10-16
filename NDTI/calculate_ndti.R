library(raster)
library(rgeos)
dir_data = "K:/NDTI/downloads"
dir_out = "K:/NDTI/minNDTI"
file_nc = "K:/NDTI/landsat_images.nc"

td = tempdir()
files_tar = list.files(dir_data, pattern=".*\\.tar\\.gz$", full.names=T)
file_dates = as.Date(substr(basename(files_tar), 10, 16), "%Y%j")

yr_counts = table(format(file_dates, "%Y"))
years = names(which(yr_counts > 4))
files_tar = files_tar[
	format(file_dates, "%Y") %in% years
]
file_dates = as.Date(substr(basename(files_tar), 10, 16), "%Y%j")

setwd(td)

for (year in years) {
	file_scene_count = paste(dir_out, "/scene_count_", year, ".tif", sep="")
	file_min_ndti = paste(dir_out, "/min_ndti_", year, ".tif", sep="")
	if (file.exists(file_scene_count) & file.exists(file_min_ndti)) {
		next
	}
	print(year)
	year = as.numeric(year)
	files_tar_yr = files_tar[
		file_dates >= as.Date(paste(year, "-01-01", sep="")) &
			file_dates <= as.Date(paste(year, "-12-31", sep=""))
	]
	for (file_tar_yr in files_tar_yr) {
		print(paste("Calculating NDTI and NDVI for", file_tar_yr))
		date = substr(basename(file_tar_yr), 10, 16)
		file_tmplt = untar(file_tar_yr, list=T, verbose=T)
		file_tmplt = file_tmplt[
			grep("sr_band3|sr_band4|sr_band5|sr_band7|cfmask\\.tif$", file_tmplt)
		]
		untar(file_tar_yr, files=file_tmplt, exdir=td)
		file_mask = file_tmplt[grep("cfmask", file_tmplt)]
		mask = getValues(raster(file_mask))
		mask[mask != 0] = NA
		# NDVI
		file_sr3 = file_tmplt[grep("sr_band3", file_tmplt)]
		sr3 = getValues(raster(file_sr3))
		sr3[is.na(mask)] = NA
		file_sr4 = file_tmplt[grep("sr_band4", file_tmplt)]
		sr4 = getValues(raster(file_sr4))
		sr4[is.na(mask)] = NA
		ndvi = (sr4 - sr3) / (sr4 + sr3)
		ndvi = setValues(raster(file_mask), ndvi)
		writeRaster(ndvi, paste("ndvi_", date, ".tif", sep=""))
		rm(list=c("sr3", "sr4", "ndvi"))
		gc()
		# NDTI
		file_sr5 = file_tmplt[grep("sr_band5", file_tmplt)]
		sr5 = getValues(raster(file_sr5))
		sr5[is.na(mask)] = NA
		file_sr7 = file_tmplt[grep("sr_band7", file_tmplt)]
		sr7 = getValues(raster(file_sr7))
		sr7[is.na(mask)] = NA
		ndti = (sr5 - sr7) / (sr5 + sr7)
		ndti = setValues(raster(file_mask), ndti)
		writeRaster(ndti, paste("ndti_", date, ".tif", sep=""))
		file.remove(file_tmplt)
		rm(list=c("mask", "sr5", "sr7", "ndti"))
		gc()
	}
	files_nd = list.files(
		pattern=paste("ndti_", year, "|ndvi_", year, sep=""))	
	i = 0
	for (file_nd in files_nd) {
		i = i + 1
		e = as(extent(raster(file_nd)), "SpatialPolygons")
		if (i == 1) {
			intersection = e
		} else {
			intersection = gIntersection(e, intersection)
		}
	}
	e = extent(intersection)
	for (file_nd in files_nd) {
		print(paste("Cropping", file_nd))
		writeRaster(
			crop(
				raster(file_nd),
				extent(intersection)
			),
			file_nd,
			overwrite=T
		)
	}
	grid_size = length(raster(files_nd[1]))
	
	print("NDTI brick")
	files_ndti = list.files(pattern=paste("ndti_", year, sep=""))
	ndti_overlay = brick(stack(files_ndti))
	
	print("NDVI brick")
	files_ndvi = list.files(pattern=paste("ndvi_", year, sep=""))
	ndvi_overlay = brick(stack(files_ndvi))
	
#	print("NDVI mask")
#	ndvi_overlay = calc(
#		ndvi_overlay,
#		function(x) {
#			x[x < -1 | x > 0.3] = NA
#			x[!is.na(x)] = 1
#			return(x)
#		}
#	)
	
	ndvi_overlay[ndvi_overlay < -1] = NA
	ndvi_overlay[ndvi_overlay > 0.3] = NA
	ndvi_overlay[!is.na(ndvi_overlay)] = 1
	
	print("Omit NDTI pixels where NDVI exceeds 0.3")
	ndti_overlay = ndti_overlay * ndvi_overlay
	rm(list="ndvi_overlay")
	
	print("Counting scenes per pixel")
	scene_count = calc(
		ndti_overlay,
		function (x) {
			return(sum(!is.na(x)))
		}
	)
	
	print("Writing scene count raster")
	writeRaster(
		scene_count,
		file_scene_count,
		overwrite=T
	)
	
	print("Creating scene count mask")
#	scene_count = calc(
#		scene_count,
#		function(x) {
#			if (x < 4) {
#				return(NA)
#			} else {
#				return(1)
#			}
#		}
#	)
	
	scene_count[scene_count < 4] = NA
	scene_count[scene_count >= 4] = 1

	print("Masking out pixels with fewer than 4 scenes")
	ndti_overlay = ndti_overlay * scene_count
	rm(list="scene_count")
	
	print("Calculating minNDTI raster")
	min_ndti = calc(
		ndti_overlay,
		function(x) {
			if (all(is.na(x))) {
				return(NA)
			} else {
				return(min(x, na.rm=T))
			}
		}
	)
	rm(list="ndti_overlay")
	
	print("Writing minNDTI raster")
	writeRaster(
		min_ndti,
		file_min_ndti,
		overwrite=T
	)
	rm(list="min_ndti")
	file.remove(files_nd)
	gc()
	tmp_files = list.files(rasterOptions()$tmpdir, full.names=T)
	file.remove(tmp_files)
	gc()
}


