library(rgdal)
library(rgeos)
library(raster)

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands"
# file_wetland_parm = paste("wetland_parameters_",strt,"to",stp,".csv",sep='')
# 
gd_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets"
# orginal dem
# dem <- raster(paste(gd_dir, 'DEM','wrb_dem.tif',sep ='/'))
# filled dem
# dem_fl <- raster(paste(gd_dir, 'DEM','wrb_filled.tif',sep ='/'))

# sinks
# sinks = raster(paste(gd_dir, "DEM", "sinks_10m.tif",sep="/"))

dir_out_maps = "wetland_maps"
dir_out_files = "wetland_files"

gdal_path = "C:/Program Files/GDAL"

rscript = "~/Documents/R/R-3.1.1/bin/x64/Rscript.exe"
wtlndsscrpt = "~/Documents/Code/wetlands/calculate_wetland_parameters.r"

ln1 = paste("start", rscript, wtlndsscrpt, 1, 84)
ln2 = paste("start", rscript, wtlndsscrpt, 85, 168)
ln3 = paste("start", rscript, wtlndsscrpt, 169, 252)
ln4 = paste("start ", rscript, wtlndsscrpt, 253, 337)

tmpf = tempfile(fileext=".bat")
writeLines(
	paste(ln1, ln2, ln3, ln4, sep="\n"),
	# paste(ln4),
	tmpf
)

system(tmpf, wait=T)

## Creating layers of max and normal surface area
# if (file.exists(gdal_path)){
	# gdalbuildvrt = "gdalbuildvrt"

	# for (lvl in c("Max", "Normal")){
		# file_list = list.files(
			# paste(wd, "/", dir_out_files, sep=''),
			# pattern = lvl,
			# full.names=T)
		# tmpf_tif_list = tempfile("tif_list_", fileext='.txt')
		# write(paste(file_list, sep='\n'), tmpf_tif_list)
		
		# outfile = paste(lvl, "_wetland_surface_area.vrt",sep='')
		
		# tmpf = tempfile("buildvrt_", fileext = ".bat")
		# cd = paste("cd", gdal_path)

		# cmd = paste(
			# gdalbuildvrt,
			# '-vrtnodata "-9999"',
			# '-srcnodata "-9999"',
			# "-input_file_list",
			# tmpf_tif_list,
			# paste(wd,
				# outfile,
				# sep='/')
		# )
		# lnes = paste(cd, cmd, sep='\n')
		# write(lnes, tmpf)
		# system(tmpf)
	# }
# } else {
	# print("GDAL not found on C drive")
# }
