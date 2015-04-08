# library(rgdal)
# library(rgeos)
# library(raster)

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands"
# file_wetland_parm = paste("wetland_parameters_",strt,"to",stp,".csv",sep='')
# 
# processes to start
procs = 4
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
C:/Users/evansdm
rscript = "C:/Users/evansdm/Documents/R/R-3.1.1/bin/x64/Rscript.exe"
wtlndsscrpt = "C:/Users/evansdm/Documents/Code/wetlands/calculate_wetland_parameters.r"
wtlnds_cleanup = "C:/Users/evansdm/Documents/Code/wetlands/run_wetlands_cleanup.r"
strt_stps = round(seq(1,337+337/procs,by=337/procs))
cmd = NULL
for (i in 1:(length(strt_stps)-1)){
	strt = strt_stps[i]
	stp = strt_stps[i+1]-1
	ln = paste(paste("start", rscript, wtlndsscrpt, strt, stp))
	cmd = c(cmd, ln)
	print(ln)
}

# ln1 = paste("start", rscript, wtlndsscrpt, 1, 84)
# ln2 = paste("start", rscript, wtlndsscrpt, 85, 168)
# ln3 = paste("start", rscript, wtlndsscrpt, 169, 252)
# ln4 = paste("start ", rscript, wtlndsscrpt, 253, 337)

tmpf = tempfile(fileext=".bat")
writeLines(
	paste(cmd, collapse="\n"),
	tmpf
)

system(tmpf, wait=T)
cmd_clean = paste(rscript,wtlnds_cleanup)
system(cmd_clean)
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
