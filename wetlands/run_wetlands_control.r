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

library(rgdal)
library(rgeos)
library(raster)

# Items that need addressing:
#     1.) How are land locked ponds/lakes incorporated in these calculations?
#     Should those areas be subtracted from each subbasin before processing?
#     2.) What is the conversion factor for the volume? Units are 10^4 m^3?

# setwd(wd)
file_wetland_parm = paste("wetland_parameters_",strt,"to",stp,".csv",sep='')
# 
gd_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets"
# orginal dem
dem <- raster(paste(gd_dir, 'DEM','raw_prj_10_m.img',sep ='/'))
# filled dem
dem_fl <- raster(paste(gd_dir, 'DEM','filled_dem_hydro_burned.img',sep ='/'))

# sinks
# sinks = raster(paste(gd_dir, "DEM", "sinks.tif",sep="/"))

dir_out_maps = "wetland_maps"
if (!exists(paste(wd, dir_out_maps, sep = '/'))){
	dir.create(paste(wd, dir_out_maps, sep = '/'))
}

dir_out_files = "wetland_files"
if (!exists(paste(wd, dir_out_files, sep = '/'))){
	dir.create(paste(wd, dir_out_files, sep = '/'))
}

gdal_path = "C:/Program Files/GDAL"

## creating ponds layer ponds
if (!file.exists(paste(gd_dir, "ponds", "ponds.tif", sep="/"))){
	py_file = tempfile("arcpy_util_", fileext = ".py")
	py_file = gsub("\\\\", "/", py_file)
	s1 = "import arcpy;from arcpy.sa import *;from arcpy import env;arcpy.CheckOutExtension('Spatial');"
	s2 = paste("env.workspace='", paste(gd_dir, "ponds", sep="/"), "';", sep="")
	s3 = paste("env.snapRaster='",
		paste(gd_dir, 'DEM','raw_prj_10_m.img',sep ='/'),
		"';env.extent='",
		paste(gd_dir, 'DEM','raw_prj_10_m.img',sep ='/')
		,"';env.cellSize='",
		paste(gd_dir, 'DEM','raw_prj_10_m.img',sep ='/'),
		"';",
		sep="")
	s4 = paste("arcpy.PolygonToRaster_conversion('",
		paste(gd_dir, "ponds", "landlocked_watersheds.shp",sep="/"),
		"', 'Subbasin', 'ponds_arc.tif', 'CELL_CENTER', '', 10);",
		sep="")
	s5 = paste("arcpy.Reclassify('ponds_arc.tif', 'Value', 
		RemapRange([[0, 338, 1]])).save('reclassed_ponds.tif')")

	script_str=paste(s1,s2,s3,s4,s5sep="")
	writeLines(script_str, py_file)

	cmd = paste("C:\\Python27\\ArcGIS10.1\\python.exe", py_file)
	system(cmd)
	
	# should already be subsetted to only those huc16s within the WRB
	watersheds_ll = readOGR(paste(gd_dir, "ponds",sep="/"), "landlocked_watersheds")

	subbasins_dissolve = gUnionCascaded(subbasins)

	#rasterizing ponds
	ponds <- rasterize(watersheds_ll, dem, field = watersheds_ll$Subbasin)
	ponds <- reclassify(ponds,     
			rbind(c(-Inf,Inf,1)))

	writeRaster(ponds, 'ponds.tif')
}
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
# cmd_clean = paste(rscript,wtlnds_cleanup)
# system(cmd_clean)
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
