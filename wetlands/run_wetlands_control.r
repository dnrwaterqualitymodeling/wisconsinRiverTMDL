library(rgdal)
library(rgeos)
library(raster)

user_path = path.expand("~")
rscript = paste(user_path, "R\\R-3.1.1\\bin\\x64\\Rscript.exe",sep="\\")
wtlndsscrpt = paste(user_path, "Code\\wetlands\\calculate_wetland_parameters.r",sep="\\")
wtlnds_cleanup = paste(user_path, "Code\\wetlands\\run_wetlands_cleanup.r",sep="\\")

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands"
gd_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets"
#### Number of processes to start
procs = 4


dir_out_maps = "wetland_maps"
dir_out_files = "wetland_files"

if (!exists(paste(wd, dir_out_maps, sep = '/'))){
	dir.create(paste(wd, dir_out_maps, sep = '/'))
}

if (!exists(paste(wd, dir_out_files, sep = '/'))){
	dir.create(paste(wd, dir_out_files, sep = '/'))
}

## creating ponds layer ponds
if (!file.exists(paste(gd_dir, "wetlands", "ponds.tif", sep="/"))){
	# orginal dem, for template
	dem <- raster(paste(gd_dir, 'DEM','raw_prj_10_m.img',sep ='/'))
	# should already be subsetted to only those huc16s within the WRB
	watersheds_ll = readOGR(paste(gd_dir, "ponds",sep="/"), "landlocked_watersheds")

	#rasterizing ponds
	ponds <- rasterize(watersheds_ll, dem, field = watersheds_ll$Subbasin)
	ponds <- reclassify(ponds,     
			rbind(c(-Inf,Inf,1)))

	writeRaster(ponds, 
		paste(wd, 'ponds.tif',sep='/')
	)
}

strt_stps = round(seq(1,337+337/procs,by=337/procs))
cmd = NULL
for (i in 1:(length(strt_stps)-1)){
	strt = strt_stps[i]
	stp = strt_stps[i+1]-1
	ln = paste(paste("START",'"',paste("WETLAND", strt),'"', rscript, wtlndsscrpt, strt, stp))
	cmd = c(cmd, ln)
	print(ln)
}

tmpf = tempfile(fileext=".bat")
writeLines(
	paste(cmd, collapse="\n"),
	tmpf
)
system(tmpf)

cmd_clean = paste(rscript, wtlnds_cleanup)

run.clean.up = F
while (!run.clean.up) {
	ps = grep("WETLAND", system('tasklist /v', intern=TRUE), value=TRUE)
	if (length(ps) < 1) {
		run.clean.up = T
		system(cmd_clean)
	} else {
		Sys.sleep(1)
		run.clean.up = F
	}
}