library(rgdal)
library(rgeos)
library(raster)

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands"

dir_modinputs <- "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/wetlands"
file_out = "wetland_parameters.csv"

dir_out_maps = "wetland_maps"
dir_out_files = "wetland_files"

gdal_path = "C:/Program Files/GDAL"

out_wtlds = list.files(pattern="wetland_parameters*")

wtlds_params = data.frame()
for (fl in out_wtlds){
	dat = read.csv(fl)
	wtlds_params = rbind(wtlds_params, dat)
}

write.csv(
	wtlds_params,
	paste(dir_modinputs, file_out, sep = '/'),
	row.names=F)

# Creating layers of max and normal surface area
if (file.exists(gdal_path)){
	gdalbuildvrt = "gdalbuildvrt"

	for (lvl in c("Max", "Normal")){
		file_list = list.files(
			paste(wd, "/", dir_out_files, sep=''),
			pattern = lvl,
			full.names=T)
		tmpf_tif_list = tempfile("tif_list_", fileext='.txt')
		write(paste(file_list, sep='\n'), tmpf_tif_list)
		
		outfile = paste(lvl, "_wetland_surface_area.vrt",sep='')
		
		tmpf = tempfile("buildvrt_", fileext = ".bat")
		cd = paste("cd", gdal_path)

		cmd = paste(
			gdalbuildvrt,
			'-vrtnodata "-9999"',
			'-srcnodata "-9999"',
			"-input_file_list",
			tmpf_tif_list,
			paste(wd,
				outfile,
				sep='/')
		)
		lnes = paste(cd, cmd, sep='\n')
		write(lnes, tmpf)
		system(tmpf)
	}
} else {
	print("GDAL not found on C drive")
}
