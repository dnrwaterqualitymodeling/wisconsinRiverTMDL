library(rgdal)
library(rgeos)
library(raster)

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/ponds"

# dir_modinputs <- "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/ponds"
file_out = "pond_geometry.csv"

out_pnds = list.files(wd, pattern="pond_geometry*")

pnds_params = data.frame()
for (fl in out_pnds){
	dat = read.csv(fl)
	pnds_params = rbind(pnds_params, dat)
}

write.csv(
	pnds_params,
	paste(wd, file_out, sep = '/'),
	row.names=F)
