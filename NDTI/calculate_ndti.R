library(raster)
library(ncdf)
dir_data = "K:/NDTI/downloads"
file_nc = "K:/NDTI/landsat_images.nc"

td = tempdir()
files_tar = list.files(dir_data, pattern=".*\\.tar\\.gz$", full.names=T)

setwd(td)

file_tmplt = untar(files_tar[1], list=T, verbose=T)[1]
untar(files_tar[1], files=file_tmplt, exdir=td)
tmplt = raster(file_tmplt)

x = seq(
	extent(tmplt)[1] + (res(tmplt)[1] / 2),
	extent(tmplt)[2] - (res(tmplt)[1] / 2),
	length.out=ncol(tmplt)
)
y = seq(
	extent(tmplt)[4] - (res(tmplt)[1] / 2),
	extent(tmplt)[3] + (res(tmplt)[1] / 2),
	length.out=nrow(tmplt)
)
t = as.integer(as.Date(substr(basename(files_tar),10, 16), format="%Y%j"))[1:2]

dimX = dim.def.ncdf("X", "Meters", x)
dimY = dim.def.ncdf("Y", "Meters", y)
dimT = dim.def.ncdf("Date", "days since 1970-01-01", t)
mv <- 0
band_5_var = var.def.ncdf(
	"band_5",
	"Surface Reflectance",
	list(dimX,dimY,dimT),
	mv,
	prec="short"
)
band_7_var = var.def.ncdf(
	"band_7",
	"Surface Reflectance",
	list(dimX,dimY,dimT),
	mv,
	prec="short"
)

create.ncdf(
	file_nc,
	list(band_5_var, band_7_var)
)

for (f in files_tar) {
	fns = untar(f, list=T, verbose=T)
	fns = fns[grepl("(.*cfmask.tif$)|(.*sr_band5.tif$)|(.*sr_band7.tif$)",fns)]
	untar(f, files=fns, exdir=td, verbose=T)
	id = strsplit(fns[1], "_")[[1]][1]
	date = as.Date(substr(fns[1], 10, 16), format="%Y%j")
	mask = getValues(raster(paste(id, "_cfmask.tif", sep="")))
	sr5 = getValues(raster(paste(id, "_sr_band5.tif", sep="")))
	sr7 = getValues(raster(paste(id, "_sr_band7.tif", sep="")))
	sr5 = sr5[mask == 0]
	sr7 = sr7[mask == 0]
}