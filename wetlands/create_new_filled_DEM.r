library(rgdal)
library(raster)

wd = "C:/Users/evansdm/filled_dem_processing"
fl_lakes = "nonLndLckedLkes_clip"
fl_flowl = "flowLines_clip"
fl_dem_msked = "dem_masked.tif"
fl_dem = "raw_prj_10_m.img"
fl_pit_filled = "wrb_filled.tif"


dem = raster(paste(wd, "raw_prj_10_m.img",sep="/"))
# ext_dem = extent(dem)
# dem_mat = getValues(dem)
lakes = readOGR(
	wd,
	fl_lakes)
flowl = readOGR(
	wd,
	fl_flowl)

dem_msked = mask(dem, lakes, inverse=TRUE)
writeRaster(dem_msked, "dem_with_lakes.tif")#fl_dem_msked)
dem_msked = mask(dem_msked, flowl, inverse=TRUE)

writeRaster(dem_msked, fl_dem_msked)

## feed to tau dem
# run tau dem pit removal
pit_rem = "mpiexec -n 8 pitremove -z "

#### In DEM = fl_dem_msked
#### Out filled = fl_pit_filled

cmd = paste(pit_rem, fl_dem_msked, " -fel ",fl_pit_filled) 

system(cmd)

# png("test.png")
# plot(dem)
# plot(lakes,add=T,col='red')
# plot(flowl,add=T,col='blue')
# dev.off()

# extent(lakes) = alignExtent(ext_dem, lakes)

# lakes_mat = getValues(lakes)

# lakes_mat[which(lakes_mat == 1)] = NA


# lakes = raster(paste(wd, "lakes.tif",sep="/"))
# extent(lakes) = alignExtent(ext_dem, lakes)
# flowl = raster(paste(wd, "flowlines.tif",sep="/"))
# extent(flowl) = alignExtent(ext_dem, flowl)