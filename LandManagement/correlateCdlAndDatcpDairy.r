library(raster)
library(rgdal)

wd = 'T:/Projects//Wisconsin_River/GIS_Datasets/Land_Management/Validations/DairyDensity_Comparisons'
setwd(wd)
cdlFile = 'CDLbyCLU_DairyPixelDensity.img'
datcpFile = 'DairyProducerDensity_Statewide.tif'

cdl = raster(cdlFile)
datcp = raster(datcpFile)

datcp = crop(datcp, cdl)

basinDir = "T:/Projects/Wisconsin_River/GIS_Datasets/Watersheds"
basinFile = "WRB_Basin"
basin = readOGR(basinDir, basinFile)

cdlMask = mask(cdl, basin)
datcpMask = mask(datcp, basin)

cdlVal = getValues(cdlMask)
datcpVal = getValues(datcpMask)

plot(datcpVal ~ cdlVal)
