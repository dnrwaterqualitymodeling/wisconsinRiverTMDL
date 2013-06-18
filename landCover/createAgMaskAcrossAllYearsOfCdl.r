library(raster)
setwd("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Crop_Data_Layers_2008_2012/WTM_Reprojection")

rasters = paste('wrb_cdl_', 2008:2012, sep="")

r2008 = raster(rasters[1])
r2009 = raster(rasters[2])
r2010 = raster(rasters[3])
r2011 = raster(rasters[4])
r2012 = raster(rasters[5])

nonRotCropVals = c(0, 63:180, 182:203)
rotCropVals = c(1:62, 66:77, 92, 181, 204:254)

agMask2008 = r2008 %in% rotCropVals
agMask2009 = r2009 %in% rotCropVals
agMask2010 = r2010 %in% rotCropVals
agMask2011 = r2011 %in% rotCropVals
agMask2012 = r2012 %in% rotCropVals

agMaskSum = agMask2008 + agMask2009 + agMask2010 + agMask2011 + agMask2012
agMask = agMaskSum >= 1

writeRaster(agMask, "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/agMask.tif", 'GTiff')
