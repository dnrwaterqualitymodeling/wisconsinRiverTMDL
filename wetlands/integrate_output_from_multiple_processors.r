args <- commandArgs(TRUE)
paths = args[1]
out_geometry_file = args[2]
raster_dir = args[3]
raster_template = args[4]
out_mosaic_dir = args[5]

library(raster)
rasterOptions(tmpdir="K:/temp")

paths = "T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry_1.csv;T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry_2.csv;T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry_3.csv;T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry_4.csv"
out_geometry_file = "T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry.csv"
raster_dir = "K:/temp/wetlands_rasters"
raster_template = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WRB_TMDL_LndCvr_Mgt_07152014.img"
out_mosaic_dir = "T:/Projects/Wisconsin_River/GIS_datasets/wetlands"

paths = strsplit(paths, ";")[[1]]

all_data = data.frame()
for (path in paths) {
    d = read.csv(path)
    all_data = rbind(all_data, d)
}
all_data = all_data[order(all_data$subbasin),]
write.csv(all_data, out_geometry_file, row.names=F)

ca_raster_files = list.files(raster_dir, pattern="^wetland_ca.+\\.tif$", full.names=T)
mxsa_raster_files = list.files(raster_dir, pattern="^wetland_mxsa.+\\.tif$", full.names=T)

ca = raster(raster_template)
ca_matrix = matrix(NA, nrow(ca), ncol(ca))
for (ca_raster_file in ca_raster_files) {
    print(ca_raster_file)
    ca_raster = raster(ca_raster_file)
    
    imin = ((extent(ca)@ymax - extent(ca_raster)@ymax) / res(ca)[1]) + 1
    imax = imin + nrow(ca_raster) - 1
    jmin = ((extent(ca_raster)@xmin - extent(ca)@xmin) / res(ca)[1]) + 1
    jmax = jmin + ncol(ca_raster) - 1
    ca_array = getValues(ca_raster)
    ca_patch_matrix = matrix(ca_array, nrow=nrow(ca_raster), ncol=ncol(ca_raster), byrow=T)
    stack = rep(NA, ncol(ca_raster) * nrow(ca_raster) * 2)
    dim(stack) = c(nrow(ca_raster), ncol(ca_raster), 2)
    stack[,,1] = ca_patch_matrix
    stack[,,2] = ca_matrix[imin:imax,jmin:jmax]
    patch = apply(stack, c(1,2), sum, na.rm=T)
    ca_matrix[imin:imax,jmin:jmax] = patch
}
ca_matrix[ca_matrix==0] = NA
ca_final = raster(ca_matrix, template=ca)
writeRaster(ca_final, paste(out_mosaic_dir, "wetland_contributing_area.tif", sep="/"),
    format="GTiff")
removeTmpFiles(h=0.01)


mxsa = raster(raster_template)
mxsa_matrix = matrix(NA, nrow(mxsa), ncol(mxsa))
for (mxsa_raster_file in mxsa_raster_files) {
    print(mxsa_raster_file)
    mxsa_raster = raster(mxsa_raster_file)
    
    imin = ((extent(mxsa)@ymax - extent(mxsa_raster)@ymax) / res(mxsa)[1]) + 1
    imax = imin + nrow(mxsa_raster) - 1
    jmin = ((extent(mxsa_raster)@xmin - extent(mxsa)@xmin) / res(mxsa)[1]) + 1
    jmax = jmin + ncol(mxsa_raster) - 1
    mxsa_array = getValues(mxsa_raster)
    mxsa_patch_matrix = matrix(mxsa_array, nrow=nrow(mxsa_raster), ncol=ncol(mxsa_raster), byrow=T)
    stack = rep(NA, ncol(mxsa_raster) * nrow(mxsa_raster) * 2)
    dim(stack) = c(nrow(mxsa_raster), ncol(mxsa_raster), 2)
    stack[,,1] = mxsa_patch_matrix
    stack[,,2] = mxsa_matrix[imin:imax,jmin:jmax]
    patch = apply(stack, c(1,2), sum, na.rm=T)
    mxsa_matrix[imin:imax,jmin:jmax] = patch
}
mxsa_matrix[mxsa_matrix==0] = NA
mxsa_final = raster(mxsa_matrix, template=mxsa)
writeRaster(mxsa_final, paste(out_mosaic_dir, "wetland_maximum_surface_area.tif", sep="/"),
    format="GTiff")
removeTmpFiles(h=0.01)

