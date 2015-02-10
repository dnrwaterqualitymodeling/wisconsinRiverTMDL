library(rgdal)
library(rgeos)
library(raster)

# Items that need addressing:
#     1.) How are land locked ponds/lakes incorporated in these calculations?
#     Should those areas be subtracted from each subbasin before processing?
#     2.) What is the conversion factor for the volume? Units are 10^4 m^3?

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/wetlands"
# setwd(wd)
file_wetland_parm = "wetland_parameters.csv"
# 
gd_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets"
# orginal dem
dem <- raster(paste(gd_dir, 'DEM','wrb_dem',sep ='/'))
# filled dem
dem_fl <- raster(paste(gd_dir, 'DEM','wrb_fill',sep ='/'))

dir_out_maps = "wetland_maps"
if (!exists(paste(wd, dir_out_maps, sep = '/'))){
	dir.create(paste(wd, dir_out_maps, sep = '/'))
}

dir_out_files = "wetland_files"
if (!exists(paste(wd, dir_out_files, sep = '/'))){
	dir.create(paste(wd, dir_out_files, sep = '/'))
}

#ponds
# watersheds_ll = readOGR("ponds", "landlocked_watersheds")
# watersheds_ll_df = watersheds_ll@data
# subbasins_dissolve = gUnionCascaded(subbasins)
# watersheds_ll = gIntersection(watersheds_ll, subbasins_dissolve, drop_not_poly=T, byid=T)
# watersheds_ll = SpatialPolygonsDataFrame(watersheds_ll,
#     data=data.frame(watersheds_ll_df, row.names=row.names(watersheds_ll)))
# #rasterizing ponds
# ponds <- rasterize(watersheds_ll, dem, field = watersheds_ll$Subbasin)
# ponds <- reclassify(pondsRas,     
#         rbind(c(-Inf,Inf,1)))
# 
# writeRaster(ponds, 'ponds.tif')
ponds <- raster(paste(wd, 'ponds.tif', sep ='/'))
# land cover
# lc_lm <- raster("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/landcoverlandmanagement.img")
# lc_lm <- resample(lc_lm, dem)
# lc_lm <- reclassify(lc_lm, 
#            rbind(
    #             c(-Inf,6.5,NA),
    #             c(6.5,9.5,1),
    #             c(9.5,Inf,NA)
#     ))
# writeRaster("wetland_landcover.tif")
# already resampled and reclassed so wetland areas are 1
lc_lm <- raster(paste(wd, "wetland_landcover.tif",sep='/'))

max_sa = lc_lm
norm_sa = lc_lm
max_sa[] = NA
writeRaster(max_sa, "test_rast.tif")
norm_sa[] = NA 
    # 7 is woody wetlands
    # 8 is herbaceous wetlands
    # 9 is cranberries
subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
                    "subbasins_minus_urban_boundaries")

geometry_table = data.frame()
# failed after 148 subbasins, due to lack of memory, 
#   added clean up lines to hopefully improve. sb 149 is huge.
for (s in 2:length(subbasins@data$Subbasin)) {
#     s <- 250
    # for elapsed time
    ptm <- proc.time()[3]
    print("###################")
    print(" ")
    print(paste("Subbasin:", s))
    print(" ")
    subbasin = subset(subbasins, Subbasin == s)
    subbasin_buffer = gBuffer(subbasin, width = 600)
    # extents to speed processing     
    e = alignExtent(subbasin, dem)
    e_buffer = alignExtent(subbasin_buffer, dem)
    print('Reducing data to subbasin area...')
    # dem and filled dem clipped
    dem_sb <- mask(crop(dem, e_buffer), subbasin_buffer) 
    filled_sb <- mask(crop(dem_fl, e_buffer), subbasin_buffer) 
    #processing lc
    lc_sb <- mask(crop(lc_lm, e_buffer), subbasin_buffer)
    # ponds, ponds = 1, else NA
    ponds_sb <- mask(crop(ponds, e_buffer), subbasin_buffer)
    # masking those wetlands that are coincident with ponds
    lc_sb <- mask(lc_sb, ponds_sb, inverse = T)
    
    # subbasin sinks and sink binary
    sinks_sb <- filled_sb - dem_sb
	sinks_sb <- mask(sinks_sb, ponds_sb, inverse = T)
    sinks_sb_crp <- mask(sinks_sb, subbasin)
    sinkBin_sb <- sinks_sb
	
    sinkBin_sb[sinkBin_sb > 0] <- 1
    sinkBin_sb[sinkBin_sb == 0] <- NA
    sinkBin_sb_crp <- mask(sinkBin_sb, subbasin)

    ### finding SA and V normal
    wet_n <- lc_sb * sinkBin_sb
    wet_n_crp <- mask(wet_n, subbasin)
    # multiplying the number of wetland cells
    #   by the pixel length and width and converting to ha
    print("Calculating Normal and maximum SA and Vols...")
    SA_N <- cellStats(wet_n_crp, stat = sum, na.rm = T) * (10*10)*0.0001
    # units are 10^4-m3?
    volRast <- (wet_n_crp * 0.5) * (10*10) 
    # what is the conversion factor?
    V_N <- cellStats(volRast, stat = sum, na.rm = T) * 0.0001
    ### finding SA and V maximum
    SA_MAX <- cellStats(sinkBin_sb_crp, stat = sum, na.rm = T) * (10*10)*0.0001
    volRast_max <- (sinks_sb_crp) * (10*10) * 0.0001 
    V_MAX <- cellStats(volRast_max, stat = sum, na.rm = T) + V_N
    # fraction draining to wetland
    WET_FR <- SA_MAX/(gArea(subbasin)*0.0001)
    
    rw <- c(s, WET_FR, SA_N, V_N, V_N, SA_MAX, V_MAX)
    geometry_table <- rbind(geometry_table, rw)
    elpsd <- proc.time()[3] - ptm
    print(paste('Elapsed time for this subbasin:',round(elpsd,2),'seconds.'))
    print("Plotting...")
	file_name_map = paste(wd, "/", dir_out_maps, "/", "Subbasin_",s,".png",sep = '')
	png(file_name_map)
	plot(subbasin, main = paste("Subbasin",s))
	plot(ponds_sb, add = T, col="#0000ff50", legend = F)
	plot(sinkBin_sb_crp, add = T, col="#ff000050", legend = F)
	legend(
		"topleft",
		legend=c("Ponds", "Max Wetland SA"),
		fill = c("#0000ff50", "#ff000050")
		)

	dev.off()
	
	print("Exporting files...")
	writeRaster(
		sinkBin_sb_crp, 
		paste(wd, "/", dir_out_files,"/Max_SA_Subbasin_",s,".tif",sep=''))

	writeRaster(
		wet_n_crp, 
		paste(wd, "/", dir_out_files,"/Normal_SA_Subbasin_",s,".tif",sep=''))

	print("###################")

}
names(geometry_table) <- c('subbasin','WET_FR','WET_NSA','WET_NVOL','WET_VOL','WET_MXSA','WET_MXVOL')
write.csv(
	geometry_table, 
	file_wetland_parm,
	row.names = F)




