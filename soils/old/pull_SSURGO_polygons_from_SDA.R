args = commandArgs(trailingOnly = T)
start = args[1]
end = args[2]
out_dir = args[3]

library(foreign)
library(rgdal)
library(reshape2)
library(rgeos)
library(aqp)
library(soiltexture)
library(stringr)
# library(mclust)
library(Hmisc)
library(soilDB)
# install.packages('soilDB', dep=TRUE)
# install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source")
library(plyr)
library(raster)
library(RCurl)
library(XML)
library(SSOAP)
library(XMLSchema)
library(plyr)
options(stringsAsFactors = F, 
        warn = 1,
        timeout = 180)
# my file names
net_soil_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"
aggregated_to_mapunit_file <- 'aggregated_soils_to_mapunit.txt'
# mukey_table <- "wrb_mukeys.txt"
tmplate <- "SWAT_US_SSURGO_Soils_wrb.txt"
fish_file = "WRB_2_mile_buffer_fishnet"
wrb_mukeys_file <- "wrb_mukeys_28_oct"
# wrb_mukeys <- read.csv(paste(net_soil_dir, mukey_table,sep = '/'))
# to collect aggregated data
agg_mky_tbl <- read.table(paste(net_soil_dir, tmplate,sep='/'), header=T)[1,] 

fishnet = readOGR(net_soil_dir, fish_file)
sda_crs = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
fishnet = spTransform(fishnet, sda_crs)

scale_to_1 = function(pcts) {
    scaled = pcts / (sum(pcts, na.rm=T))
    return(scaled)
}
sum.that.works = function(values, hrz.height, comppct) {
    d = matrix(values, nrow=hrz.height)
    pct_bool = matrix(!is.na(d), nrow=hrz.height, ncol=ncol(d))
    pct_bool[!pct_bool] = NA
    pcts = t(apply(pct_bool, 1, function(X) {X * comppct}))
    scaled_pcts = t(apply(pcts, 1, scale_to_1))
    ss = rowSums(d * scaled_pcts, na.rm=T)
	s = mean(ss, na.rm=T)
	return(c(s=s))
}



dat_cols = c(
    "dbovendry_r",
    "awc_r", 
    "ksat_r",
    "cbn",
    "claytotal_r",
    "silttotal_r",
    "sandtotal_r",
    "fragvol_r", 
    "albedodry_r",
    "kwfact",
    "ec_r", 
    "caco3_r", 
    "ph1to1h2o_r",
    "hydgrp")

file.remove(list.files(tempdir(), pattern="ssurgo_", full.names=T))
file.remove(list.files(tempdir(), pattern="tile_", full.names=T))
file.remove(list.files(tempdir(), pattern="clip_", full.names=T))

all_mukeys = NULL
check.on.these = NULL
failed.mukeys <- NULL
failed.time <- data.frame()
failed.geom <- data.frame()
png(paste('status_map', '_', start, '_', end, '.png', sep=""),
    units = 'in',
    height = 14,
    width = 10,
    res = 300)
for (id in start:end) {
#     id <- 10
    f = subset(fishnet, Id == id)
    e = extent(f)
    #cute graphics...
    if (id == start){
        plot(fishnet)
    }
    plot(f,add = T, col = 'yellow')
    # ---
    # catch time out errors
    get.time.err <- try({
        m = mapunit_geom_by_ll_bbox(c(e@xmin, e@ymin, e@xmax, e@ymax))
    }
    )
    if (class(get.time.err) == 'try-error'){
        failed.time <- rbind(failed.time, c(id, c(e@xmin, e@ymin, e@xmax, e@ymax)))
        print(paste("ID:",id,"failed."))
        plot(f,add = T, col = 'red')
        next
    }
    # ---
    m@data$mukey = as.character(m@data$mukey)
    # catch geometry errors, wtf is going on here?
#     get.geom.err <- try({
#         mtst = gIntersection(m, f, byid=T, id=m@data$mukey, drop_lower_td = TRUE)
#     })
#     if (class(get.geom.err) == 'try-error'){
#         failed.geom <- rbind(failed.geom, c(id, c(e@xmin, e@ymin, e@xmax, e@ymax)))
#         print(paste("ID:",id,"failed. Probelm with gIntersection."))
#         plot(f,add = T, col = 'red')
#         next
#     }
    # ---
    mukeys = unique(m@data$mukey[!(m@data$mukey %in% all_mukeys)])
    all_mukeys = unique(c(all_mukeys, mukeys))
#     row.names(m) = as.character(1:length(m))
#     m = SpatialPolygonsDataFrame(m,
#         data=data.frame(mukey=mukeys,
#             row.names=as.character(1:length(m))))
    ssurgo_file = gsub("\\\\", "/", tempfile(pattern="ssurgo_"))
    tile_file = gsub("\\\\", "/", tempfile(pattern="tile_"))
    clip_file = gsub("\\\\", "/", tempfile(pattern="clip_"))
    script_file = tempfile(pattern="clip_script_", fileext=".py")
    writeOGR(m, dirname(ssurgo_file), basename(ssurgo_file), "ESRI Shapefile")
    writeOGR(f, dirname(tile_file), basename(tile_file), "ESRI Shapefile")
    
    script = paste(
        'import arcpy; arcpy.Clip_analysis("', ssurgo_file, '.shp", "', tile_file, '.shp", "', clip_file, '.shp")',
        sep="")
    write(script, script_file)    
    system(paste("C:\\Python27\\ArcGIS10.1\\python.exe", script_file))
    ssurgo_clip = readOGR(dirname(clip_file), basename(clip_file))
    
    print('                                   ')
    print(paste('Collecting mukeys for tile ', id, '...',sep =''))
    plot(f,add = T, col = 'green')
    print('------------------------------------')

}
text(fishnet, 
    as.numeric(sapply(slot(fishnet, 'polygons'), 
    function(i) slot(i, 'ID')))+1, cex = 0.5)

ssurgo_tile_list = list.files(tempdir(),
    pattern="clip_.*(shp|dbf|shx|prj)$",
    full.names=T)
out_ssurgo_list = basename(ssurgo_tile_list)
out_ssurgo_list = paste(out_dir, out_ssurgo_list, sep="/")

file.copy(ssurgo_tile_list, out_ssurgo_list)

write.table(all_mukeys, 
            paste(net_soil_dir, "/", wrb_mukeys_file, "_", start, "_", end, ".txt", sep=''),
            sep = '\t',
            row.names = F)
# retrieving tabular data for mukeys in wrb
# now found in the script aggregate_SSURGO_query_tabular_data.R
