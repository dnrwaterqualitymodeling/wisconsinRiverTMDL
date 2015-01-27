options(stringsAsFactors = FALSE)
library(rgdal)
library(raster)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(foreign)
library(xtable)
# library(ggmap)
source("./Code/figures/function_proper_legend.r")
################################
# map titles
map_sub_reaches = "subbasins_reaches.png"
map_sub_reaches_flow_calib = "subbasins_reaches_flow_calib.pdf"
map_groundwater_p = "groundwater_phosphorus.png"
map_alpha_bf = "alpha_bf.png"
map_pcp_count = "pcp_na_coudnts.png"
map_tmp_count = "tmp_na_counts.png"
map_wetlands = "max_surface_area_wetlands.png"
map_ponds = "ponds.png"
map_wetlands_and_ponds = "wetlands_and_ponds.png"
map_evapo_transp = "et_error.png"
tab_lnd_cvr_mgt = "./Code/doc/tab/landcover_mgt_table_raw.tex"
# paths
dir_out = "./Code/doc/img"

dir_gis_data = "T:/Projects/Wisconsin_River/GIS_Datasets"
dir_mod_inputs = "T:/Projects/Wisconsin_River/Model_Inputs"


dir_hydro = paste(dir_mod_inputs, "SWAT_Inputs/hydro", sep = '/')
dir_observed = paste(dir_gis_data, "observed", sep = '/')

file_usgs_basin_lu = paste(dir_gis_data, "observed", "gauge_basin_lookup.csv", sep = '/')

# file_dem = paste(dir_gis_data, "DEM/wrb_dem", sep = '/')
# file_filled = paste(dir_gis_data, "DEM/wrb_fill", sep = '/')
file_sinks = paste(dir_gis_data, '/DEM/wrb_sinks.tif', sep = "")
file_ponds = paste(dir_gis_data, "wetlands/ponds.tif", sep = '/')
# 30 m versions
file_sinks_30m = paste(dir_gis_data, '/DEM/wrb_sinks_30m.tif', sep = "")
file_ponds_30m = paste(dir_gis_data, "wetlands/ponds_30m.tif", sep = '/')

file_gw_p = paste(dir_gis_data, "groundWater/phosphorus", "background_P_from_EPZ.txt", sep = '/')
file_alpha_bf = paste(dir_gis_data, "groundWater", "alphaBflowSubbasin_lookup.csv", sep = '/')

dir_clim = paste(dir_mod_inputs, "SWAT_Inputs/climate",sep = '/')
stationShape = readOGR(dir_clim, "ClimateStationLocations_WRB_2mileBuffer")
file_precip = paste(dir_mod_inputs, "SWAT_Inputs/climate/pcp.txt",sep = '/')
file_temp = paste(dir_mod_inputs, "SWAT_Inputs/climate/tmp.txt",sep = '/')
file_subbasin_precip = paste(dir_gis_data, "Urban/SubPcp.txt",sep = '/')
file_subbasin_temp = paste(dir_gis_data, "Urban/SubTmp.txt",sep = '/')

file_et_uncertainty = "H:/sim_flow/ET_uncertainty.csv"

file_swat_lu = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/SWAT_lookup.csv"
# file_landcover_mgt = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WRB_TMDL_LndCvr_Mgt_07152014.img.vat.dbf"
file_landcover_mgt = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_27012015.txt"

# reading data 
subbasins = readOGR(dir_hydro, "subbasins")
subbsins_urban = readOGR(dir_hydro, "subbasins_minus_urban_boundaries")
reaches = readOGR(dir_hydro, "hydro")

flow_calib_pts = readOGR(dir_observed, "WRB_FlowSites")

usgs_basin_lu = read.csv(file_usgs_basin_lu)

# dem = raster(file_dem)
# filled = raster(file_filled)  
# sinks = raster(file_sinks)
# ponds = raster(file_ponds)  
sinks = raster(file_sinks_30m)
ponds = raster(file_ponds_30m)

pcp_sites = read.csv(file_precip)
tmp_sites = read.csv(file_temp)

subpcp = read.delim(file_subbasin_precip)
subtmp = read.delim(file_subbasin_temp)
# data files
gwp = read.delim(file_gw_p)

alpha_bf = read.csv(file_alpha_bf)

et_uncertainty = read.csv(file_et_uncertainty)
##########################
# ----- data munging ---- #
basin = gUnionCascaded(subbasins)

# converting usgs id to character for leading '0'
usgs_basin_lu["USGS_ID"] = apply(usgs_basin_lu["USGS_ID"], 1, FUN = function(x) {paste("0", x, sep = '')})

flow_calib_pts = merge(flow_calib_pts, usgs_basin_lu, by.x = "FlowStatio", by.y = "USGS_ID")
# flow_calib_pts = flow_calib_pts[flow_calib_pts@data$Keep == 1,]
flow_calib_pts@data$obj_id = 1:nrow(flow_calib_pts@data)
# merging groundwater p and alphaBF to subbasins
subbasins = merge(subbasins, gwp, by.x = 'Subbasin', by.y = "ID")
subbasins = merge(subbasins, alpha_bf, by.x = 'Subbasin', by.y = "Subbasin")
names(subbasins) = c("Subbasin", "GRIDCODE", "gw_p", "alpha_bf")

#convert
subpcp = merge(subpcp, pcp_sites, by.x="Station", by.y="NAME")
subtmp = merge(subtmp, tmp_sites, by.x="Station", by.y="NAME")
coordinates(subpcp) = ~ LONG + LAT
coordinates(subtmp) = ~ LONG + LAT

# sinks to one value
sinks = mask(sinks, basin)
sink_mat = as.matrix(sinks)
sink_mat[which(sink_mat > 0)] = 1
sink_mat[which(sink_mat == 0)] = NA
sinks_bin = raster(sink_mat, sinks)
rm(sink_mat)
# ET
et_uncertainty$USGS_ID = paste(0, et_uncertainty$USGS_ID, sep ='')
flow_calib_pts_keep = flow_calib_pts[flow_calib_pts@data$Keep == 1,]

flow_calib_pts_keep = merge(flow_calib_pts_keep, 
	et_uncertainty, 
	by.x = "FlowStatio", 
	by.y = "USGS_ID")

not_na = !is.na(flow_calib_pts_keep@data$pbias_harg)
##########################
# map specs
wdth 	= 6
hght 	= 8
unts 	= "in"
reso 	= 800

##########################
# ggmap test
# bsn_wgs84 = spTransform(basin, CRS("+proj=longlat +ellps=WGS84"))
# bsn_ext = extent(bsn_wgs84)
# lon = mean(c(bsn_ext@xmin, bsn_ext@xmax))
# lat = mean(c(bsn_ext@ymin, bsn_ext@ymax))

 # bsn_image =
# for (i in 3:21){
	# file_name = paste("map_zoom_",i,".png",sep ='')
	# png(file_name)#,
		# width = wdth, 
		# height = hght, 
		# units = unts, 
		# res = reso,
	)
	# tst =ggmap(get_map(
		# location = c(lon = lon, lat = lat),
		# source = "google",
		# maptype = "hybrid",
		# zoom = 9))
	# dev.off()
# }


##### ##### ##### ##### ##### #####
# Subbasins with reaches 
png(paste(dir_out, map_sub_reaches, sep = "/"), 
	width = wdth, 
	height = hght, 
	units = unts, 
	res = reso)
plot(subbasins)
title(main="SWAT/nSubbasins")
dev.off()
##### ##### ##### ##### ##### #####
# Subbasins with reaches and calib points
pdf(paste(dir_out, map_sub_reaches_flow_calib, sep = "/"), 
	width = wdth, 
	height = hght)
plot(basin)
plot(reaches, 
	add = T, 
	col = 'blue',
	lwd = 0.5)

points(flow_calib_pts@coords[,1],
	flow_calib_pts@coords[,2] + 10000,
	pch = 22,
	col = 'white', 
	bg = 'white',
	cex = 3)
text(flow_calib_pts@coords[,1],
	flow_calib_pts@coords[,2],
	flow_calib_pts@data$obj_id,
	pos = 3)
plot(flow_calib_pts[flow_calib_pts@data$Keep == 1,], 
	add = T, 
	pch = 20,
	col = 'darkgreen')
plot(flow_calib_pts[flow_calib_pts@data$Keep == 0,], 
	add = T, 
	pch = 3,
	col = 'red')
title(main = "SWAT Reaches/nwith Flow Calibration Sites")
legend('bottomright', 
	legend = c("Included",	"Excluded"),
	pch = c(20, 3),
	col = c("darkgreen", "red"),
	bty = 'n')
dev.off()

##### ##### ##### ##### ##### #####
# Groundwater Phosphorus map
#	number of classes
num_classes_gwp = 3

png(paste(dir_out, map_groundwater_p, sep = "/"), 
	width = wdth, 
	height = hght, 
	units = unts, 
	res = reso)

pal_gwp = brewer.pal(num_classes_gwp, "YlGnBu")
classes_gwp = classIntervals(subbasins@data$gw_p, num_classes_gwp)
colrs_gwp = findColours(classes_gwp, pal_gwp)
newTxt = properLegend(colrs_gwp, sig_figs = 3)

plot(subbasins, 
	col = colrs_gwp, 
	border = NA)

plot(basin, add = T)

legend("bottomright",
	legend = newTxt,
	title = "mg/L",
	fill = pal_gwp,
	bty = 'n')

title(main = "Groundwater/nPhosphorus")
dev.off()
# adjust scalebar based on specific image size
# scalebar(100000, label = c('0','50','100 km'), lwd = 1.5, div = 4, type = 'bar', cex = 1)

##### ##### ##### ##### ##### #####
# Alpha BF map
#	number of classes
num_classes_abf = 9

png(paste(dir_out, map_alpha_bf, sep = "/"), 
	width = wdth, 
	height = hght, 
	units = unts, 
	res = reso)

pal_abf = brewer.pal(num_classes_abf, "YlOrRd")
classes_abf = classIntervals(subbasins@data$alpha_bf, num_classes_abf)
colrs_abf = findColours(classes_abf, pal_abf)
newTxt = properLegend(colrs_abf, sig_figs = 3)

plot(subbasins, 
	col = colrs_abf, 
	border = NA)

plot(basin, add = T)

legend("bottomright",
	legend = newTxt,
#	title = "mg/L",
	fill = pal_abf,
	bty = 'n')

title(main = "ALPHA_BF/nParameter")
dev.off()

##### ##### ##### ##### ##### #####
# Precip and Temp stations
for (v in c("pcp", "tmp")) {
	png(paste(dir_out, "/", v, "_na_count.png", sep = ""), 
		width = wdth, 
		height = hght,
		units = unts,
		res = reso)
	if (v == "pcp") {
		ttl = "Precipitation Stations"
		stations = subpcp@data$Station
		pal = brewer.pal(6, "YlGnBu")
	}
	if (v == "tmp") {
		ttl = "Temperature Stations"
		stations = subtmp@data$Station
		pal = brewer.pal(6, "YlOrBr")
	}
	stations <- substr(stations,2,nchar(stations))
	stations = unique(stations)
	stationsList <- paste(dir_clim, "/", stations, v, ".dbf", sep="")
	counts = data.frame()
	j = 0
	for (i in stationsList){
		j = j + 1
		# print(i)
		data <- read.dbf(i, as.is = TRUE)
		selection <-length(which(data[,2] == -99))
		row = data.frame(ID=stations[j], count=selection)
		counts = rbind(counts, row)
	}
	stationShape_sel = subset(stationShape, ID %in% counts$ID)
	stationShape_sel = merge(stationShape_sel, counts)
	classes = classIntervals(var=stationShape_sel@data$count, 6)
	col = findColours(classes, pal=pal)

	plot(basin, add = F)
	plot(subbasins, axes=F, border="white", bty = 'n')
	plot(subbasins, border="grey50", col="grey80", add=T)
	plot(stationShape_sel, bg=col, cex=2, add=T, pch=21)
	leg_txt = properLegend(col)
	legend("bottomright", legend=leg_txt, pt.bg=pal, cex=0.9, bty = 'n',
		title="Days missing")
	title(main=paste(ttl))
	dev.off()
}

##### ##### ##### ##### ##### #####
# Contributing Areas of Wetlands, sinks (Ponds too? Separate?)
png(paste(dir_out, map_wetlands_and_ponds, sep = "/"), 
	width = wdth, 
	height = hght, 
	units = unts, 
	res = reso)

plot(basin, add = F)
plot(sinks_bin, 
	col = "#9ecae1",
	axes = F,
	legend = F,
	add = T)
plot(ponds, 
	col = "#08519c", 
	add = T, 
	legend = F,
	axes = F)
plot(basin, add = T)
plot(basin, add = T)
title(main = "SWAT Ponds/nand Wetlands")

legend('bottomright', legend = c("Wetlands", "Ponds"), fill = c("#9ecae1", "#08519c"), bty = 'n')
dev.off()
# just wetlands
# png(paste(dir_out, map_wetlands, sep = "/"), 
	# width = wdth, 
	# height = hght, 
	# units = unts, 
	# res = reso)

# plot(sinks_bin, col = "#deebf7")
# plot(basins, add = T)
# title(main = "SWAT Wetlands")
#legend('bottomright', legend = c("Wetlands", "Ponds"), fill = c("#deebf7", "#08519c"))
# dev.off()
# just ponds
# png(paste(dir_out, map_ponds, sep = "/"), 
	# width = wdth, 
	# height = hght, 
	# units = unts, 
	# res = reso)


# plot(ponds, col = "#08519c")
# plot(basins, add = T)
# title(main = "SWAT Ponds")

# legend('bottomright', legend = c("Wetlands", "Ponds"), fill = c("#deebf7", "#08519c"))
# dev.off()
##### ##### ##### ##### ##### #####
# Et figures

pal_et = rev(brewer.pal(9, "RdYlBu"))
dat_cols <- c("pbias_harg",
    "pbias_penman",
    "pbias_priestley",
    "nashsut_harg",      
    "nashsut_penman",    
    "nashsut_priestley")

for (met in c("pbias", "nashsut")){
    met_cols <- grep(met, dat_cols)
    toClass <- et_uncertainty[,dat_cols[met_cols]]
    toClass = as.matrix(toClass)
    prop_int = classIntervals(toClass, 9)

    brks <- prop_int$brks
    for (i in met_cols){  
        toplot <- dat_cols[i]
		# png(paste(dir_out,'/',toplot,'.png',sep=''),
			# width = wdth, 
			# height = hght, 
			# units = unts, 
			# res = reso)
        plot(basin)
        plot(flow_calib_pts_keep[not_na,], 
            bg=pal_et[findInterval(flow_calib_pts_keep@data[, toplot],
                brks,
                all.inside = T)],
             add=T, pch=21, cex =2)

		# dev.off()
    }
	png(paste(dir_out,'/',met,"_legend.png",sep=''),
		width = wdth, 
		height = hght, 
		units = unts, 
		res = reso)
	plot.new()
	if (met == 'pbias'){
		legTitle = "Percent Bias"
	} else if (met == 'nashsut'){legTitle = "Nash-Sutcliffe"}
	txt <- properLegend(findColours(prop_int, pal_et), sig_figs=2)
    legend('right', legend = txt, bg = 'white', bty = 'n', pch = 21,
		pt.bg = pal_et, cex=2, pt.cex = 3.5,
        title = paste(legTitle))
	dev.off()
}
#############################################
# tables

swat_lu = read.csv(file_swat_lu)
lnd_cvr_mgt = read.table(file_landcover_mgt, sep="\t", header=T)

lnd_cvr_mgt_tbl = merge(swat_lu, lnd_cvr_mgt) #, by.x = 'VALUE', by.y = "Value")

lnd_cvr_mgt_tbl = subset(lnd_cvr_mgt_tbl, select=c("LANDUSE", "TYPE", "Definition", "Gen_Code"))

lbl = "tab:lnd_mgt_def"
cap = "The land cover classes represented within ArcSWAT are shown here with the class of land use and land management. The rotation codes are: Cg--corn grain, Cs--corn silage, So-soybean, Po--potato, Vg--vegetable, A--Alfalfa, O/A--oats/alfalfa. Tons are English tons."
x_lnd = xtable(
	lnd_cvr_mgt_tbl,
	label = lbl, 
	caption = cap)

print(
	file = tab_lnd_cvr_mgt,
	x_lnd, 
	tabular.environment = "longtable",
	NA.string = "NA",
	include.rownames = FALSE,
	floating =FALSE)










