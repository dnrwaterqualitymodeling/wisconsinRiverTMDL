library(RODBC)
library(plyr)
library(raster)
library(rgeos)
library(rgdal)

file_output.mgt = "C:/TEMP/WRB.Sufi2.SwatCup/output.mgt"
file_swat_prj = "K:/WRB/WRB.mdb"
file_lc = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/swat_lc_fullForestDef_wtm.tif"
file_lc_xw = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/SWAT_lookup.csv"
dir_min_ndti = "K:/NDTI/minNDTI"
dir_grids = "K:/WRB/Watershed/Grid"
file_mean_min_ndti = "K:/NDTI/minNDTI/mean_min_ndti.tif"
file_sol_rsd = "K:/NDTI/minNDTI/sol_rsd.tif"
file_counties = "T:/GIS/Statewide_Coverages/Political_Boundaries/WI_Counties.shp"
test_counties = c(
	"Marathon",
	"Clark",
	"Wood",
	"Portage",
	"Waushara",
	"Adams",
	"Juneau",
	"Monroe"
)
file_models_out = "T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/models.RData"

non_ag_lu_sql = "('WATR','URML','FRSD','FRSE','FRST','WETN','PAST','ONIO')"
con = odbcConnectAccess(file_swat_prj)
hrus = sqlQuery(
	con,
	paste("select subbasin,hru,landuse,soil,slope_cd from hru where landuse not in", non_ag_lu_sql)
)
close(con)
hrus$slope_cd = as.character(hrus$slope_cd)
hrus$slope_cd[hrus$slope_cd == "0-0.5"] = "1"
hrus$slope_cd[hrus$slope_cd == "0.5-1.5"] = "2"
hrus$slope_cd[hrus$slope_cd == "1.5-3"] = "3"
hrus$slope_cd[hrus$slope_cd == "3-5.8"] = "4"
hrus$slope_cd[hrus$slope_cd == "5.8-9999"] = "999"
hrus$slope_cd = as.integer(hrus$slope_cd)

col.names = strsplit(readLines(file_output.mgt, 1), "\\s+")[[1]][-1]

w = c(5,5,6,6,6,11,16,15,rep(10,5))
output.mgt = read.fwf(
	file_output.mgt,
	w,
	skip=5,
	colClasses=c(
		rep("integer", 5),
		"numeric",
		rep("character",2),
		rep("numeric", 5)
	)
)
names(output.mgt) = col.names[1:length(w)]

output.mgt = subset(output.mgt, Year >= 2001)

dates = data.frame(
	date = seq(as.Date("2001-01-01"), as.Date("2013-12-31"), "1 day")
)
mean_res = data.frame()
for (row in 1:nrow(hrus)) {
	print(hrus[row,])
	output.mgt_hru = subset(
		output.mgt,
		Sub == hrus$subbasin[row] & Hru == hrus$hru[row]
	)
	output.mgt_hru$date = as.Date(
		paste(
			output.mgt_hru$Year,
			output.mgt_hru$Mon,
			output.mgt_hru$Day,
			sep="-"
		)
	)
	area = output.mgt_hru$AREAkm2[1]
	output.mgt_hru = aggregate(
		sol_rsd ~ date,
		data=output.mgt_hru,
		FUN=mean,
		na.rm=T
	)
	res = merge(dates, output.mgt_hru, all.x=T)[c("date", "sol_rsd")]
	for (day in 2:nrow(res)) {
		if (is.na(res$sol_rsd[day])) {
			res$sol_rsd[day] = res$sol_rsd[day - 1]
		}
	}
	res = subset(res, date >= as.Date("2002-01-01"))
	res = subset(
		res,
		as.integer(format(date, "%m")) >= 3 &
			as.integer(format(date, "%m")) <= 6
	)
	mean_res_row = data.frame(
		subbasin = hrus$subbasin[row],
		hru = hrus$hru[row],
		soil = hrus$soil[row],
		slope = hrus$slope_cd[row],
		landuse = hrus$landuse[row],
		area = area,
		sol_rsd = mean(res$sol_rsd, na.rm=T)
	)
	mean_res = rbind(mean_res, mean_res_row)
}
save("mean_res", file="T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/mean_res.RData")

load("T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/mean_res.RData")
mean_res = merge(mean_res, read.csv(file_lc_xw), by.x="landuse", by.y="LANDUSE", all.x=T, all.y=F)
mean_res = data.frame(
	soil = mean_res$soil,
	slope = mean_res$slope,
	landuse = mean_res$VALUE,
	code = paste(mean_res$slope, mean_res$soil, mean_res$VALUE, sep="_"),
	sol_rsd = mean_res$sol_rsd
)
mean_res = aggregate(
	sol_rsd ~ code,
	data=mean_res,
	FUN=mean
)

#mean_res_lu = ddply(
#	mean_res,
#	"landuse",
#	summarise,
#	sol_res_mean = sum(sol_rsd * area) / sum(area)	
#)

#lc = raster(file_lc)
#lc = ratify(lc)
#lc_xw = read.csv(file_lc_xw)

#mean_res_lu = merge(mean_res_lu, lc_xw, by.x="landuse", by.y="LANDUSE")
#rcl = cbind(mean_res_lu$VALUE, mean_res_lu$sol_res_mean)

#levels(lc) = merge(
#	levels(lc),
#	mean_res_lu,
#	by.x="ID",
#	by.y="VALUE",
#	all.x=T,
#	all.y=F
#)

#rsd_ras = reclassify(lc, rcl)
#rsd_ras[rsd_ras < 100] = NA

min_ndtis = list.files(dir_min_ndti, "^min_ndti_[0-9]{4}\\.tif$", full.names=T)
ext = as(extent(raster(min_ndtis[1])), "SpatialPolygons")
for (min_ndti in min_ndtis) {
	ext = gIntersection(
		ext,
		as(extent(raster(min_ndti)), "SpatialPolygons")
	)
}
for (min_ndti in min_ndtis) {
	file_min_ndti_crop = paste(tempdir(), basename(min_ndti), sep="\\")
	crop(raster(min_ndti), ext, filename=file_min_ndti_crop)
}
min_ndti = stack(
	list.files(tempdir(), "^min_ndti_[0-9]{4}\\.tif$", full.names=T)
)
mean_min_ndti = calc(
	min_ndti,
	fun = function(x) {
		if (sum(!is.na(x)) >= 2) {
			return(mean(x, na.rm=T))
		} else {
			return(NA)
		}
	}
)
load("T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/mean_res.RData")
load("T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/hru_overlay.RData")
load("T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/mean_res_lu.RData")
load("T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Analysis/tillage/model_data.RData")
# Create HRU stack
slope = raster(paste(dir_grids, "landslope1", sep="/"))
soil = raster(paste(dir_grids, "landsoils1", sep="/"))
lu = raster(paste(dir_grids, "landuse2", sep="/"))
ext_hru = as(extent(slope), "SpatialPolygons")
ext_hru = gIntersection(ext_hru, as(extent(soil), "SpatialPolygons"))
ext_hru = gIntersection(ext_hru, as(extent(lu), "SpatialPolygons"))

slope = crop(slope, ext_hru)
soil = crop(soil, ext_hru)
lu = crop(lu, ext_hru)
hru_stack = as.matrix(stack(slope, soil, lu))
hru = paste(hru_stack[,1], hru_stack[,2], hru_stack[,3], sep="_")
na_ind = grepl("NA", hru)
hru = as.factor(hru)
hru = setValues(lu, hru)
hru_att = levels(hru)[[1]]
#hru_split = strsplit(as.character(hru_att$VALUE), "_")
#hru_split = matrix(as.integer(unlist(hru_split)), ncol=3, byrow=T)
#hru_split = as.data.frame(hru_split)
#names(hru_split) = c("slope", "soil", "landuse")
hru_att = merge(hru_att, mean_res, by.x="VALUE", by.y="code", all.x=T, all.y=F)
hru_att = data.frame(
	ID = hru_att$ID,
	sol_rsd = hru_att$sol_rsd
)
levels(hru) = hru_att
sol_rsd = deratify(hru, att="sol_rsd")

# Put NDTI and soil residue rasters into same grid domain
ext_ndti = as(extent(mean_min_ndti), "SpatialPolygons")
ext_ndti@proj4string = CRS(projection(mean_min_ndti))
ext_ndti_wtm = spTransform(ext_ndti, projection(sol_rsd))
ext_sol_rsd = as(extent(sol_rsd), "SpatialPolygons")
ext_sol_rsd@proj4string = CRS(projection(sol_rsd))
ndti_sol_rsd_int = gIntersection(ext_ndti_wtm, ext_sol_rsd)
ndti_sol_rsd_int_wgs = spTransform(ndti_sol_rsd_int, projection(mean_min_ndti))
mean_min_ndti_crop = crop(mean_min_ndti, ndti_sol_rsd_int_wgs)
sol_rsd_crop = crop(sol_rsd, ndti_sol_rsd_int)
mean_min_ndti_wtm = projectRaster(
	mean_min_ndti_crop,
	to=sol_rsd_crop
)
#writeRaster(mean_min_ndti_wtm, file_mean_min_ndti)
#writeRaster(rsd_ras_crop, file_sol_rsd)

model_data = cbind(getValues(sol_rsd_crop), getValues(mean_min_ndti_wtm))


counties = readOGR(
	dirname(file_counties),
	strsplit(basename(file_counties), "\\.")[[1]][1]
)
counties_raster = rasterize(counties, rsd_ras_crop, 'COUNTY_FIP')
counties_raster = crop(counties_raster, sol_rsd_crop)
counties_raster_vector = getValues(counties_raster)
lc_raster_vector = getValues(sol_rsd_crop_crop)
lc_x_cty = apply(
	cbind(counties_raster_vector, lc_raster_vector),
	1,
	function (x) {
		if (any(is.na(x))) {
			return(NA)
		} else {
			paste(x, collapse="_")	
		}		
	}
)
model_data = cbind(lc_x_cty, getValues(mean_min_ndti_wtm))
keep = apply(model_data, 1, function(x) {
		if (any(is.na(x))) {
			return(F)
		} else {
			return(T)
		}
	}
)
model_data = model_data[keep,]
model_data = apply(
	model_data,
	1,
	function (x) {
		return(c(
			strsplit(x[1], "_")[[1]][1],
			strsplit(x[1], "_")[[1]][2],
			x[2]
		))
	}
)
model_data = t(model_data)
model_data = data.frame(
	county = as.integer(model_data[,1]),
	landcover = as.integer(model_data[,2]),
	ndti = as.numeric(model_data[,3])
)
model_data = merge(
	model_data,
	mean_res_lu,
	by.x="landcover",
	by.y="VALUE",
	all.x=T,
	all.y=F
)
model_data = model_data[c("landcover", "county", "sol_res_mean", "ndti")]
model_data = subset(model_data, landcover >= 12 & landcover <= 54)
model_data$landcover[model_data$landcover %in% 12:14] = 12
model_data$landcover[model_data$landcover %in% 15:17] = 15
model_data$landcover[model_data$landcover %in% 18:20] = 18
model_data$landcover[model_data$landcover %in% 21:23] = 21
model_data$landcover[model_data$landcover %in% 24:26] = 24
model_data$landcover[model_data$landcover %in% 27:29] = 27
model_data$landcover[model_data$landcover %in% 30:32] = 30
model_data$landcover[model_data$landcover %in% 33:35] = 33
model_data$landcover[model_data$landcover %in% 36:38] = 36
model_data$landcover[model_data$landcover %in% 39:41] = 39
model_data$landcover[model_data$landcover %in% 42:44] = 42
model_data$landcover[model_data$landcover %in% 45:47] = 45
model_data$landcover[model_data$landcover %in% 48:49] = 48
model_data$landcover[model_data$landcover %in% 50:51] = 50
model_data$landcover[model_data$landcover %in% 52:54] = 52
rm("keep")
gc()

cty_lc_freq = count(model_data[c("county", "landcover")])
cty_lc_freq = subset(cty_lc_freq, freq >= 25)
model_data = subset(
	model_data,
	paste(county, landcover) %in% paste(cty_lc_freq$county, cty_lc_freq$landcover)
)

model_data$county = as.factor(model_data$county)
model_data$landcover = as.factor(model_data$landcover)
cty_x_lc_anova = aov(ndti ~ county + landcover, data=model_data)
TukeyHSD(cty_x_lc_anova)




#lc_x_cty = calc(
#	stack(lc_crop, counties_raster),
#	function (x) {
#		if (any(is.na(x))) {
#			return(NA)
#		} else {
#			as.factor(paste(x, collapse="_"))	
#		}		
#	}
#)
#
#model_data = data.frame(
#	ndti = getValues(mean_min_ndti_wtm),
#	rsd = getValues(rsd_ras_crop)
#)
# Add a county column
# Factorize ag system -- aggregate together as much as possible.
# Filter by sample size for each ag system county combination




central_model = lm(ndti ~ rsd, data=model_data)

# County-specific models



county_models = list()
for (test_county in test_counties) {
	county = subset(counties, COUNTY_NAM == test_county)
	mean_min_ndti_county = crop(mean_min_ndti_wtm, county)
	rsd_ras_county = crop(rsd_ras_crop, county)
	model_data = data.frame(
		ndti = getValues(mean_min_ndti_county),
		rsd = getValues(rsd_ras_county)
	)
	county_model = lm(ndti ~ rsd, data=model_data)
	county_models[[test_county]] = county_model
}

save(list=c("central_model", "county_models"), file=file_models_out)

model_stats = data.frame(
	model="Central",
	slope=summary(central_model)$coefficients[2,1],
	p.value=summary(central_model)$coefficients[2,4],
	r2=summary(central_model)$r.squared
)
for (test_county in test_counties) {
	m = county_models[[test_county]]
	model_stats_row = data.frame(
		model=test_county,
		slope=summary(m)$coefficients[2,1],
		p.value = summary(m)$coefficients[2,4],
		r2 = summary(m)$r.squared
	)
	model_stats = rbind(model_stats, model_stats_row)
}

