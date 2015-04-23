library(rgdal)
library(rgeos)
library(raster)
library(reshape2)
library(ggplot2)
options(stringsAsFactors=F)
source("~/Code/calibration/functions_query_output.r")
source("~/Code/swat_output_assessment/batch_grep.r")

file_subbasins = "T:\\Projects\\Wisconsin_River\\Model_Inputs\\SWAT_Inputs\\hydro\\subbasins.shp"

dir_ets <- "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/et_data"
txtInout = "H:\\WRB\\Scenarios\\Default\\TxtInOut"
gdal_path = "C:/Program Files/GDAL"

tiffs = list.files(dir_ets, pattern="wrb*", full.names=T)

sbs = c(1,171,191,141,74,53,257,149,329,80,275,268,276,299,221,247,189,197)
# extr_subs_fun(
	# subbasins=sbs,
	# src_folder=gsub("\\\\","/",wd),
	# hru=T,
	# rch=F,
	# dst_folder="H:/")
# Creating layers of max and normal surface area
# if (!file.exists(gdal_path)){
	# print("Whoops! Requires GDAL on your C drive")
# }
# gdalwarp = "gdalwarp"
# gdaltranslate = "gdal_translate"
subbasins = shapefile(file_subbasins)

# wtm = paste('EPSG:3070',sep='')
# for (tif in tiffs){
	# tif = gsub("/", "\\\\",tif)
	# wtm_file = paste(
		# dirname(tif),
		# paste("wtm_",basename(tif), sep="")
	# ,sep='/')
	

	# ln1 = paste(
		# gdalwarp,
		# "-t_srs",
		# wtm,
		# "-of GTiff",
		# tif,
		# wtm_file)
	# system(ln1, wait=T)
# }

files_modis = list.files(dir_ets, "wtm*",full.names=T)

# par(mfrow=c(1,5))
stck = stack()
for (fl in 1:length(files_modis)){
	print(fl)
	mds = raster(files_modis[fl])
	mdsvals = getValues(mds)
	# to remove values of waterbodies
	mdsvals[mdsvals>9000] = NA
	# to remove 0 values
	mdsvals[mdsvals==0] = NA
	# to convert to mm/yr
	mdsvals = mdsvals * 0.1
	mdsr = setValues(mds, mdsvals)

	stck = stack(stck,mdsr)
	
	# plot(mdsr,
		# axes=F,
		# main=substr(basename(files_modis[fl]),17,23))
}

brck = brick(stck)

sbst = subset(subbasins, GRIDCODE %in% sbs)

et_df = extract(brck, sbst, fun=mean,na.rm=T,df=F,sp=T)
et_df = et_df@data
et_df = melt(et_df,id.vars='Subbasin', measure.vars=names(et_df)[3:length(et_df)])
et_df$YEAR = as.numeric(substr(et_df$variable, 20, 24))

gplt = ggplot(et_df, aes(x=YEAR, y=value, color=as.factor(Subbasin)))
gplt + geom_line()
