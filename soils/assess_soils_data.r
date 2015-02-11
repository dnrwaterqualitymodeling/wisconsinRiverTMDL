options(stringsAsFactors=F)
library(ggplot2)
library(scatterplot3d)
library(car)

dir_net_soil = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"

file_default_soils = "SWAT_US_SSURGO_Soils_wrb.txt"
file_agg_soil_units = "aggregated_soil_units.txt"
file_agg_profs = "aggregated_profiles.txt"

dfault_soils = read.delim(paste(dir_net_soil, file_default_soils, sep ='/'))
swat_soils = read.delim(paste(dir_net_soil, file_agg_soil_units, sep ='/'))
col_titles = names(dfault_soils)
######### ---- ## ---- ## ---- ## ---- ## ---- ## ---- ## ---- ## ---- #
calc_horz_wtd_mean = function(col_name, df){
	cls = col_titles[grepl(col_name, col_titles)][1:5]
	dat.cls = df[cls]
	dat.cls[dat.cls<=0] = NA
	wts = col_titles[grepl("SOL_Z\\d", col_titles)][1:5]
	wts = df[wts]
	wts = wts-cbind(0,wts[,1:4])
	wts[wts<=0] = NA
	wts = wts/rowSums(wts, na.rm=T)
	dat.cls = rowSums(dat.cls * wts, na.rm=T)
}
########## ---- ## ---- ## ---- ## ---- ## ---- ## ---- ## ---- ## ---- #
dfault_soils["sand"] = calc_horz_wtd_mean("SAND", dfault_soils)
dfault_soils["silt"] = calc_horz_wtd_mean("SILT", dfault_soils)
dfault_soils["clay"] = calc_horz_wtd_mean("CLAY", dfault_soils)

dfault_soils["SOL_AWC"]= calc_horz_wtd_mean("SOL_AWC", dfault_soils)
dfault_soils["USLE_K"] = calc_horz_wtd_mean("USLE_K", dfault_soils)
dfault_soils["SOL_K"] = calc_horz_wtd_mean("SOL_K", dfault_soils)
dfault_soils["SOL_BD"]= calc_horz_wtd_mean("SOL_BD", dfault_soils)
dfault_soils["SOL_Z"] = calc_horz_wtd_mean("SOL_Z", dfault_soils)

dfault_soils = subset(dfault_soils, select=c("MUID", "HYDGRP", "SOL_ZMX", "sand", "silt", "clay", "SOL_AWC", "USLE_K", "SOL_K", "SOL_BD", "SOL_Z"))
dfault_soils["Data_Type"] = "SWAT_Default"
##### ----- ##### -----##### ----- ##### -----##### ----- ##### -----##### ----- #####

swat_soils["sand"] = calc_horz_wtd_mean("SAND", swat_soils)
swat_soils["silt"] = calc_horz_wtd_mean("SILT", swat_soils)
swat_soils["clay"] = calc_horz_wtd_mean("CLAY", swat_soils)

swat_soils["SOL_AWC"]= calc_horz_wtd_mean("SOL_AWC", swat_soils)
swat_soils["USLE_K"] = calc_horz_wtd_mean("USLE_K", swat_soils)
swat_soils["SOL_K"] = calc_horz_wtd_mean("SOL_K", swat_soils)
swat_soils["SOL_BD"]= calc_horz_wtd_mean("SOL_BD", swat_soils)
swat_soils["SOL_Z"] = calc_horz_wtd_mean("SOL_Z", swat_soils)

swat_soils = subset(swat_soils, select=c("MUID", "HYDGRP", "SOL_ZMX", "sand", "silt", "clay", "SOL_AWC", "USLE_K", "SOL_K", "SOL_BD", "SOL_Z"))
swat_soils["Data_Type"] = "WRB_Custom"
##### ----- ##### -----##### ----- ##### -----##### ----- ##### -----##### ----- #####
dat = rbind(dfault_soils, swat_soils)
dat = subset(dat, !is.na(HYDGRP))


png("./miscellaneous/new_clusters_texture_distribution_assessment.png",
	height = 7,
	width = 7,
	units = 'in',
	res=500)
text = ggplot(dat, aes(x=sand, y=clay, color=silt, shape=HYDGRP)) + theme_bw()  + ggtitle("Particle Size Distribution of \n New Clusters")
text = text + geom_point(size=5) + scale_shape_manual(values = c("A","B","C","D")) 
text + facet_grid(Data_Type ~ HYDGRP)
dev.off()

pdf("scatterplot_tests.pdf")
for (angl in seq(10, 360, 10)){
scatterplot3d(
	x=swat_soils$sand,
	y=swat_soils$clay,
	z=swat_soils$silt,
	pch = swat_soils$HYDGRP,
	main=paste("Angle =", angl,sep=''),
	angle=angl)
}
dev.off()
scatter3d(sand ~ clay + SOL_AWC | factor(HYDGRP),
	data = swat_soils,
	surface.alpha = 0,
	neg.res.col = '#000001ff',
	pos.res.col='#000001ff',
	labels = swat_soils$HYDGRP,
	id.n = nrow(swat_soils)-1)