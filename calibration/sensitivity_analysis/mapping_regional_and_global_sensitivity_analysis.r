library(ncdf)
library(rgdal)
library(raster)
library(RColorBrewer)
library(classInt)
library(rgeos)

options(stringsAsFactors = FALSE)
source("./Code/figures/function_proper_legend.r")
dir_sens = "T:/Projects/Wisconsin_River/GIS_Datasets/Sensitivity_Analysis"

subbasins = readOGR(
	dsn="T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
	layer="subbasins")
basin = readOGR(
	dsn=getwd(),
	layer="arc_basins")

regions = readOGR(
	dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Water_Budget",
	layer="WRB_Budget_Divisions")

file_subbasin_region_lu = "./Code/calibration/sensitivity_analysis/subbasin_region_lookup.txt"

# setwd("H:/sensitivity_text_files")
vars = list(
	c("water_yield_annual", "annual water yield (mm)"),
	c("water_yield_spring", "spring water yield (mm)"),
	c("sediment_annual", "annual sediment (Mg)"), 
	c("tot_phosphorus_annual", "annual P yield (kg)")
	# c("sediment", "Average Daily Sediment Load (tons)"),
	# c("phosphorus", "Average Daily P Load (kg)")
)

out_regional_file = paste(dir_sens, "regional_sensitivity.txt", sep="/")

sb_region_lu = read.delim(file_subbasin_region_lu)
out_subbasin_files = list.files(dir_sens, pattern="^subbasin*",full.name=T)


all_params = NULL
for (sub_file in out_subbasin_files) {
	param = substr(basename(sub_file), 10, 20)

	sb_fl = read.delim(sub_file)
	sb_fl['param_name'] = param
	all_params = rbind(all_params, sb_fl)
}

all_params = subset(all_params, !(param_name %in% c("TIMP_bsn_se", "SMTMP_bsn_s", "SMFMX_bsn_s", "SMFMN_bsn_s", "SLSUBBSN_hr", "SLSOIL_hru_", "SFTMP_bsn_s", "HRU_SLP_hru")))
# for easy merging
names(all_params)[1] = "Subbasin"
pal.brew = brewer.pal(9,"YlGnBu")

for (v in vars){
	print(v)
	toClass = all_params[v[1]]
	toClass = as.matrix(toClass)
	prop_int = classIntervals(toClass, 9, 'kmeans')
#     prop_colr = findColours(prop_int, pal)
    brks <- prop_int$brks
	pdf(paste(v[1],'_subbasin_maps.pdf',sep=''))
    for (param in unique(all_params$param_name)){
		print(paste("Working on ",param))
		ind_param = which(all_params$param_name == param)
		param_df = all_params[ind_param,]
		param_subbasins = merge(subbasins, param_df)

		plot(subbasins,
			col=pal.brew[findInterval(toClass[ind_param,v[1]], 
                brks,
                all.inside = T)],
			main = paste(param,v[1]),
			border = NA)
		title(main=v[1])
        txt <- properLegend(findColours(prop_int, pal.brew), sig_figs=4)
        legend('bottomright', 
			legend = txt, 
			bg = 'white', 
			bty = 'n',
            fill = pal.brew,
            title = paste(param))
		plot(basin, add=T)
	}
	dev.off()
}

#########################################################################
#### For regional and global plotting ###################################
#########################################################################
# out_regional = read.delim(out_regional_file)
# names(out_regional)[1] = "Name"
# for (v in vars){
	# df_regional = subset(out_regional, Variable == v[1])
	# toClass = as.matrix(df_regional[,4:5])
	# prop_int_mean = classIntervals(toClass[,1], 9, 'kmeans')
	# prop_int_sd = classIntervals(toClass[,2], 9, 'kmeans')
	
	# brks_mean <- prop_int_mean$brks
	# brks_sd = prop_int_sd$brks
	
	# stat_list = list(
		# list("delta_mean", prop_int_mean, brks_mean),
		# list("delta_sd", prop_int_sd, brks_sd))
	
	# pdf(paste(v,"_testHists.pdf",sep=''),width=11,height=8)
	# for (param in unique(df_regional$Parameter)){
		# print(paste("Working on", param))
		# ind_param = which(df_regional$Parameter == param)
		# df_param = df_regional[ind_param,]
		
		# param_regions = merge(regions, df_param)
		# par(mfrow=c(1,2))
		# for (stat in stat_list){
			# hist(param_regions@data$delta_mean, main = paste(param, stat[[1]]))

			# plot(regions,
				# col=pal.brew[findInterval(toClass[ind_param, stat[[1]]], 
					# stat[[3]],
					# all.inside = T)],
				# main = paste(param,v[1], stat[[1]]),
				# border = NA)
			# txt <- properLegend(findColours(stat[[2]], pal.brew), sig_figs=4)
			# legend('bottomright', 
				# legend = txt, 
				# bg = 'white', 
				# bty = 'n',
				# fill = pal.brew,
				# title = paste(param, stat[[1]]))
		# }
	# }
	# dev.off()
# }

#### to assess how to classify the data
# pal.brew = brewer.pal(9,"YlGnBu")

# toClass = all_params[v[1]]
# toClass = as.matrix(toClass)

# reduced_toClass = sample(toClass, nrow(toClass)*0.4)
### classification methods: 
# class_methods = c("sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher") # skipping "fixed", ", "jenks")
# pdf("potential_classification_methods_streamflow.pdf")
# for (mth in class_methods){
	# print(paste("working on ",mth))
	# prop_int = classIntervals(reduced_toClass, 9, mth)
	# plot(prop_int, pal.brew,main=mth)
# }
# dev.off()




















