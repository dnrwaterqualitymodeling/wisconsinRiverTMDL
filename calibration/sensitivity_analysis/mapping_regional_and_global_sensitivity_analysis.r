library(ncdf)
library(rgdal)
library(raster)
library(RColorBrewer)
library(classInt)
library(rgeos)

options(stringsAsFactors = FALSE)
source("./Code/figures/function_proper_legend.r")
dir_sens = "T:/Projects/Wisconsin_River/GIS_Datasets/Sensitivity_Analysis"

pal.brew = brewer.pal(9,"YlGnBu")

subbasins = readOGR(
	dsn="T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
	layer="subbasins")
basin = readOGR(
	dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology",
	layer="wrb_basin")

regions = readOGR(
	dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Water_Budget",
	layer="WRB_Budget_Divisions")

file_subbasin_region_lu = "./Code/calibration/sensitivity_analysis/subbasin_region_lookup.txt"

sub_vars = list(
	c("water_yield_annual", "annual water yield (mm)"),
	c("water_yield_autumn", "autumn water yield (mm)"),
	c("water_yield_spring", "spring water yield (mm)"),
	c("water_yield_summer", "summer water yield (mm)"),
	c("water_yield_winter", "winter water yield (mm)"),
	c("sediment_annual", "annual sediment (Mg)"),
	c("sediment_autumn", "autumn sediment (Mg)"),
	c("sediment_spring", "spring sediment (Mg)"),
	c("sediment_summer", "summer sediment (Mg)"),
	c("sediment_winter", "winter sediment (Mg)"),
	c("tot_phosphorus_autumn", "autumn P yield (kg)"),
	c("tot_phosphorus_spring", "spring P yield (kg)"),
	c("tot_phosphorus_summer", "summer P yield (kg)"),
	c("tot_phosphorus_winter", "winter P yield (kg)"),
	c("tot_phosphorus_annual", "annual P yield (kg)")
)
rch_vars = list(
	c("streamflow_annual", "Streamflow (cms)"),
	c("streamflow_winter", "Streamflow (cms)"),
	c("streamflow_spring", "Streamflow (cms)"),
	c("streamflow_summer", "Streamflow (cms)"),
	c("streamflow_autumn", "Streamflow (cms)"),
	c("sediment_annual", "Average Daily Sediment Load (tons)"),
	c("sediment_winter", "Average Daily Sediment Load (tons)"),
	c("sediment_spring", "Average Daily Sediment Load (tons)"),
	c("sediment_summer", "Average Daily Sediment Load (tons)"),
	c("sediment_autumn", "Average Daily Sediment Load (tons)"),
	c("phosphorus_annual", "Average Daily P Load (kg)"),
	c("phosphorus_winter", "Average Daily P Load (kg)"),
	c("phosphorus_spring", "Average Daily P Load (kg)"),
	c("phosphorus_summer", "Average Daily P Load (kg)"),
	c("phosphorus_autumn", "Average Daily P Load (kg)")
)

out_regional_file = paste(dir_sens, "regional_sensitivity.txt", sep="/")

sb_region_lu = read.delim(file_subbasin_region_lu)
out_subbasin_files = list.files(
	paste(dir_sens, "subbasin", sep="/"),
	pattern="^subbasin*",full.name=T)
#############################################
### First for just route/reach parameters

rte_params = NULL
for (sub_file in out_subbasin_files[grepl("rte", out_subbasin_files)]) {
	param = basename(sub_file)
	param = gsub("subbasin_", "", param)
	param = gsub("_sensitivity.txt", "", param)

	sb_fl = read.delim(sub_file)
	sb_fl['param_name'] = param
	rte_params = rbind(rte_params, sb_fl)
}

names(rte_params)[1] = "Subbasin"

for (v in rch_vars){
	print(v)
	toClass = rte_params[v[1]]
	toClass = as.matrix(toClass)
	prop_int = classIntervals(toClass, 9, 'kmeans')
#     prop_colr = findColours(prop_int, pal)
    brks <- prop_int$brks
	pdf(paste(v[1],'_subbasin_maps.pdf',sep=''))
    for (param in unique(rte_params$param_name)){
		print(paste("Working on ",param))
		ind_param = which(rte_params$param_name == param)
		param_df = rte_params[ind_param,]
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
#############################################
### For just water yield output

sub_params = NULL
for (sub_file in out_subbasin_files[!grepl("rte", out_subbasin_files)]) {
	param = basename(sub_file)
	param = gsub("subbasin_", "", param)
	param = gsub("_sensitivity.txt", "", param)

	sb_fl = read.delim(sub_file)
	sb_fl['param_name'] = param
	sub_params = rbind(sub_params, sb_fl)
}

names(sub_params)[1] = "Subbasin"
### removing CNOPs 
sub_params = subset(sub_params, !grepl("CNOP", param_name))

for (v in sub_vars){
	print(v)
	toClass = sub_params[v[1]]
	toClass = as.matrix(toClass)
	prop_int = classIntervals(toClass, 9, 'kmeans')
#     prop_colr = findColours(prop_int, pal)
    brks <- prop_int$brks
	pdf(paste(v[1],'_subbasin_maps.pdf',sep=''))
    for (param in unique(sub_params$param_name)){
		print(paste("Working on ",param))
		ind_param = which(sub_params$param_name == param)
		param_df = sub_params[ind_param,]
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
#########   Never quite figured out how to ge this to work ##############
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

