arguments = commandArgs(trailingOnly = T)
nc_file = arguments[1]
dir_out = arguments[2]

dir_nc = "D:/WRB_sensitivity_analysis"
# dir_nc = "/media/d/WRB_sensitivity_analysis"
# dir_out = dir_nc
file_subbasin_region_lu = "D:/Water_Budget/subbasin_region_lookup.txt"
# file_subbasin_region_lu = "/media/d/Water_Budget/subbasin_region_lookup.txt"

library(ncdf)
library(rgdal)
library(raster)
library(reshape2)

options(stringsAsFactors=FALSE)
 
setwd(dir_out)

sub_vars = list(
	c("water_yield", "Annual Average water yield (mm)"),
	c("sediment", "Average daily sediment yield (metric tons/ha)"),
	c("org_phosphorus", "Average daily organic P yield (kg/ha)"),
	c("sol_phosphorus", "Average daily organic P yield (kg/ha)"),
	c("sed_phosphorus", "Average daily organic P yield (kg/ha)"),
	c("tot_phosphorus", "Average daily total P yield (kg/ha)")
)

rch_vars = list(
	c("streamflow", "Annual Average streamflow (cms)"),
	c("sediment", "Average Daily Sediment Load (tons)"),
	c("phosphorus", "Average Daily P Load (kg)"),
	c("deltaQ", "Net Streamflow (cms)")
)

sb_region_lu = read.delim(file_subbasin_region_lu)

dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
mos = format(dates, "%m")
seasons = mos
seasons = replace(seasons, as.integer(mos) %in% 3:5, "spring")
seasons = replace(seasons, as.integer(mos) %in% 6:8, "summer")
seasons = replace(seasons, as.integer(mos) %in% 9:11, "autumn")
seasons = replace(seasons, as.integer(mos) %in% c(12,1,2), "winter")

all_nc_files = list.files(dir_nc, pattern="\\.nc$", full.names=T)

percent_change = function(x) {
	n_days = dim(x)[1]
	x = cbind(x, rep(0,n_days))
	offset = x
	offset[,2:26] = offset[,1:25]
	offset[,1] = rep(0,n_days)
	dq = abs(x - offset)
	dq = dq[,2:25]
	return(mean(dq))
}

regional_all = NULL


if (grepl("rch", nc_file){
	vars = rch_vars
	dat = "rch"
} else {
	vars = sub_vars
	dat = "sub"
}
# nc_files = all_nc_files[grepl(dat, all_nc_files)]
for (nc_file in nc_files) {
	nc = open.ncdf(nc_file)
	p = strsplit(nc_file, "\\.")[[1]][1]
	
	subbasins_all = NULL

	for (v in vars) {
		print(c(p, v[1]))
		if (v[1] != "tot_phosphorus") {
			d = get.var.ncdf(nc, varid=v[1], start=c(1,1,1), count=c(-1,-1,-1))
		} else {
			i = 0
			for (p_species in paste(c("org", "sol", "sed"), "phosphorus", sep="_")) {
				i = i + 1
				if (i == 1) {
					d = get.var.ncdf(nc, varid=p_species, start=c(1,1,1), count=c(-1,-1,-1))
				} else {
					d = d + get.var.ncdf(nc, varid=p_species, start=c(1,1,1), count=c(-1,-1,-1))
				}
			}	
		}
		p_change = apply(d, 2, percent_change)
		subbasins_var = cbind(1:dim(d)[2], p_change)
		for (season in unique(seasons)) {
			d_season = d[which(seasons == season),,]
			p_change = apply(d_season, 2, percent_change)
			subbasins_var = cbind(subbasins_var, p_change)
		}
		subbasins_var = data.frame(subbasins_var)
		names(subbasins_var)[1] = "Subbasin"
		names(subbasins_var)[2] = paste(v[1], "_annual", sep="")
		names(subbasins_var)[3:6] = paste(v[1], "_", unique(seasons), sep="")
		subbasins_var = melt(subbasins_var, id.var="Subbasin")
		subbasins_all = rbind(subbasins_all, subbasins_var)
	}
	subbasins_all = merge(subbasins_all, sb_region_lu[,2:3], all.x=T, all.y=F)
	regional_means = aggregate(
			value ~ region + variable, 
			data=subbasins_all, 
			mean)
	regional_sd = aggregate(
			value ~ region + variable, 
			data=subbasins_all, 
			sd)
	subbasins_all$region = "Global"
	global_means = aggregate(
			value ~ region + variable, 
			data=subbasins_all, 
			mean)
	global_sd = aggregate(
			value ~ region + variable, 
			data=subbasins_all, 
			sd)
	
	regional_means$variable = paste(regional_means$variable, "_mean", sep="")
	regional_sd$variable = paste(regional_sd$variable, "_sd", sep="")
	global_means$variable = paste(global_means$variable, "_mean", sep="")
	global_sd$variable = paste(global_sd$variable, "_sd", sep="")
	
	regional_stats = rbind(regional_means, regional_sd, global_means, global_sd)
	regional_stats = cbind(data.frame(parameter=basename(p)), regional_stats)
	regional_all = rbind(regional_all, regional_stats)
	
	subbasins_all = subbasins_all[c("Subbasin", "variable", "value")]
	names(subbasins_all)[1] = "subbasin"
	subbasins_all = subbasins_all[order(subbasins_all$subbasin),]
	subbasins_all = dcast(subbasins_all, subbasin ~ variable)
	file_name = paste("subbasin_", basename(p), "_sensitivity.txt", sep="")
	write.table(
			subbasins_all,
			file_name,
			sep='\t',
			row.names=F)
	close.ncdf(nc)
}
file_region = paste0(dat, "regional_sensitivity.txt")
write.table(
		regional_all,
		file_region,
		sep='\t',
		row.names=F)

