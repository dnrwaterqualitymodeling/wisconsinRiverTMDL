library(ncdf)
library(rgdal)
library(raster)

options(stringsAsFactors=FALSE)

subbasins = readOGR(
	dsn="T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
	layer="subbasins_minus_urban_boundaries")
# regions = readOGR(
	# dsn="T:/Projects/Wisconsin_River/GIS_Datasets/Water_Budget",
	# layer="WRB_Budget_Divisions")

file_subbasin_region_lu = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Budget/subbasin_region_lookup.txt"
# setwd("C:/Users/ruesca/Desktop/WRB_sensitivity")

vars = list(
	c("streamflow", "Annual Average streamflow (cms)"),
	c("sediment", "Average Daily Sediment Load (tons)"),
	c("phosphorus", "Average Daily P Load (kg)")
)

sb_region_lu = read.delim(file_subbasin_region_lu)
nc_files = list.files("H:/WRB_sensitivity", pattern="\\.nc$")
regional_all = NULL
for (nc_file in nc_files) {
	nc = open.ncdf(paste("H:/WRB_sensitivity",nc_file,sep='/'))
	p = strsplit(nc_file, "\\.")[[1]][1]
	
	# to be changed Moonday
	subbasin_all = NULL
	for (v in vars) {
		print(c(p, v[1]))
		d = get.var.ncdf(nc, varid=v[1], start=c(1,1,1), count=c(-1,-1,-1))
		p_change = apply(d, 2,
			function(x) {
				x = cbind(x, rep(0,4383))
				offset = x
				offset[,2:26] = offset[,1:25]
				offset[,1] = rep(0,4383)
				dq = abs(x - offset)
				dq = dq[,2:25]
				return(mean(dq))
			}
		)
		
		subbasin_all = cbind(subbasin_all, p_change)
		
		sb_region_lu$delta = p_change
		regional_means = aggregate(
			delta ~ region, 
			data=sb_region_lu, 
			function (x) {
				cbind(mean(x),sd(x))})
		regional_means = data.frame(
			Region=regional_means$region,
			Parameter=p,
			Variable=v[1],
			delta_mean=regional_means$delta[,1],
			delta_sd=regional_means$delta[,2])
		regional_means = rbind(regional_means, data.frame(
			Region="Global",
			Parameter=p,
			Variable=v[1],
			delta_mean=mean(p_change),
			delta_sd=sd(p_change)))
		regional_all = rbind(regional_all, regional_means)
		
	}
	subbasin_all = cbind(1:337, subbasin_all)
	subbasin_all = data.frame(subbasin_all)
	names(subbasin_all) = c("subbasin", vars[[1]][1], vars[[2]][1], vars[[3]][1])
	
	file_name = paste("subbasin_", p, "_sensitivity_tst.txt", sep="")
	write.table(
		subbasin_all,
		file_name,
		sep='\t',
		row.names=F)
	close.ncdf(nc)
}
file_region = "regional_sensitivity.txt"
write.table(
	regional_all,
	file_region,
	sep='\t',
	row.names=F)
	
