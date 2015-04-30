options(stringsAsFactors=F)
library(ncdf)

path_cnop_tillage = "H:/output_data/sensitivity_output/CNOP_tillage_mgt.nc"
path_cnop_planting = "H:/output_data/sensitivity_output/CNOP_planting_mgt.nc"

vars = list(
	c("water_yield", "Annual Average water yield (mm)"),
	c("sediment", "Average daily sediment yield (metric tons/ha)"),
	c("org_phosphorus", "Average daily organic P yield (kg/ha)"),
	c("sol_phosphorus", "Average daily organic P yield (kg/ha)"),
	c("sed_phosphorus", "Average daily organic P yield (kg/ha)")
)

mod_per = seq(
	as.Date("2002-01-01"),
	as.Date("2013-12-31"),
	by="1 day"
)

sbs = c(1,53,74,80,141,149,171,189,191,197,221,247,257,268,275,276,299,329)
v = vars[[1]]
for (op in c(path_cnop_tillage, path_cnop_planting)){
	nc = open.ncdf(op)
	p = strsplit(basename(op), "\\.")[[1]][1]
	d = get.var.ncdf(nc, varid=v[1], start=c(1,1,1), count=c(-1,-1,-1))
	pdf("cnop_tillage.pdf")
	for (sb in sbs){
		sb_dat = d[,sb,]
		
		
		plot(x=mod_per[1:365],y=sb_dat[1:365,7],type='l',main=sb)
		lines(x=mod_per[1:365],y=sb_dat[1:365,1], col='blue', lwd=2)
		
		mean.diff = mean(sb_dat[,1] - sb_dat[,7])
		print(paste("    Mean difference for subbasin", sb))
		print(paste(mean.diff))
	}
	dev.off()
}