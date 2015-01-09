library(ncdf)

setwd("D:/WRB_sensitivity")
vars = list(
	c("streamflow", "Annual Average streamflow (cms)"),
	c("sediment", "Average Daily Sediment Load (tons)"),
	c("phosphorus", "Average Daily P Load (kg)")
)
aggregate_by_region = T

if (aggregate_by_region){
	sb_region_lu = read.delim(file_subbasin_region_lu)
}

nc_files = list.files(pattern="\\.nc$")

for (nc_file in nc_files) {
	nc = open.ncdf(nc_file)
	p = strsplit(nc_file, "\\.")[[1]][1]
	for (v in vars) {
		print(c(p, v[1]))
		d = get.var.ncdf(nc, varid=v[1], start=c(1,1,1), count=c(-1,-1,-1))
		subbasin_means = apply(d, c(2,3), mean)
		pdf_file = paste(p, "_", v[1], ".pdf", sep="")
		pdf(pdf_file, width=8.5, height=11)
		par(mfrow=c(4,3))
		for (i in 1:338) {
			plot(1:25, subbasin_means[i,], 
				ylab=v[2], xlab="Iteration",
				main=paste("Subbasin", i))
		}
		dev.off()
	}
	close.ncdf(nc)
}
