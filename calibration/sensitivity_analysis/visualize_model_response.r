library(ncdf)

setwd("C:/Users/ruesca/Desktop")

nc_file = "ESCO.nc"
nc = open.ncdf(nc_file)

d = get.var.ncdf(nc, varid="streamflow", start=c(1,1,1), count=c(-1,-1,-1))
close.ncdf(nc)

subbasin_means = apply(d, c(2,3), mean)

pdf("ESCO.pdf", width=8.5, height=11)
par(mfrow=c(4,3))
for (i in 1:338) {
	plot(1:25, subbasin_means[i,], 
		ylab="Annual Average Streamflow (cms)", xlab="Iteration",
		main=paste("Subbasin", i))
}
dev.off()
