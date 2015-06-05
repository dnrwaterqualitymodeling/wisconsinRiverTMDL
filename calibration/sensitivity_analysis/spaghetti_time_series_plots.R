library(ncdf)

nc = open.ncdf("C:/Users/ruesca/Desktop/SMTMP_bsn.nc")

dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by="1 day")

d = get.var.ncdf(nc, "sediment", start=c(1,1,1), count=c(-1,-1,-1))

subbasin = 137

pdf("C:/Users/ruesca/Desktop/SMTMP_rte_spaghetti.pdf", width=8.5, height=11)
for (year in 2002:2013) {
	date_inds = which(format(dates, "%Y") == year)
	min_q = min(d[date_inds,subbasin,1])
	max_q = max(d[date_inds,subbasin,1])
#	min_q = log(min(d[date_inds,subbasin,1]))
#	max_q = log(max(d[date_inds,subbasin,1]))
	plot(
		d[date_inds,subbasin,1] ~ dates[date_inds],
#		log(d[date_inds,subbasin,1]) ~ dates[date_inds],
		ylim=c(min_q, max_q),
		xlab="Date",
		ylab="Streamflow (cms)",
		type="l",
		main=year,
		col="#0000FF55"
	)
	ci = apply(
		d[date_inds,subbasin,],
		1,
		function(X) {
			e = (qnorm(0.975)*sd(X))/sqrt(length(X))
			return(c(mean(X) - e, mean(X) + e))
		}
	)
	for (i in 2:25) {
		lines(
			dates[date_inds],
			d[date_inds,subbasin,i]
#			log(d[date_inds,subbasin,i])
		)
	}
		
}
dev.off()
close.ncdf(nc)