net_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/stationData/observations"
pdf_dir = "C:/Users/ruesca/Desktop"

setwd(net_dir)
fs = list.files()
for (f in fs) {
	pdf_file = paste(pdf_dir, "/", f, ".pdf", sep="")
	pdf(pdf_file, width=11, height=8.5)
	d = read.csv(f, stringsAsFactors=F)[,c(1,5)]
	d[d[,2] <= -699 | d[,2] >= 699,2] = NA
	d[,1] = as.Date(d[,1])
	ys = unique(format(d[,1], "%Y"), sort=T)
	for (y in ys) {
		yd = subset(d, format(Date, "%Y") == y)
		plot(Average.Relative.Humidity..Percent. ~ Date,
			data = yd,
			type = "l",
			ylab = "Relative humidity",
			xlab = "Date",
			main = paste(substr(f,1,3), y)
		)
	}
	dev.off()
}