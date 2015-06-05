file_daymet = "E:/daymet_precip_summary.txt"
file_ncdc = "E:/climate_stations_precip_summary.txt"

daymet = read.table(file_daymet, header=T)
ncdc = read.table(file_ncdc, header=T)

hist(
	ncdc$mean,
	ylim=c(0,125),
	xlim=c(1.4, 3.0),
	xlab="Daily Average Precip (mm)",
	col="#FF000055")
hist(
	daymet$mean,
	add=T,
	col="#0000FF55")

