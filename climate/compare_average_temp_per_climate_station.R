file_tmp = "C:/Users/radeca/Desktop/WRB/Scenarios/Default/TxtInOut/Tmp1.Tmp"
file_summary = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/tmp_summary.txt"

stations = readLines(file_tmp, n=1)
stations = substr(stations, 10, nchar(stations))
stations = strsplit(stations, ",")[[1]]

data = read.fwf(
	file_tmp,
	widths=c(7, rep(5,2*length(stations))),
	skip=4
)
names(data)[1] = "date"
names(data)[seq(2,by=2,length.out=length(stations))] = 
	paste(stations, "min", sep="_")
names(data)[seq(3,by=2,length.out=length(stations))] = 
	paste(stations, "max", sep="_")

data$date = paste(
	substr(as.character(data$date),1,4),
	substr(as.character(data$date),5,7),
	sep="-"
)

data$date = as.Date(data$date, format="%Y-%j")

data = subset(data, date > as.Date("2002-01-01"))

station_means_min = colMeans(data[seq(2,by=2,length.out=length(stations))])
station_means_max = colMeans(data[seq(3,by=2,length.out=length(stations))])

write.table(
	data.frame(
		station=strsplit(names(station_means_min),"_")[[1]][1],
		mean_min=station_means_min,
		mean_max=station_means_max
	),
	file_summary,
	sep="\t",
	row.names=F,
	quote=F
)
