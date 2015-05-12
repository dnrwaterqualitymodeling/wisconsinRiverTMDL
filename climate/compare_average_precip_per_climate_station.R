file_pcp = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut/pcp1.pcp"
file_summary = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/precip_summary.txt"

stations = readLines(file_pcp, n=1)
stations = substr(stations, 10, nchar(stations))
stations = strsplit(stations, ",")[[1]]

data = read.fwf(
	file_pcp,
	widths=c(7, rep(5,length(stations))),
	skip=4
)
names(data) = c("date",stations)
data$date = paste(
	substr(as.character(data$date),1,4),
	substr(as.character(data$date),5,7),
	sep="-"
)

data$date = as.Date(data$date, format="%Y-%j")

data = subset(data, date > as.Date("2002-01-01"))

station_means = colMeans(data[2:ncol(data)])

write.table(
	data.frame(
		station=names(station_means),
		mean=station_means
	),
	file_summary,
	sep="\t",
	row.names=F,
	quote=F
)
