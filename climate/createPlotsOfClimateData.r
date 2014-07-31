netdir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/"
setwd(netdir)

stationFiles = c('pcp.txt', 'tmp.txt')
pcpStations = read.csv('pcp.txt')
tmpStations = read.csv('tmp.txt')

stations = tmpStations$NAME

dates = seq(as.POSIXlt("1990-01-01"), as.POSIXlt("2013-12-30"), "day")

pdf(file="temperature_plots.pdf", width=8.5, height=11)
par(mfrow=c(3,1))
for (station in stations) {
    t_filename = paste(station, 'txt', sep='.')
    p_filename = paste(station, 'txt', sep='.')
    substr(p_filename, 1, 1) = 'p'
    tmp = read.csv(t_filename, skip=1)
    tmp = cbind(dates, tmp)
    tmp = tmp[tmp[,1] > as.POSIXlt("2002-01-01"),]
    if (file.exists(p_filename)) {
        pcp = read.csv(p_filename, skip=1)
        pcp = cbind(dates, pcp)
        pcp = pcp[pcp[,1] > as.POSIXlt("2002-01-01"),]
    }
    plot(rowMeans(tmp[,2:3]) ~ tmp[,1],
         xlab="Date", ylab="Average Daily Temperature (C)", type='l', main=station)
#     lines(tmp[,1], tmp[,3], col="red")
}
dev.off()

stations = pcpStations$NAME

pdf(file="precipitation_plots_monthly.pdf", width=8.5, height=11)
par(mfrow=c(3,1))
for (station in stations) {
    p_filename = paste(station, 'txt', sep='.')
    pcp = read.csv(p_filename, skip=1)
    pcp = cbind(dates, pcp)
    pcp = pcp[pcp[,1] > as.POSIXlt("2002-01-01"),]
    mo = strftime(pcp[,1], "%m")
    yr = strftime(pcp[,1], "%Y")
    pcp = data.frame(mo=mo, yr=yr, pcp=pcp[,2])
    pcp = aggregate(pcp ~ mo + yr, pcp, FUN=sum)
    mo = as.POSIXlt(paste(pcp$yr, pcp$mo, "01", sep="-"))
    pcp = data.frame(mo=mo, pcp=pcp$pcp)
    plot(pcp[,2] ~ pcp[,1], xlab="Date", type="h", ylab="Total monthly precipitation (mm)", main=station)
}
dev.off()