library(RODBC)

# CHANGE THESE ###########
# SWAT project
projectDir = "C:/SWAT/Reservoirs_2"
# Subbasin ID
subbasinID = 137
# Raw USGS data
obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/baraboo/dv.txt"
monthly = T
run = "added_castle_rock"
# DON'T CHNAGE ANYTHING BELOW HERE #####



plotDir = paste(projectDir, "plots", sep="/")

if (!file.exists(plotDir)) {
	dir.create(plotDir)
}

obsData = read.table(obsDataFile, skip=2, sep="\t", header=T)
obsData = obsData[-1,]
obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
	FLOW_OBSERVED=as.numeric(as.character(obsData[,4])))
if (monthly) {
	months = as.POSIXlt(obsData$DATE)$mo + 1
	years = as.POSIXlt(obsData$DATE)$year + 1900
	date = paste(years, months, "01", sep="-")
	obsMonthly = aggregate(obsData$FLOW_OBSERVED, list(date), mean) 
	obsData = data.frame(DATE=as.Date(unique(date)),
		FLOW_OBSERVED=obsMonthly[,2])
}


outDb = paste(projectDir, "Scenarios/Default/TablesOut/SWATOutput.mdb", sep="/")
con = odbcConnectAccess(outDb)

query = paste("SELECT YEAR,MON,FLOW_OUTcms FROM rch WHERE SUB =", subbasinID)

modData = sqlQuery(con, query)
close(con)

mon = formatC(modData$MON, width = 2, format = "d", flag = "0")

modTS = as.Date(paste(modData$YEAR, mon, "01", sep="-"))
modData = data.frame("DATE" = modTS, FLOW_MODELED = modData$FLOW_OUTcms * 35.3)

pdf(paste(plotDir, "/", run, ".pdf", sep=""), width=11, height=8.5)
d = merge(obsData, modData)
ylim = c(min(d[,2:3]), max(d[,2:3]))
plot(FLOW_OBSERVED ~ DATE,
	data=d,
	type="l",
	col="aquamarine",
	ylab="Flow (cfs)",
	xlab="Date",
	ylim=ylim,
	lwd=3)
lines(FLOW_MODELED ~ DATE, data=d, col="coral1", lwd=3)
legend("topright", c("Modeled", "Observed"), fill=c("coral1","aquamarine"))
plot(FLOW_MODELED ~ FLOW_OBSERVED, data=d, pch=20, ylab="Modeled flow (cfs)", xlab="Observed flow (cfs)")
dev.off()
