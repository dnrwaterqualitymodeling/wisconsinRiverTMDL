# library(RODBC)
setInternet2(TRUE)
#source("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/validation/functions_query_output.r")


# CHANGE THESE ###########
# SWAT project
projectDir = "H:/WRB"
# Scenario
scenario = "Default"
# Subbasin ID
subbasinID = 137
obs_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/reservoir"
monthly = F
# DON'T CHANGE ANYTHING BELOW HERE #####

sb_swat_id_lu = read.delim(paste(obs_dir, "reservoir_subbasin_id_lu.txt",sep ='/'))

obs_files = list.files(obs_dir, "^subbasin", full.names=T)

plotDir = paste(projectDir, "plots", sep="/")

if (!file.exists(plotDir)) {
	dir.create(plotDir)
}

output.rsv = paste(projectDir, "Scenarios", scenario, "TxtInOut/output.rsv", sep="/")
dat = readLines(output.rsv)
dat = dat[10:length(dat)]
dat = gsub("\\s+", ",", dat)
tf = tempfile(fileext=".csv")
writeLines(dat, tf)
dat = read.table(tf, sep=",")
dat = dat[,c(2,3,4,6)]
names(dat) = c("res", "mon", "vol", "flow")

# changing swat res ID to subbasin id
for (rs in unique(dat$res)){
	subID = sb_swat_id_lu[which(sb_swat_id_lu$swat_id == rs), "subbasin_id"]
	res.ind = which(dat$res == rs)
	dat[res.ind, "res"] = subID
}

for (obsDataFile in obs_files) {
	obsData = read.table(obsDataFile, sep=",", header=T, stringsAsFactors=F)
	obsData = data.frame(DATE = as.Date(as.character(obsData[,1]), format="%m/%d/%Y"),
		FLOW_OBSERVED=as.numeric(as.character(obsData[,2])))
	months = as.POSIXlt(obsData$DATE)$mo + 1
	years = as.POSIXlt(obsData$DATE)$year + 1900
	date = paste(years, months, "01", sep="-")
	if (monthly) {
		obsMonthly = aggregate(FLOW_OBSERVED ~ date, data=obsData, mean)
		obsData = data.frame(DATE=as.Date(obsMonthly[,1]),
			FLOW_OBSERVED=obsMonthly[,2])
		obsData = obsData[order(obsData$DATE),]
	} else {
	  obsData = data.frame(DATE=as.Date(obsData[,1]),
		  FLOW_OBSERVED=obsData[,2])
	  obsData = obsData[order(obsData$DATE),]
	}
	obsData = obsData[which(obsData$DATE >= as.Date("2002-01-01") ),]

	res_id = as.integer(strsplit(basename(obsDataFile), "_|\\.")[[1]][2])
	print(paste("Working on subbasin", res_id))
	modData = subset(dat, res==as.integer(res_id))
	
	if (monthly) {
		modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month")
	} else {
		modTS = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
	}
	modData = data.frame("DATE" = modTS,
		VOL_MODELED = modData$vol,
		FLOW_MODELED = modData$flow)

	if (monthly) {
		# pdf(paste(plotDir, "/", run, ".pdf", sep=""), width=11, height=8.5)
		# d = merge(obsData, modData)
		# ylim = c(min(d[,2:3]), max(d[,2:3]))
		# plot(FLOW_OBSERVED ~ DATE,
			# data=d,
			# type="l",
			# col="aquamarine",
			# ylab="Flow (cfs)",
			# xlab="Date",
			# ylim=ylim,
			# lwd=3)
		# lines(FLOW_MODELED ~ DATE, data=d, col="coral1", lwd=3)
		# legend("topright", c("Modeled", "Observed"), fill=c("coral1","aquamarine"))
		# plot(FLOW_MODELED ~ FLOW_OBSERVED, data=d, pch=20, ylab="Modeled flow (cfs)", xlab="Observed flow (cfs)")
		# dev.off()
	} else {
		pdf(paste(plotDir, "/reservoir_check_", res_id,".pdf", sep=""), width=11, height=8.5)
		d = merge(obsData, modData)
		for (yr in 2002:2013) {
			print(paste("plotting year", yr))
			date_query = d$DATE >= as.Date(paste(yr,"-01-01", sep="")) &
				d$DATE <= as.Date(paste(yr,"-12-31", sep=""))
			d_yr = d[date_query,]
			ylim = c(min(d_yr[,c(2,4)]), max(d_yr[,c(2,4)]))
			plot(FLOW_OBSERVED ~ DATE,
				 data=d_yr,
				 main=yr,
				 type="l",
				 col="aquamarine",
				 ylab="Flow (cms)",
				 xlab="Date",
				 ylim=ylim,
				 lwd=3)
			axis(side = 2, col = 'black')
			lines(FLOW_MODELED ~ DATE, data=d_yr, col="coral", lwd=3)
			par(new = TRUE) 
			plot(VOL_MODELED ~ DATE, 
				data = d_yr, 
				col = 'grey50', 
				axes = F,
				type = 'l',
				ylab = '',
				xlab = '')
			axis(side=4, col = 'grey50')
			legend("topleft", 
				c("Modeled Flow", 
					"Observed Flow", 
					"Modeled Volume"), 
				fill=c("coral","aquamarine", "grey50"))
			# plot(FLOW_MODELED ~ FLOW_OBSERVED,
				 # data=d_yr,
				 # pch=20,
				 # main=yr,
				 # ylab="Modeled flow (cfs)",
				 # xlab="Observed flow (cfs)")
		}
		dev.off()
	}
}
