library(stringr)
options(stringsAsFactors=T)

dir_exc = "entire_90_pct_exc"
annual_basis = TRUE
exc_val = 0.10

cal_dir = 
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/calibration"
out_dir = paste(cal_dir, dir_exc, sep="/")
pdf(paste(out_dir, "/",dir_exc,".pdf",sep=''), width=11, height=8)
# subset_dir = paste(cal_dir, "/10_pct_exceedance_mam", sep="")
gauge_basin_lu_file =
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"

gauge_basin_lu = read.csv(gauge_basin_lu_file,
	colClasses=c("character", "character", "integer", "integer", "character"))
gauge_basin_lu = subset(gauge_basin_lu, Keep == 1)

obs_files = list.files(cal_dir, pattern="^0[0-9]+\\.txt$", full.names=T)
gauge_ids = str_extract(basename(obs_files), "^0[0-9]+")
obs_files = obs_files[gauge_ids %in% gauge_basin_lu$USGS_ID]

model_period = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")

for (obs_file in obs_files) {
	obsData_raw = read.table(obs_file, skip=2, sep="\t", header=T)
	obsData_raw = obsData_raw[-1,]
	obsData_raw[obsData_raw[,4] == "Ice", 4] = NA
    obsData = obsData_raw[obsData_raw[,5] == "A",]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW=as.numeric(as.character(obsData[,4])))
	# construct boolean for subsetting
	mam_days = model_period[
			as.integer(format(model_period, "%m")) %in% 1:12 #currently for june, july, aug, or 3:5 for mar, april, may
	]
	pct_exc = quantile(obsData$FLOW, exc_val)
	if (annual_basis){
		sub_data = data.frame()
		model_years = unique(format(model_period, "%Y"))
		for (yr in model_years){
			bool = format(as.Date(obsData_raw$datetime),"%Y") == yr 
			annual_sub_data = subset(obsData_raw, bool)
			annual_pct_exc = quantile(obsData[which(format(obsData$DATE, "%Y") == yr),"FLOW"], exc_val)
			exc_bool = as.numeric(as.character(annual_sub_data[,4])) <= annual_pct_exc
			annual_sub_data = subset(annual_sub_data, exc_bool)
			sub_data = rbind(sub_data, annual_sub_data)
		}
	} else {
		bool = as.Date(obsData_raw$datetime) %in% mam_days &
			as.numeric(as.character(obsData_raw[,4])) >= pct_exc
		sub_data = subset(obsData_raw, bool)
	}
	
	out_file = paste(out_dir, basename(obs_file), sep="/")
	raw_txt = readLines(obs_file)
	header_lines = grep("#", raw_txt)
	header_lines = 1:(max(header_lines) + 2)
	header = raw_txt[header_lines]
	writeLines(header, out_file)
	write.table(sub_data,
		out_file,
		sep="\t",
		append=T,
		row.names=F,
		col.names=F,
		quote=F)
	gage =  gsub(".txt", "", basename(obs_file))
	
	obsData_raw[,4] = as.numeric(as.character(obsData_raw[,4]))
	obsData_raw["datetime"] = as.Date(obsData_raw[,"datetime"])
	sub_data[,4] = as.numeric(as.character(sub_data[,4]))
	sub_data["datetime"] = as.Date(sub_data[,"datetime"])
	total_data = nrow(sub_data)
	obsData_raw = merge(obsData_raw, 
		data.frame(datetime=seq(
			as.Date(obsData_raw[1,3]), as.Date(obsData_raw[nrow(obsData_raw),3]),by='1 day')),
		all.x=T, all.y=T
	)
	sub_data = merge(sub_data, 
		data.frame(datetime=seq(
			as.Date(sub_data$datetime[1]), as.Date(sub_data$datetime[nrow(sub_data)]),by='1 day')),
		all.x=T, all.y=T
	)
	plot(y=obsData_raw[,4], x=obsData_raw$datetime,type='l', main=paste("Gauge:",gage))
	lines(y=sub_data[,4], x=sub_data$datetime, col="#FF0000",lty=1,lwd=2)
	legend(
		"topright",
		legend=c("Full", "Subset"),
		lty=c(1,1),
		lwd=c(1,2),
		col=c("black",'red'))
	text(x=
		as.Date(format(as.Date(sub_data$datetime[1]),"%Y"),"%Y"), y=min(obsData_raw[,4]), paste("Subset size:",total_data))
	
}
dev.off()

