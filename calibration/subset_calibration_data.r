library(stringr)
options(stringsAsFactors=F)

# VARIABLES THAT NEED TO BE EDITED

dir_exc = "AMJJASO_50_pct_exc_northern"
exc_val = 0.5
mos = 4:10
annual_basis = TRUE
ecos =
#	"Driftless Area and Southeastern Till Plains"
#	"Central Sands and Moraines"
#	"North Central Hardwood Forests"
	"Northern Lakes and Forests"

# This function needs to be edited depending on how the flow data should be sliced 
subset_cal_data = function(data_raw, mos, exc_val) {
	pct_exc = quantile(data_raw[,4], 1 - exc_val)
	mo_bool = as.integer(format(data_raw[,3], "%m")) %in% mos
	exc_bool = data_raw[,4] >= pct_exc # this is likely the only line to change
	bool = mo_bool & exc_bool
	return(bool)
}
cal_dir = 
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_raw/calibration"
file_sb_eco_lu = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/SB_by_ECO_lookup.txt"

# STOP EDITING BELOW HERE

sb_eco_lu = read.table(file_sb_eco_lu, sep="\t", header=T)

out_dir = paste(cal_dir, dir_exc, sep="/")
if (!file.exists(out_dir)) {
	dir.create(out_dir)
}
pdf(paste(out_dir, "/", dir_exc, ".pdf",sep=''), width=11, height=8)
# subset_dir = paste(cal_dir, "/10_pct_exceedance_mam", sep="")
gauge_basin_lu_file =
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"

gauge_basin_lu = read.csv(gauge_basin_lu_file,
	colClasses=c(rep("character", 4), "integer", "integer", "character"))
gauge_basin_lu = subset(gauge_basin_lu, Keep == 1)

obs_files = list.files(cal_dir, pattern="^0[0-9]+\\.txt$", full.names=T)
gauge_ids_eco = subset(gauge_basin_lu, ECOREGION %in% ecos)$USGS_ID
obs_files = obs_files[grep(paste(gauge_ids_eco, collapse="|"), obs_files)]

model_period = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")

for (obs_file in obs_files) {
	obsData_raw = read.table(
		obs_file,
		skip=2,
		sep="\t",
		header=T
	)
	obsData_raw = obsData_raw[-1,]
	obsData_raw[,3] = as.Date(obsData_raw[,3])
	obsData_raw = obsData_raw[obsData_raw[,4] != "Ice",]
	obsData_raw[,4] = as.numeric(obsData_raw[,4])
    obsData_raw = obsData_raw[obsData_raw[,5] == "A",]

	if (annual_basis){
		sub_data = data.frame()
		obs_years = unique(format(obsData_raw[,3], "%Y"))
		for (yr in obs_years){
			yr_bool = format(obsData_raw[,3], "%Y") == yr 
			annual_sub_data = subset(obsData_raw, yr_bool)
			annual_sub_data = subset(
				annual_sub_data,
				subset_cal_data(annual_sub_data, mos, exc_val)
			)
			sub_data = rbind(sub_data, annual_sub_data)
		}
	} else {
		sub_data = subset(
			obsData_raw,
			subset_cal_data(obsData_raw, mos, exc_val))
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
#	# Below used for plotting only
	gage =  gsub(".txt", "", basename(obs_file))
	total_data = nrow(sub_data)
	obsData_raw = merge(obsData_raw, 
		data.frame(datetime=seq(
			obsData_raw[1,3], obsData_raw[nrow(obsData_raw),3], by='1 day')),
		all.x=T, all.y=T)
	sub_data = merge(sub_data, 
		data.frame(datetime=seq(
			sub_data$datetime[1], sub_data$datetime[nrow(sub_data)], by='1 day')),
		all.x=T, all.y=T)
	
	plot(y=obsData_raw[,4], x=obsData_raw$datetime,type='l', main=paste("Gage:",gage))
	lines(y=sub_data[,4], x=sub_data$datetime, col="#FF0000",lty=1,lwd=2)
	legend(
		"topright",
		legend=c("Full", "Subset"),
		lty=c(1,1),
		lwd=c(1,2),
		col=c("black",'red'))
	text(x=as.Date(format(as.Date(sub_data$datetime[1]),"%Y"),"%Y"),
		y=max(obsData_raw[,4],na.rm=T), paste("Subset size:",total_data))
}
dev.off()

