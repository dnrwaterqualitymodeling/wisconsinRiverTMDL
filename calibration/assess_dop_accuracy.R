library(dplyr)
library(tidyr)
library(sm)

file_gage_subbasin_lu = 
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"
file_output.rch = "C:/TEMP/WRB.Sufi2.SwatCup/output.rch"
dir_loads = 
	"T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads/DOP_00671/Loads"

load_ids = as.integer(sub("\\.gz", "", list.files(dir_loads)))

gage_subbasin_lu =
	read.csv(file_gage_subbasin_lu,
	colClasses=c(
		rep("character", 5),
		"integer",
		"integer",
		"character"
	)
)

gage_subbasin_lu = gage_subbasin_lu %>%
	filter(Keep == 1, LOAD_ID %in% load_ids) %>%
	mutate(LOAD_ID = as.integer(LOAD_ID)) %>%
	select(LOAD_ID, WRB_SubbasinID) %>%
	rename(REACH = WRB_SubbasinID)

w = c(-5,5,-9,6,-12,-12,-12,-12,12,-12)
output.rch = read.fwf(file_output.rch, widths=w, skip=9)
names(output.rch) = c("REACH", "DATE", "DOP_sim")

output.rch = output.rch %>%
	filter(REACH %in% gage_subbasin_lu$REACH, DATE <= 12)

sim_dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month")
sim_dates = format(sim_dates, "%Y-%m")

output.rch_sort = NULL
for (subbasin in gage_subbasin_lu$REACH) {
	output.rch_subbasin = output.rch %>%
		filter(REACH == subbasin)
	last_row = nrow(output.rch_subbasin)
	output.rch_subbasin = output.rch_subbasin[-last_row,]
	output.rch_subbasin$DATE = sim_dates
	output.rch_sort = rbind(output.rch_sort, output.rch_subbasin)
	
}

loads = NULL
for (load_id in gage_subbasin_lu$LOAD_ID) {
	file_load = paste(dir_loads, "/", load_id, ".gz", sep="")
	load = read.table(file_load, sep="\t", header=T)
	load$date = format(as.Date(load$date, "%m/%d/%Y"), "%Y-%m")
	load_grp = load %>%
		select(LOAD_ID=station_id, DATE=date, dload_00671) %>%
		group_by(LOAD_ID, DATE) %>%
		summarise(
			DOP_obs = sum(dload_00671, na.rm=T)
		)
		
	loads = rbind(loads, load_grp)
}

calib_tbl = output.rch_sort %>%
	left_join(gage_subbasin_lu) %>%
	left_join(loads) %>%
	select(REACH, DATE, DOP_obs, DOP_sim)

lims = c(
	min(c(calib_tbl$DOP_obs,calib_tbl$DOP_sim), na.rm=T),
	max(c(calib_tbl$DOP_obs,calib_tbl$DOP_sim), na.rm=T)
)

plot(
	DOP_obs ~ DOP_sim,
	data=calib_tbl,
	log="xy",
	xlim=lims,
	ylim=lims
)
abline(0,1,col="red")


hist(log(calib_tbl$DOP_obs), freq=F, col="#FF000099")
hist(log(calib_tbl$DOP_sim), freq=F, col="#00FF0099", add=T)


diff = aov(DOP_obs ~ DOP_sim, data=calib_tbl)

calib_tbl_long = calib_tbl %>%
	gather(source_data, DOP, DOP_sim:DOP_obs)

boxplot(DOP ~ source_data, data=calib_tbl_long, log="y")

sm.density.compare(calib_tbl_long$DOP, calib_tbl_long$source_data)
