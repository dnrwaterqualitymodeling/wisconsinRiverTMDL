library(RODBC)

file_wt = "T:/Projects/Wisconsin_River/Model_Outputs/tp_bias_crrection/weighting_factors.txt"
file_output.rch = "C:/TEMP/WRB.Sufi2.SwatCup/output.rch"
file_gage_basin_lu = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"
dir_conc = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads/TP_00665/Loads"
dir_conc_alt = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_loads/USGS_NWIS/for_bias_correction"
subbasins = c(78,127,137,140,141,142,149,150,151,152,155,157,158,159,162,184,195,199,226,268,326)
file_out = "T:/Projects/Wisconsin_River/Model_Outputs/tp_bias_crrection/tp_bias_correction_factors.txt"


col.names = c("rch", "gis", "mo", "da", "yr", "area", "flow_in", "flow_out", "sed_out", "tp")
output.rch = read.table(file_output.rch, skip=9)[,-1]
names(output.rch) = col.names
output.rch = subset(output.rch, select=c(rch,mo,da,yr,flow_out,tp))
output.rch = with(output.rch,
	data.frame(
		rch=rch,
		date=as.Date(paste(yr, mo, da, sep="-")),
		tp=(tp/flow_out)*11.57407
	)
)

lu = read.csv(file_gage_basin_lu)[c("LOAD_ID", "WRB_SubbasinID")]
lu = subset(lu, WRB_SubbasinID %in% subbasins)

bias = data.frame()
for (subbasin in subbasins) {
	print(subbasin)
	id = lu$LOAD_ID[lu$WRB_SubbasinID == subbasin]
	file_obs = paste(dir_conc, "/", id, ".gz", sep="")
	if (!file.exists(file_obs)) {
		file_obs = paste(dir_conc_alt, "/0", id, ".txt", sep="")
		obs = read.table(file_obs, stringsAsFactors=F)
		names(obs) = as.character(obs[1,])
		obs = obs[-(1:2),]
		obs = obs[obs[,5] == "A" & obs[,7] == "A",]
		obs$datetime = as.Date(obs$datetime)
		obs = subset(
			obs, 
			as.integer(format(datetime, "%m")) >= 5 &
				as.integer(format(datetime, "%m")) <= 10
		)
		obs = (as.numeric(obs[,6]) / as.numeric(obs[,4])) * 185.399
		obs = median(as.numeric(obs), na.rm=T)
	} else {
		obs = read.table(file_obs, stringsAsFactors=F, sep="\t", header=T)
		obs$date = as.Date(obs$date, format="%m/%d/%Y")
		obs = subset(obs,
			!is.na(actual_00665) &
				(as.integer(format(date, "%m")) >= 5 &
					as.integer(format(date, "%m")) <= 10)
		)
		obs = (obs$actual_00665 / obs$dflow)*408.735097
		obs = median(obs, na.rm=T)
	}
	pred = subset(
		output.rch,
		rch == subbasin & 
			(as.integer(format(date, "%m")) >= 5 &
				as.integer(format(date, "%m")) <= 10)
	)
	pred = median(pred$tp, na.rm=T)
	bias_row = data.frame(
		subbasin = subbasin,
		bias = obs / pred
	)
	bias = rbind(bias, bias_row)
}

wts = read.table(file_wt, header=T, stringsAsFactors=F)
wts = merge(wts, bias, by.x="calibration_reach", by.y="subbasin", all.x=T)
wts$bias[wts$direction == "none"] = 1
wts$adj_bias[wts$direction == "none"] = 1
wts$direction[wts$direction %in% c("upstream", "downstream")] = "adjacent"
names(wts)[3] = "method"
wts$adj_bias = wts$norm_adj * wts$bias
adj_tbl = aggregate(adj_bias ~ reach + method, data=wts, FUN=sum)

write.table(adj_tbl, file_out, row.names=F, sep="\t")
