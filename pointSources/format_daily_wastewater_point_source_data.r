library(rgdal)
library(rgeos)
library(foreign)
options(stringsAsFactors=F)

# define inputs
of_file = "T:/Projects/Wisconsin_River/GIS_Datasets/Outfalls/WRB_Permitted_Outfall_Data_DRAFT_Jan2015.txt"
lu_file = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/DRAFT WASTEWATER DATASET FILES/WRB_Outfalls_DRAFT_JAN2015.dbf"
out_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Outfalls/arcswat_text_files"
# loc_file = "T:/Projects/Wisconsin_River/GIS_Datasets/Outfalls/WRB_Outfalls_DRAFT_JAN2015.shp"
# sb_file = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
# read in point source monthly from tab delimited file
of_data=read.delim(of_file)

# locs = readOGR(dirname(loc_file), strsplit(basename(loc_file), "\\.")[[1]][1])
# sbs = readOGR(dirname(sb_file), strsplit(basename(sb_file), "\\.")[[1]][1])
lu_table = read.dbf(lu_file)
# loop through each point source
sample_pts=unique(of_data$SAMPLE_ID)

dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), by="1 day")
dates = data.frame(
	YEAR = as.integer(format(dates, "%Y")),
	MON = as.integer(format(dates, "%m")),
	DAY = as.integer(format(dates, "%j"))
)
output_holder = data.frame()
for(sample_pt in sample_pts){
	print(sample_pt)
	# associate point source with subbasin
	# ps_pt = subset(locs, SAMPLE_ID == sample_pt)
	# intsct = gIntersection(sbs, ps_pt, byid=T, id=as.character(sbs@data$Subbasin))
	# sb_id = rownames(intsct@coords)
	id_index = which(lu_table$SAMPLE_PT == sample_pt)
	sb_id = lu_table$SUBBASIN[id_index]
	# calculate average daily water mass load
	pt_data = subset(of_data, SAMPLE_ID == sample_pt)
	flow_data = subset(pt_data, STORET_PARM_DESC == "Flow Rate")
	mean_flow = (flow_data$MODEL_VALUE)*3785.41
	# calculate average daily phosphorous and sediment loads
	p_data = subset(pt_data, STORET_PARM_DESC == "Phosphorus, Total")
	p_conc = (p_data$MODEL_VALUE)
	p_load = p_conc*mean_flow*0.001
	
	sed_data = subset(pt_data, STORET_PARM_DESC == "Suspended Solids, Total")
	units = sed_data$PARM_UNIT_TYPE[1]
	if(units == "mg/L"){
		sed_conc = (sed_data$MODEL_VALUE)
		sed_load = sed_conc*mean_flow*0.000001
	} else if(units == "lbs/day"){
		sed_conc = (sed_data$MODEL_VALUE)
		sed_load = sed_conc*0.000454
	} else if(units == "kg/day"){
		sed_conc = (sed_data$MODEL_VALUE)
		sed_load = sed_conc*.001
	} 
	
	out_mon = cbind(p_data["YEAR_"], p_data["MONTH_"], mean_flow, sed_load, p_load, sb_id)
	names(out_mon)[1:2] = c("YEAR", "MON")
	out_day = merge(dates, out_mon, all.x=T, all.y=F)
	out_day$sb_id = sb_id
	output_holder = rbind(output_holder, out_day)
	
}
output_holder[is.na(output_holder)] = 0

# write text file with daily loads
aggregated_of_by_subs = aggregate(
	cbind(mean_flow, sed_load, p_load) ~ YEAR + DAY + sb_id, 
	data=output_holder, 
	FUN=sum,
	na.rm=T)


aggreg_of_by_subs_sort = aggregated_of_by_subs[order(aggregated_of_by_subs[,2], aggregated_of_by_subs[,1]),]
arcswat_hdr = c("Day","Year","Floday","Sedday","Orgnday","Orgpday","No3day","Nh3day","No2day","Minpday","Cbodday","Disoxday","Chladay","Solpstday","Srbpstday","Bactpday","Bactlpday","Cmtl1day","Cmtl2day","Cmtl3day")

out_tbl = matrix(0, nrow=nrow(dates), ncol=length(arcswat_hdr))
out_tbl = data.frame(out_tbl)
names(out_tbl) = arcswat_hdr
out_tbl$Day = dates$DAY
out_tbl$Year = dates$YEAR

# update with point source specific data
setwd(out_dir)
for (sb in 1:337){
	file_name = paste("recday_", sb, ".txt", sep='')
	sb_dat = subset(aggregated_of_by_subbasins, sb_id == sb)
	sb_dat_with_zeros = cbind(sb_dat, out_tbl)
	write.table(sb_dat_with_zeros, file = file_name, sep="\t", row.names = F)
}






