library(foreign)
options(stringsAsFactors=F)

# define inputs
of_file = "T:/Projects/Wisconsin_River/GIS_Datasets/Outfalls/WRB_Permitted_Outfall_Data_DRAFT_Jan2015.txt"
muni_file = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads.txt"
lu_file = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/DRAFT WASTEWATER DATASET FILES/WRB_Outfalls_DRAFT_JAN2015.dbf"
out_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/point_sources"


# read in point source monthly from tab delimited file
of_data = read.delim(of_file)
muni_data = read.delim(muni_file)
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
for (sb_id in unique(lu_table$SUBBASIN)) {
	# sample_pt in sample_pts) {
	print(sb_id)
	ofs = subset(lu_table, SUBBASIN == sb_id)$SAMPLE_PT
	if (length(ofs) > 0) {
		of_bool = T
		#####################################
		# calculate average wastewater loads by subbasin
		sub_of_data = data.frame()
		for (of in ofs) {
			pt_data = subset(of_data, SAMPLE_ID == of)
			flow_data = subset(pt_data, STORET_PARM_DESC == "Flow Rate")
			mean_flow = (flow_data$MODEL_VALUE)*3785.41
			# calculate average daily phosphorous and sediment loads
			p_data = subset(pt_data, STORET_PARM_DESC == "Phosphorus, Total")
			p_conc = (p_data$MODEL_VALUE)
			p_load = p_conc*mean_flow*0.001
			
			sed_data = subset(pt_data, STORET_PARM_DESC == "Suspended Solids, Total")
			units = sed_data$PARM_UNIT_TYPE[1]
			if(tolower(units) == "mg/l" | units == "MGD") { # MGD was a mistake in the database---actually mg/L
				sed_conc = (sed_data$MODEL_VALUE)
				sed_load = sed_conc*mean_flow*0.000001
			} else if(units == "lbs/day"){
				sed_conc = (sed_data$MODEL_VALUE)
				sed_load = sed_conc*0.000454
			} else if(units == "kg/day"){
				sed_conc = (sed_data$MODEL_VALUE)
				sed_load = sed_conc*.001
			} 
			out_mon = cbind(p_data["YEAR_"], p_data["MONTH_"], mean_flow, sed_load, p_load)
			names(out_mon)[1:2] = c("YEAR", "MON")
			out_day = merge(dates, out_mon, all.x=T, all.y=F)
			out_day$SAMPLE_ID = of
			sub_of_data = rbind(sub_of_data, out_day)
		}
		aggregated_of = aggregate(
			cbind(mean_flow, sed_load, p_load) ~ YEAR + DAY, 
			data=sub_of_data, 
			FUN=sum,
			na.rm=T)
		aggregated_of = merge(dates, aggregated_of, all.x=T, all.y=F)
		# Seperate P into 58% organic and 42% mineral
		aggregated_of$p_org_load = aggregated_of$p_load * 0.58
		aggregated_of$p_min_load = aggregated_of$p_load * 0.42
		aggregated_of = aggregated_of[,-6]
	} else {
		of_bool = F
	}	
	########################################
	# Cacluate urban loads by subbasin
	muni_sub = subset(muni_data, subbasin == sb_id)
	if (nrow(muni_sub) > 0) {
		muni_bool = T
		muni_sub_agg = aggregate(
			cbind(flow_m3, TSS_tons, P_filt_kg, P_part_kg) ~ date + subbasin,
			muni_sub,
			FUN=sum,
			na.rm=T
		)
		muni_sub_agg = data.frame(
			YEAR = as.integer(format(as.Date(muni_sub_agg$date), "%Y")),
			DAY = as.integer(format(as.Date(muni_sub_agg$date), "%j")),
			mean_flow = muni_sub_agg$flow_m3,
			sed_load = muni_sub_agg$TSS_tons,
			p_min_load = muni_sub_agg$P_filt_kg,
			p_org_load = muni_sub_agg$P_part_kg
		)
		muni_sub_agg = merge(
			dates,
			muni_sub_agg,
			all.x=T,
			all.y=F
		)
	} else {
		muni_bool = F
	}
	if (!muni_bool & !of_bool) {
		next
	} else if (muni_bool & !of_bool) {
		all_point_sources = muni_sub_agg
	} else if (!muni_bool & of_bool) {
		all_point_sources = aggregated_of
	} else if (muni_bool & of_bool) {
		all_point_sources = muni_sub_agg
		COIs = c("mean_flow", "sed_load", "p_org_load", "p_min_load")
		muni_sub_agg = muni_sub_agg[order(muni_sub_agg$YEAR, muni_sub_agg$DAY), ]
		aggregated_of = aggregated_of[order(aggregated_of$YEAR, aggregated_of$DAY), ]
		all_point_sources[COIs] = muni_sub_agg[COIs] + aggregated_of[COIs]
	}
	# Append the dailies to a master data.frame with subbasin ID associated
	all_point_sources$sb_id = sb_id
	output_holder = rbind(output_holder, all_point_sources)
}
output_holder[is.na(output_holder)] = 0

arcswat_hdr = c(
	"DATE",
	"Floday",
	"Sedday",
	"Orgnday",
	"Orgpday",
	"No3day",
	"Nh3day",
	"No2day",
	"Minpday",
	"Cbodday",
	"Disoxday",
	"Chladay",
	"Solpstday",
	"Srbpstday",
	"Bactpday",
	"Bactlpday",
	"Cmtl1day",
	"Cmtl2day",
	"Cmtl3day")

out_tbl = matrix("0.000", nrow=nrow(dates), ncol=length(arcswat_hdr))
out_tbl = data.frame(out_tbl)
names(out_tbl) = arcswat_hdr
dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), by="1 day")
out_tbl$DATE = format(dates, "%m/%d/%Y")
out_tbl$DATE = gsub("^0", "", out_tbl$DATE)
out_tbl$DATE = gsub("(/0)", "/", out_tbl$DATE)

# update with point source specific data

setwd(out_dir)
for (sb in unique(output_holder$sb_id)) {

	file_name = paste("recday_", sb, ".txt", sep='')
	print(file_name)
	sb_dat = subset(output_holder, sb_id == sb)

	out_sb_tbl = out_tbl
	out_sb_tbl[c("Floday", "Sedday", "Minpday", "Orgpday")] =
		sb_dat[c("mean_flow", "sed_load", "p_min_load", "p_org_load")]

	out_sb_tbl[is.na(out_sb_tbl)] = 0
	dts = out_sb_tbl$DATE
	
	out_sb_tbl = out_sb_tbl[,2:length(out_sb_tbl)]
	print("formatting output...")
	out_sb_tbl = apply(
		out_sb_tbl,
		MARGIN=2,
		FUN=function(x){
			chrs = format(x, nsmall=3, digits=3)
		}
	)
	out_sb_tbl = cbind(dts, out_sb_tbl)
	hdr = paste('"', 
		paste(arcswat_hdr, collapse='","'),
		'"',
		sep="")
	print('writing data...')

	writeLines(hdr, file_name)
	write.table(out_sb_tbl,
		file=file_name,
		row.names=F,
		col.names=F,
		quote=F,
		sep=",",
		append=T)
}
