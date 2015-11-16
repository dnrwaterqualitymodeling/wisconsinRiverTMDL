library(foreign)
options(stringsAsFactors=F)

# define inputs
of_file ="T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/FINAL_WASTEWATER_DATASET/WRB_Permitted_Outfall_Data_FINAL_May2015.txt"
muni_file = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads.txt"
lu_file = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/DRAFT WASTEWATER DATASET FILES/WRB_Outfalls_DRAFT_JAN2015.dbf"
out_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/point_sources"
log_file = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/DRAFT WASTEWATER DATASET FILES/issues.log"

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

vars = list(
	c("Floday", "mean_flow", "cubic meters per second"),
	c("Sedday","sed_load", "Mg/day"),
	c("Minpday", "p_min_load", "kg/day"),
	c("Orgpday", "p_org_load", "kg/day"))

# read in point source monthly from tab delimited file
of_data = read.delim(of_file)
muni_data = read.delim(muni_file)
lu_table = read.dbf(lu_file)
file.create(log_file, overwrite=T)
# loop through each point source
sample_pts=unique(of_data$SAMPLE_ID)

dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), by="1 day")
dates = data.frame(
	DATE = dates,
	YEAR = as.integer(format(dates, "%Y")),
	MON = as.integer(format(dates, "%m")),
	DAY = as.integer(format(dates, "%j"))
)
pdf("Pt_src_assessment.pdf", width=11,height=8)
output_holder = data.frame()
# for (sb_id in unique(lu_table$SUBBASIN)) {
for (sb_id in 1:337) {
	# file_pdf = paste("pt_srcs_subbasin_", sb_id, ".pdf", sep="")
	# pdf(file_pdf, width=11,height=8)
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
			cbind(mean_flow, sed_load, p_load) ~ DATE, 
			data=sub_of_data, 
			FUN=sum,
			na.rm=T)
		# aggregated_of = merge(dates, aggregated_of, all.x=T, all.y=F)
		# Seperate P into 58% organic and 42% mineral
		aggregated_of$p_org_load = aggregated_of$p_load * 0.58
		aggregated_of$p_min_load = aggregated_of$p_load * 0.42
		# aggregated_of = aggregated_of[,-6]
		# aggregated_of = aggregated_of[order(aggregated_of$DATE),]

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
			DATE = muni_sub_agg$date,
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
		muni_sub_agg = muni_sub_agg[order(muni_sub_agg$DATE),]
	} else {
		muni_bool = F
	}
	
	if (!muni_bool & !of_bool) {
		print("	Neither municipal nor wastewater sources.")
		next
	}
	comb_muni_wstwtr = read.delim(
		list.files(out_dir, full.names=T)[grepl(paste("_",sb_id,".txt",sep=''),list.files(out_dir, full.names=T))]
	)

	comb_muni_wstwtr = strsplit(comb_muni_wstwtr[,1], split=",")
	nrws = length(comb_muni_wstwtr)
	ncls = length(comb_muni_wstwtr[[1]])
	comb_muni_wstwtr = unlist(comb_muni_wstwtr)
	comb_muni_wstwtr = matrix(comb_muni_wstwtr, nrow=nrws, ncol=ncls, byrow=TRUE)
	comb_muni_wstwtr = as.data.frame(comb_muni_wstwtr)
	comb_muni_wstwtr[,2:ncls] = apply(
		comb_muni_wstwtr[,2:ncls],
		MARGIN=2,
		FUN=as.numeric
	)
	comb_muni_wstwtr[,1] = as.Date(comb_muni_wstwtr[,1], format="%m/%d/%Y")
	names(comb_muni_wstwtr) = arcswat_hdr
	comb_muni_wstwtr = comb_muni_wstwtr[4384:nrws,]
	comb_muni_wstwtr[,2:ncls][comb_muni_wstwtr[,2:ncls] == 0] = NA

	for (var in vars){
		print(var)
		lgnd = c("Total Pt Src Dischg")
		clrs = c("#BEBEBE99")
		if (sum(is.na(comb_muni_wstwtr[,var[1]])) == nrow(comb_muni_wstwtr)){
			next
		}
		plot(comb_muni_wstwtr[,var[1]] ~ DATE, 
			data=comb_muni_wstwtr,
			# type='l',
			col="#BEBEBE99",
			lwd=4,
			ylab=var[3],
			main=var[2],
			ylim=c(0, max(comb_muni_wstwtr[,var[1]],na.rm=T)),
			sub=paste("Subbasin", sb_id))
		
		if (muni_bool) {
			points(muni_sub_agg[,var[2]] ~ DATE,
				data=muni_sub_agg,
				col="#ff000080",
				pch=21)
			lgnd = c(lgnd, "Municipal Src")
			clrs = c(clrs, "#ff000080")
			# all_point_sources = muni_sub_agg
		}
		if (of_bool) {
			lines(aggregated_of[,var[2]] ~ DATE,
				data=aggregated_of,
				col="#0000ff80")
			lgnd = c(lgnd, "WasteWater Src")
			clrs = c(clrs, "#0000ff80")
		}
		legend("topright",
			legend=lgnd,
			fill=clrs)
	}

}
dev.off()
	# Append the dailies to a master data.frame with subbasin ID associated
	# all_point_sources$sb_id = sb_id
	# output_holder = rbind(output_holder, all_point_sources)

# output_holder[is.na(output_holder)] = 0



# out_tbl = matrix("0.000", nrow=nrow(dates), ncol=length(arcswat_hdr))
# out_tbl = data.frame(out_tbl)
# names(out_tbl) = arcswat_hdr
# dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), by="1 day")
# out_tbl$DATE = format(dates, "%m/%d/%Y")
# out_tbl$DATE = gsub("^0", "", out_tbl$DATE)
# out_tbl$DATE = gsub("(/0)", "/", out_tbl$DATE)
