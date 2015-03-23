library(ggplot2)
library(reshape)
options(stringsAsFactors=F)
source("~/Code/calibration/functions_query_output.r")
source("~/Code/swat_output_assessment/batch_grep.r")
##### grabbing a subbasin with roughly equal liquid and solid man applied
wd = "H:\\WRB\\Scenarios\\irrigation_assessment\\sca3_eff90_mx90_asq02\\TxtInOut"
extr_subs_fun(
	subbasins=c(46,328),
	rch=F,
	src_folder=gsub("\\\\","/",wd),
	dst_folder="H:/")
# file.copy(wd, "H:/WRB/Scenarios/irrigation_assessment/sca3_eff50_mx90_asq02", recursive=T)
#######-----#######-----#######-----#######-----#######

col_mat = matrix(c(
	"BERM","#8dd3c780",
	"RNGE","#ffffb380",
	"FRSD","#bebada80",
	"WETN","#fb807280",
	"ALFA","#80b1d380",
	"HAY ","#fdb46280",
	"CORN","#b3de6980",
	"SOYB", "#fccde580",
	"SCRN", "#d9d9d980",
	"POTA", "#bc80bd80",
	"CSIL", "#ccebc580",
	"GRBN","#ffed6f80"),
12,2,byrow=T)

mod_per = seq(
	as.Date("2002-01-01", "%Y-%m-%d"),
	as.Date("2013-12-31", "%Y-%m-%d"),
	by = '1 day'
)


###### working on hru data
dat = readLines("H:/output_hru/328.txt")
select_cols = list(
	cols = c(
		"LULC", "HRU", "AREA","BIOM","YLD","ORGP", "SEDP", "SOLP"),
	dtypes = c(
		as.character,
		as.integer, 
		as.numeric, 
		as.numeric, 
		as.numeric,
		as.numeric,
		as.numeric,
		as.numeric)
)

modData = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
modData = as.data.frame(modData)
names(modData) = select_cols$cols
for (row in 1:length(select_cols$cols)) {
	col_name = select_cols$cols[row]
	dtype = select_cols$dtypes[row][[1]]
	vals = query_output.hru(dat, col_name)
	vals = sapply(vals, dtype)
	modData[col_name] = data.frame(vals, stringsAsFactors=F)
}
modData$DATE = rep(mod_per, each=length(unique(modData$HRU)))

dairy_rotations = c(4815, 4816, 4817, 4818, 4819, 4820, 4823)

pdf("manure_assessment.pdf",height=8,width=11)
for (rot in dairy_rotations){
	par(mfrow=c(3,1))
	hru = subset(modData, HRU == rot)
	# for (p_type in c("ORGP", "SEDP", "SOLP")){
		
		# plot(y=hru[,p_type], x=hru$DATE,
			# type='l',
			# main=paste(rot, p_type),
			# ylab=paste(p_type, "Export"))
	plot(BIOM ~ DATE, data=hru, type='l', main="Subbasin: 328; HRU: 4823")	
		for (lu in unique(hru$LULC)){
			lu_sb = subset(hru, LULC == lu)
			lu_sb = merge(lu_sb, data.frame(DATE = mod_per), all.x = T, all.y=T)
			colr=col_mat[which(col_mat[,1] == lu),2]
			lines(BIOM ~ DATE, data=lu_sb, col=colr, lwd=6)
			# lines(y=lu_sb[,p_type], x=lu_sb$DATE, col=colr, lwd=6)
		}
		legend(
			"topright",
			legend=col_mat[which(col_mat[,1] %in% unique(hru$LULC)),1],
			fill=col_mat[which(col_mat[,1] %in% unique(hru$LULC)),2])
	}
}
dev.off()



























