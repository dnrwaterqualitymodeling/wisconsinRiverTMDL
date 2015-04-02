library(ggplot2)
library(reshape)
options(stringsAsFactors=F)
source("~/Code/calibration/functions_query_output.r")
source("~/Code/swat_output_assessment/batch_grep.r")
#### Set file.cio and run swat
wd = 'H:\\WRB\\Scenarios\\Default\\TxtInOut'
file.cio = readLines(paste(wd, "file.cio", sep = "\\"))

iprint.ind = which(substr(file.cio, 23, 28) == "IPRINT")
## 0 for monthly
## 1 for daily
## 2 for yearly
substr(file.cio[iprint.ind], 16, 16) = "1"

## Reach output variables
file.cio[65] = "   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
## Subbasin output variables
file.cio[67] = "   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
## HRU output variables
###### PRECIP, IRR, PET, ET, SURQ_GEN, SURQ_CNT, LAT_Q, GW_Q, WYLD, DAILYCN, TMP_AV, ORGP, SOLP, BIOM, LAI, YLD 
##### file.cio[69] = "   1   4   5   6  17  18  20  21  22  23  24  52  58  64  65  66   0   0   0   0"
file.cio[69] = "  0    0    0    0    0    0    0    0    0    0   0     0    0    0   0   0   0   0   0   0"
## HRU data to be printed
### only subset of potato vegetable rotations
file.cio[71] = "  0    0    0    0    0    0    0    0    0    0   0     0    0    0   0   0   0   0   0   0"

writeLines(file.cio, paste(wd, "file.cio", sep = "\\"))

#### Run SWAT and spin off individual text files for each subbasin
setwd(wd)
bat = tempfile(pattern="runswat_", fileext=".bat")
writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
system(bat)
##### grabbing potato veggie subbasins
extr_subs_fun(

	subbasins=c(5,59,65,73,74,75,76,77,107,141,142,143,144,145,149,176,178,191,193,199,208,216,228,234,247,248,251,254,255,257,260,296,311,330,333),
	src_folder=gsub("\\\\","/",wd),

	dst_folder="H:/")
# file.copy(wd, "H:/WRB/Scenarios/irrigation_assessment/sca3_eff50_mx90_asq02", recursive=T)
#######-----#######-----#######-----#######-----#######
wd = "H:/WRB/Scenarios/irrigation_assessment"
scenarios = c("sca0_eff90_mx90_asq02", "sca1_eff90_mx90_asq02", "sca3_eff90_mx90_asq02", "sca3_eff90_mx50_asq02","sca3_eff90_mx10_asq02")
scenarios = scenarios[1:3]

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

sbDat.hru = data.frame()
sbDat.rch = data.frame()
for (scen in scenarios){
	print(scen)
	scenario_path = paste(wd, scen, "output_hru", sep = "/")
	sbfiles = list.files(scenario_path)
	# sbfiles = sbfiles[grepl("74|145|149|191|257", sbfiles)]
	
	for (sb in sbfiles){
		print(paste("Subbasin file",sb))
		
		###### working on hru data
		dat = readLines(paste(scenario_path, sb, sep="/"))
		select_cols = list(
			cols = c(
				"LULC",
				"SUB", 
				"HRU", 
				"AREA", 
				"MON",
				"BIOM",
				"YLD",
				"IRR",
				"SA_ST",
				"LATQ",
				"GW_RCHG",
				"ET",
				"DAILYCN",
				"SURQ_GEN",
				"SURQ_CNT",
				"GW_Q"),
			dtypes = c(as.character,
				as.integer, 
				as.integer, 
				as.numeric, 
				as.integer, 
				as.numeric, 
				as.numeric,
				as.numeric, 
				as.numeric,
				as.numeric,
				as.numeric,
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
		modData$SCENARIO = scen
		sbDat.hru = rbind(sbDat.hru, modData)
		
		###### working on rch data
		# rch.dat = readLines(paste(wd, scen, "output_rch",sb, sep = "/"))
		# rch.dat = gsub("\\s+", ",", rch.dat)
		# rch.dat = gsub("REACH,", "", rch.dat)
		# rch.dat = strsplit(rch.dat, ",")
		# nrows = length(rch.dat)
		# ncols = length(rch.dat[[1]])
		# rch.dat = unlist(rch.dat)
		# rch.dat = matrix(rch.dat, nrow=nrows, ncol=ncols, byrow=T)
		# rch.dat = apply(rch.dat, 2, as.numeric)
		# rch.dat = rch.dat[,c(1,3:7)]
		# rch.dat = as.data.frame(rch.dat)
		# names(rch.dat) = c("SUB","MON","AREA","FLOW","SED","PHO")
		# rch.dat$DATE = mod_per
		# rch.dat$SCENARIO = scen

		# sbDat.rch = rbind(sbDat.rch, rch.dat)
	}
}
bkup = sbDat.hru
# new_scen_name = c("No Irrigation", "Draw from Reach", "Draw from Shallow Aquifer")
# for (sc in 1:3){
	# sbDat.hru$SCENARIO[which(sbDat.hru$SCENARIO == scenarios[sc])] = new_scen_name[sc]
# }
# sbDat.hru = subset(sbDat.hru, SURQ_GEN > 0)

sderr = function(x){
	if (is.data.frame(x)){stop}
	b4 = length(na.omit(x))
	x[x==0] = NA
	# print(paste("Before 	",b4))
	# print(paste("After		",length(na.omit(x))))
	sqrt(var(x,na.rm=T)/length(na.omit(x)))
}

mean_tbl = aggregate(
	cbind(SURQ_CNT, SURQ_GEN, GW_Q, GW_RCHG, DAILYCN, ET, SA_ST, BIOM, LATQ) ~ SUB + SCENARIO, 
	data=sbDat.hru,
	FUN=mean,
	na.rm=TRUE)
se_tbl = aggregate(
	cbind(SURQ_CNT, SURQ_GEN, GW_Q, GW_RCHG, DAILYCN, ET, SA_ST, BIOM, LATQ) ~ SUB + SCENARIO, 
	data=sbDat.hru,
	FUN=sderr)
smry_tbl = merge(mean_tbl, se_tbl, by=c("SCENARIO", "SUB"))

# c("#7fc97f", "#beaed4", "#ffff99")
pdf("irrigation_scenarios.pdf", height=11,width=8)
## gw q
limits = aes(ymin=(GW_Q.x - GW_Q.y),
	ymax=(GW_Q.x + GW_Q.y))	
gplt_gwq = ggplot(smry_tbl, aes(x=factor(SUB), y=GW_Q.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_gwq = gplt_gwq + geom_bar(stat="identity", position=dodge)
gplt_gwq = gplt_gwq + scale_fill_manual(values=c("dodgerblue4",	"darkolivegreen4",	"goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_gwq+ theme_bw() + labs(x="Subbasin", y="GW Discharge to Reach") + coord_flip() 

#surq
limits = aes(ymin=(SURQ_CNT.x - SURQ_CNT.y),
	ymax=(SURQ_CNT.x + SURQ_CNT.y))	
gplt_surq_cnt = ggplot(smry_tbl, aes(x=factor(SUB), y=SURQ_CNT.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_surq_cnt = gplt_surq_cnt + geom_bar(stat="identity", position=dodge)
gplt_surq_cnt = gplt_surq_cnt + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_surq_cnt+ theme_bw() + labs(x="Subbasin",y="Surface Runoff Contribution to Reach") + coord_flip()

# gwrch
limits = aes(ymin=(GW_RCHG.x - GW_RCHG.y),
	ymax=(GW_RCHG.x + GW_RCHG.y))
gplt_gw_rchg = ggplot(smry_tbl, aes(x=factor(SUB), y=GW_RCHG.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_gw_rchg = gplt_gw_rchg + geom_bar(stat="identity", position=dodge)
gplt_gw_rchg = gplt_gw_rchg + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_gw_rchg+ theme_bw() + labs(x="Subbasin",y="Water Entering Aquifers") + coord_flip()

# BIOM
limits = aes(ymin=(BIOM.x - BIOM.y),
	ymax=(BIOM.x + BIOM.y))
gplt_BIOM = ggplot(smry_tbl, aes(x=factor(SUB), y=BIOM.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_BIOM = gplt_BIOM + geom_bar(stat="identity", position=dodge)
gplt_BIOM = gplt_BIOM + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_BIOM+ theme_bw() + labs(x="Subbasin",y="Biomass") + coord_flip()

# DAILYCN
limits = aes(ymin=(DAILYCN.x - DAILYCN.y),
	ymax=(DAILYCN.x + DAILYCN.y))
gplt_DAILYCN = ggplot(smry_tbl, aes(x=factor(SUB), y=DAILYCN.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_DAILYCN = gplt_DAILYCN + geom_bar(stat="identity", position=dodge)
gplt_DAILYCN = gplt_DAILYCN + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_DAILYCN + theme_bw() + labs(x="Subbasin",y="Daily Curve Number") + coord_flip()

# ET
limits = aes(ymin=(ET.x - ET.y),
	ymax=(ET.x + ET.y))
gplt_ET = ggplot(smry_tbl, aes(x=factor(SUB), y=ET.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_ET = gplt_ET + geom_bar(stat="identity", position=dodge)
gplt_ET = gplt_ET + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_ET + theme_bw() + labs(x="Subbasin",y="ET") + coord_flip()

# SURQ_GEN
limits = aes(ymin=(SURQ_GEN.x - SURQ_GEN.y),
	ymax=(SURQ_GEN.x + SURQ_GEN.y))
gplt_SURQ_GEN = ggplot(smry_tbl, aes(x=factor(SUB), y=SURQ_GEN.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_SURQ_GEN = gplt_SURQ_GEN + geom_bar(stat="identity", position=dodge)
gplt_SURQ_GEN = gplt_SURQ_GEN + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_SURQ_GEN + theme_bw() + labs(x="Subbasin",y="Surface Runoff Generated") + coord_flip()

# SA_ST
limits = aes(ymin=(SA_ST.x - SA_ST.y),
	ymax=(SA_ST.x + SA_ST.y))
gplt_SA_ST = ggplot(smry_tbl, aes(x=factor(SUB), y=SA_ST.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_SA_ST = gplt_SA_ST + geom_bar(stat="identity", position=dodge)
gplt_SA_ST = gplt_SA_ST + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_SA_ST + theme_bw() + labs(x="Subbasin",y="Amt of Water in Shall Aquifer") + coord_flip()

# LATQ
limits = aes(ymin=(LATQ.x - LATQ.y),
	ymax=(LATQ.x + LATQ.y))
gplt_LATQ = ggplot(smry_tbl, aes(x=factor(SUB), y=LATQ.x, fill=SCENARIO))
dodge = position_dodge(width=0.9)
gplt_LATQ = gplt_LATQ + geom_bar(stat="identity", position=dodge)
gplt_LATQ = gplt_LATQ + scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4", "goldenrod1")) + geom_errorbar(limits, position=dodge, width=0.25)
gplt_LATQ + theme_bw() + labs(x="Subbasin", y="Lateral Flow to Reach") + coord_flip()

dev.off()

###### ----- Reach Data ----- ######
sb = subset(sbDat.rch, SUB == 145)

scen_tbl = data.frame(
	scenario = scenarios,
	colrs = c("dodgerblue4", "darkolivegreen4", "goldenrod1","darkorchid3"),
	scen_name = c("No Irrigation", "Draw from Reach", "Draw from Shallow Aquifer", "from Aquifer, Half Max")
)
scen_tbl$rgb_colrs = rbind(col2rgb(scen_tbl$colrs),alpha=rep(200,4)) 
for (i in 1:4){
	scen = scenarios[i]
	sb_scen = subset(sb, SCENARIO == scen)
	if (i == i){
		plot(FLOW ~ DATE,
			data=sb_scen,
			type="l",
			col=rgb(t(as.matrix(scen_tbl$rgb_colrs[,i])),maxColorValue=255)
		)
	} else {
		lines(FLOW ~ DATE,
			data=sb_scen,
			col=rgb(t(as.matrix(scen_tbl$rgb_colrs[,i])),maxColorValue=255)
		)
	}
	legend(
		"topleft",
		legend=scen_tbl$scen_name,
		fill=scen_tbl$colrs
	)
}

# for (scen in scenarios){
	# hru.scen = subset(tst.hru, SCENARIO == scen)
	# print(sum(hru.scen$IRR))
# }

## potato vegetable from subbasin 145
# pdf("Subbasin_145.pdf",width=11,height=8)
# for (hru in unique(sbDat.hru$HRU)){
	# tst.hru = subset(sbDat.hru, HRU == hru)
	# if ("WATR" %in% unique(tst.hru$LULC)){next}
	# print(paste(hru))
	
	# plot(DAILYCN ~ DATE,
			# data=tst.hru,
			# type="l",
			# sub=paste("HRU:", hru))
	# lulcs = unique(tst.hru$LULC)
	# for (lu in lulcs){
		# colr = col_mat[which(col_mat[,1] == lu),2]
		# d_sbst = subset(tst.hru, LULC == lu)
		# d_sbst = merge(d_sbst, data.frame(DATE = mod_per), all.x = T, all.y=T)
		
		# lines(DAILYCN ~ DATE,
			# data=d_sbst,
			# type="l",
			# col=colr,
			# lwd=5)
	# }
	
	# leg_col_mat = col_mat[which(col_mat[,1] %in% lulcs),]
	# if (length(lulcs) == 1){
		# leg_col_mat = t(matrix(leg_col_mat))
	# }
	# legend(
		# "bottomleft",
		# legend=leg_col_mat[,1],
		# fill=leg_col_mat[,2],
		# bty='white',
		# bg='white'
		# )
# }
# dev.off()



# hru_data = data.frame()
# rch_data = data.frame()
# for (scen in wstrs){
	# print(scen)
	# scenario_path = paste(wd, scen, "TxtInOut", sep = "/")
	# output.hru = readLines(paste(scenario_path, "output.hru",sep="/"))
	# colmn_names = output.hru[9]
	# output.hru = output.hru[10:length(output.hru)]
	# output.hru = gsub("\\s+", ",", output.hru)
	## output.hru = gsub("REACH,", "", output.hru)
	# output.hru = strsplit(output.hru, ",")
	## dealing with the separation betwixt area and MON
	# output.hru = lapply(output.hru, function(x){
		# area = unlist(strsplit(x[6], ".", fixed=T))[2]
		# area = paste(".", area, sep='')
		# x[6] = area
		# return(x)
		# }
	# )
	# nrows = length(output.hru)
	# ncols = length(output.hru[[1]])
	# output.hru = unlist(output.hru)
	# output.hru = matrix(output.hru, nrow=nrows, ncol=ncols, byrow=T)
	# num_output.hru = apply(output.hru, 2, as.numeric)
	# output.hru = cbind(output.hru[,1], as.data.frame(num_output.hru))
	# output.hru = output.hru[,c(1,3:length(output.hru))]
	# output.hru = subset(output.hru, select=c(1, 3, 5, 4, 7, 8, 11, 12, 13, 17, 28))
	# names(output.hru) = c("LULC", 
		# "HRU",
		# "FILE_NAME",
		# "SUBBASIN",
		# "mgt", 
		# "AREA", 
		# "PRECIP", 
		# "IRR", 
		# "PET",
		# "ET", 
		# "SURQ_GEN", 
		# "GW_Q", 
		# "DAILYCN", 
		# "TMP_AV", 
		# "ORGP", 
		# "SOLP", 
		# "BIOM", 
		# "LAI",
		# "YLD")
	# dates = NULL
	# n_hrus = length(unique(output.hru$HRU))
	# for (d in 1:length(mod_per)){
		# one_date = mod_per[d]
		# dates = c(dates, rep(one_date, n_hrus))
	# }
	# dates = as.Date(dates,origin="1970-01-01")
	# output.hru["DATE"] = dates
	## irrigated_subs = subset(output.hru,   LULC %in% c("SCRN","POTA", "GRBN"), select = c("SUBBASIN","HRU", "FILE_NAME"))
	# print("Grabbing sand concentration...")
	# output.hru["mean_sand"] = apply(
		# as.matrix(output.hru$FILE_NAME),
		# MARGIN = 1,
		# FUN = function(x){
			# lding_0s = 9-nchar(x)
			# zeros = unlist(paste(rep("0", times = lding_0s), collapse = ''))
			# name_of_file = paste(zeros, x, ".sol", sep = '')
			# nme = paste(scenario_path, name_of_file, sep='/')
			# print(paste(nme))
			# just grabbing sand content
			# sand = readLines(nme)[15]
			# sand = unlist(strsplit(sand, ":"))
			# sand = unlist(strsplit(sand, "\\s+"))
			# sand = mean(as.numeric(sand),na.rm=T)
			# return(sand)
		# }
	# )

	# output.hru["Scenario"] = scen
	# hru_data = rbind(hru_data, output.hru)
	
	# rch.dat =  readLines(paste(scenario_path, "output.rch", sep="\\"))
	# rch.dat = rch.dat[10:length(rch.dat)]
	# rch.dat = gsub("\\s+", ",", rch.dat)
	# rch.dat = gsub("REACH,", "", rch.dat)
	# rch.dat = strsplit(rch.dat, ",")
	# nrows = length(rch.dat)
	# ncols = length(rch.dat[[1]])
	# rch.dat = unlist(rch.dat)
	# rch.dat = matrix(rch.dat, nrow=nrows, ncol=ncols, byrow=T)
	# rch.dat = apply(rch.dat, 2, as.numeric)
	# rch.dat = rch.dat[,c(1,3,4,6,7)]
	# rch.dat = as.data.frame(rch.dat)
	# names(rch.dat) = c("SUBBASIN", "MON", "AREAkm2", "FLOW_OUTcms", "EVAP")
	
	# rch.dat["Scenario"] = scen
	# rch_data = rbind(rch_data, rch.dat)
}

	# "CORN", "#7fc97f80",
	# "SOYB", "#beaed480",
	# "CSIL", "#fdc08680",
	# "ALFA", "#ffff9980"

# hru_data[which(hru_data$mean_sand > 50), "sandy_flag"] = "Sandy"
# hru_data[which(hru_data$mean_sand <= 50), "sandy_flag"] = "Not Sandy"

# hru_data[which(hru_data.sbst$ET > (3*sd(hru_data.sbst$ET) + mean(hru_data.sbst$ET))), "ET"] = NA

# hru_data_sbst = subset(hru_data, BIOM > 0)

# pet_gwq = ggplot(hru_data, aes(x=PET, y=GW_Q, color = LULC)) + geom_point(shape=20, size = 3)
# pet_gwq + facet_grid(Scenario ~ sandy_flag ) + scale_color_manual(values=c("#7fc97f80", "#beaed480", "#ffff9980"))

# lai_dailycn = ggplot(hru_data, aes(x=LAI, y=DAILYCN, color = LULC)) + geom_point(shape=20, size = 3)
# lai_dailycn + facet_grid(Scenario ~ sandy_flag) + scale_color_manual(values=c("#7fc97f80", "#beaed480", "#ffff9980"))+ theme_bw()

# irri = ggplot(subset(hru_data, BIOM > 0), aes(y=ET, x=GW_Q, color = LULC)) + geom_point(shape=20, size = 3)
# irri = irri + facet_grid(Scenario + sandy_flag ~ . ) + scale_color_manual(values=c("#7fc97f80", "#beaed480", "#ffff9980")) 
# irri + theme_bw() + ggtitle("ET and Baseflow (GW_Q) for irrigated \nand non irrigated and sandy and non-sandy")

# et_irr = ggplot(hru_data, aes(y=BIOM, x=DATE, color = LULC)) 
# et_irr + geom_line(size=0.5) + facet_grid(Scenario~ . ) + scale_color_manual(values=c("#7fc97f", "#beaed4", "#ffff99"))

# et_gwq = ggplot(hru_data, aes(x=IRR, y=GW_Q, color = LULC)) + geom_point(shape=20, size = 3)
# et_gwq + facet_grid(Scenario ~ sandy_flag ) #+ coord_flip()


# et_irri = ggplot(hru_data.sbst, aes(x=ET, y=IRRI, color = LULC)) + geom_point(shape=20, size = 3)
# et_irri + facet_grid(Scenario ~ .) + coord_flip()

# gwrchg_irri = ggplot(hru_data.sbst, aes(x=GWRCHG, y=IRRI, color = LULC)) + geom_point(shape=20, size = 3)
# gwrchg_irri + facet_grid(Scenario ~ .) + coord_flip()


## plotting reaches with irrigation
# irri_rchs = rch_data[which(rch_data$SUBBASIN %in% unique(hru_data.sbst$SUBBASIN)),]
# irri_rchs = irri_rchs[which(irri_rchs$Scenario != "with_irrigation_wstrs_2"),]
# irri_rchs = irri_rchs[which(irri_rchs$MON != 12),]
# irri_rchs["DATE"] = as.Date(paste(irri_rchs$MON, "-01-01",sep=''), format = "%Y-%m-%d")

# irri_means = aggregate(cbind(FLOW_OUTcms, EVAP, AREAkm2) ~ Scenario + DATE, data = irri_rchs, FUN = mean)
# mean_flow = ggplot(irri_means, aes(x=DATE, y = FLOW_OUTcms/AREAkm2, color = EVAP))+ geom_line()
# mean_flow + facet_grid(Scenario~.)

# flow_irri = ggplot(irri_rchs, aes(x=DATE, y = FLOW_OUTcms, color = factor(SUBBASIN)))+ geom_line()

# flow_irri + facet_grid(Scenario~.)


# flow_tst = ggplot(irri_rchs, aes(x=DATE, y = FLOW_OUTcms, color = factor(SUBBASIN)), stat="summary", fun = "mean")+ geom_line()

# flow_tst + facet_grid(Scenario~.)






############################################






# hru.dat[which(hru.dat$LULC %in% c("SCRN","POTA", "GRBN")), "irri_flag"] = "Irrigated"
# hru.dat[which(!hru.dat$LULC %in% c("SCRN","POTA", "GRBN")), "irri_flag"] = "Not Irrigated"

# hru.dat[which(hru.dat$mean_sand > 50), "sandy_flag"] = "Sandy"
# hru.dat[which(hru.dat$mean_sand <= 50), "sandy_flag"] = "Not Sandy"

# hru.dat.sbst = subset(hru.dat, select = -c(FILE_NAME, AREA, PET))
# hru.dat.sbst[which(hru.dat.sbst$ET > (3*sd(hru.dat.sbst$ET) + mean(hru.dat.sbst$ET))), "ET"] = NA

# precip = ggplot(hru.dat.sbst, aes(x=PRECIP, y=GW_Q, color = LULC)) + geom_point(shape=20)
# precip + facet_grid( ~ sandy_flag + irri_flag)

# et_gwq = ggplot(hru.dat.sbst, aes(x=ET, y=GW_Q, color = LULC)) + geom_point(shape=20, size = 3)
# et_gwq + facet_grid( ~ sandy_flag + irri_flag) + coord_flip()

# irri_gwq = ggplot(hru.dat.sbst[which(hru.dat.sbst$irri_flag == "Irrigated"),], aes(x=IRRI, y=GW_Q, color = LULC)) + geom_point(shape=20)
# irri_gwq + facet_grid( ~ sandy_flag)

# irri_et = ggplot(hru.dat.sbst[which(hru.dat.sbst$irri_flag == "Irrigated"),], aes(x=IRRI, y=ET, color = PRECIP)) + geom_point(shape=20,size=4)
# irri_et + facet_grid( ~ sandy_flag)

# gwq = ggplot(hru.dat.sbst[which(hru.dat.sbst$irri_flag == "Irrigated"),], aes(x=IRRI, y=ET, color = GW_Q)) + geom_point(shape=20,size=3)
# gwq + facet_grid( ~ sandy_flag)
# hru.dat.sbst = melt(hru.dat.sbst, id.vars = c('LULC', "SUBBASIN", "HRU"))



# ggplot(hru.dat.sbst, aes(LULC, value)) +
	# geom_boxplot(aes(fill=variable), stat='boxplot') +



# boxplot(IRRI/PRECIP ~ LULC, hru.data = irrigated_hrus)


# hru_data[which(hru_data$LULC %in% c("SCRN","POTA", "GRBN")), "irri_flag"] = "Irrigated"
# hru_data[which(!hru_data$LULC %in% c("SCRN","POTA", "GRBN")), "irri_flag"] = "Not Irrigated"
# hru_data[which(hru_data$IRRI > 0), "irri_flag"] = "Irrigated"
# hru_data[which(hru_data$IRRI == 0), "irri_flag"] = "Not Irrigated"