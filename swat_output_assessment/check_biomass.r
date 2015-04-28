# Check ponds

source("~/Code/calibration/functions_query_output.r")
source("~/Code/swat_output_assessment/batch_grep.r")

txtInout = "H:\\WRB\\Scenarios\\Default\\TxtInOut"
mod_per = seq(
	as.Date("2002-01-01", "%Y-%m-%d"),
	as.Date("2013-12-31", "%Y-%m-%d"),
	by = '1 day'
)
# a subset of subbasins
# sbs = c(1,53,74,80,141,149,171,189,191,197,221,247,257,268,275,276,299,329)
procs = 5
strt_stps = round(seq(1,337+337/procs,by=337/procs))

for (i in 1:(length(strt_stps)-1)){
	strt = strt_stps[i]
	stp = strt_stps[i+1]-1
	print(paste(strt,stp))
	
	extr_subs_fun(
		subbasins=strt:stp,
		src_folder=gsub("\\\\","/",txtInout),
		hru=T,
		rch=F,
		wtr=F,
		dst_folder="H:/")
	
	g.head = F
	while (!g.head) {
		ps = grep("Batch GREP", system('tasklist /v', intern=TRUE), value=TRUE)
		if (length(ps) < procs) {
			g.head = T
		} else {
			Sys.sleep(1)
			g.head = F
		}
	}
}

wtr =  data.frame()
for (sb in 1:337){
	
	fl_name = paste0("H:/output_wtr/", sb, ".txt")
	if (file.info(fl_name)$size ==0){ next }
	print(basename(fl_name))
	select_cols = list(
		cols = c(
			"LULC",
			"SUB", 
			"HRU", 
			"AREA", 
			"PND_IN",
			"PND_OUT",
			"PND_EVAP",
			"PND_VOL",
			"PND_SEEP"),
		dtypes = c(
			as.character,
			as.integer, 
			as.integer, 
			as.numeric, 
			as.numeric, 
			as.numeric,
			as.numeric, 
			as.numeric,
			as.numeric)
	)
	dat = readLines(fl_name)
	pndData = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
	pndData = as.data.frame(pndData)
	names(pndData) = select_cols$cols
	for (row in 1:length(select_cols$cols)) {
		col_name = select_cols$cols[row]
		dtype = select_cols$dtypes[row][[1]]
		vals = query_output.wtr(dat, col_name)
		vals = sapply(vals, dtype)
		pndData[col_name] = data.frame(vals, stringsAsFactors=F)
	}
	pndData$DATE = rep(mod_per, each=length(unique(pndData$HRU)))
	pndData = aggregate(
		cbind(PND_IN, PND_OUT, PND_EVAP, PND_VOL, PND_SEEP) ~ SUB + DATE,
		data=pndData,
		FUN=sum
	)
	wtr = rbind(wtr, pndData)
}

wtr$NET_FLOW = wtr$PND_IN - wtr$PND_OUT



wetland_geometry = read.csv(file_wetland_geometry)
pond_geometry = read.csv(file_pond_geometry)

pdf('pond_vols.pdf', height=8, width=11)
for (sb in sbs){
	sbst = subset(wtr_agg, SUB == sb)
	if (nrow(sbst) == 0) { next }
	pnd_evol = wetland_geometry[which(wetland_geometry$subbasin == sb),"WET_MXVOL"]
	pnd_evol = pnd_evol + pond_geometry[which(pond_geometry$subbasin == sb),"PND_EVOL"]
	if (!sb %in% pond_geometry$subbasin) { next }
	pnd_evol = pnd_evol * 10000
	
	pnd_pvol = wetland_geometry[which(wetland_geometry$subbasin == sb),"WET_NVOL"]
	pnd_pvol = pnd_pvol + pond_geometry[which(pond_geometry$subbasin == sb),"PND_PVOL"]
	pnd_pvol = pnd_pvol * 10000 
	par(mfrow=c(1,1))
	
	plot(PND_VOL ~ DATE, data=sbst,
		type='l',
		ylim=c(pnd_pvol, pnd_evol),
		ylab="Pond and Wetland Volume (m3)",
		main=paste("Subbasin", sb))
	mtext("Pond Volume")
	abline(h=pnd_pvol, col='blue')
	abline(h=pnd_evol, col='red')
	
	legend(
		'bottomright',
		legend=c("Volume", "Emrg Vol", "Prnc Vol"),
		fill=c('black', 'red', 'blue'))
	# par(mfrow=c(2,1))
	plot(NET_FLOW ~ DATE, data=sbst,
		type='l',
		ylab="Net Flow (mm)"
	)
	mtext(paste("Subbasin", sb))
	
	# plot(PND_OUT ~ DATE, data=sbst,
		# type='l',
		# ylab="Flow Out (mm)"
	# )
}
dev.off()
