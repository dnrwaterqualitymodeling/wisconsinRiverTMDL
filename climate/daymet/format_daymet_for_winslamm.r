library(rgdal)
library(rgeos)
library(RODBC)

#in and out directories
dir_in = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/daymet"
dir_out = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/Daymet_files"
wd = "C:/TEMP/daymet"
dir_ncdc = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"
pcpLookup = "T:/Projects/Wisconsin_River/GIS_Datasets/Urban/SLAMM Model Area/MS4_Subbasins/muni_pcp_lu.csv"

#read in arban area boundaries
UABs = readOGR(
	"T:/Projects/Wisconsin_River/GIS_Datasets/Urban/Urban Area Boundaries/SWAT_Urban_Areas.gdb", "UrbAreas_by_muni")

files = list.files(dir_in, "MCD", full.names=F)
files = gsub(" ", "_", files)

for (f in files){
	muni = gsub(".csv", "", gsub("_2002_2013", "", gsub("MCD_NAME_", "", f)))
	test = read.csv(paste(dir_in, "/", f, sep=""), skip=7)
	test = subset(test, test$prcp..mm.day > 0)
	odf = data.frame(
		s_date =  strftime(as.Date(paste(test$year, formatC(test$yday, width=3, flag="0"), sep=""), format = "%Y%j"), "%m/%d/%y"),
		s_time = "0:00",
		e_date = strftime(as.Date(paste(test$year, formatC(test$yday, width=3, flag="0"), sep=""), format = "%Y%j"), "%m/%d/%y"),
		e_time = "12:00",
		amt = round(test$prcp..mm.day. * 0.0393701, 2) #convert to inches
	)
	
	pcp_lu = read.csv(pcpLookup, header=T)
	pcp_lu$NAMELSAD10 = gsub(" ", "_", pcp_lu$NAMELSAD10)
	for (i in c("_village", "_city", "_town")){
		pcp_lu$NAMELSAD10 = gsub(i, "", pcp_lu$NAMELSAD10)
	}
	
	pcpFile = subset(pcp_lu, NAMELSAD10 == muni)

	print(muni)

	out_file = paste(dir_out, "/", muni, ".csv", sep="")
		
	fullTimeSeries = seq(as.Date('1990-01-01'), as.Date('2013-12-31'), 'day')
	
	old_var = read.csv(
			paste(dir_ncdc,
				"/",
				pcpFile$Best_PCP2,
				".txt",sep = ""
			),
			skip=1,
			header = F
	)
			
	leaps = c(
	"2004-12-31", 
	"2008-12-31", 
	"2012-12-31"
	)
		
	leap_events = data.frame(
		s_date = strftime(leaps, "%m/%d/%y"),
		s_time = "0:00",
		e_date = strftime(leaps, "%m/%d/%y"),
		e_time = "12:00",
		amt = round(old_var[fullTimeSeries %in% as.Date(leaps),] * 0.0393701, 2)
	)
	
	leap_events = subset(leap_events, amt > 0)
	
	odf = rbind(odf, leap_events)
	odf = odf[order(as.Date(odf$s_date, "%m/%d/%y")),]
	
	file.create(out_file)
	write(nrow(test), file = out_file)
	write.table(odf, out_file, row.names=F, col.names=F, quote=F, append=T, sep=",")
}