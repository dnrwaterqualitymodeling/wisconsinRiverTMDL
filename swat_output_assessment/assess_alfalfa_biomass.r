library(ggplot2)
library(reshape)
options(stringsAsFactors=F)
source("C:/Users/evansdm/Documents/Code/calibration/functions_query_output.r")
#### Set file.cio and run swat
wd = 'H:\\WRB\\Scenarios\\Default\\TxtInOut'
file.cio = readLines(paste(wd, "file.cio", sep = "\\"))

iprint.ind = which(substr(file.cio, 23, 28) == "IPRINT")
#### 0 for monthly
#### 1 for daily
#### 2 for yearly
substr(file.cio[iprint.ind], 16, 16) = "1"

#### Reach output variables
file.cio[65] = "   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
#### Subbasin output variables
file.cio[67] = "   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
#### HRU output variables
file.cio[69] = "   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
#### HRU data to be printed
file.cio[71] = "   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"

writeLines(file.cio, paste(wd, "file.cio", sep = "\\"))

###### Run SWAT
setwd(wd)
bat = tempfile(pattern="runswat_", fileext=".bat")
writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
system(bat)

file.copy(wd, "H:/WRB/Scenarios/subbasin_1_daily", recursive=T)
#######-----#######-----#######-----#######-----#######
wd = "H:/WRB/Scenarios/subbasin_1_daily"

dat = readLines(paste(wd, "/TxtInOut/output.hru", sep="\\"))
dat = dat[10:length(dat)]

select_cols = list(
	cols = c("LULC", "SUB", "HRU", "AREA", "MON", "BIOM", "YLD"),
	dtypes = c(as.character, as.integer, as.integer, as.numeric, as.integer, as.numeric, as.numeric)
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

mod_per = seq(
	as.Date("2002-01-01", "%Y-%m-%d"),
	as.Date("2013-12-31", "%Y-%m-%d"),
	by = '1 day'
)

pdf("~/daily_biomass_subbasin_1.pdf",width=11,height=7)
for (i in 1:17){
	hru = subset(modData, HRU == i)
	hru$DATE = mod_per
	for (lu in unique(hru$LULC)){
		if (lu == "WATR"){
			print("Skipping water HRU...")
			next
		}
		hru_lulc = subset(hru, LULC == lu)
		
		hru_lulc[which(hru_lulc == 0),] = NA
		plot(BIOM ~ DATE,
			data=hru_lulc,
			type="l",
			col="coral1",
			main=paste("HRU: ",i, "; Land cover: ",lu,sep='')
		)
		lines(YLD/1000 ~ DATE,
			data=hru_lulc,
			col="coral4",
			lwd=2)
		legend(
			"topleft",
			legend=c("Biomass", "Yield"),
			fill=c("coral1","coral4"))

	}
}
dev.off()

# ---- # ---- # ---- # ---- # ---- # ---- # ---- #
