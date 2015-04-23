# run SWAT for different crops using a range of bio E
### default bioes 
## corn 39.00
## soyb 25.00
## alfa 20.00
#   This should be run with MONTHLY output (code 0)
setInternet2(use = TRUE)

arguments = commandArgs(trailingOnly = T)
txtinout = arguments[1]
dir_out = arguments[2]
td = arguments[3]
crop = arguments[4]
bio_e = arguments[5]

source("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/calibration/functions_query_output.r")

td = paste(td, "txtinout_", sample(100000:999999, 1), sep="")
dir.create(td)
file.copy(txtinout,
	td,
	recursive = T)
txtinout = paste(td, "TxtInOut", sep="/")

file.cio = readLines(paste(txtinout, "file.cio", sep = "\\"))
iprint.ind = which(substr(file.cio, 23, 28) == "IPRINT")
# 0 for monthly
substr(file.cio[iprint.ind], 16, 16) = "0"
##### Reach output variables
file.cio[65] = "   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
##### Subbasin output variables
file.cio[67] = "   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
##### HRU output variables
file.cio[69] = "   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
##### HRU data to be printed
file.cio[71] = "   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0   0   0   0   0   0"
writeLines(file.cio, paste(txtinout, "file.cio", sep = "\\"))
# ---
setwd(txtinout)

file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(txtinout, "SWAT_64rel.exe", sep="\\"))


print(paste("Running SWAT with",crop,"and a Bio E of", bio_e))
plant.dat = readLines(paste(txtinout, "plant.dat", sep="\\"))
plant.dat.ind = which(substr(plant.dat, 7, 10) == crop)
if (bio_e == 10) {
	original_bio_e = substr(plant.dat[plant.dat.ind + 1], 3, 7)
}
substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(bio_e, digits=2, nsmall=2)
writeLines(plant.dat, paste(txtinout, "plant.dat", sep="\\"))
bat = tempfile(pattern="runswat_", fileext=".bat")
writeLines(paste("cd ", txtinout, "\nSWAT_64rel.exe", sep=""), bat) 

system(bat)

dat = readLines(paste(txtinout, "output.hru", sep="\\"))
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
# taking only crop of interest and yearly averages
modData = subset(modData, LULC == crop & MON > 13)

area_wt_yld = merge(aggregate(YLD * AREA ~ MON, data=modData, sum),
	aggregate(AREA ~ MON, data=modData, sum))
area_wt_yld$MEAN = area_wt_yld[['YLD * AREA']] / area_wt_yld[['AREA']]

rw = data.frame(crop=crop, bio_e=bio_e, yld=mean(area_wt_yld$MEAN))
write.csv(rw, 
	paste(dir_out, "/", crop, "_", bio_e, ".csv", sep=""),
	row.names=F)

