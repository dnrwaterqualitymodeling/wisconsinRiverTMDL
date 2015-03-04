# run SWAT for different crops using a range of bio E
### default bioes 
## corn 39.00
## soyb 25.00
## alfa 20.00
#   This should be run with MONTHLY output (code 0)
wd = "H:\\WRB\\Scenarios\\Default\\TxtInOut"

file.cio = readLines(paste(wd, "file.cio", sep = "\\"))
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
writeLines(file.cio, paste(wd, "file.cio", sep = "\\"))
# ---
source("./Code/calibration/functions_query_output.r")
setwd(wd)

file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))

cal_table = data.frame()
for (crop in c("CORN", "SOYB", "ALFA")) {
    for (bio_e in seq(10,90,15)) {
        print(paste("Running SWAT with",crop,"and a Bio E of", bio_e))
        plant.dat = readLines(paste(wd, "plant.dat", sep="\\"))
        plant.dat.ind = which(substr(plant.dat, 7, 10) == crop)
        if (bio_e == 10) {
            original_bio_e = substr(plant.dat[plant.dat.ind + 1], 3, 7)
        }
        substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(bio_e, digits=2, nsmall=2)
        writeLines(plant.dat, paste(wd, "plant.dat", sep="\\"))
        bat = tempfile(pattern="runswat_", fileext=".bat")
        writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
        
        system(bat)
        
        dat = readLines(paste(wd, "output.hru", sep="\\"))
        dat = dat[10:length(dat)]
        
        select_cols = list(
            cols = c("LULC", "SUB", "HRU", "AREA", "MON", "YLD"),
            dtypes = c(as.character, as.integer, as.integer, as.numeric, as.integer, as.numeric)
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
        modData = subset(modData, LULC == crop & MON > 13)
        
        area_wt_yld = merge(aggregate(YLD * AREA ~ MON, data=modData, sum),
            aggregate(AREA ~ MON, data=modData, sum))
        area_wt_yld$MEAN = area_wt_yld[['YLD * AREA']] / area_wt_yld[['AREA']]
        
        rw = data.frame(crop=crop, bio_e=bio_e, yld=mean(area_wt_yld$MEAN))
        cal_table = rbind(cal_table, rw)
        write.csv(cal_table, "~/bio_calibration.csv", row.names=F)
    }
    substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(original_bio_e, digits=2, nsmall=2)
    writeLines(plant.dat, paste(wd, "plant.dat", sep="\\"))
}

source("C:/Users/evansdm/Documents/Code/calibration/crop_yield/calibrate_crop_yields_using_response_curve_plotting.r")