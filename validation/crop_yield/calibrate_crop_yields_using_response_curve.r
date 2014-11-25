# run SWAT for different crops using a range of bio E
#   This should be run with MONTHLY output (code 0)
wd = "H:\\WRB\\Scenarios\\Default\\TxtInOut"
setwd(wd)
source("C:/Users/evansdm/Documents/Code/validation/functions_query_output.r")

file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))

cal_table = data.frame()
for (crop in c("CORN", "SOYB", "ALFA")) {
    for (bio_e in seq(10,90,15)) {
        print(paste("Running SWAT with",crop,"having a Bio E of", bio_e))
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
        
        row = data.frame(crop=crop, bio_e=bio_e, yld=mean(area_wt_yld$MEAN))
        cal_table = rbind(cal_table, row)
        write.csv(cal_table, paste(wd, "bio_calibration.csv", sep="\\"), row.names=F)
    }
    substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(original_bio_e, digits=2, nsmall=2)
    writeLines(plant.dat, paste(wd, "plant.dat", sep="\\"))
}

source("C:/Users/evansdm/Documents/Code/validation/calibrate_crop_yields_using_response_curve_plotting.r")