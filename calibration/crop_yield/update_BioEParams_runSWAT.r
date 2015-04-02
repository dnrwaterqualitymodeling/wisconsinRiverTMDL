# this function replaces default values of bio_e for corn, alfalfa and soybeans
# then runs swat
source("C:/Users/evansdm/Documents/Code/calibration/crop_yield/subbasin_AvgYields_function_v2.r")
wd = 'H:\\WRB\\Scenarios\\Default\\TxtInOut'
setwd(wd)

# set to monthly
file.cio = readLines(paste(wd, "file.cio", sep = "\\"))

iprint.ind = which(substr(file.cio, 23, 28) == "IPRINT")
# 0 for monthly
# 1 for daily
# 2 for yearly
substr(file.cio[iprint.ind], 16, 16) = "0"
writeLines(file.cio, paste(wd, "file.cio", sep = "\\"))
# ----- # ----- #

dir_crop_valid = "T:/Projects/Wisconsin_River/GIS_Datasets/validation/crop_yield"
# fitted bio_e from calibration
bio_calib <- read.csv(paste(dir_crop_valid, 'Bio_E_Calibration_Report.csv', sep = '/'))

for (crop in c("CORN", "SOYB", "ALFA")) {
    bio_e <- bio_calib[which(bio_calib$Crop == crop), 'FittedBioE'] #change to "OriginalBioE" to reset
    plant.dat = readLines(paste(wd, "plant.dat", sep="\\"))
    plant.dat.ind = which(substr(plant.dat, 7, 10) == crop)
    substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(bio_e, digits=2, nsmall=2)
    writeLines(plant.dat, paste(wd, "plant.dat", sep="\\"))
}

file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))
bat = tempfile(pattern="runswat_", fileext=".bat")
writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
        
system(bat)

yldCalib('corn_grain', basinWide = T)
yldCalib('haylage', basinWide = T)
yldCalib('corn_silage', basinWide = T)
yldCalib('soybeans', basinWide = T)