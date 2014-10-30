wd <- 'H:\\WRB\\Scenarios\\Default\\TxtInOut'
setwd(wd)
# fitted bio_e from calibration
bio_calib <- data.frame(crop = c("CORN", "SOYB", "ALFA"),
                        FittedBioE = c(31.32,54.65,9.72), #alfa = 11.14 for 13%moist
                        originalBioE = c(39, 25, 20))

for (crop in c("CORN", "SOYB", "ALFA")) {
    bio_e <- bio_calib[which(bio_calib$crop == crop),'FittedBioE']
    plant.dat = readLines(paste(wd, "plant.dat", sep="\\"))
    plant.dat.ind = which(substr(plant.dat, 7, 10) == crop)
    substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(bio_e, digits=2, nsmall=2)
    writeLines(plant.dat, paste(wd, "plant.dat", sep="\\"))
}
        

file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))
bat = tempfile(pattern="runswat_", fileext=".bat")
writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
        
system(bat)
