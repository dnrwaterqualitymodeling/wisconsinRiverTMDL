library(rgdal)
wd = 'T:/Projects/Wisconsin_River/Model_Inputs/SWAT_WRB_052914/Scenarios/Default/TxtInOut'
bkp_wd = 'K:/TEMP'
setwd(wd)

climDir = 'T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate'
prjTemplateFile = 'ClimateStationLocations_WRB_2mileBuffer'
prjTemplate = readOGR(climDir, prjTemplateFile)

pcpStationFile = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/pcp.txt"
pcpStation = read.csv(pcpStationFile)
coordinates(pcpStation) = ~ LAT + LONG
proj4string(pcpStation) = proj4string(prjTemplate)
pcpStation = spTransform(pcpStation, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
pcpStation = data.frame(ID = pcpStation$ID,
                        NAME = pcpStation$NAME,
                        LAT = pcpStation@coords[,2],
                        LONG = pcpStation@coords[,1],
                        ELEVATION = pcpStation$ELEVATION)
write.csv(pcpStation, file = paste(climDir,'pcp.txt',sep='/'), row.names=F,quote=F)

tmpStationFile = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/tmp.txt"
tmpStation = read.csv(tmpStationFile)
coordinates(tmpStation) = ~ LAT + LONG
proj4string(tmpStation) = proj4string(prjTemplate)
tmpStation = spTransform(tmpStation, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
tmpStation = data.frame(ID = tmpStation$ID,
                        NAME = tmpStation$NAME,
                        LAT = tmpStation@coords[,2],
                        LONG = tmpStation@coords[,1],
                        ELEVATION = tmpStation$ELEVATION)
write.csv(tmpStation, file = paste(climDir,'tmp.txt',sep='/'), row.names=F,quote=F)