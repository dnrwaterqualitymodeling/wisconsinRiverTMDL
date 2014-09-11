library(foreign)
library(raster)
library(rgdal)
options(warn=2)
overwrite = T

netdir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"
wd = 'C:/TEMP/climate'
filenames = dir(netdir)
sapply(filenames, function(x) {
    file.copy(from=paste(netdir, x, sep="/"),
        to=paste(wd, x, sep="/"),
        overwrite=overwrite)
})

setwd(wd)

stationFile = "wgnstations.dbf"
stations = read.dbf(stationFile)

prjTemplateFile = 'ClimateStationLocations_WRB_2mileBuffer'
prjTemplate = readOGR(wd, prjTemplateFile)

demFile = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/DEM/30m_dem_wrb_2milebuffer.img"
dem = raster(demFile)

spinupStartDate = '19900101'
daysOfSpinup = length(seq(as.POSIXlt("1990-01-01"), as.POSIXlt("2001-12-31"), "day"))

for (var in c("pcp", "tmp")) {
    varSpecificStationList = NULL
    for (i in 1:nrow(stations)) {
#         if (i > 10) {break}
        station = stations[i,]
        outClimFile = paste(substr(var,1,1), station$NAME, ".txt", sep="")
        if (file.exists(outClimFile) & !overwrite) {
            next
        }
        climFile = paste(station$NAME, var, ".dbf", sep="")
        if (!file.exists(climFile)) {
            next
        }
        print(as.character(station$NAME))
        varSpecificStationList = c(varSpecificStationList, as.character(station$NAME))
        clim = read.dbf(climFile)
        fullNeighborList = NULL
        end = F
        while (!end) {
            d = sqrt((station$XPR-stations$XPR)^2 + (station$YPR-stations$YPR)^2)
            d = cbind(stations, d)
            d = d[d$d > 0 & !(d$NAME %in% fullNeighborList),]
            neighbors = as.character(d[d$d == min(d$d),"NAME"])                        
            for (neighbor in neighbors) {
                fullNeighborList = c(fullNeighborList, neighbor)
                neighborFile = paste(neighbor, var, ".dbf", sep="")
                if (!file.exists(neighborFile)) {
                    next
                }
                print(paste("   ", neighbor))
                neighborClim = read.dbf(neighborFile)
                if (var == "pcp") {
				
                    clim[clim$PCP == -99, "PCP"] = neighborClim[clim$PCP == -99, "PCP"]
                    if (length(which(clim$PCP == -99)) == 0) {
                        end = T
                    }
                } else {
                    clim[clim$MIN == -99, "MIN"] = neighborClim[clim$MIN == -99, "MIN"]
                    clim[clim$MAX == -99, "MAX"] = neighborClim[clim$MAX == -99, "MAX"]
                    if (length(which(clim$MIN == -99)) == 0 &
                            length(which(clim$MAX == -99)) == 0) {
                        end = T
                    }
                }
            }
        }
        if (var == "pcp") {
            if (any(clim$PCP < 0)) {
                stop("Precip less than zero")
            } else if (any(clim$PCP > 500)) {
                stop("Daily precip greater than 1000-yr event")
            }
        }
        if (var == "tmp") {
            if (any(clim$MAX < -100 | clim$MIN < -100)) {
                stop("Temperature lower than the lowest on record")
            } else if (any(clim$MAX > 100 | clim$MIN > 100)) {
                stop("Temperature greater than the highest on record")
            }
        }
        file.create(outClimFile)
        write(spinupStartDate, file = outClimFile, ncolumns=1)
        if (var == "pcp") {
            write(rep(-99,daysOfSpinup), file=outClimFile, ncolumns=1, append=T)
            write(clim$PCP, file=outClimFile, ncolumns=1, append=T)
        } else {
            write(rep("-99,-99",daysOfSpinup), file=outClimFile, ncolumns=1, append=T)
            write(t(as.matrix(clim[c("MIN", "MAX")])),
                  file=outClimFile,
                  ncolumns=2,
                  append=T,
                  sep=",")
        }
    }
    stationTable = data.frame(
        ID = 1:length(varSpecificStationList),
        NAME = varSpecificStationList
    )
    stationTable = merge(stationTable, stations[c("NAME","XPR","YPR")])
    names(stationTable)[3:4] = c("LONG", "LAT")
    stationTableSp = stationTable
    coordinates(stationTableSp) = ~ LONG + LAT
    elev = extract(dem, stationTableSp)
    stationTable$ELEVATION = elev
    proj4string(stationTableSp) = proj4string(prjTemplate)
    stationTableSp = spTransform(stationTableSp, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
    stationTable = data.frame(        
        ID = stationTable$ID,
        NAME = paste(substr(var,1,1), stationTable$NAME, sep=""),
        LAT = stationTableSp@coords[,2],
        LONG = stationTableSp@coords[,1],
        ELEVATION = stationTable$ELEVATION)
    write.csv(stationTable, paste(var,".txt",sep=""), row.names=F, quote=F)
}

filenames = dir(wd)
sapply(filenames, function(x) {
    file.copy(from=paste(wd, x, sep="/"),
              to=paste(netdir, x, sep="/"),
              overwrite=overwrite)
})
