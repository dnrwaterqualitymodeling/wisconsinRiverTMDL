library(foreign)
library(plyr)
library(RODBC)
library(rgdal)

# CHANGE THESE ###########
# SWAT project
climDir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"
setwd(climDir)
projectDir = "C:/SWAT/Reservoirs_2"

inDb = paste(projectDir, "Reservoirs_2.mdb", sep="/")
con = odbcConnectAccess(inDb)

stations = as.character(sqlQuery(con, "SELECT Station FROM SubPcp")[,1])
close(con)

stationShape = readOGR(climDir, "ClimateStationLocations_WRB_2mileBuffer")

stations <- sub("p","", stations)
stations = unique(stations)
stationsList <- paste(climDir, "/", stations, "pcp.dbf", sep="")

#loop through all precip dbfs

counts = data.frame()
j = 0
for (i in stationsList){
	j = j + 1
	print(i)
	data <- read.dbf(i, as.is = TRUE)
	selection <-length(which(data[,2] == -99))
	row = data.frame(station=stations[j], count=selection)
	counts = rbind(counts, row)
}

stationShape_sel = subset(stationShape, ID %in% counts$station) 
