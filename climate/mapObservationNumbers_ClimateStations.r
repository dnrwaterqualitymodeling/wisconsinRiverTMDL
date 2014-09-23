library(foreign)
library(RODBC)
library(rgdal)
library(classInt)
library(RColorBrewer)
# CHANGE THESE ###########
# SWAT project
climDir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"
setwd(climDir)
projectDir = "C:/SWAT/Wetlands_2"
subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro","subbasins")
counties = readOGR("T:/GIS/Statewide_Coverages/Political_Boundaries","WI_Counties")
pal = brewer.pal(6, "YlGnBu")

inDb = paste(projectDir, "/", basename(projectDir), ".mdb", sep="")
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
	row = data.frame(ID=stations[j], count=selection)
	counts = rbind(counts, row)
}

stationShape_sel = subset(stationShape, ID %in% counts$ID)
stationShape_sel = merge(stationShape_sel, counts)
classes = classIntervals(var=stationShape_sel@data$count, 6)
col = findColours(classes, pal=pal)

png("na_counts.png", width=6.5, height=9, units="in", res=600)
plot(subbasins, axes=T, border="white")
plot(counties, add=T)
plot(subbasins, border="grey50", col="grey80", add=T)
plot(stationShape_sel, bg=col, cex=2, add=T, pch=21)
legend("topleft", attr(print(classes),"dimnames")[[1]], fill=pal, cex=0.9,
       title="Number of missing observations")
dev.off()       
       
       
       