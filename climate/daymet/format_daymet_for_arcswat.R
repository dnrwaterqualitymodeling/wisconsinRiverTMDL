library(rgdal)
library(rgeos)
library(RODBC)

#in and out directories
dir_in = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/daymet"
#dir_out = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/daymet"
dir_out = "E:/daymet2"
#read in subbasins
subbasins = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro", "subbasins_minus_urban_boundaries")

#get centroid coordinates for each subbasin
centroids = gCentroid(subbasins, byid=TRUE, id=subbasins@data$Subbasin)
#transform them from planar projection to WGS84 - daymet takes lat/long
centroids = spTransform(centroids, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#set model spinup period
spinupStartDate = '19900101'
daysOfSpinup = length(seq(as.POSIXlt("1990-01-01"), as.POSIXlt("2001-12-31"), "day"))

#variable of interest within daymet data
var_col_lu = list(
	pcp = "prcp..mm.day.",
	tmp = c("tmax..deg.c.", "tmin..deg.c."),
	solar = "srad..W.m.2.",
	rh = "vp..Pa."	#this is actually vapor pressure, not relative humidity
)

fullTimeSeries = seq(as.Date('1990-01-01'), as.Date('2013-12-31'), 'day')
inDb = "E:/WRB.mdb"
con = odbcConnectAccess(inDb)
sub_var_lu = list(
	pcp = "SubPcp",
	tmp = "SubTmp",
	solar = "SubSlr",
	rh = "SubHmd"
)

elevations=NULL
for (subbasin in subbasins@data$Subbasin) {
	file_in = paste(dir_in, "/subbasin_", subbasin, "_2002_2013.csv", sep="")
	elev_line = readLines(file_in, n=4)[4]
	elev = strsplit(elev_line, "\\s+")[[1]][2]
	elevations = c(as.numeric(elevations), elev)
}

for (subbasin in subbasins@data$Subbasin) {
	file_in = paste(dir_in, "/subbasin_", subbasin, "_2002_2013.csv", sep="")
	
	data_daymet = read.csv(file_in, skip=7)
	
	for (var in c("pcp", "tmp", "solar", "rh")) {
		file_out_clim = paste(dir_out, "/", var, "_", subbasin, ".txt", sep="")
		print(file_out_clim)
		clim = data_daymet[var_col_lu[[var]]]
		
		query = paste("SELECT Station FROM ", sub_var_lu[var], sep = "")
		v_table = sqlQuery(con, query)
		old_var = read.csv(paste("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/", v_table$Station[subbasin], ".txt", sep = ""), header = T)
		leaps = c(old_var[fullTimeSeries == as.Date("2004-12-31"),], old_var[fullTimeSeries == as.Date("2008-12-31"),], old_var[fullTimeSeries == as.Date("2012-12-31"),])
		#insert values for these dates into data daymet using slicing and rbind
		clim = rbind(clim[1:5478, ], leaps[1], clim[5479:6938, ], leaps[2], clim[6939:8398, ], leaps[3], clim[8399:nrow(clim),])
		
		if (var == "pcp") {
			if (any(clim < 0)) {
				stop("Precip less than zero")
			} else if (any(clim > 500)) {
				stop("Daily precip greater than 1000-yr event")
			}
		}
		if (var == "tmp") {
			if (any(clim[,1] < -100 | clim[,2] < -100)) {
				stop("Temperature lower than the lowest on record")
			} else if (any(clim[,1] > 100 | clim[,2] > 100)) {
				stop("Temperature greater than the highest on record")
			}
			avgt = (clim[,1] + clim[,2]) / 2
		}
		if (var == "solar") {
			clim = clim * data_daymet$dayl..s. / 1000000	#conversion from avg W/m^2 to total daily Kj/m^2 per daymet documentation
		}
		if (var == "rh") {
			satvp = 10**(8.07131-(1730.63/(233.426+avgt))) * 133.322368		#solved Antoine equation for P, then converted mmhg to Pa
			clim = clim / satvp		#rh = actual vapor pressure / vp at saturation
		}
		message = paste(subbasin, var, sep = " ")
		print(message)
		
		file.create(file_out_clim)
        write(spinupStartDate, file = file_out_clim, ncolumns=1)
        if (var %in% c("pcp", "solar", "rh")) {
            write(rep(-99,daysOfSpinup), file=file_out_clim, ncolumns=1, append=T)
            write(clim[,1], file=file_out_clim, ncolumns=1, append=T)
        } else {
            write(rep("-99,-99",daysOfSpinup), file=file_out_clim, ncolumns=1, append=T)
            write(t(as.matrix(clim)),
                  file=file_out_clim,
                  ncolumns=2,
                  append=T,
                  sep=",")
        }
    }
}

setwd(dir_out)
for (var in c("pcp", "tmp", "solar", "rh")) {
	print(paste(var, "station table"))
	stationTable = data.frame(
		ID=1:337,
		NAME=paste(var, "_", 1:337, sep=""),
		LAT=centroids$y,
		LONG=centroids$x,
		ELEVATION=elevations
	)
    write.csv(stationTable, paste(var,".txt",sep=""), row.names=F, quote=F)
}