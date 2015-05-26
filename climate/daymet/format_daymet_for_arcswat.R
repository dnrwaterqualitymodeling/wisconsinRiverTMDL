library(rgdal)
library(rgeos)
library(RODBC)

#in and out directories
dir_in = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/daymet"
dir_out = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate/daymet"
wd = "C:/TEMP/daymet"
dir_ncdc = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"
inDb = "C:/Users/ruesca/Desktop/WRB_climate_station/WRB.mdb"

#read in subbasins
subbasins = readOGR(
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro",
	"subbasins_minus_urban_boundaries")

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
	solar = c("dayl..s.", "srad..W.m.2."),
	rh = "vp..Pa."	#this is actually vapor pressure, not relative humidity
)

fullTimeSeries = seq(as.Date('1990-01-01'), as.Date('2013-12-31'), 'day')
daymetTimeSeries = seq(as.Date('2002-01-01'), as.Date('2013-12-31'), 'day')
daymetTimeSeries = daymetTimeSeries[
	c(
		-which(daymetTimeSeries == as.Date("2004-12-31")),
		-which(daymetTimeSeries == as.Date("2008-12-31")),
		-which(daymetTimeSeries == as.Date("2012-12-31"))
	)
]
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

con = odbcConnectAccess(inDb)
for (subbasin in subbasins@data$Subbasin) {
	file_in = paste(dir_in, "/subbasin_", subbasin, "_2002_2013.csv", sep="")
	
	data_daymet = read.csv(file_in, skip=7)
	
	for (var in c("pcp", "tmp", "solar", "rh")) {
		file_out_clim = paste(wd, "/", var, "_", subbasin, ".txt", sep="")
		print(file_out_clim)

		var_col = var_col_lu[[var]]
		clim = data_daymet[var_col]
		
		message = paste(subbasin, var, sep = " ")
		print(message)
		
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
			#conversion from avg W/m^2 to total daily Kj/m^2 per daymet documentation
			clim = data.frame((clim[,1] * clim[,2]) / 1000000)
			var_col = "srad..W.m.2."
			names(clim) = var_col
		}
		if (var == "rh") {
			#solved Antoine equation for P, then converted mmhg to Pa
			satvp = 10**(8.07131-(1730.63/(233.426+avgt))) * 133.322368	
			#rh = actual vapor pressure / vp at saturation
			clim = clim / satvp		
		}
		
		query = paste("SELECT Subbasin, Station FROM ", sub_var_lu[var], sep = "")
		v_table = sqlQuery(con, query)
		old_var = read.csv(
			paste(dir_ncdc,
				"/",
				subset(v_table, Subbasin == subbasin, select="Station")[1,1],
				".txt",sep = ""
			),
			skip=1,
			header = F)
		leaps = data.frame(rbind(
			old_var[fullTimeSeries == as.Date("2004-12-31"),],
			old_var[fullTimeSeries == as.Date("2008-12-31"),],
			old_var[fullTimeSeries == as.Date("2012-12-31"),]))
		names(leaps) = var_col
		#insert values for these dates into data daymet using slicing and rbind
		
		if (var == "tmp") {
			clim = rbind(
				clim[1:which(daymetTimeSeries == as.Date("2004-12-30")),var_col],
				leaps[1,],
				clim[
					which(daymetTimeSeries == as.Date("2005-01-01")):
						which(daymetTimeSeries == as.Date("2008-12-30")),var_col],
				leaps[2,],
				clim[
					which(daymetTimeSeries == as.Date("2009-01-01")):
						which(daymetTimeSeries == as.Date("2012-12-30")),var_col],
				leaps[3,],
				clim[
					which(daymetTimeSeries == as.Date("2013-01-01")):nrow(clim),var_col]
			)
		} else {
			clim = c(
				clim[1:which(daymetTimeSeries == as.Date("2004-12-30")),var_col],
				leaps[1,],
				clim[
					which(daymetTimeSeries == as.Date("2005-01-01")):
						which(daymetTimeSeries == as.Date("2008-12-30")),var_col],
				leaps[2,],
				clim[
					which(daymetTimeSeries == as.Date("2009-01-01")):
						which(daymetTimeSeries == as.Date("2012-12-30")),var_col],
				leaps[3,],
				clim[
					which(daymetTimeSeries == as.Date("2013-01-01")):nrow(clim),var_col]
			)
		}
		
		file.create(file_out_clim)
		write(spinupStartDate, file = file_out_clim, ncolumns=1)
		if (var %in% c("pcp", "solar", "rh")) {
			write(rep(-99,daysOfSpinup), file=file_out_clim, ncolumns=1, append=T)
			write(clim, file=file_out_clim, ncolumns=1, append=T)
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
close(con)

setwd(wd)
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

clim_files = list.files(wd, full.names=T)
for (clim_file in clim_files) {
	print(clim_file)
	file.copy(clim_file, dir_out)
}
