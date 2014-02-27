stFile = 'D:/TEMP/ish-history.csv'
shp = 'D:/TEMP/WRB_Basin_25mile_Buffer_WGS84.shp'

library(rgdal)
library(rgeos)

shpDir = dirname(shp)
shpFilename = strsplit(basename(shp), '\\.')[[1]][1]

b = readOGR(shpDir, shpFilename)
st = read.csv(stFile)
st$LAT = st$LAT / 1000
st$LON = st$LON / 1000
st = st[(st$LAT >= -90 &
            st$LON >= -180 &
            st$LAT <= 90 &
            st$LON <= 180 &
            !is.na(st$LAT) &
            !is.na(st$LON)) # &
#             st$WBAN != 99999)
,]
# st = SpatialPointsDataFrame(
#     coords = st[c('LON','LAT')],
#     data = st[c('USAF', 'WBAN', 'STATION.NAME', 'CTRY')]
# )
coordinates(st) = ~LON+LAT
proj4string(st) = proj4string(b)
ol = over(st, b)
st = subset(st, ol$Id == 0)
plot(st, xlim=c(-92,-86), ylim=c(42,48))
plot(b, add=T)

writeOGR(st, shpDir, 'WRB_ISD_hourly_precip_climate_stations', 'ESRI Shapefile')