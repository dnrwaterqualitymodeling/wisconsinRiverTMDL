library(RCurl)
library(rgdal)
library(rgeos)
library(reshape2)

ftpSite = 'ftp.ncdc.noaa.gov'
outdir = 'T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/winSlammPrecip/stations'
shp = 'D:/TEMP/WRB_Basin_2mile_Buffer_WGS84.shp'

stFile = paste(tempdir(), 'ghcnd-inventory.txt', sep='\\')

ftpString = paste(
    'open ', ftpSite, '\n',
    'anonymous\n',
    'asruesch@gmail.com\n',
    'lcd "', tempdir(), '"\n',
    'cd pub/data/ghcn/daily/\n',
    'binary\n',
    'mget ghcnd-inventory.txt\n',
    'disconnect\n',
    'quit',
    sep=''
)
ftpCmdFile = tempfile()
write(ftpString, ftpCmdFile)
cmd = paste('ftp -i -s:', ftpCmdFile, sep='')
a = system(cmd)
unlink(ftpCmdFile)

shpDir = dirname(shp)
shpFilename = strsplit(basename(shp), '\\.')[[1]][1]
b = readOGR(shpDir, shpFilename)

st = read.fwf(stFile, widths=c(11,9,10,5,5,5))
names(st) = c("ID", "LAT", "LON", "VAR", "BEG", "END")
st = st[st$VAR == ' PRCP' & st$BEG <= 2002 & st$END >= 2014,]

coordinates(st) = ~LON+LAT
proj4string(st) = proj4string(b)
ol = over(st, b)
st = subset(st, ol$Id == 0)
# plot(st, xlim=c(-92,-86), ylim=c(42,48))
# plot(b, add=T)
widths = c(11,4,2,4, rep(c(5,1,1,1), 31))
valCols = seq(4,124,4)
ids = as.character(st@data$ID)
names(p) = c(
    "id",
    "year",
    "month",
    "var",
    paste(rep(c("v","m","q","s"), 31), sort(rep(1:31,4)), sep="")
)
for (id in ids) {
    outfile = paste(outdir, '/', id, '.csv', sep='')
    if (file.exists(outfile)) {next}
    d = data.frame()
    ftpString = paste(
        'open ', ftpSite, '\n',
        'anonymous\n',
        'asruesch@gmail.com\n',
        'lcd "', tempdir(), '"\n',
        'cd pub/data/ghcn/daily/all/\n',
        'binary\n',
        'mget ', id, '*\n',
        'disconnect\n',
        'quit',
        sep=''
    )
    ftpCmdFile = tempfile()
    write(ftpString, ftpCmdFile)
    cmd = paste('ftp -i -s:', ftpCmdFile, sep='')
    a = system(cmd)
    unlink(ftpCmdFile)
    fname =  list.files(tempdir(), pattern=as.character(id))
    if (!file.exists(paste(tempdir(), fname, sep='\\'))) {next}
    p = read.fwf(paste(tempdir(), fname, sep='\\'),
        header=F, widths=widths
    )
    unlink(paste(tempdir(), fname, sep='/'))
    p = p[p[,2] >= 2002 & p[,4] == 'PRCP',]
    p[p == -9999] = NA
    
    }
    write.csv(d, outfile, row.names=F)
}