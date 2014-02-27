library(RCurl)
ftpSite = 'ftp.ncdc.noaa.gov'
outdir = 'T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/winSlammPrecip/stations'
usafs = c(726452,
          726503,
          726465,
          726504,
          726626,
          726410,
          726574,
          726449,
          727415,
          726426,
          726417,
          726436,
          726463,
          726404,
          726416,
          726437,
          726468,
          726509
          )
widths=c(4,3,3,3,6,6,6,6,6,6,6,6)
for (usaf in usafs) {
    outfile = paste(outdir, '/', usaf, '.csv', sep='')
    if (file.exists(outfile)) {next}
    d = data.frame()
    for (year in 2002:2013) {
        ftpString = paste(
            'open ', ftpSite, '\n',
            'anonymous\n',
            'asruesch@gmail.com\n',
            'lcd "', tempdir(), '"\n',
            'cd pub/data/noaa/isd-lite/', year, '/\n',
            'binary\n',
            'mget ', usaf, '*\n',
            'disconnect\n',
            'quit',
            sep=''
        )
        ftpCmdFile = tempfile()
        write(ftpString, ftpCmdFile)
        cmd = paste('ftp -i -s:', ftpCmdFile, sep='')
        system(cmd)
        unlink(ftpCmdFile)
        fname =  list.files(tempdir(), pattern=as.character(usaf))
        p = read.fwf(
            gzfile(
                paste(tempdir(), fname, sep='\\')
            ),
            header=F, widths=widths
        )
        unlink(paste(tempdir(), fname, sep='/'))
        p = p[,c(1,2,3,4,11,12)]
        names(p) = c('year', 'month', 'day', 'hour', 'precip_1hr', 'precip_6hr')
        d = rbind(d,p)
    }
    write.csv(d, outfile, row.names=F)
}