wd = 'K:/workInProgress/SWAT_WRB_052914/Scenarios/Default/TxtInOut/'
bkp_wd = 'K:/TEMP'
setwd(wd)
system('rev622_64rel.exe', intern = FALSE,
       ignore.stdout = FALSE, ignore.stderr = FALSE,
       wait = TRUE, input = NULL, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)

reformat = function(fileID, configExt, newVal, param) {
    # We need to concatenate the filename elements to create a file path
    filename = paste(fileID, ".", configExt, sep="")
    # Read in the data in filename
    swatConfig = readLines(filename)
    lineToEdit = grep(param, swatConfig)
    oldStr = swatConfig[lineToEdit]
    oldValStr = strsplit(oldStr, '\\|')[[1]][1]
    newValStr = paste(format(newVal, width=nchar(oldValStr)-4), "    ", sep="")
    newStr = sub(oldStr, newValStr, oldStr)
    swatConfig[lineToEdit] = newStr
    # we need to create a backup file path
    bkpFilename = paste(fileID, configExt, "bkp", sep=".")
    file.copy(filename, bkpFilename)
    writeLines(swatConfig, filename)
}

filenames = list.files(pattern="\\.rte$")
for (filename in filenames) {
    print(filename) # this is helpful for us to track the execution of the code
    filenameElements = strsplit(filename, "\\.")[[1]]
    # The fileID is a numeric code associated with each file
    fileID = filenameElements[1]
    if (fileID == "000000000") {
        next
    }
    configExt = filenameElements[2]
    newVal = 0.01
    param = 'CH_S2'
    reformat(fileID, configExt, newVal, param)
}

# revert to original
for (filename in filenames) {
    bkpFilename = paste(fileID, paramType, "bkp", sep=".")
    print(filename) # this is helpful for us to track the execution of the code
    filenameElements = strsplit(filename, "\\.")[[1]]
    fileID = filenameElements[1]
    configExt = filenameElements[2]
    bkpFilename = paste(fileID, configExt, "bkp", sep=".")
    file.remove(filename)
    file.rename(from=bkpFilename,to=filename)
}

# plot flow over time
dates = seq(as.POSIXlt("1990-01-01"), as.POSIXlt("2013-12-31"), "day")
watout = read.fwf('watout.dat', skip=6, widths=c(5,5,7,rep(11,19)))
d = data.frame(Date=dates, Flow=watout[,4] * 35.3146667)
pdf(file="outletFlow.pdf", width=8.5, height=11)
par(mfrow=c(4,3))
for (yr in 2002:2013) {
    yd = d[d[,1] > as.POSIXlt(paste(yr,"-01-01", sep="")) &
                                  d[,1] < as.POSIXlt(paste(yr+1, "-01-01", sep="")),]
    plot(yd[,2] ~ yd[,1], xlab='date', ylab='Flow (cfs)', type='l', main=paste("Year", yr))
}
dev.off()