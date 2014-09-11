library(RColorBrewer)
library(classInt)
library(ggplot2)

setwd("T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/Pat_BJames_SWIMS_ReservoirData")

sites <- c("013016.csv", "283132.csv", "373135.csv", "373136.csv", "373137.csv", "373445.csv", 
           "503163.csv", "10007758.csv", "10031116.csv", "10031168.csv", "10031169.csv", "10031170.csv",
           "10031171.csv", "10031172.csv", "10031173.csv", "10031174.csv","10031175.csv", "10031184.csv", 
           "10031185.csv", "10031186.csv")
pal = brewer.pal(6, "YlGnBu")[2:6]

dateFunc = function(X) {
    X = as.character(X)
    if (nchar(strsplit(X, "/")[[1]][3]) == 2) {
        return(as.POSIXct(X, format="%m/%d/%y"))
    } else {
        return(as.POSIXct(X, format="%m/%d/%Y"))
    }
}


for(site in sites){
    sitename = strsplit(site, "\\.")[[1]][1]
    pdf(paste(sitename, "pdf", sep="."), width=8.5, height=11)
    par(mfrow=c(4,1), oma=c(4,1,1,1))
    d = read.csv(site)
    params = unique(d[,"PARAMETER"])
    params = params[which(params != 'PH LAB' & params != "")]
    count = 0
    for (param in params) {
        # if (param %in% c("CONDUCTIVITY FIELD", "DISSOLVED OXYGEN FIELD")) {
            # pal = brewer.pal(6, "YlGnBu")[2:6]
        # } else if (param == "PH FIELD") {
            # pal = brewer.pal(6, "RdPu")[2:6]
        # } else if (param == "TEMPERATURE FIELD") {
            # pal = brewer.pal(6, "YlOrRd")[2:6]
        # }
        pd = d[d$PARAMETER == param & !is.na(d$DEPTH) & !is.na(d$VALUE) & d$DEPTH != "NULL",]
        if (nrow(pd) == 0) {next}
        count = count + 1
        nonMetric = grep("Meters", pd$DEPTH, ignore.case=T, invert=T)
        valsRegex = regexpr("[-+]?[0-9]*\\.?[0-9]+", pd$DEPTH)
        depthVals = substr(
            pd$DEPTH,
            valsRegex,
            attr(valsRegex, "match.length") + valsRegex - 1)
        depthVals[nonMetric] = as.numeric(depthVals[nonMetric]) * 0.3048
        pd$DEPTH = -as.numeric(depthVals)
        pd$DATE = as.POSIXct(
            apply(pd["DATE"], 1, FUN=dateFunc),#f
            origin=as.POSIXct("1970-01-01"))
        if (param == "TEMPERATURE FIELD") {
            nonMetric = grep("DEGREES F", pd$UNITS, ignore.case=T)
            pd$VALUE[nonMetric] = (pd$VALUE[nonMetric] - 32) / 1.8
        }
        classes = classIntervals(pd$VALUE, 5)
        col = findColours(classes, pal)
        main = paste(sitename, param, sep=" - ")
        plot(DEPTH ~ DATE, data=pd, col=col, pch=20, cex=1.5,
             main=main, ylab="Depth", xlab="Date")
        if (count %% 4 == 0) {
            par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
            plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
            legend("bottom",
                   c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1"),
                   title="Percentile",
                   col = brewer.pal(6, "Greys")[2:6],
                   pt.cex=3,
                   xpd = TRUE,
                   horiz = TRUE,
                   inset = c(0, 0),
                   bty = "n",
                   pch = 20)
        }
    }
    dev.off()
}