library(RColorBrewer)
library(classInt)
library(ggplot2)
dst <- "C:/Users/evansdm/Documents/waterQplots/"
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
#site <- sites[1]
# site <- "10031173.csv"
count = 0

for(site in sites){
    
	sitename = strsplit(site, "\\.")[[1]][1]
    paste("Working on",sitename)
	#pdf(paste(sitename, "pdf", sep="."), width=8.5, height=11)
    #par(mfrow=c(4,1), oma=c(4,1,1,1))
    d = read.csv(site)
    # params = unique(d[,"PARAMETER"])
    # params = params[which(params != 'PH LAB' & params != "")]
	# params = params[which(params == 'TEMPERATURE FIELD' | params == "DISSOLVED OXYGEN FIELD")]
    
	pd <- d[d$PARAMETER == 'TEMPERATURE FIELD' | d$PARAMETER == "DISSOLVED OXYGEN FIELD",]
	pd <- pd[!is.na(pd$DEPTH) & !is.na(pd$VALUE) & pd$DEPTH != "NULL",]
# data processing and conversions.
	if (nrow(pd) == 0) {next}
# 	fourDigYears <- which(nchar(as.character(pd$DATE)) == 10)
#     pd$DATE[fourDigYears] <- 
	nonMetric = grep("Meters", pd$DEPTH, ignore.case=T, invert=T)
	valsRegex = regexpr("[-+]?[0-9]*\\.?[0-9]+", pd$DEPTH)
	depthVals = substr(
		pd$DEPTH,
		valsRegex,
		attr(valsRegex, "match.length") + valsRegex - 1)
	depthVals[nonMetric] = as.numeric(depthVals[nonMetric]) * 0.3048
	pd$DEPTH = -as.numeric(depthVals)
	#pd$DATE <- apply(pd["DATE"], 1, FUN=dateFunc)
	pd$DATEpsx = as.POSIXct(
		apply(pd["DATE"], 1, FUN=dateFunc),
		origin=as.POSIXct("1970-01-01"))
	pd <- pd[order(pd$DATEpsx),]
	nonMetric = grep("DEGREES F", pd$UNITS, ignore.case=T)
	pd$VALUE[nonMetric] = (pd$VALUE[nonMetric] - 32) / 1.8
	
	daytes <- unique(pd[,"DATEpsx"])
	# separating by dates
	pdf(paste(dst, sitename, '.pdf',sep = ''))
	for (dayte in daytes){
        dayte = as.Date(dayte/86400, origin=as.POSIXct("1970-01-01"))
		#if (count > 15) {break}
		#dayte <- daytes[1]
        #dayte <- "08/08/2012"
		count = count + 1
		mainTitle <- paste('Site: ', sitename,', Date: ', dayte, sep = '')
		#dayte <- daytes[1]
		print(paste("Working on",dayte))
		t_pd <- pd[pd$DATEpsx == as.POSIXct(dayte),]
		# maybe just call temp and DO explicitly
		temp <- t_pd[t_pd$PARAMETER=="TEMPERATURE FIELD",]
		DO <- t_pd[t_pd$PARAMETER=="DISSOLVED OXYGEN FIELD",]
		temp <- temp[order(temp$DEPTH, decreasing =T),]
		DO <- DO[order(DO$DEPTH, decreasing =T),]
		print(paste('Plotting number:', count))
		#pdf(paste(dst, sitename,' ', gsub('/','_', dayte), '.pdf',sep = ''))
		if (nrow(temp) > 0) {
			plot(DEPTH ~ VALUE, data = temp, 
				type = 'l', xlab = '', xlim = c(10, 30),
				xaxt = 'n', col = 'red', lwd = 2,
				main=mainTitle, ylab="Depth, meters")
			axis(side = 1, col = 'red')
		} 
		if (nrow(DO) >0) {
            par(new = TRUE) 
            if (nrow(temp) > 0) { axes=F } 
			plot(DEPTH ~ VALUE, data = DO,
				type = 'l', xlab = '', xlim = c(0, 12),
				axes = axes, col = 'blue', lwd = 2, ylab = '')
			if (!axes) { axis(side = 1, line = 3, col = 'blue') }
            }
		
		legend('bottomright', legend = c(expression(paste("Temperature ",degree,"C")), 'Dissolved Oxygen, mg/L'),
			fill = c('red','blue'), bty = 'n')
		#dev.off()
	}
	dev.off()
}
		# for (param in params){
			# pt_pd <- t_pd[t_pd$PARAMETER==param,]
			# plot(DEPTH ~ VALUE, data = pt_pd, type = 'l')
		# }	
	#}
	# for (param in params) {
        # param <- "TEMPERATURE FIELD"
		# if (param %in% c("CONDUCTIVITY FIELD", "DISSOLVED OXYGEN FIELD")) {
            # pal = brewer.pal(6, "YlGnBu")[2:6]
        # } else if (param == "PH FIELD") {
            # pal = brewer.pal(6, "RdPu")[2:6]
        # } else if (param == "TEMPERATURE FIELD") {
            # pal = brewer.pal(6, "YlOrRd")[2:6]
        # }
        # pd = d[d$PARAMETER == param & !is.na(d$DEPTH) & !is.na(d$VALUE) & d$DEPTH != "NULL",]
        
        #classes = classIntervals(pd$VALUE, 5)
        #colr = findColours(classes, pal)
        # main = paste(sitename, param, sep=" - ")
        # plot(DEPTH ~ DATE, data=pd, col=colr, pch=20, cex=1.5,
             # main=main, ylab="Depth", xlab="Date")
        # if (count %% 4 == 0) {
            # par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
            # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
            # legend("bottom",
                   # c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1"),
                   # title="Percentile",
                   # col = brewer.pal(6, "Greys")[2:6],
                   # pt.cex=3,
                   # xpd = TRUE,
                   # horiz = TRUE,
                   # inset = c(0, 0),
                   # bty = "n",
                   # pch = 20)
        # }
    # }
    #dev.off()
