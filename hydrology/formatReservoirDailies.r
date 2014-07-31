library(foreign)
wvicFiles = c(
    "T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology/WVIC Flows/WVIC-Flows-For-TDML-2002-2013.csv",
    "T:/Projects/Wisconsin_River/GIS_Datasets/Hydrology/WVIC Flows/WVIC-Flows-For-TDML-2002-2013-Second-Set.csv"
)
outDir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/reservoir"

colSubXwalk = cbind(
    c(
        "Rainbow",
        "Kings",
        "Willow",
        "Rice",
        "Tomahawk",
        "Spirit",
        "Alexander",
        "Wausau",
        "Eau.Pleine",
        "Dubay",
        "Stevens.Point",
        "Petenwell",
        "Castle.Rock",
        "Buckatabon",
        "Seven.Mile",
        "Nine.Mile",
        "Little.St..Germain",
        "Rhinelander"
    ), c(
        167,
        163,
        166,
        164,
        161,
        111,
        158,
        154,
        87,
        81,
        148,
        74,
        59,
        129,
        121,
        317,
        318,
        221
    )
)

wvic1 = read.csv(wvicFiles[1])
wvic2 = read.csv(wvicFiles[2])
wvic = cbind(wvic1, wvic2[,2:ncol(wvic2)])

for (col in colSubXwalk[,1]) {
    wvic[[col]] = as.character(wvic[[col]])
    wvic[[col]][which(wvic[[col]] == "#N/A")] = NA
    wvic[[col]] = as.numeric(wvic[[col]])
}

wvic$Date = as.POSIXlt(wvic$Date, format="%m/%d/%Y")
doy = wvic$Date$yday
spinupDates = seq(as.POSIXlt("1990-01-01"), as.POSIXlt("2001-12-31"), "day")
spinupDoy = data.frame(doy=as.POSIXlt(spinupDates)$yday)

pdf(paste(outDir, "dailyTimeSeries.pdf", sep="/"), width=8.5, height=11)
par(mfrow=c(3,1))
for (i in 1:nrow(colSubXwalk)) {
    if (!is.na(colSubXwalk[i,2])) {
        print(colSubXwalk[i,1])
        d = data.frame(
            DATE = as.POSIXlt(wvic[["Date"]]),
            RESOUTFLOW = as.numeric(as.character(wvic[[colSubXwalk[i,1]]])) * 0.0283168
        )
        interpolated = rep(F, nrow(wvic))
        if (any(is.na(d$RESOUTFLOW))) {
            for (j in 1:nrow(d)) {
                if (is.na(d$RESOUTFLOW[j])) {
                    end = F
                    k = j + 1
                    while(!end) {
                        if (is.na(d$RESOUTFLOW[k])) {k=k+1;next} else {end=T}
                    }
                    startVal = d$RESOUTFLOW[j-1]
                    endVal = d$RESOUTFLOW[k]
                    dist = k - j
                    fillVals = seq(
                        from = startVal,
                        to = endVal,
                        length.out=dist+2)[-c(1,dist+2)]
                    d$RESOUTFLOW[j:(k-1)] = fillVals
                    interpolated[j:(k-1)] = T
                }
            }
        }
        plot((RESOUTFLOW/0.0283168) ~ DATE, data=d, xlab="Date", ylab="Flow (cfs)", type="l",
             main=sub("\\.\\.", "\\. ", sub("\\.", " ", colSubXwalk[i,1])), col="blue")
        if (any(interpolated)) {
            d_interpolated = d
            d_interpolated$RESOUTFLOW[!interpolated] = NA
            lines((RESOUTFLOW/0.0283168) ~ DATE, data=d_interpolated, col="red")
        }
        legend("topright", legend=c("raw","interpolated"), fill=c("blue", "red"))
        dailyAverage = data.frame(doy = 0:365,
            flow = aggregate(d$RESOUTFLOW, by=list(doy), mean)$x)
        spinupTimeSeries = spinupDoy
        spinupTimeSeries$flow = NULL
        for (day in 1:nrow(spinupTimeSeries)) {
            spinupTimeSeries$flow[day] = dailyAverage$flow[which(dailyAverage$doy == spinupTimeSeries$doy[day])]
        }
        spinupTimeSeries = data.frame(
            DATE = format(spinupDates, format="%m/%d/%Y"),
            RESOUTFLOW = spinupTimeSeries$flow
        )
        d$DATE = format(d$DATE, format="%m/%d/%Y")
        d = rbind(spinupTimeSeries, d)
        outFile = paste(outDir, "/subbasin_", colSubXwalk[i,2], ".txt", sep="")
        write.csv(d, outFile, row.names=F, quote=F)
    }
}
dev.off()
