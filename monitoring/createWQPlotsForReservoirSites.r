setwd("T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/Pat_BJames_SWIMS_ReservoirData")
projectDir = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/Pat_BJames_SWIMS_ReservoirData/Analysis"

plotDir = paste(projectDir, "plots", sep="/")
if (!file.exists(plotDir)) {
    dir.create(plotDir)
}

sites <- c("013016.csv", "283132.csv", "373135.csv", "373136.csv", "373137.csv", "373445.csv", 
           "503163.csv", "10007758.csv", "10031116.csv", "10031168.csv", "10031169.csv", "10031170.csv",
           "10031171.csv", "10031172.csv", "10031173.csv", "10031174.csv","10031175.csv", "10031184.csv", 
           "10031185.csv", "10031186.csv")

for(site in sites){
    d = read.csv(site)
    dateIDs = unique(d[,"DATE"])
    for(id in dateIDs) {
        day = d[which(d$DATE==id), ]
        params = unique(day[,"PARAMETER"])
        xlim = range(day$VALUE, na.rm=T)
        count = 0
        for (param in params) {
            count = count + 1
            WQtype = day[which(day$PARAMETER==param), ]
            depths_list = strsplit(as.character(WQtype$DEPTH), " ")
            depths = NULL
            for (depth in 1:length(depths_list)) {
                depths = rbind(depths, depths_list[[depth]])
            }
            WQtype$DEPTH = depths[,1]
            WQtype$UNITS = depths[,2]
            if (count == 1) {
                plot(DEPTH ~ VALUE, 
                     data=WQtype,
                     type = "b",
                     main = paste(siteNum, "-", id),
                     xlab = "WQ Parameter",
                     ylab = "Depth",
                     xlim = xlim)
            } else {
                lines(DEPTH ~ VALUE, 
                     data=WQtype,
                     type = "b")
            }
        }
    }
}

siteNum = strsplit(site, split = ".", fixed = TRUE) [[1]][1]

plot(day[,5], day[,7], type = "b", main = paste(siteNum, "-", id), xlab = "WQ Parameter", ylab = "Depth")

barchart(day[,5]~da