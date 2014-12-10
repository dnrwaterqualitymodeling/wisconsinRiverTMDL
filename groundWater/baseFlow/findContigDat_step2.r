# This script runs after dv_processing_step_1.r, and is thus step 2
#    the bflow recession analysis model requires only continguous streamflow
#   records and so these need to be extracted from the daily flow records, which is
#   what this script does.
wd = "C:/evans/bflow/wi_dailyValues"
setwd(wd)
# this is the folder containing the bflow.exe executable
dst <- "C:/evans/bflow/baseflow/"

lessThanAYear <- NULL
site_file = "data_files/dvSiteDesc.txt"
sites = read.table(site_file, skip=30, sep="\t", comment.char="",
                   colClasses=c(rep("character", 8), 'numeric'))
# grab sites with drainage area of certain sizes,  #site <- "04026190"
#   from 50 sq km to 500
#         1000 sq km = 386.102
# this loop moves through each site file (downloaded in step 1),
#   checks to see if the entire record has a year of 
sites = sites[sites[,9] > 19.3051 & sites[,9] < 386.102 & !is.na(sites[,9]),]
pdf('ContiguousPeriods_Hydrographs_v4.pdf')
for (site in sites[,2]){
    #site = '040263205'
    print (site)
    #if(site == "04029990"){break}
    data_file = paste("dailies/", site, ".txt", sep="")
    
    err <- try(
        (daily_data = read.table(data_file))
        ,silent = T)
    if(class(err)=="try-error"){
        print(paste("Skipping:", site, "no lines."))
        next}
    daily_data = daily_data[3:nrow(daily_data),c(3,4)]
    names(daily_data) = c("Date", "Flow")
    daily_data$Date = as.Date(daily_data$Date)
    daily_data$Flow = as.character(daily_data$Flow)
    daily_data$Flow[daily_data$Flow == "Ice"] = NA
    daily_data$Flow = as.numeric(daily_data$Flow)
    site_name = sites[sites[,2]==site,3]
    #   checks to see if the entire record has a year of data
    # If less than 1 year of data, toss
    if (nrow(daily_data)< 365){
        dc <- c(site, "< 1 year, total")
        lessThanAYear <- rbind(lessThanAYear, dc)
        plot(Flow ~ Date, data=daily_data, type="l", main=paste(site_name, site), sub = '<1 year')
    } else {
        print(paste("Working on", site_name, '...'))
        # here breaks are counted, and the places where the time gaps
        #   of longer than 10 days occur are noted
        contigList <- NULL
		breakcounter <- 1
		jmp <- 1
        for (day in 1:nrow(daily_data)){
            today <- daily_data$Date[day]
            nextRecord <- daily_data$Date[day+1]
            if (!is.na(nextRecord - today) & ((nextRecord - today) >= 10)){
                # look at the records, if there are more than 10 days in between
                #   grab the contiguous record up to that point
                contig <- data.frame(daily_data[jmp:day,])
                if (nrow(contig)>=365){
                    #if that contiguous record is a year or more
                    #   write it to a separate file for input to blow
                    # and plot the hydrograph
                    contig[,1] <- gsub("-", "", contig[,1])
                    if (!file.exists(paste(dst, site ,'_',breakcounter,'.flw',sep=''))) {
                        print(paste("Writing", dst,site_name,breakcounter, "to file..."))
                        write.table(contig, paste(dst, site,'_',breakcounter,'.flw',sep=''), 
                                row.names = F, quote = F)}
                    plot(Flow ~ Date, data=daily_data[jmp:day,], type="l", main=paste(site_name, site), sub = paste('Break Number:',breakcounter, '>= 1 year'))
                } else { 
                    # for those breaks that are less than a year, plot them, note that its
                    #   less than a year, and don't write a file
                    dc <- c(site, paste('breakNum:',breakcounter,'<1 year'))
                    lessThanAYear <- rbind(lessThanAYear, dc)
                    plot(Flow ~ Date, data=daily_data[jmp:day,], type="l", 
                         main=paste(site_name, site), sub = paste('Break Number:',breakcounter, '<1 year'))
                }
                jmp <- which(daily_data$Date == nextRecord)
                breakcounter <- breakcounter + 1
            } else { #if there are no breaks in the record,
                # check if the record is a year or more,
                #   if it is, then write a file and plot it (plot it regardless of length)
                if (daily_data[day,1] == daily_data[nrow(daily_data),1]){
                    contig <- data.frame(daily_data[jmp:day,])
                    if (nrow(contig) >= 365){
                        if (!file.exists(paste(dst, site,'_',breakcounter,'.flw',sep=''))) {
                            print(paste("Writing", dst, site_name, breakcounter, "to file..."))
                            write.table(contig, paste(dst, site,'_',breakcounter,'.flw',sep=''),
                                    row.names = F, quote = F)}
                        plot(Flow ~ Date, data=daily_data[jmp:day,], type="l", 
                            main=paste(site_name, site), sub = paste('Break Number:',breakcounter, '>= 1 year'))
                    } else {
                        dc <- c(site, paste('breakNum:',breakcounter,'<1 year'))
                        lessThanAYear <- rbind(lessThanAYear, dc)
                        if (sum(is.na(contig[,2])) != nrow(contig)){
                            plot(Flow ~ Date, data=daily_data[jmp:day,], type="l", 
                             main=paste(site_name, site), sub = paste('Break Number:',breakcounter, '<1 year'))}
                    }
                }  
            }
        }
    }
}
dev.off()