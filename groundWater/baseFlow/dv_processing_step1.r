# The first step in processing usgs daily value streamflow data
# Downloads daily flows of streams in wisconsin
options(warn=0)
wd = "T:/Projects/Wisconsin_River/Code/groudWater/baseflow"
setwd(wd)
    # data retrieved 2014-08-22
        # re retrieved 2014-08-27 to grab more sites after filtering fixed.
url_prefix = "http://waterservices.usgs.gov/nwis/dv/?"
format = "rdb"
# grabbing data from 1950 to the end of 2013
startDT = "1950-01-01"
endDT = "2013-12-31"
siteType = "ST"
statCD = "00003"
variable = "00060"
# The site description file, containing site numbers, names, 
#   latlong and drainage area. This was downloaded separately from 
#   usgs at http://waterdata.usgs.gov/nwis/dv?referred_module=qw&search_criteria=state_cd&submitted_form=introduction
site_file = "data_files/dvSiteDesc.txt"
sites = read.table(site_file, skip=30, sep="\t", comment.char="",
    colClasses=c(rep("character", 8), "numeric"))
    # grab sites with drainage area (column 9) of certain sizes
sites = sites[sites[,9] > 19.3051 & sites[,9] < 386.102 & !is.na(sites[,9]),]

#   Loop grabs the daily flow data from usgs website,
#       and adds the hydrograph to a pdf
pdf("hydrographs_v4.pdf", width=8.5, height=11)
par(mfrow=c(4,1))
for (site in sites[,2]) {
    print(site)
    #if (site %in% c("05361000","05544000",
    #                "04025000","04026000",
    #                "04028500","04029000")) {next}
#     if (site == "05430500") {break}

    url = paste(url_prefix,
        "format=", format,
        "&sites=", site,
        "&startDT=", startDT,
        "&endDT=", endDT,
        "&siteType=", siteType,
        "&statCd=", statCD,
        "&variable=", variable,
        sep=""
    )
    data_file = paste("dailies/", site, ".txt", sep="")
    if (!file.exists(data_file)) {
        #writeLines(readLines(url), data_file)
        err <- try(
            (writeLines(readLines(url), data_file))
            ,silent = T)
        if(class(err)=="try-error"){
            print(paste("Skipping:", site, "404 Error."))
            next}
    }
    err <- try(
        (daily_data = read.table(data_file))
        ,silent = T)
    if(class(err)=="try-error"){
        print(paste("Skipping:", site, "No Lines."))
        next}
    daily_data = daily_data[3:nrow(daily_data),c(3,4)]
    names(daily_data) = c("Date", "Flow")
    daily_data$Date = as.Date(daily_data$Date)
    all_dates = data.frame(
        Date=seq(as.Date(min(daily_data$Date)), as.Date(max(daily_data$Date)), 1)
    )
    daily_data$Flow = as.character(daily_data$Flow)
    daily_data$Flow[daily_data$Flow == "Ice"] = NA
    daily_data$Flow = as.numeric(daily_data$Flow)
    daily_data = merge(all_dates, daily_data, all.x=T)
    site_name = sites[sites[,2]==site,3]
    plot(Flow ~ Date, data=daily_data, type="l", main=paste(site_name, site),
         ylim=c(min(daily_data$Flow,na.rm=T),max(daily_data$Flow,na.rm=T)))
}
dev.off()
