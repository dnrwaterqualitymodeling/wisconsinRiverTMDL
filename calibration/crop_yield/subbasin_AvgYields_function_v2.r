# This script produces simulated vs observed plots for crop yields at the county and basinwide level
# CHANGE THESE ###########

yldCalib = function(crop, scenario = 'Default', unit = 'metric', projectDir = 'H:/WRB', basinWide = F){
    
    options(stringsAsFactors=F)
    # Subbasin IDs in each County
    dir_crop_valid = "T:/Projects/Wisconsin_River/GIS_Datasets/validation/crop_yield"
    subbasinIDs = read.csv(paste(dir_crop_valid, 
        "subbasins_byWRB_County.txt", 
        sep='/'))
    
    # Raw NASS data
    obsDataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/upDated_NASSstats_30092014.csv"
    
    #   crop Lookup      
    cropTab = data.frame(
        CropName = c('corn_grain', 'corn_silage', 'haylage', 'hay', 'soybeans', 'wheat'), 
        CropKeySWAT = c('CORN', 'CSIL', 'ALFA',NA,'SOYB',NA),
        CropKeyNASS = c('CORN, GRAIN - YIELD, MEASURED IN BU / ACRE',
                      'CORN, SILAGE - YIELD, MEASURED IN TONS / ACRE',
                      'HAY & HAYLAGE, ALFALFA - YIELD, MEASURED IN TONS / ACRE, DRY BASIS',
                      'HAY, ALFALFA - YIELD, MEASURED IN TONS / ACRE',
                      'SOYBEANS - YIELD, MEASURED IN BU / ACRE',
                      'WHEAT - YIELD, MEASURED IN BU / ACRE')
    )
    cropKeySWAT = cropTab[which(cropTab$CropName == crop),'CropKeySWAT']  
    cropKeyNASS = cropTab[which(cropTab$CropName == crop),'CropKeyNASS'] 
    
    # set equal to your scenario of choice 
    output.hru = paste(projectDir,"/Scenarios/",scenario,"/TxtInOut/output.hru",sep='')
    widths = c(4,5,10,5,5,5, rep(10,67), 11, 11, rep(10, 10))
    print('Importing simulated data...')
    dat = readLines(output.hru)
    dat = dat[10:length(dat)]
    
    col_names=c("LULC","HRU","GIS","SUB","MGT","MON","AREA","PRECIP","SNOWFALL","SNOWMELT",
            "IRR","PET","ET","SW_INIT","SW_END","PERC","GW_RCHG","DA_RCHG","REVAP","SA_IRR",
            "DA_IRR","SA_ST","DA_ST","SURQ_GEN","SURQ_CNT","TLOSS","LATQ","GW_Q","WYLD","DAILYCN",
            "TMP_AV","TMP_MX","TMP_MN","SOL_TMP","SOLAR","SYLD","USLE","N_APP","P_APP","NAUTO",
            "PAUTO","NGRZ","PGRZ","CFERTN","CFERTP","NRAIN","NFIX","F_MN","A_MN","A_SN",
            "F_MP","AO_LP","L_AP","A_SP","DNIT","NUP","PUP","ORGN","ORGP","SEDP",
            "NSURQ","NLATQ","NO3L","NO3GW","SOLP","P_GW","W_STRS","TMP_STRS","N_STRS","P_STRS",
            "BIOM","LAI","YLD","BACTP","BACTLP","WTAB","WTABELO","SNO_HRU","CMUP_KGH","CMTOT_KGH",
            "QTILE","TNO3","LNO3","GW_Q_D","LATQ_CNT")
    
    query_output.hru = function(x, widths, col_name, col_names) {
        cum_col = c(0, cumsum(widths))
        col_index = which(col_names == col_name)
        start = cum_col[col_index] + 1
        end = start + widths[col_index] - 1
        print(paste(start, end))
        return(substr(x, start, end))
    }
    
    select_cols = list(
        cols = c("LULC", "SUB", "HRU", "MON", "YLD", "BIOM", "AREA"),
        dtypes = c(as.character, as.integer, as.integer, as.integer, as.numeric, as.numeric, as.numeric)
    )
    modData_allSubs = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
    modData_allSubs = as.data.frame(modData_allSubs)
    names(modData_allSubs) = select_cols$cols
    for (row in 1:length(select_cols$cols)) {
        col_name = select_cols$cols[row]
        dtype = select_cols$dtypes[row][[1]]
        vals = query_output.hru(dat, widths, col_name, col_names)
        vals = sapply(vals, dtype)
        modData_allSubs[col_name] = data.frame(vals, stringsAsFactors=F)
    }
	modData_allSubs = subset(modData_allSubs, LULC == cropKeySWAT & MON > 13)

    # Grabbing and formating obs for specific county
    yldDat = read.csv(obsDataFile)
    # convert to Date format and select from 2002 onward
    yldDat$DATE = as.Date(as.character(paste(yldDat[,'Year'],1,1,sep ='-')))
    yldDat = yldDat[yldDat$DATE>='2002-01-01',] 
    
    # reducing Observed (NASS) data to crop of interest
    yldDat = yldDat[which(yldDat$Data.Item == cropKeyNASS),]
    
    # Converting to metric units
    if (unit =='metric'){
        if (cropKeySWAT == 'CSIL'){
            # to reduce observed to dry weight, assuming NASS
            yldDat$YLD = (yldDat$Value * 2.24 * 0.35) 
        } else if (cropKeySWAT == 'ALFA'){
            # to reduce to dry weight, assuming NASS of 13%
            yldDat$YLD = (yldDat$Value * 2.24 * 0.87) 
        } else if (cropKeySWAT == 'CORN'){
            #NASS has 15% and 56#/bu
            #(paste('Converting corn from bu/acre to tons per Ha'))
            yldDat$YLD = (yldDat$Value *56)*(2.471/2205)*(0.845)#(1/1.155)     
        } else if (cropKeySWAT == 'SOYB'){
            #NASS has 12.5% moisture for soy and 60#/bu
            yldDat$YLD = (yldDat$Value *60)*(2.471/2205)*(0.875)#(1/1.125) 
        }
    }
    
    counties = unique(yldDat$County)
    all_cnty_data = data.frame()

    for (cnty in counties){
#         cnty = 'ADAMS'
        print(paste("Starting", cnty, "County"))
        subIDs = subbasinIDs[toupper(subbasinIDs$CTY_NAME) == cnty, 11]
        cnty_SWAT_data = data.frame()
        # taking just the yearly harvest values, ASSUMING MONTHLY
        modData = modData_allSubs[modData_allSubs$SUB %in% subIDs,]
        if (nrow(modData) == 0) {next}
        area_wt_yld = merge(aggregate(YLD * AREA ~ MON, data=modData, sum),
            aggregate(AREA ~ MON, data=modData, sum))
        area_wt_yld$MEAN = area_wt_yld[['YLD * AREA']] / area_wt_yld[['AREA']]
        cnty_data = data.frame(
            CNTY=cnty,
            DATE=as.Date(paste(area_wt_yld$MON,"01-01", sep="-")),
            YLD=area_wt_yld$MEAN
        )
        all_cnty_data = rbind(all_cnty_data, cnty_data)   
    }
    obsData = aggregate(YLD ~ County + DATE, data=yldDat, mean)
    print('Plotting county by county...')
    ylm = range(c(obsData$YLD, all_cnty_data$YLD))
    pdf(paste(crop,scenario,'_Obs_v_Sim_byCounty.pdf',sep = ''))
    for (cnty in counties){
        plot(YLD ~ DATE,
         data=obsData[which(obsData$County == cnty),],
         pch = 20,
         col="aquamarine",
         type="b",
         ylim=ylm,
         ylab= paste(crop, "Mg/ha"),
         xlab="Date",
         cex = 1.5,
         main=paste(cnty,'County'))

        points(YLD ~ DATE, data=all_cnty_data[which(all_cnty_data$CNTY == cnty),],
           type = 'b', pch=20, col="coral1", cex = 1.5)
    
        legend('topright', legend = c('Observed','Simulated'),
            fill = c('aquamarine', 'coral1'))
    }
    dev.off()
    
    # for basin wide
    if (basinWide){
        print('Plotting basin-wide...')
        obsData_basin = aggregate(YLD ~ DATE, data=yldDat, mean)
        
        simData_basin = aggregate(YLD ~ DATE, data=all_cnty_data, mean)

        pdf(paste(crop, scenario,'_Obs_v_Sim_basinWide.pdf',sep = ''))
        plot(YLD ~ DATE,
             data=obsData_basin,
             pch = 20,
             col="aquamarine",
             type="b",
             ylim=ylm,
             ylab= paste(crop, "Mg/ha"),
             xlab="Date",
             cex = 1.5,
             main=paste('Basin wide Annual Averages'))
    
        points(YLD ~ DATE, data=simData_basin,
            type = 'b', pch=20, col="coral1", cex = 1.5)
        
        legend('topright', legend = c('Observed','Simulated'),
            fill = c('aquamarine', 'coral1'))
        dev.off()
    }
    print('Complete.')
    
}


# yldCalib('corn_grain', basinWide = T)
# yldCalib('haylage', basinWide = T)
# yldCalib('corn_silage', basinWide = T)
# yldCalib('soybeans', basinWide = T)