# SOME of the mapunits in the SSURGO database have two designations for hydrologic soil group, these 
# are A/D, B/D, and C/D, the former assuming the land is drained, and the latter assuming that the 
# soil is not drained. The SWAT soils database assumes that all of the soils are drained and there-
# fore assumes the not D option. 
#     This script looks at each of those mapunits with dual HSGs and examines the amound of cropland
# within the polygon. We (A. Ruesch and D. Evans) that if an area contains 50% or more agriculutral
# land (as defined by CDL) then it can be assumed to be drained. We found that NONE of those sites 
# that have two HSG designations in the WR basin have greater than 50% agland within. Thus all of the
# MUKEYs were updated with the D designation.

library(foreign)
library(raster)
library(rgdal)
library(RODBC)
options(stringsAsFactors = F)

net_soil_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets/Soils/"
soil_mdb = "SWAT_US_SSURGO_Soils/SWAT_US_SSURGO_Soils.mdb"
con <- odbcConnectAccess(paste(net_soil_dir, soil_mdb,sep = ''))
soil_tbl = sqlFetch(con, "SSURGO_Soils")
odbcClose(con)

lc = raster("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WRB_TMDL_LndCvr_Mgt_07152014.img")
lc[lc <= 9 | lc >= 55] = 0
lc[lc > 9 & lc < 55] = 1

wtm <- proj4string(lc)
mupolygon = readOGR(paste(net_soil_dir, "WRB_Soils_2mile_Buffer_gSSURGO.gdb",sep = ''), "MUPOLYGON")
mupolygon <- spTransform(mupolygon, CRS(wtm))
mukeys = unique(mupolygon@data$MUKEY)
soil_tbl = subset(soil_tbl, MUID %in% mukeys)
dual_hsg = subset(soil_tbl, HYDGRP_ORIG %in% c("A/D", "B/D", "C/D"))
dual_mukeys <- unique(dual_hsg$MUID)
mupoly_dual <- subset(mupolygon, MUKEY %in% dual_mukeys)

# This step takes a long time, the resulting data frame from the extract() function 
#   was written out to a table
# mupoly_dual_ex <- extract(lc, mupoly_dual, fun = mean, na.rm = T, sp = T)
# 
# mupoly_dual_df <- mupoly_dual_ex@data
# write.table(mupoly_dual_df, 
#             paste(net_soil_dir, 'updated_HYDGRP.txt',sep=''),
#             sep = '\t',
#             row.names = F)
mupoly_dual_df <- read.delim(paste(net_soil_dir,'updated_HYDGRP.txt',sep =''))

# dual_df_lyrBYarea <- aggregate(layer * Shape_Area ~ MUKEY, FUN = sum, data = mupoly_dual_df, na.rm = F)
# dual_df_area <- aggregate(Shape_Area ~ MUKEY, FUN = sum, data = mupoly_dual_df, na.rm = T)
# mupoly_df <- merge(dual_df_lyrBYarea, dual_df_area, all = T)
# mupoly_df$prpAg <- mupoly_df[,2]/mupoly_df$Shape_Area 
# 
# rm(dual_df_lyrBYarea, dual_df_area, mupoly_dual_df)
# write.table(mupoly_df, paste(net_soil_dir,'mukey_dualHSG_propAg.txt',sep=''), sep = '\t', row.names = F)
mupoly_df <- read.delim(paste(net_soil_dir,'mukey_dualHSG_propAg.txt',sep=''))
sum(mupoly_df$prpAg>=0.5, na.rm = T)
sum(is.na(mupoly_df$prpAg))
soil_tbl$HYDGRP_wr <- soil_tbl$HYDGRP
numChnged <- 0
for (ky in unique(soil_tbl$MUID)) {
#     ky <- 395333
    if (! ky %in% mupoly_df$MUKEY){next}
    oldHSG <- soil_tbl$HYDGRP_ORIG[soil_tbl$MUID == ky] 
    drnd <- mupoly_df$prpAg[mupoly_df$MUKEY == ky] > 0.5
    if(is.na(drnd)){
        # upon inspection, those mapunits that fall outside of 
        # the WI river basin with dual hsgs are mucks, as all
        # the organic soils in the wrb were not found to be 
        # in ag areas, they were assumed to be undrained,
        # so the same assumption is made here, without data
        hsg <- strsplit(oldHSG, split = '/')[[1]][2]
        print(paste('Changed to', hsg))
        print(' ')
        numChnged <- numChnged + 1
    } else if (drnd){
        hsg <- strsplit(oldHSG, split = '/')[[1]][1]
    } else {
        hsg <- strsplit(oldHSG, split = '/')[[1]][2]
        print(paste('Changed to', hsg))
        print(' ')
        numChnged <- numChnged + 1
    }
    soil_tbl$HYDGRP_wr[soil_tbl$MUID == ky] <- hsg 
#     print(paste('Old HSG:', oldHSG))
#     print(paste('New HSG:', hsg))
}

con <- odbcConnectAccess(paste(net_soil_dir, soil_mdb,sep = ''))

for (row in 1:nrow(soil_tbl)) {
#     row <- 1
    query = paste(
        "UPDATE SSURGO_Soils ",
        "SET HYDGRP = '", soil_tbl$HYDGRP_wr[row], "' ",
        "WHERE MUID = '", soil_tbl$MUID[row], "';",
        sep = ""
    )
    stdout = sqlQuery(con, query)
    if (length(stdout)>1){
        print(stdout)
    }
}
odbcClose(con)