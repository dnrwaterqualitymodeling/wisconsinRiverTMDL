# SOME of the mapunits in the SSURGO database have two designations for hydrologic soil group, these 
# are A/D, B/D, and C/D, the former assuming the land is drained, and the latter assuming that the 
# soil is not drained. The SWAT soils database assumes that all of the soils are drained and there-
# fore assumes the not D option. 
#     This script looks at each of those mapunits with dual HSGs and examines the amound of cropland
# within the polygon. We (A. Ruesch and D. Evans) that if an area contains 50% or more agriculutral
# land (as defined by CDL) then it can be assumed to be drained. We found that NONE of those sites 
# that have two HSG designations in the WR basin have greater than 50% agland within. Thus all of the
# MUKEYs were updated with the D designation.

library(raster)
library(rgdal)
options(stringsAsFactors = F, warn = 1)
net_soil_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"
dual_hsgs = paste(net_soil_dir, 'dual_hsgs_to_single.txt', sep="/")
wrb_mukeys_file = paste(net_soil_dir, "wrb_mukeys.txt", sep="/")
comp_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/component.txt", sep="/")
hrzn_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chorizon.txt", sep="/")
chfr_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chfrags.txt", sep="/")

#########
# mukeys only within the WRB
wrb_mukeys = unique(read.table(wrb_mukeys_file, header=T, sep=',')[["MUKEY"]])
wrb_mukeys = wrb_mukeys[!is.na(wrb_mukeys)]
# read in gSSURGO tables and process
mupolygon = readOGR(paste(net_soil_dir, "WRB_Soils_2mile_Buffer_gSSURGO.gdb", sep="/"),
    "MUPOLYGON__2mile_buffer_wMich_2014")
mupolygon = subset(mupolygon, MUKEY %in% wrb_mukeys$MUKEY)
##############
dat_cols = c(
    "dbovendry_r",
    "awc_r",
    "ksat_r",
    "cbn",
    "claytotal_r",
    "silttotal_r",
    "sandtotal_r",
    "fragvol_r", 
    "albedodry_r",
    "kwfact",
    "ec_r", 
    "caco3_r", 
    "ph1to1h2o_r",
    "hydgrp")
comp_cols = c("mukey",
    "cokey",
    "compname",
    "comppct_r",
    "hydgrp",
    "albedodry_r")
hrzn_cols = c("cokey",
    "chkey",
    "hzdept_r",
    "hzdepb_r",
    "dbovendry_r",
    "awc_r",
    "om_r",
    "ksat_r",
    "claytotal_r",
    "silttotal_r",
    "sandtotal_r",
    "kwfact",
    "ec_r",
    "caco3_r",
    "ph1to1h2o_r")
chfr_cols = c("chkey", "fragvol_r")
print("Reading component, horizon, and chfrags tables. Please wait...")
comp = read.table(comp_file, header=T, sep="\t")[comp_cols]
hrzn = read.table(hrzn_file, header=T, sep="\t")[hrzn_cols]
chfr = read.table(chfr_file, header=T, sep="\t")[chfr_cols]
comp = subset(comp, mukey %in% wrb_mukeys) # Only mukeys in WRB
comp = merge(comp, hrzn, by="cokey", all.x=T, all.y=T) # Join component with chorizon
chfr = aggregate(fragvol_r ~ chkey, chfr, sum, na.rm=T) # Sum rock volumes by hrzn
comp = merge(comp, chfr, by="chkey", all.x=T) # Join component/horizon with chfrags
comp$fragvol_r[is.na(comp$fragvol_r)] = 0 # Force NA rock fragments to zero
##############
lc = raster("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WRB_TMDL_LndCvr_Mgt_07152014.img")
lc[lc <= 9 | lc >= 55] = 0
lc[lc > 9 & lc < 55] = 1

wtm <- proj4string(lc)
mupolygon <- spTransform(mupolygon, CRS(wtm))

dual_hsg = subset(comp, hydgrp %in% c("A/D", "B/D", "C/D"))
dual_mukeys <- unique(dual_hsg$mukey)
mupoly_dual <- subset(mupolygon, MUKEY %in% dual_mukeys)

# This step takes a long time, the resulting data frame from the extract() function 
#   was written out to a table
mupoly_dual_ex <- extract(lc, mupoly_dual, fun = mean, na.rm = T, sp = T)
# writeOGR(obj = mupoly_dual_ex, 
#     dsn= net_soil_dir,
#     layer = 'mupolygon_proportion_ag',
#     driver = 'ESRI Shapefile')
# # 
# mupoly_dual_df <- mupoly_dual_ex@data
drained_mukeys <- mupoly_dual_ex@data$MUKEY[mupoly_dual_df$layer > 0.5] 
drained_mukeys <- unique(drained_mukeys)
write.table(drained_mukeys, 
    paste(net_soil_dir, 'drained_mukeys.txt',sep='/'),
    sep = '\t',
    row.names = F)
# write.table(mupoly_dual_df, 
#             paste(net_soil_dir, 'mupolygon_ag_analysis.txt',sep='/'),
#             sep = '\t',
#             row.names = F)

