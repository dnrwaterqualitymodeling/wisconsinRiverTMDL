dir=("C:/Users/radeca/Documents/WRB.Sufi2.SwatCup")
hrudata = read.table("C:/Users/radeca/Documents/WRB.Sufi2.SwatCup/output.hru",
                     skip = 9, header = F)

hrudata$Area=substr(hrudata$V6, 5, 14)
hrudata$V6=substr(hrudata$V6, 1, 4)
colnames(hrudata)=c("LULC", "HRU", "GIS", "SUB", "MGT", "YEAR", "PRCP", "WYLD", "SYLD", "ORGP", "SEDP", "SOLP", "AREA")
 
eglrvrsub=subset(hrudata, SUB==116 | SUB==117 | SUB==118 | SUB==119 | SUB==120
                 | SUB==121 | SUB==122 | SUB==123 | SUB==136 | SUB==209 
                 | SUB==223 | SUB==224 | SUB==225 | SUB==286 | SUB==315 
                 | SUB==316 | SUB==317 | SUB==320, 
                  select = c(LULC, YEAR, HRU, SUB, WYLD, SYLD, ORGP, SEDP, SOLP))

eglrvr=subset(eglrvrsub, YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013,
                  select = c(LULC, YEAR, SUB, WYLD, SYLD, ORGP, SEDP, SOLP))

eglrvr$LULC=sub("ALFA", "AGRL", eglrvr$LULC)
eglrvr$LULC=sub("CSIL", "AGRL", eglrvr$LULC)


WYLD_mean=aggregate(WYLD~LULC, eglrvr, FUN=mean, na.rm=T )
SYLD_mean=aggregate(SYLD~LULC, eglrvr, FUN=mean, na.rm=T )
ORGPYLD_mean=aggregate(ORGP~LULC, eglrvr, FUN=mean, na.rm=T )
SEDPYLD_mean=aggregate(SEDP~LULC, eglrvr, FUN=mean, na.rm=T )
SOLPYLD_mean=aggregate(SOLP~LULC, eglrvr, FUN=mean, na.rm=T )

data=cbind(WYLD_mean ,SYLD_mean$SYLD, ORGPYLD_mean$ORGP, SEDPYLD_mean$SEDP, SOLPYLD_mean$SOLP)
colnames(data) = c("LULC_SWAT", "WaterYield mm", "Sediment Yield t_ha", "Organic P kg_ha", "Sediment P kg_ha ", "Solution P kg_ha")

LULC_NLCD=data$LULC_SWAT
LULC_NLCD=sub("AGRL", "cultivated_crop", LULC_NLCD)
LULC_NLCD=sub("FESC", "grassland_herbaceous", LULC_NLCD)
LULC_NLCD=sub("HAY", "pasture_hay", LULC_NLCD)
LULC_NLCD=sub("BERM", "developed", LULC_NLCD)
LULC_NLCD=sub("FRSE", "evergreen_forest", LULC_NLCD)
LULC_NLCD=sub("FRST", "mixed_forest", LULC_NLCD)
LULC_NLCD=sub("WETN", "wetland", LULC_NLCD)
LULC_NLCD=sub("WATR", "open_water", LULC_NLCD)
LULC_NLCD=sub("FRSD", "deciduous_forest", LULC_NLCD)

data= cbind(LULC_NLCD, data)

write.table(data, file = "C:/Users/radeca/Documents/eagleriver.txt", col.names = T, sep="\t", row.names=F)