options(stringsAsFactors=F)

landuse = read.table("C:/Users/radeca/Desktop/Excelwork/landuse.txt", header=T)
lu_lookup = read.table("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_Update.csv", sep=",", header=T)
lulc_lookup = read.table("C:/Users/radeca/Desktop/Excelwork/rotationxhru.txt", header=T)
year = read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/YEAR.txt")
colnames(year) = "YEAR"
area = read.table("C:/Users/radeca/Desktop/Excelwork/areas.txt", header=T)


lu_lookup$TYPE = sub("Water", "Non-Ag", lu_lookup$TYPE)
lu_lookup$TYPE = sub("Forest", "Non-Ag", lu_lookup$TYPE)
lu_lookup$TYPE = sub("Grassland", "Non-Ag", lu_lookup$TYPE)
lu_lookup$TYPE = sub("Wetland", "Non-Ag", lu_lookup$TYPE)
lu_lookup$TYPE = sub("Developed", "Non-Ag", lu_lookup$TYPE)
lu_lookup$TYPE = sub("Pasture/Hay", "Non-Ag", lu_lookup$TYPE)
lu_lookup$TYPE = sub("Cranberries", "Non-Ag", lu_lookup$TYPE)

hru_data = read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/output.hru", skip=9, header=F, as.is=T)
hru_data = hru_data[-6]
hrunames ="LULC  HRU GIS SUB MGT MON AREAkm2 BIOMt_ha LAI YLDt_ha TMP_AVdgC WYLDmm 
  SYLDt_ha PAPPkg_ha  PUPkg_ha ORGPkg_ha SEDPkg_ha SOLPkg_ha PGWkg_ha  SOL_TMPdgC"
hrunames = strsplit(hrunames, "\\s+|:")
colnames(hru_data) = hrunames[[1]][c(1,2,3,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20)]
hru_data=cbind(year, landuse, area, hru_data)




dairy_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Dairy"]
dairy_hru = hru_data$LANDUSE %in% dairy_lu
hru_data$LANDUSE[dairy_hru] = "Dairy"

cg_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Cash Grain"]
cg_hru = hru_data$LANDUSE %in% cg_lu
hru_data$LANDUSE[cg_hru] = "Cash Grain"

pv_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Potato/Vegetable"]
pv_hru = hru_data$LANDUSE %in% pv_lu
hru_data$LANDUSE[pv_hru] = "Potato/Vegetable"

non_ag_lu = lu_lookup$LANDUSE[lu_lookup$TYPE==c("Non-Ag")]
non_ag_hru = hru_data$LANDUSE %in% non_ag_lu
hru_data$LANDUSE[non_ag_hru] = "Non-Ag"


hru_data2 = read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/outputsol_sol5_noP.hru", skip=9, header=F, as.is=T)
hru_data2 = hru_data2[-6]
hrunames ="LULC  HRU GIS SUB MGT MON AREAkm2 BIOMt_ha LAI YLDt_ha TMP_AVdgC WYLDmm 
  SYLDt_ha PAPPkg_ha  PUPkg_ha ORGPkg_ha SEDPkg_ha SOLPkg_ha PGWkg_ha  SOL_TMPdgC"
hrunames = strsplit(hrunames, "\\s+|:")
colnames(hru_data2) = hrunames[[1]][c(1,2,3,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20)]
hru_data2=cbind(year, landuse, hru_data2)

dairy_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Dairy"]
dairy_hru = hru_data2$LANDUSE %in% dairy_lu
hru_data2$LANDUSE[dairy_hru] = "Dairy"

cg_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Cash Grain"]
cg_hru = hru_data2$LANDUSE %in% cg_lu
hru_data2$LANDUSE[cg_hru] = "Cash Grain"

pv_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Potato/Vegetable"]
pv_hru = hru_data2$LANDUSE %in% pv_lu
hru_data2$LANDUSE[pv_hru] = "Potato/Vegetable"

non_ag_lu = lu_lookup$LANDUSE[lu_lookup$TYPE=="Non-Ag"]
non_ag_hru = hru_data2$LANDUSE %in% non_ag_lu
hru_data2$LANDUSE[non_ag_hru] = "Non-Ag"


solp1=aggregate(SOLPkg_ha~LANDUSE, hru_data, FUN=mean)
solp2=aggregate(SOLPkg_ha~LANDUSE, hru_data2, FUN=mean)
diff_solP=(solp1$SOLPkg_ha-solp2$SOLPkg_ha)

solp_diff=cbind(solp1$LANDUSE, solp1$SOLPkg_ha, solp2$SOLPkg_ha, diff_solP)
colnames(solp_diff) = c("Landuse","SolPwP", "SolPwoP", "SolubleP_reduct kg/ha")

sedp1=aggregate(SEDPkg_ha~LANDUSE, hru_data, FUN=mean)
sedp2=aggregate(SEDPkg_ha~LANDUSE, hru_data2, FUN=mean)
diff_sedP=(sedp1$SEDPkg_ha-sedp2$SEDPkg_ha)

sedp_diff=cbind(sedp1$LANDUSE, sedp1$SEDPkg_ha, sedp2$SEDPkg_ha, diff_sedP)
colnames(sedp_diff) = c("Landuse","SedPwP", "SedPwoP", "SEDP_reduct kg/ha")

orgp1=aggregate(ORGPkg_ha~LANDUSE, hru_data, FUN=mean)
orgp2=aggregate(ORGPkg_ha~LANDUSE, hru_data2, FUN=mean)
diff_orgP=(orgp1$ORGPkg_ha-orgp2$ORGPkg_ha)

orgp_diff=cbind(orgp1$LANDUSE, orgp1$ORGPkg_ha, orgp2$ORGPkg_ha, diff_orgP)
colnames(orgp_diff) = c("Landuse","ORGPwP", "ORGPwoP", "ORGP_reduct kg/ha")

hru_data$TOTPkg_ha=hru_data$SEDPkg_ha+hru_data$ORGPkg_ha+hru_data$SOLPkg_ha
hru_data2$TOTPkg_ha=hru_data2$SEDPkg_ha+hru_data2$ORGPkg_ha+hru_data2$SOLPkg_ha

totp1=aggregate(TOTPkg_ha~LANDUSE, hru_data, FUN=mean)
totp2=aggregate(TOTPkg_ha~LANDUSE, hru_data2, FUN=mean)
diff_totP=(totp1$TOTPkg_ha-totp2$TOTPkg_ha)

totp_diff=cbind(totp1$LANDUSE, totp1$TOTPkg_ha, totp2$TOTPkg_ha, diff_totP)
colnames(totp_diff) = c("Landuse","TOTPwP", "TOTPwoP", "TOTP_reduct kg/ha")
