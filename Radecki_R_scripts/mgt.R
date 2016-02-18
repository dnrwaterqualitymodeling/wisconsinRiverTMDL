data= read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/dairy.txt", skip=1, header=F)
hru_data= read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/hru4.txt", header=T)
projectDir=("C:/Users/radeca/Desktop/USLE_P/WRB.Sufi2.SwatCup")
library("stringr", lib.loc="~/R/R-3.2.0/library")
lu_lookup = read.table("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_27012015.csv", sep=",", header=T)
files_mgt = list.files("C:/Users/radeca/Desktop/USLE_P/WRB.Sufi2.SwatCup", "*.mgt")

keep=data.frame()

for (fl in files_mgt){
  lne1 = readLines(paste(projectDir, fl, sep="/" ), 1 )
  hru = substr(lne1, 26, 30)
  lulc = substr(lne1, 55, 58)
  hru_d = hru_data$GIS[hru_data$HRU %in% hru]
  lu_d = lu_lookup$TYPE[lu_lookup$LANDUSE %in% lulc]
  
}



colnames(data)=c("d300a_s", "d300a_h", "d300b_s", "d300b_h", "d300c_s", "d300c_h",
                 "d301a_s", "d301a_h", "d301b_s", "d301b_h", "d301c_s", "d301c_h",
                 "d302a_s", "d302a_h", "d302b_s", "d302b_h", "d302c_s", "d302c_h",
                 "d303a_s", "d303a_h", "d303b_s", "d303b_h", "d303c_s", "d303c_h",
                 "d304a_s", "d304a_h", "d304b_s", "d304b_h", "d304c_s", "d304c_h",
                 "d305a_s", "d305a_h", "d305b_s", "d305b_h", "d305c_s", "d305c_h",
                 "d306a_s", "d306a_h", "d306b_s", "d306b_h", "d306c_s", "d306c_h",
                 "d307a_s", "d307a_h", "d307b_s", "d307b_h", "d307c_s", "d307c_h",
                 "d308a_s", "d308a_h", "d308b_s", "d308b_h", "d308c_s", "d308c_h",
                 "d309a_s", "d309a_h", "d309b_s", "d309b_h", "d309c_s", "d309c_h",
                 "d310a_s", "d310a_h", "d310b_s", "d310b_h", "d310c_s", "d310c_h")

d300a = unique(data$d300a_s*10000 + data$d300a_h)
d300b = unique(data$d300b_s*10000 + data$d300b_h)
d300c = unique(data$d300c_s*10000 + data$d300c_h)
d301a = unique(data$d301a_s*10000 + data$d301a_h)
d301b = unique(data$d301b_s*10000 + data$d301b_h)
d301c = unique(data$d301c_s*10000 + data$d301c_h)
d302a = unique(data$d302a_s*10000 + data$d302a_h)
d302b = unique(data$d302b_s*10000 + data$d302b_h)
d302c = unique(data$d302c_s*10000 + data$d302c_h)
d303a = unique(data$d303a_s*10000 + data$d303a_h)
d303b = unique(data$d303b_s*10000 + data$d303b_h)
d303c = unique(data$d303c_s*10000 + data$d303c_h)
d304a = unique(data$d304a_s*10000 + data$d304a_h)
d304b = unique(data$d304b_s*10000 + data$d304b_h)
d304c = unique(data$d304c_s*10000 + data$d304c_h)
d305a = unique(data$d305a_s*10000 + data$d305a_h)
d305b = unique(data$d305b_s*10000 + data$d305b_h)
d305c = unique(data$d305c_s*10000 + data$d305c_h)
d306a = unique(data$d306a_s*10000 + data$d306a_h)
d306b = unique(data$d306b_s*10000 + data$d306b_h)
d306c = unique(data$d306c_s*10000 + data$d306c_h)
d307a = unique(data$d307a_s*10000 + data$d307a_h)
d307b = unique(data$d307b_s*10000 + data$d307b_h)
d307c = unique(data$d307c_s*10000 + data$d307c_h)
d308a = unique(data$d308a_s*10000 + data$d308a_h)
d308b = unique(data$d308b_s*10000 + data$d308b_h)
d308c = unique(data$d308c_s*10000 + data$d308c_h)
d309a = unique(data$d309a_s*10000 + data$d309a_h)
d309b = unique(data$d309b_s*10000 + data$d309b_h)
d309c = unique(data$d309c_s*10000 + data$d309c_h)
d310a = unique(data$d310a_s*10000 + data$d310a_h)
d310b = unique(data$d310b_s*10000 + data$d310b_h)
d310c = unique(data$d310c_s*10000 + data$d310c_h)

dlist = list(d300a, d300b, d300c, d301a, d301b, d301c,
             d302a, d302b, d302c, d303a, d303b, d303c,
             d304a, d304b, d304c, d305a, d305b, d305c, 
             d306a, d306b, d306c, d307a, d307b, d307c,
             d308a, d308b, d308c, d309a, d309b, d309c, 
             d310a, d310b, d310c)

d=c(d300a, d300b, d300c, d301a, d301b, d301c,
    d302a, d302b, d302c, d303a, d303b, d303c,
    d304a, d304b, d304c, d305a, d305b, d305c, 
    d306a, d306b, d306c, d307a, d307b, d307c,
    d308a, d308b, d308c, d309a, d309b, d309c, 
    d310a, d310b, d310c)

d = d[!is.na(d)]
drot_gis = hru_data$GIS[hru_data$GIS %in% d]
drot_gis = str_pad(drot_gis, 9, pad ="0", side="left")
drot_gis = paste(drot_gis,".mgt", sep="")
drot_hru = hru_data$HRU[hru_data$GIS %in% d]
drot_lulc = hru_data$LULC[hru_data$GIS %in% d]
drot_pappkg.ha = hru_data$P_APPkg.ha[hru_data$GIS %in% d]
drot_yld = hru_data$YLDt.ha[hru_data$GIS %in% d]
drot_sub = hru_data$SUB[hru_data$GIS %in% d]
drot_pupkg.ha = hru_data$PUPkg.ha[hru_data$GIS %in% d]
drot_mton.ha = drot_pupkg.ha/9
drot_kg.ha = drot_mton.ha*1000

drot = data.frame(drot_gis, drot_hru, drot_sub, drot_lulc, drot_pappkg.ha, 
                  drot_yld, drot_pupkg.ha, drot_mton.ha, drot_kg.ha)

colnames(drot) = c("gis", "hru", "sub", "lulc", "p_appkg.ha", "yld", "pupkg.ha", 
                   "mton.ha", "kg.ha")




d_pup=aggregate(drot$pupkg.ha~drot$lulc+drot@sub, FUN=mean)

files_mgt = list.files("C:/Users/radeca/Desktop/USLE_P/WRB.Sufi2.SwatCup", "*.mgt")

for (fl in aa){
  lnes = readLines(paste(projectDir, fl, sep="/" )  )
  
}

