library(RODBC)
projectDir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement"
setwd(projectDir)
# autofertparamfile = "autofertparams.csv"
# 
# autofertparam = read.csv(autofertparamfile)

inDb = paste("OpSchedules_fert.mdb")
con = odbcConnectAccess(inDb)

opsData = sqlQuery(con, "SELECT * FROM OpSchedules")

# Deletes the previous auto fertilization operation schedules(MGT_OP=11), 
#   and leaves the default auto fert schedules (identified by HUSC< 0.001)
delete_query <- paste("DELETE * FROM OpSchedules WHERE MGT_OP=11 AND HUSC < 0.001")
sqlQuery(con, delete_query)
#sid <- paste(unique(opsData$SID)[35])
for (sid in unique(opsData$SID)){
    
    if (!(sid %in% c('BARR','FRSD', 'WATR', 'URML', 'RNGB',
                     'RNGE','WETF', 'WETN','CRRT', 'AGRR', 
                     'FRSE', 'FRST', 'HAY', 'PAST', 'SWRN',
                     'UCOM', 'UIDU', 'UINS', 'URBN', 'URHD',
                     'URLD', 'URMD', 'UTRN', 'WETL'))){
    for (year in 1:6) {
        auto_fert_temp = sqlFetch(con, "autofert")[1,]
        auto_fert_temp$YEAR = year
        auto_fert_temp$SID = sid
        formatTempFile = tempfile()
        write.csv(auto_fert_temp, formatTempFile, row.names=F, quote=T)
        colNames = readLines(formatTempFile, 1)
        colNames = gsub("\"", "", colNames)
        values = readLines(formatTempFile, -1)[2]
        values = gsub("\"", "'", values)
        values = gsub("NA", "NULL", values)
        fert_query = paste(
            "INSERT INTO OpSchedules (",
            colNames,
            ") VALUES (",
            values,
            ");",
            sep=""
        )

            sqlQuery(con, fert_query)
        }
    }
}
close(con)
    

#             fert_query = paste('INSERT INTO OpSchedules (SID, SUBBASIN, HRU,LANDUSE,SOIL,SLOPE_CD,CROP,YEAR,MONTH,DAY,HUSC,MGT_OP,HEATUNITS,PLANT_ID,CURYR_MAT,LAI_INIT,BIO_INIT,HI_TARG,BIO_TARG,CNOP,IRR_AMT,FERT_ID,FRT_KG,FRT_SURFACE,PEST_ID,PST_KG,TILLAGE_ID,HARVEFF,HI_OVR,GRZ_DAYS,MANURE_ID,BIO_EAT,BIO_TRMP,MANURE_KG,WSTRS_ID,AUTO_WSTRS,AFERT_ID,AUTO_NSTRS,AUTO_NAPP,AUTO_NYR,AUTO_EFF,AFRT_SURFACE,SWEEPEFF,FR_CURB,IMP_TRIG,FERT_DAYS,CFRT_ID,IFRT_FREQ,CFRT_KG,PST_DEP,IHV_GBM,IRR_SALT,IRR_EFM,IRR_SQ,IRR_EFF,IRR_MX,IRR_ASQ,CPST_ID,PEST_DAYS,IPEST_FREQ,CPST_KG,BURN_FRLB,OP_NUM,IRR_SC,IRR_NO,IRR_SCA,IRR_NOA) VALUES ("',sid,' ",0,0,"0","0","0","0",1,1,1,0,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,4,0.85,50,250,1.3,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0);', sep="")
    
    