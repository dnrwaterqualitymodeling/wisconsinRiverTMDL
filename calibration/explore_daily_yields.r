output.hru = "H:/WRB/Scenarios/TwoCut_daily_2013/TxtInOut/output.hru"
widths = c(4,5,10,5,5,5,rep(10,67),11,11,rep(10,10))
dat = readLines(output.hru)
dat = dat[10:length(dat)]
# dat = read.fwf(output.hru, widths=widths, skip=9)

col_names=c("LULC","HRU","GIS","SUB","MGT","MON","AREA","PRECIP","SNOWFALL","SNOWMELT",
        "IRR","PET","ET","SW_INIT","SW_END","PERC","GW_RCHG","DA_RCHG","REVAP","SA_IRR",
        "DA_IRR","SA_ST","DA_ST","SURQ_GEN","SURQ_CNT","TLOSS","LATQ","GW_Q","WYLD","DAILYCN",
        "TMP_AV","TMP_MX","TMP_MN","SOL_TMP","SOLAR","SYLD","USLE","N_APP","P_APP","NAUTO",
        "PAUTO","NGRZ","PGRZ","CFERTN","CFERTP","NRAIN","NFIX","F_MN","A_MN","A_SN",
        "F_MP","AO_LP","L_AP","A_SP","DNIT","NUP","PUP","ORGN","ORGP","SEDP",
        "NSURQ","NLATQ","NO3L","NO3GW","SOLP","P_GW","W_STRS","TMP_STRS","N_STRS","P_STRS",
        "BIOM","LAI","YLD","BACTP","BACTLP","WTAB","WTABELO","SNO_HRU","CMUP_KGH","CMTOT_KGH",
        "QTILE","TNO3","LNO3","GW_Q_D","LATQ_CNT")

# names = c("LULC","HRU","GIS","SUB","MGT","MON","AREAkm2","PRECIPmm","SNOFALLmm","SNOMELTmm","IRRmm","PETmm","ETmm","SW_INITmm","SW_ENDmm","PERCmm","GW_RCHGmm","DA_RCHGmm","REVAPmm","SA_IRRmm","DA_IRRmm","SA_STmm","DA_STmm","SURQ_GENmm","SURQ_CNTmm","TLOSSmm","LATQGENmm","GW_Qmm","WYLDmm","DAILYCN","TMP_AVdgC","TMP_MXdgC","TMP_MNdgC","SOL_TMPdgC","SOLARMJ/m2","SYLDt/ha","USLEt/ha","N_APPkg/ha","P_APPkg/ha","NAUTOkg/ha","PAUTOkg/ha","NGRZkg/ha","PGRZkg/ha","NCFRTkg/ha","PCFRTkg/ha","NRAINkg/ha","NFIXkg/ha","F-MNkg/ha","A-MNkg/ha","A-SNkg/ha","F-MPkg/ha","AO-LPkg/ha","L-APkg/ha","A-SPkg/ha","DNITkg/ha","NUPkg/ha","PUPkg/ha","ORGNkg/ha","ORGPkg/ha","SEDPkg/ha","NSURQkg/ha","NLATQkg/ha","NO3Lkg/ha","NO3GWkg/ha","SOLPkg/ha","P_GWkg/ha","W_STRS","TMP_STRS","N_STRS","P_STRS","BIOMt/ha","LAI","YLDt/ha","BACTPct","BACTLPct","WTAB","CLIm","WTAB","SOLm","SNOmm","CMUPkg/ha","CMTOTkg/ha","QTILEmm","TNO3kg/ha","LNO3kg/ha","GW_Q_Dmm","LATQCNTmm")

query_output.hru = function(x, widths, col_name, col_names) {
    cum_col = c(0, cumsum(widths))
    col_index = which(col_names == col_name)
    start = cum_col[col_index] + 1
    end = start + widths[col_index] - 1
    print(paste(start, end))
    return(substr(x, start, end))
}

select_cols = c("LULC", "HRU", "MON", "YLD", "BIOM")
select_dat = matrix(NA, nrow=length(dat), ncol=length(select_cols))
select_dat = as.data.frame(select_dat)
names(select_dat) = select_cols
for (col_name in select_cols) {
    vals = query_output.hru(dat, widths, col_name, col_names)
    select_dat[col_name] = data.frame(vals, stringsAsFactors=F)
}

alfa_dat = select_dat[as.integer(select_dat$HRU) == 2,]
plot(as.integer(alfa_dat$MON), as.numeric(alfa_dat$BIOM), type="l")
