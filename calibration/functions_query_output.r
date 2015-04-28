hru_col_names=c("LULC","HRU","GIS","SUB","MGT","MON","AREA","PRECIP","SNOWFALL","SNOWMELT",
	"IRR","PET","ET","SW_INIT","SW_END","PERC","GW_RCHG","DA_RCHG","REVAP","SA_IRR",
	"DA_IRR","SA_ST","DA_ST","SURQ_GEN","SURQ_CNT","TLOSS","LATQ","GW_Q","WYLD","DAILYCN",
	"TMP_AV","TMP_MX","TMP_MN","SOL_TMP","SOLAR","SYLD","USLE","N_APP","P_APP","NAUTO",
	"PAUTO","NGRZ","PGRZ","CFERTN","CFERTP","NRAIN","NFIX","F_MN","A_MN","A_SN",
	"F_MP","AO_LP","L_AP","A_SP","DNIT","NUP","PUP","ORGN","ORGP","SEDP",
	"NSURQ","NLATQ","NO3L","NO3GW","SOLP","P_GW","W_STRS","TMP_STRS","N_STRS","P_STRS",
	"BIOM","LAI","YLD","BACTP","BACTLP","WTAB","WTABELO","SNO_HRU","CMUP_KGH","CMTOT_KGH",
	"QTILE","TNO3","LNO3","GW_Q_D","LATQ_CNT")

hru_widths = c(4,5,10,5,5,5,rep(10,67),11,11,rep(10,10))

query_output.hru = function(x, col_name, widths=hru_widths, col_names=hru_col_names) {
    cum_col = c(0, cumsum(widths))
    col_index = which(col_names == col_name)
    start = cum_col[col_index] + 1
    end = start + widths[col_index] - 1
	print(col_name)
    # print(paste(start, end))
    return(substr(x, start, end))
}

rch_col_names=c("REACH","RCH","GIS","MON","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS","SED_IN","SED_OUT",
    "SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN","ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN",
    "NO2_OUT","MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT","DISOX_IN","DISOX_OUT","SOLPST_IN",
    "SOLPST_OUT","SORPST_IN","SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST","DIFFUSEPST","REACBEDPST","BURYPST",
    "BED_PST","BACTP_OUT","BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P","NO3CONC")

rch_widths = c(5,5,9,6,rep(12,47),6)

query_output.rch = function(x, col_name, widths=rch_widths, col_names=rch_col_names) {
    cum_col = c(0, cumsum(widths))
    col_index = which(col_names == col_name)
    start = cum_col[col_index] + 1
    end = start + widths[col_index] - 1
    print(paste(start, end))
    return(substr(x, start, end))
}

### for output.wtr
wtr_col_names=c("LULC","HRU","GIS","SUB","MGT","MON","AREA","PND_PCP","PND_IN","PNDSED_IN",
	"PND_EVAP","PND_SEEP","PND_OUT","PNDSED_OUT","PND_VOL","PND_ORGN","PND_NO3","PND_ORGP","PND_MINP","PND_CHLA",
	"PND_SECI","WET_PCP","WET_SEDIN","WET_EVAP","WET_SEEP","SET_OUT","WET_SEDOUT","WET_VOL","WET_ORGN","WET_NO3",
	"WET_ORGP","WET_MINP","WET_CHLA","WET_SECI","POT_PCP","OSED_I","POTEVP","POTSEP","POT_OUT","OSED_O",
	"POTVOL","POT_SA","HRU_SURQ","PLANT_ET","SOIL_ET")

wtr_widths = c(4,5,10,5,5,5,rep(10,39))

query_output.wtr = function(x, col_name, widths=wtr_widths, col_names=wtr_col_names) {
    cum_col = c(0, cumsum(widths))
    col_index = which(col_names == col_name)
    start = cum_col[col_index] + 1
    end = start + widths[col_index] - 1
	print(col_name)
    # print(paste(start, end))
    return(substr(x, start, end))
}