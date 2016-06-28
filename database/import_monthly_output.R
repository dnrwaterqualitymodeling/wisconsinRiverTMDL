library(dplyr)

dir_output = "C:/Users/ruesca/Documents/WRB.Sufi2.SwatCup/output_monthly"
file_db = "C:/Users/ruesca/Documents/WRB.Sufi2.SwatCup/wrb_swat_spatial_db.sqlite"
setwd(dir_output)

############# output.hru

colnames=c("LULC","HRU","GIS","SUB","MGT","AREA","PRECIP","SNOFALL",
	"SNOMELT","IRR","PET","ET","SW_INIT","SW_END","PERC","GW_RCHG",
	"DA_RCHG","REVAP","SA_IRR","DA_IRR","SA_ST","DA_ST","SURQ_GEN",
	"SURQ_CNT","TLOSS","LATQ","GW_Q","WYLD","DAILYCN","TMP_AV","TMP_MX",
	"TMP_MN","SOL_TMP","SOLAR","SYLD","USLE","N_APP","P_APP","NAUTO",
	"PAUTO","NGRZ","PGRZ","CFERTN","CFERTP","NRAIN","NFIX","F_MN",
	"A_MN","A_SN","F_MP","AO_LP","L_AP","A_SP","DNIT","NUP","PUP",
	"ORGN","ORGP","SEDP","NSURQ","NLATQ","NO3L","NO3GW","SOLP","P_GW",
	"W_STRS","TMP_STRS","N_STRS","P_STRS","BIOM","LAI","YLD","BACTP",
	"BACTLP","WTAB","WTABELO","SNO_HRU","CMUP","CMTOT","QTILE",
	"TNO3","LNO3","GW_Q_D","LATQ_CNT")
colclasses = c("character","integer","character","integer","integer","character",
	rep("numeric",78)
)
d = read.table(
	"output.hru",
	skip=9,
	colClasses = colclasses
)
names(d) = colnames
d = d %>%
	filter(!grepl('(^20)|(^12\\.0)', AREA))		
yrs = NULL
for (y in 2002:2013) {
	yrs = c(yrs, rep(y, dim(d)[1] / 12))
}
spl = strsplit(d$AREA, "\\.")
d = d %>%
	mutate(
		AREA=as.numeric(
			sapply(spl, function(x) {paste("0.", x[2], sep="")})),
		MON=as.integer(
			sapply(spl, function(x) {x[1]})),
		YR=yrs
	)
d = d[
	c("LULC","HRU","GIS","SUB","MGT","MON","YR","AREA","PRECIP","SNOFALL",
		"SNOMELT","IRR","PET","ET","SW_INIT","SW_END","PERC","GW_RCHG",
		"DA_RCHG","REVAP","SA_IRR","DA_IRR","SA_ST","DA_ST","SURQ_GEN",
		"SURQ_CNT","TLOSS","LATQ","GW_Q","WYLD","DAILYCN","TMP_AV","TMP_MX",
		"TMP_MN","SOL_TMP","SOLAR","SYLD","USLE","N_APP","P_APP","NAUTO",
		"PAUTO","NGRZ","PGRZ","CFERTN","CFERTP","NRAIN","NFIX","F_MN",
		"A_MN","A_SN","F_MP","AO_LP","L_AP","A_SP","DNIT","NUP","PUP",
		"ORGN","ORGP","SEDP","NSURQ","NLATQ","NO3L","NO3GW","SOLP","P_GW",
		"W_STRS","TMP_STRS","N_STRS","P_STRS","BIOM","LAI","YLD","BACTP",
		"BACTLP","WTAB","WTABELO","SNO_HRU","CMUP","CMTOT","QTILE",
		"TNO3","LNO3","GW_Q_D","LATQ_CNT")
]
con = src_sqlite(file_db)
stdout = copy_to(con, d, "output_hru_monthly", temporary=F)
	
############# output.sub

colnames=c("BIGSUB","SUB","GIS","AREA","PRECIP","SNOMELT","PET","ET","SW",
	"PERC","SURQ","GW_Q","WYLD","SYLD","ORGN","ORGP","NSURQ","SOLP",
	"SEDP","LATQ","LATNO3","GWNO3","CHOLA","CBODU","DOXQ","TNO3")
colclasses=c("character","integer","character","character", rep("numeric",22))
d = read.table(
	"output.sub",
	skip=9,
	colClasses=colclasses
)
names(d) = colnames
d = d[,-1]
d = d %>%
	filter(!grepl('(^20)|(^12\\.0)', AREA))		
yrs = NULL
for (y in 2002:2013) {
	yrs = c(yrs, rep(y, dim(d)[1] / 12))
}
spl = strsplit(d$AREA, "\\.")
d = d %>%
	mutate(
		AREA=as.numeric(
			sapply(spl, function(x) {paste("0.", x[2], sep="")})),
		MON=as.integer(
			sapply(spl, function(x) {x[1]})),
		YR=yrs
	)
d = d[
	c("SUB","GIS","MON","YR","AREA","PRECIP","SNOMELT","PET","ET","SW",
		"PERC","SURQ","GW_Q","WYLD","SYLD","ORGN","ORGP","NSURQ","SOLP",
		"SEDP","LATQ","LATNO3","GWNO3","CHOLA","CBODU","DOXQ","TNO3")
]
con = src_sqlite(file_db)
stdout = copy_to(con, d, "output_sub_monthly", temporary=F)

############   output.rch

colnames=c("REACH","RCH","GIS","MON","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS",
	"SED_IN","SED_OUT","SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN",
	"ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN","NO2_OUT",
	"MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT",
	"DISOX_IN","DISOX_OUT","SOLPST_IN","SOLPST_OUT","SORPST_IN",
	"SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST",
	"DIFFUSEPST","REACBEDPST","BURYPST","BED_PST","BACTP_OUT",
	"BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P",
	"NO3CONC","WTMP")
colclasses=c("character","integer","character","character",rep("numeric", 47))
d = read.table(
	"output.rch",
	skip=9,
	colClasses=colclasses
)
names(d) = colnames
d = d[,-1]
d = d %>%
	filter(!grepl('(^20)|(^12\\.0)', MON)) %>%
	mutate(MON = as.integer(MON))
yrs = NULL
for (y in 2002:2013) {
	yrs = c(yrs, rep(y, dim(d)[1] / 12))
}
d = mutate(d, YR=yrs)
d = d[
	c("RCH","GIS","MON","YR","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS",
		"SED_IN","SED_OUT","SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN",
		"ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN","NO2_OUT",
		"MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT",
		"DISOX_IN","DISOX_OUT","SOLPST_IN","SOLPST_OUT","SORPST_IN",
		"SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST",
		"DIFFUSEPST","REACBEDPST","BURYPST","BED_PST","BACTP_OUT",
		"BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P",
		"NO3CONC","WTMP")
]
con = src_sqlite(file_db)
stdout = copy_to(con, d, "output_rch_monthly", temporary=F,
	indexes = list(c("RCH", "MON", "YR")))

rm(con)


############   output.rsv

colnames=c(
	"RES","MON","VOLUME","FLOW_IN","FLOW_OUT","PRECIP","EVAP","SEEPAGE",
	"SED_IN","SED_OUT","RES_SED","ORGN_IN","ORGN_OUT","RES_ORGN","ORGP_IN",
	"ORGP_OUT","RES_ORGP","NO3_IN","NO3_OUT","RES_NO3","NO2_IN","NO2_OUT",
	"RES_NO2","NH3_IN","NH3_OUT","RES_NH3","MINP_IN","MINP_OUT","RES_MINP",
	"CHLA_IN","CHLA_OUT","SECCHIDEPTH","PEST_IN","REACTPST","VOLPST","SETTLPST",
	"RESUSP_PST","DIFFUSEPST","REACBEDPST","BURYPST","PEST_OUT","PSTCNCW",
	"PSTCNCB")
colclasses=c("character","integer","integer",rep("numeric", 41))
d = read.table(
	"output.rsv",
	skip=9,
	colClasses=colclasses
)
d = d[,-1]
names(d) = colnames
d = d %>%
	filter(MON <= 12)
yrs = NULL
for (y in 2002:2013) {
	yrs = c(yrs, rep(y, dim(d)[1] / 12))
}
d = mutate(d, YR=yrs)
d = d[
	c(
	"RES","MON","YR","VOLUME","FLOW_IN","FLOW_OUT","PRECIP","EVAP","SEEPAGE",
	"SED_IN","SED_OUT","RES_SED","ORGN_IN","ORGN_OUT","RES_ORGN","ORGP_IN",
	"ORGP_OUT","RES_ORGP","NO3_IN","NO3_OUT","RES_NO3","NO2_IN","NO2_OUT",
	"RES_NO2","NH3_IN","NH3_OUT","RES_NH3","MINP_IN","MINP_OUT","RES_MINP",
	"CHLA_IN","CHLA_OUT","SECCHIDEPTH","PEST_IN","REACTPST","VOLPST","SETTLPST",
	"RESUSP_PST","DIFFUSEPST","REACBEDPST","BURYPST","PEST_OUT","PSTCNCW",
	"PSTCNCB")
]
con = src_sqlite(file_db)
stdout = copy_to(con, d, "output_rsv_monthly", temporary=F,
	indexes = list(c("RES", "MON", "YR")))

rm(con)