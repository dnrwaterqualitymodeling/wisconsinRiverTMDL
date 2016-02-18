hru_data=read.table("C:/Users/radeca/Desktop/draft_revisions.Sufi2.SwatCup/output.hru", skip=9, header=F)
colnames(hru_data) = c("LULC","HRU","GIS","SUB","MGT","AREA","PRECIP","SNOWFALL","SNOWMELT",
            "IRR","PET","ET","SW_INIT","SW_END","PERC","GW_RCHG","DA_RCHG","REVAP","SA_IRR",
            "DA_IRR","SA_ST","DA_ST","SURQ_GEN","SURQ_CNT","TLOSS","LATQ","GW_Q","WYLD","DAILYCN",
            "TMP_AV","TMP_MX","TMP_MN","SOL_TMP","SOLAR","SYLD","USLE","N_APP","P_APP","NAUTO",
            "PAUTO","NGRZ","PGRZ","CFERTN","CFERTP","NRAIN","NFIX","F_MN","A_MN","A_SN",
            "F_MP","AO_LP","L_AP","A_SP","DNIT","NUP","PUP","ORGN","ORGP","SEDP",
            "NSURQ","NLATQ","NO3L","NO3GW","SOLP","P_GW","W_STRS","TMP_STRS","N_STRS","P_STRS",
            "BIOMt_ha","LAI","YLDt_ha","BACTP","BACTLP","WTAB","WTABELO","SNO_HRU","CMUP_KGH","CMTOT_KGH",
            "QTILE","TNO3","LNO3","GW_Q_D","LATQ_CNT")

year = read.table("C:/Users/radeca/Desktop/Excelwork/YEAR.txt")
colnames(year) = "YEAR"
WI_NASS_YLDS = read.table('C:/Users/radeca/Desktop/Excelwork/WI_annual_ylds.txt', header=T)
nass_ylds = aggregate(yld~LULC, WI_NASS_YLDS, FUN=mean )

hru_data=cbind(year, hru_data)
hru_data = subset(hru_data, YEAR != 12)

annual_ylds = aggregate(`YLDt_ha`~LULC+YEAR, hru_data, FUN=mean)

avg_ylds = aggregate(`YLDt_ha`~LULC, hru_data, FUN=mean)
avg_ylds = avg_ylds[c(1, 3, 4, 9, 11, 12, 13),]

alfa = avg_ylds[1,2] * 0.446089561
corn = avg_ylds[2,2] * 15.93 / 0.845
csil = avg_ylds[3,2] * 0.446089561 / 0.35
grbn = avg_ylds[4,2] * 0.446089561 / 0.1
pota = avg_ylds[5,2] * 8.92 / 0.20
scrn = avg_ylds[6,2] * 0.446089561 / 0.25
soyb = avg_ylds[7,2] * 14.87 / 0.87

mc = c(0, 15.5, 65, 90, 80, 75, 13 )
ylds_us = rbind(alfa, corn, csil, grbn, pota, scrn, soyb)
units=list("t/ac", "bu/ac", "t/ac", "t/ac", "cwt/ac",  "t/ac", "bu/ac")
ylds_us = cbind(mc, units, ylds_us)
colnames(ylds_us)=c(" % Moisture", "units", "SWAT_ylds_us")
ylds = cbind(avg_ylds, ylds_us, nass_ylds)

dir_out = "C:/Users/radeca/Documents/rcode/plots"
pdf("biomass.pdf", width=11, height=8.5)

alfa=subset(hru_data, LULC=="ALFA", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(alfa$`BIOMt_ha`,50)
hist(alfa$`YLDt_ha`,50)
plot(alfa$`BIOMt_ha`, alfa$`YLDt_ha`)
print(range(alfa$`BIOMt_ha`))
print(range(alfa$`YLDt_ha`))
print(mean(alfa$`BIOMt_ha`))
print(mean(alfa$`YLDt_ha`))

corn=subset(hru_data, LULC=="CORN", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(corn$`BIOMt_ha`,50)
hist(corn$`YLDt_ha`,50)
plot(corn$`BIOMt_ha`, corn$`YLDt_ha`)
print(range(corn$`BIOMt_ha`))
print(range(corn$`YLDt_ha`))
print(mean(corn$`BIOMt_ha`))
print(mean(corn$`YLDt_ha`))

csil=subset(hru_data, LULC=="CSIL", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(csil$`BIOMt_ha`,50)
hist(csil$`YLDt_ha`,50)
plot(csil$`BIOMt_ha`, csil$`YLDt_ha`)
print(range(csil$`BIOMt_ha`))
print(range(csil$`YLDt_ha`))
print(mean(csil$`BIOMt_ha`))
print(mean(csil$`YLDt_ha`))

soy=subset(hru_data, LULC=="SOYB", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(soy$`BIOMt_ha`,50)
hist(soy$`YLDt_ha`,50)
plot(soy$`BIOMt_ha`, soy$`YLDt_ha`)
print(range(soy$`BIOMt_ha`))
print(range(soy$`YLDt_ha`))
print(mean(soy$`BIOMt_ha`))
print(mean(soy$`YLDt_ha`))

pota=subset(hru_data, LULC=="POTA", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(pota$`BIOMt_ha`,50)
hist(pota$`YLDt_ha`,50)
plot(pota$`BIOMt_ha`, pota$`YLDt_ha`)
print(range(pota$`BIOMt_ha`))
print(range(pota$`YLDt_ha`))
print(mean(pota$`BIOMt_ha`))
print(mean(pota$`YLDt_ha`))

grbn=subset(hru_data, LULC=="GRBN", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(grbn$`BIOMt_ha`,50)
hist(grbn$`YLDt_ha`,50)
plot(grbn$`BIOMt_ha`, grbn$`YLDt_ha`)
print(range(grbn$`BIOMt_ha`))
print(range(grbn$`YLDt_ha`))
print(mean(grbn$`BIOMt_ha`))
print(mean(grbn$`YLDt_ha`))

scrn=subset(hru_data, LULC=="SCRN", select = c(LULC, SUB, HRU, `YLDt_ha`, `BIOMt_ha`))
hist(scrn$`BIOMt_ha`,50)
hist(scrn$`YLDt_ha`,50)
plot(scrn$`BIOMt_ha`, scrn$`YLDt_ha`)
print(range(scrn$`BIOMt_ha`))
print(range(scrn$`YLDt_ha`))
print(mean(scrn$`BIOMt_ha`))
print(mean(scrn$`YLDt_ha`))

dev.off()