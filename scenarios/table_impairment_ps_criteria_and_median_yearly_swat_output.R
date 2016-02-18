library(foreign)

RCH_data = read.table("C:/Users/radeca/Desktop/BROM_2/BROM_2.Sufi2.SwatCup/output.rch", skip=9, header=F)
colnames(RCH_data)=c("Omit","RCH", "GIS", "MONTH", "DAY", "YEAR", "Area", "Flow_in", "Flow_out", 
                    "SED_in",  "SED_out", "SED_con", "ORGP_in", "ORGP_out", "MINP_in", "MINP_out", "Tot_P")

NPS = read.table("C:/Users/radeca/Documents/R/R_data/NPS_subs.txt", header=T)
reach = read.dbf("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/hydro.dbf")
attach(reach)


for (x in 1:337){
sel_id = x
up_ids = NULL

repeat {
  prior_l = length(sel_id)
  sel_id = c(sel_id, FROM_NODE[TO_NODE %in% sel_id])
  sel_id = unique(sel_id)
  after_l = length(sel_id)
  if (prior_l == after_l) { break }
}

upstream_sub = sort(sel_id) 

upstr_ps = NPS$PointSource[Subbasin %in% upstream_sub]
upstr_ps = sum(upstr_ps)

NPS$PS_UP[x] =  upstr_ps>0
}

TP_data = subset(RCH_data, MONTH==5 | MONTH==6 | MONTH==7 | MONTH==8 | MONTH==9 | MONTH==10)
TP_data$TPconc = (TP_data$Tot_P/TP_data$Flow_out)*0.01157407
median_tp = aggregate(TPconc~RCH, TP_data, FUN=median)
median_tp_yr = aggregate(TPconc~RCH+YEAR, TP_data, FUN=median)
NPS$median_tp = median_tp$TPconc

TP_2002 = subset(median_tp_yr, YEAR==2002, select=c(RCH, TPconc))
TP_2003 = subset(median_tp_yr, YEAR==2003, select=c(RCH, TPconc))
TP_2004 = subset(median_tp_yr, YEAR==2004, select=c(RCH, TPconc))
TP_2005 = subset(median_tp_yr, YEAR==2005, select=c(RCH, TPconc))
TP_2006 = subset(median_tp_yr, YEAR==2006, select=c(RCH, TPconc))
TP_2007 = subset(median_tp_yr, YEAR==2007, select=c(RCH, TPconc))
TP_2008 = subset(median_tp_yr, YEAR==2008, select=c(RCH, TPconc))
TP_2009 = subset(median_tp_yr, YEAR==2009, select=c(RCH, TPconc))
TP_2010 = subset(median_tp_yr, YEAR==2010, select=c(RCH, TPconc))
TP_2011 = subset(median_tp_yr, YEAR==2011, select=c(RCH, TPconc))
TP_2012 = subset(median_tp_yr, YEAR==2012, select=c(RCH, TPconc))
TP_2013 = subset(median_tp_yr, YEAR==2013, select=c(RCH, TPconc))

NPS$TP_2002 = TP_2002$TPconc
NPS$TP_2003 = TP_2003$TPconc
NPS$TP_2004 = TP_2004$TPconc
NPS$TP_2005 = TP_2005$TPconc
NPS$TP_2006 = TP_2006$TPconc
NPS$TP_2007 = TP_2007$TPconc
NPS$TP_2008 = TP_2008$TPconc
NPS$TP_2009 = TP_2009$TPconc
NPS$TP_2010 = TP_2010$TPconc
NPS$TP_2011 = TP_2011$TPconc
NPS$TP_2012 = TP_2012$TPconc
NPS$TP_2013 = TP_2013$TPconc

MC=data.frame(NPS$median_tp <= NPS$Criteria)
MC02=data.frame(NPS$TP_2002 <= NPS$Criteria)
MC03=data.frame(NPS$TP_2003 <= NPS$Criteria)
MC04=data.frame(NPS$TP_2004 <= NPS$Criteria)
MC05=data.frame(NPS$TP_2005 <= NPS$Criteria)
MC06=data.frame(NPS$TP_2006 <= NPS$Criteria)
MC07=data.frame(NPS$TP_2007 <= NPS$Criteria)
MC08=data.frame(NPS$TP_2008 <= NPS$Criteria)
MC09=data.frame(NPS$TP_2009 <= NPS$Criteria)
MC10=data.frame(NPS$TP_2010 <= NPS$Criteria)
MC11=data.frame(NPS$TP_2011 <= NPS$Criteria)
MC12=data.frame(NPS$TP_2012 <= NPS$Criteria)
MC13=data.frame(NPS$TP_2013 <= NPS$Criteria)

MC_yrs=MC02+MC03+MC04+MC05+MC06+MC07+MC08+MC09+MC10+MC11+MC12+MC13
colnames(MC_yrs) = "MC_yrs"

NPS$MC_yrs=MC_yrs$MC_yrs

