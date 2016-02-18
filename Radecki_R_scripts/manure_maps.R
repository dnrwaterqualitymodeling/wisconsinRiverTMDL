library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(foreign)
library(classInt)
library(RColorBrewer)


dir = "C:/Users/radeca/Desktop/USLE_P"
subs = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro", "subbasins")
file_data1 = "C:/Users/radeca/Desktop/USLE_P/HRUouts/output_man_09.hru"
file_data2 = "C:/Users/radeca/Desktop/USLE_P/HRUouts/output_man_013.hru"
year = read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/YEAR.txt")
colnames(year) = "YEAR"
pal = brewer.pal(6, "Greys")
dir_out = "C:/Users/radeca/Documents/Plots/BMP"

d = list()
i = 0
for (file_d in c(file_data1, file_data2)) {
  i = i + 1
  d_i = read.table(file_d, skip=9, header=F, stringsAsFactors=F)
  d_i = d_i[-6]
  d_i = d_i[,c(1,2,4,8,14,15,16)]
  colnames(d_i) = c("LULC", "HRU", "SUB", "YLD", "OrgP", "SolP", "SedP")
  d_i = cbind(year, d_i)
  d_i = subset(d_i, year != 12)
  d_i$TotP = d_i$OrgP + d_i$SolP + d_i$SedP
  
  di_tot_p = aggregate(TotP ~ SUB + YEAR, data=d_i, FUN=sum)
  colnames(di_tot_p) = c("Subbasin", "Year", "TotP")
  di_sed_p = aggregate(SedP ~ SUB + YEAR, data=d_i, FUN=sum)
  colnames(di_sed_p) = c("Subbasin", "Year", "SedP")
  di_org_p = aggregate(OrgP ~ SUB + YEAR, data=d_i, FUN=sum)
  colnames(di_org_p) = c("Subbasin", "Year", "OrgP")
  di_sol_p = aggregate(SolP ~ SUB + YEAR, data=d_i, FUN=sum)
  colnames(di_sol_p) = c("Subbasin", "Year", "SolP")
  d_all = cbind(di_sol_p[,c(1,2,3)], di_org_p[,3], di_sed_p[,3], di_tot_p[,3] )
  
  d[[i]] = d_all
}

Yr = list(c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013"))


for (method in c("Total", "Percent")) {
  if (method == "Total") {
    p_diff = d[[1]]$di_tot_p - d[[2]]$di_tot_p
    units = " ( kg/ha )"
  } else {
    p_diff = ((d[[1]]$di_tot_p - d[[2]]$di_tot_p) / d[[1]]$di_tot_p) * 100
    units = " (%) "0
  }
  
  Ptot_class = classIntervals(p_diff, 6, style = "quantile")
  Ptot_col = findColours(Ptot_class, pal)
  col_tbl = data.frame(
    Subbasin = d[[1]]["Subbasin"],
    Year = d[[1]]["Year"],
    Color = Ptot_col
  )
  
  bins = names(print(Ptot_class))
  dir_out_meth = paste(dir_out, "/", method, sep="")
  if (!file.exists(dir_out_meth)) {
    dir.create(dir_out_meth)
}


for (y in 2002:2013) {
  d_y = subset(col_tbl, Year == y)
  subs_y = merge(subs, d_y)
  out_png = paste(dir_out_meth,"/Tot_P_", tolower(y), ".png", sep="")
  png(out_png, width=6, height=8, units="in", res=100)
  plot(subs_y, col=as.character(subs_y@data$Color))
  title(paste( method, "Phosphorus Yield Reductions", "\n", "for", y))
  legend(
    "right",
    subs_y,
    legend=bins,
    fill=pal,
    cex= .7,
    title = paste(method, "Phosphorus", units)
  )
dev.off()
  }
}
 
d_1 = cbind(year, d_1)
d_1 = subset(d_1,  year != 12)
d_1$TotP = d_1$ORGP + d_1$SOLP + d_1$SEDP
tot_p = aggregate(TotP ~ SUB + YEAR, data=d_1, FUN=sum)
sed_p = aggregate(SEDP ~ SUB + YEAR, data=d_1, FUN=sum)
org_p = aggregate(ORGP ~ SUB + YEAR, data=d_1, FUN=sum)
sol_p = aggregate(SOLP ~ SUB + YEAR, data=d_1, FUN=sum)
all_p1 = cbind(sol_p[,c(1,2,3)], org_p[,3], sed_p[,3], tot_p[,3] )
colnames(all_p1) = c("SUB", "YEAR", "SolP", "OrgP", "SedP", "TotP" )

d_1 = read.table(file_data1, skip = 9, header=F)
d_1 = d_1[-6]
d_1names ="LULC  HRU GIS SUB MGT MON AREAkm2 BIOMt_ha LAI YLDt_ha TMP_AVdgC WYLDmm 
  SYLDt_ha PAPPkg_ha  PUPkg_ha ORGP SEDP SOLP PGWkg_ha  SOL_TMPdgC"
d_1names = strsplit(d_1names, "\\s+|:")
colnames(d_1) = d_1names[[1]][c(1,2,3,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20)]


d_2 = cbind(year, d_2)
d_2 = subset(d_2,  year != 12)
d_2$TotP = d_2$ORGP + d_2$SOLP + d_2$SEDP
tot_p = aggregate(TotP ~ SUB + YEAR, data=d_2, FUN=sum)
sed_p = aggregate(SEDP ~ SUB + YEAR, data=d_2, FUN=sum)
org_p = aggregate(ORGP ~ SUB + YEAR, data=d_2, FUN=sum)
sol_p = aggregate(SOLP ~ SUB + YEAR, data=d_2, FUN=sum)
all_p2 = cbind(sol_p[,c(1,2,3)], org_p[,3], sed_p[,3], tot_p[,3] )
colnames(all_p2) = c("SUB", "YEAR", "SolP", "OrgP", "SedP", "TotP" )

