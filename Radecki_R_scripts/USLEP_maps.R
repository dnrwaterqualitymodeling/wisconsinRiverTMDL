library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(foreign)
library(classInt)
library(RColorBrewer)



subs = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro", "subbasins")
file_data1 = "C:/Users/radeca/Desktop/USLE_P/SUBouts/output_0.sub"
file_data2 = "C:/Users/radeca/Desktop/USLE_P/SUBouts/output_25.sub"
# P0_data = read.table("C:/Users/radeca/Desktop/USLE_P/SUBouts/output_0.sub", skip=9, header=F)
# P25_data = read.table("C:/Users/radeca/Desktop/USLE_P/SUBouts/output_25.sub", skip=9, header=F)

dir_out = "C:/Users/radeca/Desktop/rcode/plots"


pal = brewer.pal(6, "YlOrRd")
dates = seq(as.Date("2002-01-01"), as.Date("2013-12-01"), "1 month")

year = read.csv("C:/Users/radeca/Documents/R/R_data/swatyear.csv", header=F)
colnames(year) = c("month", "year")



d = list()
i = 0
for (file_d in c(file_data1, file_data2)) {
  i = i + 1
  d_i = read.table(file_d, skip=9, header=F, stringsAsFactors=F)
  d_i = d_i[,c(2,16,18,19)]
  colnames(d_i) = c("Sub", "OrgP", "SolP", "SedP")
  d_i = cbind(year, d_i)
  d_i = subset(d_i, month <= 12 & year != 0)
  d_i$TotP = d_i$OrgP + d_i$SolP + d_i$SedP
  d_i_sub_month = aggregate(TotP ~ Sub + month, data=d_i, FUN=sum)
  colnames(d_i_sub_month) = c("Subbasin", "Month", "TotP")
  d[[i]] = d_i_sub_month
}

D = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

for (method in c("Total", "Percent")) {
  if (method == "Total") {
    p_diff = d[[1]]$TotP - d[[2]]$TotP
    units = " ( kg/ha )"
  } else {
    p_diff = ((d[[1]]$TotP - d[[2]]$TotP) / d[[1]]$TotP) * 100
    units = " (%) "
  }
  
  #p_diff[p_diff == 0] = NA
  Ptot_class = classIntervals(p_diff, 6, style = "quantile")
  Ptot_col = findColours(Ptot_class, pal)
  col_tbl = data.frame(
    Subbasin = d[[1]]["Subbasin"],
    Month = d[[1]]["Month"],
    Color = Ptot_col
  )  
  
  bins = names(print(Ptot_class))
  dir_out_meth = paste(dir_out, "/", method, sep="")
  if (!file.exists(dir_out_meth)) {
    dir.create(dir_out_meth)
  }
  for (m in 1:12) {
    d_m = subset(col_tbl, Month == m)
    subs_m = merge(subs, d_m)
    mon=D[m]
    out_pdf = paste(dir_out_meth,"/USLE_P_", tolower(mon), ".pdf", sep="")
    pdf(out_pdf, width=8.5, height=11)
    plot(subs_m, col=as.character(subs_m@data$Color))
    title(paste( method, "Phosphorus Yield Reductions", "\n", "for", mon))
    legend(
      "right",
      subs_m,
      legend=bins,
      fill=pal,
      cex= .7,
      title = paste(method, "Phosphorus", units)
    )
    dev.off()
  }
}

