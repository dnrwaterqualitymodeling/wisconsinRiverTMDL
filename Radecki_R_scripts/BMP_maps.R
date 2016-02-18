library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(foreign)
library(classInt)
library(RColorBrewer)


subs = readOGR("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro", "subbasins")

DRAFT = read.table("C:/Users/radeca/Desktop/reach_output/draft_output.rch", skip=9, header=F)
colnames(DRAFT)=c("Omit","RCH", "GIS", "Day", "Area", "Flow_in", "Flow_out", "Sed_out", "Tot_P")

HALF_P = read.table("C:/Users/radeca/Desktop/reach_output/half_frtP_output.rch", skip=9, header=F)
colnames(HALF_P)=c("Omit","RCH", "GIS", "Day", "Area", "Flow_in", "Flow_out", "Sed_out", "Tot_P")

NO_P = read.table("C:/Users/radeca/Desktop/reach_output/no_p_output.rch", skip=9, header=F)
colnames(NO_P)=c("Omit","RCH", "GIS", "Day", "Area", "Flow_in", "Flow_out", "Sed_out", "Tot_P")

USLEP = read.table("C:/Users/radeca/Desktop/reach_output/uslep_05_output.rch", skip=9, header=F)
colnames(USLEP)=c("Omit","RCH", "GIS", "Day", "Area", "Flow_in", "Flow_out", "Sed_out", "Tot_P")

BIOMIX = read.table("C:/Users/radeca/Desktop/reach_output/biomix_output.rch", skip=9, header=F)
colnames(BIOMIX)=c("Omit","RCH", "GIS", "Day", "Area", "Flow_in", "Flow_out", "Sed_out", "Tot_P")

NEW_PAR = read.table("C:/Users/radeca/Desktop/draft_revisions.Sufi2.SwatCup/output.rch", skip=9, header=F)
colnames(NEW_PAR)=c("Omit","RCH", "GIS", "Day", "Area", "Flow_in", "Flow_out", "Sed_out", "Tot_P")


DRAFT$draft = (DRAFT$Tot_P/DRAFT$Flow_out)*0.01157407
Draft = aggregate(draft~RCH, DRAFT, FUN=median)

HALF_P$half_p = (HALF_P$Tot_P/HALF_P$Flow_out)*0.01157407 
Half_p = aggregate(half_p~RCH, HALF_P, FUN=median)

NO_P$no_p = (NO_P$Tot_P/NO_P$Flow_out)*0.01157407
No_p = aggregate(no_p~RCH, NO_P, FUN=median)

USLEP$uslep = (USLEP$Tot_P/USLEP$Flow_out)*0.01157407
Uslep = aggregate(uslep~RCH, USLEP, FUN=median)

BIOMIX$biomix = (BIOMIX$Tot_P/BIOMIX$Flow_out)*0.01157407
Biomix = aggregate(biomix~RCH, BIOMIX, FUN=median)

NEW_PAR$new_par = (NEW_PAR$Tot_P/NEW_PAR$Flow_out)*0.01157407
New_par = aggregate(new_par~RCH, NEW_PAR, FUN=median)

subs@data$draft = Draft$draft
subs@data$half_p = Half_p$half_p
subs@data$no_p = No_p$no_p
subs@data$uslep = Uslep$uslep
subs@data$biomix = Biomix$biomix
subs@data$new_par = New_par$new_par

pal = brewer.pal(6, "YlOrRd")

colnames(subs@data) = c("GRIDCODE", "Subbasin", "DRAFT", "HALF PHOSPHORUS", "NO PHOSPHORUS", "USLEP",  "BIOMIX", "NEWRUN")

scen_cols = c("DRAFT", "NEWRUN", "NO PHOSPHORUS", "USLEP", "HALF PHOSPHORUS", "BIOMIX")
Ptot_class = classIntervals(
  as.matrix(subs@data[scen_cols]),
  6,
  style = "quantile"
)
Ptot_col = findColours(Ptot_class, pal)
dim(Ptot_col) = c(337, length(scen_cols))
Ptot_col = as.data.frame(Ptot_col)
color_cols = paste(scen_cols, "_color", sep="")
names(Ptot_col) = color_cols
subs@data[color_cols] = Ptot_col

# plot(subs, col=as.character(Ptot_col[,1]))

bins = names(print(Ptot_class))
dir_out = "C:/Users/radeca/Desktop/rcode/plots"
pdf("BMP_maps.pdf", width=8.5, height=11)
for (color_col in color_cols) {
  lab=strsplit(color_col, "_")
  plot(subs, col=as.character(subs@data[[color_col]]))
  title(paste(lab[[1]][1]))
  legend("topleft", 
         title="Phosphorus Concentration", 
         legend=bins,
         fill=pal,
         cex=.9
  )
}
dev.off()
