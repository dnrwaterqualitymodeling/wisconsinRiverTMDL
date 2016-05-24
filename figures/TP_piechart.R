library(RColorBrewer)
library(dplyr)
library(plotrix)

setwd("T:/Projects/Wisconsin_River/Model_Outputs/plots/pie_charts")
db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_spatial_db.sqlite")
pal = c("#FDC086", "#7FC97F", "#FFFF99", "#BEAED4")

yld_sub = collect(tbl(db, "yields_by_sub_cum"))

rchs = data.frame(
  rch = c(1,81,158,203,272,167,86,262,73,60,59),
  desc = c(
    "Below Lake Wisconsin",
    "Below Lake Du Bay",
    "Wisconsin River at Merrill",
    "Wisconsin River at Nekoosa",
	"Wisconsin River above Tomahawk",
	"Wisconsin River above Rhinelander",
    "Below Big Eau Reservoir",
    "Above Lake Du Bay",
    "Below Lake Petenwell",
    "Above Yellow River arm of Castle Rock Lake",
	"Below Castle Rock Lake"
    )
  )

for (r in 1:nrow(rchs)) {
  png(
	  paste(rchs$desc[r], "png", sep="."),
	  width=3,
	  height=3,
	  units="in",
	  res=300,
	  bg="transparent"
  )
  par(mar=rep(0,4), oma=rep(0,4))
  d = t(subset(yld_sub, rch==rchs$rch[r], select=c(tp_ag, tp_bg, tp_pt , tp_urb)))
  print(rchs$desc[r])
  print(d/sum(d))
  pcts = round(d/sum(d, na.rm=T) * 100)
  lbls = paste(pcts, "%", sep="")
  pie3D(d, col=pal, theta=pi/3, height=0.15) #, labels=lbls, col=pal, main=rchs$desc[r])
  dev.off()
}