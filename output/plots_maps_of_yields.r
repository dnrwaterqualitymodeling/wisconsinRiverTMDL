library(dplyr)
library(rgdal)
library(RColorBrewer)
library(classInt)

dir_out = "T:/Projects/Wisconsin_River/Model_Outputs/plots/maps"
file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_spatial_db.sqlite"
db = src_sqlite(file_db)

yld = collect(tbl(db, "yields_by_sub")) %>%
	mutate(
		sed = sed_np + sed_pt + sed_urb,
		tp = tp_np + tp_pt + tp_urb)

subs = readOGR(file_db, "subbasins")
subs@data$sub = as.integer(row.names(subs))
yld = yld %>%
	right_join(subs@data)
subs@data = as.data.frame(yld)

setwd(dir_out)

pal = brewer.pal(5, "YlOrRd")

for (p in c(
	"sed",
	"sed_ag",
	"sed_bg",
	"sed_np",
	"sed_pt",
	"sed_urb",
	"tp",
	"tp_ag",
	"tp_bg",
	"tp_np",
	"tp_pt",
	"tp_urb")
	) {
	png(paste(p, "png", sep="."),
		width=6.5,
		height=9,
		units="in",
		res=500,
		bg="transparent")
	if (grepl("^sed", p)) {
		bks = c(0,0.01,0.02,0.03,0.04,0.05)
	} else if (grepl("^tp", p)) {
		bks = c(0,0.1,0.2,0.3,0.4,40)
	}
	ci = classIntervals(subs@data[[p]], 5, "fixed", fixedBreaks=bks)
	ci$brks = round(ci$brks, 3)
	col = findColours(ci, pal)
	plot(subs, col=col)
	lgnd = names(print(ci))
	lgnd[5] = paste(">",bks[5])
	legend("bottomright",lgnd,fill=pal)
	dev.off()
}


yld = collect(tbl(db, "yields_by_sub_cum")) %>%
	mutate(
		sed = sed_np + sed_pt + sed_urb,
		tp = tp_np + tp_pt + tp_urb,
		sed_perc_ag = sed_ag / sed,
		sed_perc_bg = sed_bg / sed,
		sed_perc_np = sed_np / sed,
		sed_perc_pt = sed_pt / sed,
		sed_perc_urb = sed_urb / sed,
		sed_perc_pt_urb = sed_perc_pt + sed_perc_urb,
		tp_perc_ag = tp_ag / tp,
		tp_perc_bg = tp_bg / tp,
		tp_perc_np = tp_np / tp,
		tp_perc_pt = tp_pt / tp,
		tp_perc_urb = tp_urb / tp,
		tp_perc_pt_urb = tp_perc_pt + tp_perc_urb)

hydro = readOGR(file_db, "hydro")
hydro@data$rch = as.integer(row.names(hydro))
hydro@data = select(hydro@data, rch)
yld = yld %>%
	right_join(hydro@data)
hydro@data = as.data.frame(yld)

pal = brewer.pal(5, "YlOrRd")
for (p in c(
	"sed_perc_ag",
	"sed_perc_bg",
	"sed_perc_np",
	"sed_perc_pt",
	"sed_perc_urb",
	"sed_perc_pt_urb",
	"tp_perc_ag",
	"tp_perc_bg",
	"tp_perc_np",
	"tp_perc_pt",
	"tp_perc_urb",
	"tp_perc_pt_urb")
	) {
	png(paste("cum_", p, ".png", sep=""),
		width=6.5,
		height=9,
		units="in",
		res=500,
		bg = "transparent")
	ci = classIntervals(
		hydro@data[[p]],
		6,
		"fixed",
		fixedBreaks=c(0,.2,.4,.6,.8,1))
	ci$brks = round(ci$brks, 2)
	col = findColours(ci, pal)
	plot(subs, col="grey90", border="grey70")
	plot(hydro, col=col, lwd=3, add=T)
	lgnd = names(print(ci))
	legend("bottomright",lgnd,col=pal,lty=1, lwd=3)
	dev.off()
}