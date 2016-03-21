library(dplyr)
library(stringr)
library(rgdal)
library(RColorBrewer)
library(classInt)

file_subs = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"
file_plots = "T:/Projects/Wisconsin_River/Model_Outputs/plots/FILTERW_plots/median_tp_may_oct/filterw_scens_wo_pt_srcs.pdf"

pal = brewer.pal(5, "YlOrRd")

subs = readOGR(
	dirname(file_subs),
	strsplit(basename(file_subs), "\\.")[[1]][1]
)

con = src_sqlite(file_db)
scens = tbl(con, sql("SELECT DISTINCT filterw_scen FROM filterw_scenarios")) %>%
	collect()
scens = scens[[1]]
rm(con)

pdf(file_plots, height=11, width=8.5)
for (scen in scens) {
	con = src_sqlite(file_db)
	scen_d = tbl(con, "filterw_scenarios") %>%
		filter(FILTERW_SCEN == scen) %>%
		mutate(MC_EST = FWMC * (1/1.3)) %>%
		collect()
	rm(con)
	
	sub_scen = subs
	sub_scen@data = merge(
		sub_scen@data,
		scen_d,
		sort=F,
		by.x="Subbasin",
		by.y="RCH",
		all.x=T,
		all.y=F
	)
	ci = classIntervals(
		sub_scen@data$MC_EST,
		5,
		style = "fixed",
		fixedBreaks=c(0, 40, 75, 100, 200, max(sub_scen@data$MC_EST))
	)
	cols = findColours(ci, pal)
	ttl = paste(scen*100, "% increase calibrated FILTERW value", sep="")
	plot(sub_scen, col=cols, main=ttl)
	cls = names(print(ci))
	legend("bottomleft", legend=cls, fill=pal, bty="n", title="median TP (ug/L)")
}
dev.off()