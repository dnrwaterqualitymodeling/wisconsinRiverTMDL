library(dplyr)
library(rgdal)
library(classInt)
library(RColorBrewer)

file_output.rch = "C:/TEMP/WRB.Sufi2.SwatCup/output.rch"
file_subbasins = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
dir_out_fig = "T:/Projects/Wisconsin_River/Model_Outputs/plots/tp_concentration"

output.rch = read.table(
	file_output.rch,
	skip=9,
	colClasses=c(
		"NULL",
		"integer",
		"NULL",
		"integer",
		"integer",
		"integer",
		"NULL",
		"NULL",
		"numeric",
		"numeric",
		"numeric"
	)
)

names(output.rch) = c(
	"reach",
	"mo",
	"da",
	"yr",
	"flow_cms",
	"sed_mt",
	"tp_kg"
)

s = readOGR(
	dirname(file_subbasins),
	strsplit(basename(file_subbasins), "\\.")[[1]][1]
)

## Plot median concentration, flow-weighted mean concentration,
##     and their ratio

fwmc = output.rch %>%
	filter(mo >= 5, mo <= 10) %>%
	mutate(tp_ug_L = (tp_kg / flow_cms) * 11.57407) %>%
	group_by(reach) %>%
	summarize(mc = median(tp_ug_L, na.rm=T)) %>%
	left_join(output.rch %>%
		group_by(reach) %>%
		summarise(fwmc = (sum(tp_kg) / sum(flow_cms)) * 11.57407)
	) %>%
	mutate(ratio = fwmc / mc)
fwmc$ratio[fwmc$ratio == Inf] = NA	

s@data = s@data %>%
	left_join(fwmc, by=c("Subbasin" = "reach"))

p = brewer.pal(5, "Reds")


mc_ci = classIntervals(s@data$mc, 5, style = "quantile")
mc_ci$brks = round(mc_ci$brks, 0)
mc_col = findColours(mc_ci, p)

fwmc_ci = classIntervals(s@data$fwmc, 5, style = "quantile")
fwmc_ci$brks = round(fwmc_ci$brks, 0)
fwmc_col = findColours(fwmc_ci, p)

ratio_ci = classIntervals(s@data$ratio, 5, style = "quantile")
ratio_ci$brks = round(ratio_ci$brks, 1)
ratio_col = findColours(ratio_ci, p)

setwd(dir_out_fig)

png("median_concentration.png", height=8, width=6, unit="in", res=300)
plot(s, col=mc_col, main="Median concentration, ug/L")
legend("bottomright", legend=names(print(mc_ci)), fill=p)
dev.off()

png("flow_weighted_mean_concentration.png", height=8, width=6, unit="in", res=300)
plot(s, col=fwmc_col, main="Flow-weighted mean concentration, ug/L")
legend("bottomright", legend=names(print(fwmc_ci)), fill=p)
dev.off()

png("ratio.png", height=8, width=6, unit="in", res=300)
plot(s, col=ratio_col, main="Ratio of flow-weighted to median concentration")
legend("bottomright", legend=names(print(ratio_ci)), fill=p)
dev.off()


