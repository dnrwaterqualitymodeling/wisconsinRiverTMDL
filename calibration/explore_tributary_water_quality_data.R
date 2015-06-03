# TODO: Add comment
# 
# Author: ruesca
###############################################################################

require(xlsx)
options(stringsAsFactors=F)
file_wq = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/for_USGS_1.xlsx"
file_pdf = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/pollutant_plots.pdf"

sites_table = read.xlsx2(file_wq, 1)
all_data = NULL
for (sheet in 1:nrow(sites_table)) {
	sheet_data = read.xlsx2(file_wq, sheet + 1)
	all_data = rbind(all_data, sheet_data)
}
all_data$Date_trunc = as.POSIXct(
		(as.integer(all_data$Date_trunc) - 25569) * 86400,
		tz="GMT",
		origin="1970-01-01")
all_data$SuspSolids = sub("<", "", all_data$SuspSolids)
all_data$TotalPhosphorus_P = sub("<", "", all_data$TotalPhosphorus_P)
all_data[all_data == "NA"] = NA
for (v in c("flow","SuspSolids","TotalPhosphorus_P")) {
	all_data[v] = apply(all_data[v],
		1,
		function (x) {
			if (x == "NA") {
				return(NA)
			} else {
				return(as.numeric(x))
			}
		}
	)
}

all_data$SuspSolids_tons =
		(all_data$SuspSolids * (all_data$flow * 28.3168) * 86400) * (1/1000^3)
all_data$TotalPhosphorus_P_kg =
		(all_data$TotalPhosphorus_P * (all_data$flow * 28.3168) * 86400) * (1/1000^2)

pdf(file_pdf, height=11, width=8.5)
par(mfrow=c(2,1))
sites = unique(all_data$USGS_ID)
for (site in sites) {
	site_data = subset(all_data, USGS_ID == site)
	site_name = site_data$Flow_Name[1]
	if (any(!is.na(site_data$SuspSolids_tons))) {
		plot(flow * 0.0283168 ~ SuspSolids_tons,
			data=site_data,
			ylab="TSS (metric tons)",
			xlab="Flow (cms)",
			main=site_name)
	} else {
		plot.new()
	}
	if (any(!is.na(site_data$TotalPhosphorus_P_kg))) {
		plot(flow * 0.0283168 ~ TotalPhosphorus_P_kg,
			data=site_data,
			ylab="TP (kg)",
			xlab="Flow (cms)",
			main=site_name)
	 } else {
		 plot.new()
	 }
}
dev.off()
