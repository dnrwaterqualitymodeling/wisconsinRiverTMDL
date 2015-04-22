###########################################
# NOTE: This script should be run AFTER   #
# re-write SWAT Input Files, which is run #
# in ArcSWAT or SWATEditor                #
###########################################
library(stringr)

options(stringsAsFactors=F)

 # projectDir = "C:/Users/ruesca/Desktop/WRB"
projectDir = "H:/WRB"
txtinout = paste(projectDir, "Scenarios", "Default", "TxtInOut", sep="/")

file_wetland_geometry = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/wetlands/wetland_parameters.csv"
file_pond_geometry = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"
## UPDATE POINT SOURCE TEXT FILES AND FIG.FIG
ps_files = list.files(
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/point_sources",
	"^recday_[0-9]+\\.txt$",
	full.names=T
)

fig.fig = readLines(paste(txtinout, "fig.fig", sep="/"))

for (ps_file in ps_files) {
	sb = str_extract(basename(ps_file), "[0-9]+")
	ptrn = paste("\\s", sb, "p.dat",sep='')
	indx = grep(ptrn, fig.fig)
	indx = indx - 1 
	lne = fig.fig[indx]
	lne = gsub(
		"reccnst       11",
		"recday        10",
		lne)
	fig.fig[indx] = lne
}
if (!file.exists(paste(txtinout, "fig.fig_bkp", sep="/"))){
	file.copy(paste(txtinout, "fig.fig", sep="/"),
		paste(txtinout, "fig.fig_bkp", sep="/"))
}
writeLines(fig.fig, paste(txtinout, "fig.fig", sep="/"))

dates = seq(
	as.Date("1990-01-01"),
	as.Date("2013-12-31"),
	"1 day")
days = formatC(
	gsub(
		"^[0]+", "",
		format(dates, "%j")
	),
	width=4,
	flag=" ")
years = formatC(
	gsub(
		"^[0]+", "",
		format(dates, "%Y")
	),
	width=5,
	flag=" ")

for (ps_file in ps_files) {
	sb = str_extract(basename(ps_file), "[0-9]+")
	print(paste("writing .dat file for", sb))
	ps_data = read.csv(ps_file)
	ps_data = ps_data[,2:ncol(ps_data)]
	cols = c(
		" DAY",
		" YEAR",
		format(toupper(names(ps_data)), width=17, justify="right")
	)
	ps_data_fmt = apply(
		ps_data,
		c(1,2),
		function (x) {
			sprintf(" %.10E", x)
		}
	)
	ps_data_fmt = rbind(cols, cbind(days, years, ps_data_fmt))
	ps_data_str = apply(
		ps_data_fmt,
		1,
		function (x) {
			paste(x, collapse="")
		}
	)
	ps_data_str = c("\n\n\n\n", ps_data_str)
	out_file = paste(txtinout, "/", sb, "p.dat", sep="")
	writeLines(ps_data_str, out_file)
}

## UPDATE POND AND WETLAND PARAMETERS

wetland_geometry = read.csv(file_wetland_geometry)
pond_geometry = read.csv(file_pond_geometry)
files_pnds = list.files(txtinout, "*.pnd")

for (fl in files_pnds){
	lnes = readLines(paste(txtinout, fl, sep="/"))
	sb = as.numeric(substr(fl, 1, 5))
	
	
	print(paste("Working on Subbasin", sb))
	
	if (sb %in% pond_geometry$subbasin){
		indx = which(pond_geometry$subbasin == sb)
		substr(lnes[3], 9, 16) = sprintf("%8.3f", pond_geometry[indx,"PND_FR"])
		substr(lnes[4], 9, 16) = sprintf("%8.3f", pond_geometry[indx,"PND_PSA"])
		substr(lnes[5], 9, 16) = sprintf("%8.3f", pond_geometry[indx,"PND_PVOL"])
		substr(lnes[6], 9, 16) = sprintf("%8.3f", pond_geometry[indx,"PND_ESA"])
		substr(lnes[7], 9, 16) = sprintf("%8.3f", pond_geometry[indx,"PND_EVOL"])
	}
	indx = which(wetland_geometry$subbasin == sb)
	substr(lnes[29], 9, 16) = sprintf("%8.3f", wetland_geometry[indx,"WET_FR"])
	substr(lnes[30], 9, 16) = sprintf("%8.3f", wetland_geometry[indx,"WET_NSA"])
	substr(lnes[31], 9, 16) = sprintf("%8.3f", wetland_geometry[indx,"WET_NVOL"])
	substr(lnes[32], 9, 16) = sprintf("%8.3f", wetland_geometry[indx,"WET_MXSA"])
	substr(lnes[33], 9, 16) = sprintf("%8.3f", wetland_geometry[indx,"WET_MXVOL"])
	substr(lnes[34], 9, 16) = sprintf("%8.3f", wetland_geometry[indx,"WET_VOL"])
	writeLines(lnes, paste(txtinout, fl, sep="/"))
}
