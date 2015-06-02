###########################################
# NOTE: This script should be run AFTER   #
# re-write SWAT Input Files, which is run #
# in ArcSWAT or SWATEditor                #
###########################################
library(stringr)

options(stringsAsFactors=F)

projectDir = "C:/Users/ruesca/Desktop/WRB"
#projectDir = "H:/WRB"
txtinout = paste(projectDir, "Scenarios", "Default", "TxtInOut", sep="/")

file_wetland_geometry = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/wetlands/wetland_parameters.csv"
file_pond_geometry = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_geometry.csv"
file_merged_idas = "T:/Projects/Wisconsin_River/GIS_Datasets/ponds/pond_and_wetland_geometry.txt"
file_fert_dat = "T:/Projects/Wisconsin_River/swat_install_software/SWAT_EXE_and_file_updates/fert.dat"
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

## UPDATE POND AND WETLAND PARAMETERS (combined)

wetland_geometry = read.csv(file_wetland_geometry)
pond_geometry = read.csv(file_pond_geometry)
files_pnds = list.files(txtinout, "*.pnd")

merge_idas = data.frame()
for (fl in files_pnds){
	lnes = readLines(paste(txtinout, fl, sep="/"))
	sb = as.numeric(substr(fl, 1, 5))

	pnd_fr = 0
	pnd_psa = 0
	pnd_pvol = 0
	pnd_esa = 0
	pnd_evol = 0
	
	print(paste("Working on Subbasin", sb))
	
	if (sb %in% pond_geometry$subbasin){
		indx = which(pond_geometry$subbasin == sb)
		pnd_fr =  pond_geometry[indx,"PND_FR"]
		pnd_psa =  pond_geometry[indx,"PND_PSA"]
		pnd_pvol = pond_geometry[indx,"PND_PVOL"]
		pnd_esa =  pond_geometry[indx,"PND_ESA"]
		pnd_evol =  pond_geometry[indx,"PND_EVOL"]
	}

	indx = which(wetland_geometry$subbasin == sb)
	pnd_fr = pnd_fr + wetland_geometry[indx,"WET_FR"]
	pnd_psa = pnd_psa + wetland_geometry[indx,"WET_NSA"]
	pnd_pvol = pnd_pvol + wetland_geometry[indx,"WET_NVOL"]
	pnd_esa = pnd_esa + wetland_geometry[indx,"WET_MXSA"]
	pnd_evol = pnd_evol + wetland_geometry[indx,"WET_MXVOL"]

	substr(lnes[3], 9, 16) = sprintf("%8.3f", pnd_fr)
	substr(lnes[4], 9, 16) = sprintf("%8.3f", pnd_psa)
	substr(lnes[5], 9, 16) = sprintf("%8.3f", pnd_pvol)
	substr(lnes[6], 9, 16) = sprintf("%8.3f", pnd_esa)
	substr(lnes[7], 9, 16) = sprintf("%8.3f", pnd_evol)
	substr(lnes[8], 9, 16) = sprintf("%8.3f", pnd_pvol)
	lnes[12] =
		"               7    | IFLOD1: Beginning month of non-flood season"
	lnes[13] =
		"               2    | IFLOD2: Ending month of non-flood season"
	lnes[14] =
		"          15.000    | NDTARG: Number of days needed to reach target storage from current pond storage"
	
	writeLines(lnes, paste(txtinout, fl, sep="/"))
	
	row = data.frame(
		subbasin = as.integer(substr(fl,3,5)),
		PND_FR = pnd_fr,
		PND_PSA = pnd_psa,
		PND_PVOL = pnd_pvol,
		PND_ESA = pnd_esa,
		PND_EVOL = pnd_evol
	)
	merge_idas = rbind(merge_idas, row)	
}
write.table(merge_idas, file_merged_idas, sep="\t", row.names=F)

file.copy(file_fert_dat, paste(txtinout, "/fert.dat", sep=""), overwrite=T)
file_basins_bsn = paste(txtinout, "/basins.bsn", sep="")
write("               1    | manp_flag : 1 = use manure P model\n               0    | isolp flag !! if = 1  prints out solp.out\n",
	file_basins_bsn,
	append=T
)

