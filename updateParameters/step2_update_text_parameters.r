library(stringr)

options(stringsAsFactors=F)

# projectDir = "C:/Users/ruesca/Desktop/WRB"
projectDir = "H:/WRB"

file_bio_e = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/crop/Bio_E_Calibration_Report.csv"

## UPDATE POINT SOURCE TEXT FILES AND FIG.FIG
ps_files = list.files(
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/point_sources",
	"^recday_[0-9]+\\.txt$",
	full.names=T
)

txtinout = paste(projectDir, "Scenarios", "Default", "TxtInOut", sep="/")
fig.fig = readLines(paste(txtinout, "fig.fig", sep="/"))

for (ps_file in ps_files) {
	sb = str_extract(basename(ps_file), "[0-9]+")
	ptrn = paste("\\s", sb, "p.dat",sep='')
	indx = grep(ptrn, fig.fig)
	indx = indx - 1 
	lne = fig.fig[indx]
	lne = gsub("reccnst       11",
		"recday        10",
		lne)
	fig.fig[indx] = lne
}

file.copy(paste(txtinout, "fig.fig", sep="/"), paste(txtinout, "fig.fig_bkp", sep="/"))
writeLines(fig.fig, paste(txtinout, "fig.fig", sep="/"))

dates = seq(as.Date("1990-01-01"), as.Date("2013-12-31"), "1 day")
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

#### UPDATE PLANT.DAT: 
######## 	insert calibrated BIO E and correct issue with Forest, re: PB's suggestion
pth_plant.dat = paste(projectDir, "/Scenarios/Default/TxtInOut/plant.dat",sep='')

df_bio_e = read.csv(file_bio_e)

plant.dat = readLines(pth_plant.dat)

for (crop in c("CORN", "SOYB", "ALFA")) {
    bio_e <- df_bio_e[which(df_bio_e$Crop == crop), 'FittedBioE']
    # plant.dat = readLines(paste(wd, "plant.dat", sep="\\"))
    plant.dat.ind = which(substr(plant.dat, 7, 10) == crop)
    substr(plant.dat[plant.dat.ind + 1], 3, 7) = format(bio_e, digits=2, nsmall=2)
    
}
# fixing forest parameter PB's suggestion
plant.dat.ind = which(substr(plant.dat, 7, 10) == "FRST") #"FRSD"
substr(plant.dat[plant.dat.ind + 3],80, 84) = sprintf("%2.3f", 0)

writeLines(plant.dat, pth_plant.dat)




