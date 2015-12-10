dirs_loads = I(c(
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_loads/all/SS/calibration",
	"T:/Projects/Wisconsin_River/GIS_Datasets/observed/usgs_loads/all/TP/calibration"
))
vars = I(c("SED_OUT", "TOT_P"))
#files_start_end = I(c(
#	"T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/ss_start_end_dates.txt",
#	"T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/tp_start_end_dates.txt"
#))
file_out = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/main_stem_pollutants_formatted_for_swat_cup.txt"

in_info = data.frame(
	dirs = dirs_loads,
	vars = vars
#	file_start_end = files_start_end
)
#in_info = as.data.frame(apply(in_info, c(1,2), as.character))

bsn_id_lkp = data.frame(
	subbasin = c(59,74,81,145,148,154,203),
	id = c(10017791,293130,10014652,723002,503059,10031102,723259)
)

dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month")
dates = data.frame(
	YEAR = as.integer(format(dates, "%Y")),
	MO = as.integer(format(dates, "%m")),
	INDEX = 1:length(dates)
)
file.create(file_out)
for (var_row in 1:nrow(in_info)) {
#	start_end = read.table(
#		in_info$file_start_end[var_row],
#		sep="\t",
#		header=T)
	dir_load = in_info$dirs[var_row]
	for (bsn_row in 1:nrow(bsn_id_lkp)) {
		id = bsn_id_lkp$id[bsn_row]
		file_load = paste(dir_load, "/", id, ".txt", sep="")
		if (!file.exists(file_load)) {next}
		data_load = read.table(file_load, sep="\t", header=T)
		data_load = merge(data_load, dates, all.x=T, all.y=F)
		data_load = data_load[order(data_load$YEAR, data_load$MO),]
		load_col = names(data_load)[3]
		if (load_col == "SS_KG") {
			data_load$SS_MT = data_load$SS_KG / 1000
			load_col = "SS_MT"
		}
		var_name = in_info$vars[var_row]
		var_name_sub = paste(var_name, bsn_id_lkp$subbasin[bsn_row], sep="_")
		ln1 = paste(
			var_name_sub,
			"   : this is the name of the variable and the subbasin number to be included in the objective function",
			sep=""
		)
		ln2 = paste(
			nrow(data_load),
			"   : number of data points for this variable as it follows below. First column is a sequential number from beginning\n      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value.",
			sep=""
		)
		write(ln1, file_out, append=T)
		write(ln2, file_out, append=T)
		write("\n", file_out, append=T)
		out_txt = apply(
			data_load,
			1,
			function(x, load_col, var_name) {
				paste(
					x["INDEX"],
					"\t",
					var_name,
					"_",
					x["MO"],
					"_",
					x["YEAR"],
					"\t",
					x[load_col],
					sep=""
				)
			},
			load_col=load_col,
			var_name=var_name
		)
		write(out_txt, file_out, append=T)
		write("\n", file_out, append=T)
	}
}