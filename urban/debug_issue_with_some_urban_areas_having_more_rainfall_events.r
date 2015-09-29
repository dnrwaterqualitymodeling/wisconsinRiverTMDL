ul = read.delim("T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads_check_anns_changes.txt")
muni_sub_count = aggregate(date ~ muni + subbasin + soil_type, data=ul, FUN=length)
names(muni_sub_count)[3] = "n_events"
write.table(
	muni_sub_count,
	"T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/WinSLAMM_Daymet/rainfall_event_count_anns_changes.txt",
	row.names=F,
	sep="\t"
)



data_dir = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/WinSLAMM_Daymet/cleanedCsvs"
out_plot_pdf = "C:/Users/ruesca/Desktop/urban_output_histograms.pdf"


data_files = list.files(data_dir, full.names=T, pattern="\\.csv$")
all_data = list()
for (data_file in data_files) {
	print(data_file)
	d = read.csv(data_file)
	name = strsplit(basename(data_file), "\\.")[[1]][1]
	for (v in c("Rain.Depth..in.", "Average.Flow..cfs.", "Suspended.Solids.Mass..lbs.", "Total.Phosphorus..lbs.")) {
		all_data[[v]] = rbind(
			all_data[[v]],
			data.frame(
				name=name,
				value=d[[v]]
			)
		)
	}
}
pdf(out_plot_pdf, width=6, height=6)
event_count = aggregate(value ~ name, data=all_data[["Rain.Depth..in."]], FUN=length)
hist(event_count$value, xlab="Event count", main="Histogram of numbers of rain events")
for (v in c("Rain.Depth..in.", "Average.Flow..cfs.", "Suspended.Solids.Mass..lbs.", "Total.Phosphorus..lbs.")) {
	summary_table = aggregate(value ~ name, data=all_data[[v]], FUN=sum)
	hist(summary_table$value, xlab=v, main=v)
}
dev.off()
