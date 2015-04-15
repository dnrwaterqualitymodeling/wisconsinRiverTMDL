options(stringsAsFactors=FALSE)
#wd = "C:\\Users\\hydrig\\Documents\\Projects\\WinSLAMM"
wd = "T:\\Projects\\Wisconsin_River\\Model_Inputs\\WinSLAMM_Inputs\\WinSLAMM\\cleanedCsvs"
dir_figs = "T:\\Projects\\Wisconsin_River\\Model_Inputs\\WinSLAMM_Inputs\\WinSLAMM\\figs"

if (!file.exists(dir_figs)){
	dir.create(dir_figs)
}

fls = list.files(wd, pattern = "pUS", full.names=T)
precip_codes = basename(fls)
precip_codes = substr(precip_codes, 1, 12)
precip_codes = unique(precip_codes)
combos = list(
	c("SL", "SA"),
	c("SL", "CL"),
	c("SA", "CL")
)
col_names = c(
	"Runoff.Volume..cf.",
	"Rain.Depth..in.",
	"Total.Solids..lbs.",
	"Particulate.Phosphorus..lbs.",
	"Filterable.Phosphorus..lbs.")
for (cde in precip_codes){
	cde_files = fls[grepl(cde, fls)]
	txtrs = substr(basename(cde_files),14,15)
	prcp_list = NULL
	for (fl in cde_files){
		df = read.csv(fl)
		df$Runoff.Volume..cf. = as.numeric(gsub(",|\\s","",df$Runoff.Volume..cf.))
		prcp_list = c(prcp_list, list(df))
	}
	names(prcp_list) = txtrs
	delta_df = data.frame(DATE=
		as.Date(prcp_list[[1]][,"Rain.Start.Date"], "%m/%d/%y"))
	for (clmn in col_names){
		print(paste("Working on column", clmn))
		for (cmbo in combos){
			soil_diff = prcp_list[[cmbo[1]]][,clmn] - prcp_list[[cmbo[2]]][,clmn]
			diff_clmn_name = gsub("\\.", "", clmn)
			diff_clmn_name = paste(diff_clmn_name, paste(cmbo, collapse="_"), sep="_")
			delta_df[diff_clmn_name] = abs(soil_diff)
		}
	}
}


plot(ParticulatePhosphoruslbs_SL_SA ~ DATE,
	data=delta_df[1:100,],
	type='l',
	col="red")

lines(ParticulatePhosphoruslbs_SL_CL ~ DATE,
	data=delta_df[1:100,],
	col="blue")

lines(ParticulatePhosphoruslbs_SA_CL ~ DATE,
	data=delta_df[1:100,],
	col="darkgreen")
legend(
	"topleft",
	legend=c("SL SA", "SL CL", "SA CL"),
	fill=c('red','blue','darkgreen'))
	