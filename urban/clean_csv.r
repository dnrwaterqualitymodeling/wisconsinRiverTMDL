options(stringsAsFactors=FALSE)
#wd = "C:\\Users\\hydrig\\Documents\\Projects\\WinSLAMM"
wd = "T:\\Projects\\Wisconsin_River\\Model_Inputs\\WinSLAMM_Inputs\\WinSLAMM"

dirs = list.files(wd, pattern = "pUS.*[0-9]$", full.names=T)
dir_out = paste(wd, "cleanedCsvs",sep="/")
if (!file.exists(dir_out)){
	dir.create(dir_out)
}
for (dir in dirs){
	csvs = list.files(dir, pattern="*.csv$",full.names=T)
	dir_name = basename(dir)
	print(dir_name)
	for (csv in csvs){
		dat = readLines(csv)
		hdr = dat[1]
		rowNum = length(dat)
		dat = dat[1:(rowNum-14)]
		fl_name = basename(csv)
		fl_name = gsub(" -Output|-Output", "", fl_name)
		fl_name = paste(dir_out, fl_name, sep="/")
		writeLines(dat, fl_name)
		
	}
}