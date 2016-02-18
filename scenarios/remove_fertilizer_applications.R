dir_swat_prj = "C:/TEMP/WRB.Sufi2.SwatCup"
dir_out = "C:/TEMP/mgt_no_fert"

mgt_files = list.files(dir_swat_prj, pattern="\\.mgt$", full.names=T)
for (mgt_file in mgt_files) {
	print(mgt_file)
	mgt = readLines(mgt_file)
	if (length(mgt) <= 34) {next}
	mgt_op = mgt[-(1:30)]
	mgt_op = mgt_op[!grepl(" 3", substr(mgt_op, 16, 18))]
	mgt = c(mgt[1:30], mgt_op)
	writeLines(mgt, paste(dir_out, basename(mgt_file), sep="/"))
}