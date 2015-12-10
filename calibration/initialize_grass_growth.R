setwd("C:/TEMP/WRB.Sufi2.SwatCup/Backup")

fs = list.files(pattern="\\.mgt$")
for (f in fs) {
	ln1 = readLines(f, n=1)
	hru_info = strsplit(ln1, "\\s+|:")[[1]]
	hru_info = hru_info[hru_info != ""]
	lc = hru_info[11]
	if (!(lc %in% c("PAST", "CRRT"))) { next }
	print(f)
	mgt = readLines(f)
	substr(mgt[4], 16, 16) = "1"
	substr(mgt[5], 15, 16) = "36"
	writeLines(mgt, f)
}
