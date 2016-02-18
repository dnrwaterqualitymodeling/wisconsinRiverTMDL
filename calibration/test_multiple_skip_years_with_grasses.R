setwd("C:/TEMP/WRB.Sufi2.SwatCup")

fs = list.files(pattern="\\.mgt$")
for (f in fs) {
	ln1 = readLines(f, n=1)
	hru_info = strsplit(ln1, "\\s+|:")[[1]]
	hru_info = hru_info[hru_info != ""]
	lc = hru_info[11]
	if (!(lc %in% c("PAST", "CRRT"))) { next }
	mgt = readLines(f)
#	break
	mgt[29] = "              12    | NROT: number of years of rotation"
	mgt[33] = "          1.200  5                  0.00000"
	mgt[33:44] = "                17"
	print(f)
	writeLines(mgt, f)
}
