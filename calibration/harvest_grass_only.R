setwd("C:/TEMP/WRB.Sufi2.SwatCup")

fs = list.files(pattern="\\.mgt$")
for (f in fs) {
	ln1 = readLines(f, n=1)
	hru_info = strsplit(ln1, "\\s+|:")[[1]]
	hru_info = hru_info[hru_info != ""]
	lc = hru_info[11]
	if (!(lc %in% c("PAST", "CRRT"))) { next }
	mgt = readLines(f)
	mgt[32] = "          1.200  8                  0.00000"
	print(f)
	writeLines(mgt, f)
}

