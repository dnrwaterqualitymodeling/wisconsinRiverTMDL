setwd("C:/Users/ruesca/Desktop/CNOP_adjust/")

fs = list.files(pattern="\\.mgt$")
for (f in fs) {
	ln1 = readLines(f, n=1)
	hru_info = strsplit(ln1, "\\s+|:")[[1]]
	hru_info = hru_info[hru_info != ""]
	lc = hru_info[11]
	if (!(lc %in% c("PAST", "CRRT"))) { next }
	mgt = readLines(f)
#	break
	mgt = mgt[-33]
	print(f)
	writeLines(mgt, f)
}

