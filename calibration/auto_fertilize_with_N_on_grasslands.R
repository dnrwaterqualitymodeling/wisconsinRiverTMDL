setwd("C:/TEMP/WRB.Sufi2.SwatCup")

#ADD

fs = list.files(pattern="\\.mgt$")
af_op = "          0.010 11    4             0.75000 100.00   500.00000 2.00   1.00"
for (f in fs) {
	ln1 = readLines(f, n=1)
	hru_info = strsplit(ln1, "\\s+|:")[[1]]
	hru_info = hru_info[hru_info != ""]
	lc = hru_info[11]
	if (!(lc %in% c("PAST", "CRRT"))) { next }
	mgt = readLines(f)
	if (mgt[31] == af_op) { next }
	mgt = c(mgt[1:30], af_op, mgt[31:length(mgt)])
	print(f)
	writeLines(mgt, f)
}

#REMOVE

fs = list.files(pattern="\\.mgt$")
af_op = "                11    4             1.00000 100.00   500.00000 2.00   1.00"
for (f in fs) {
	ln1 = readLines(f, n=1)
	hru_info = strsplit(ln1, "\\s+|:")[[1]]
	hru_info = hru_info[hru_info != ""]
	lc = hru_info[11]
	if (!(lc %in% c("PAST", "CRRT"))) { next }
	mgt = readLines(f)
	if (mgt[31] == af_op) {
		mgt = mgt[-31]
		print(f)
		writeLines(mgt, f)
	}
}

