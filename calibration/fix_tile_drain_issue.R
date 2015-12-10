dirs = c(
	"C:/TEMP/WRB.Sufi2.SwatCup",
	"C:/TEMP/WRB.Sufi2.SwatCup/Backup"
)
nat_lc = c("WATR","URML","PAST","WETN","ONIO","FRSD","FRSE","FRST","CRRT")
for (dir in dirs) {
	files_mgt = list.files(dir, pattern="\\.mgt$", full.names=T)
	for (file_mgt in files_mgt) {
		hdr = readLines(file_mgt, 1)
		hdr = strsplit(hdr, " |:")[[1]]
		hdr = hdr[-which(hdr == "")]
		sol = as.integer(hdr[13])
		if (sol < 500) {
			mgt = readLines(file_mgt)
			if (substr(mgt[25], 10, 16) == "900.000") {
				print(file_mgt)
				substr(mgt[25:27], 10, 16) = "  0.000"
				writeLines(mgt, file_mgt)				
			}
		} else {
			mgt = readLines(file_mgt)
			lu = hdr[11]
			if (lu %in% nat_lc) {
				print(file_mgt)
				substr(mgt[25:27], 10, 16) = "  0.000"
				writeLines(mgt, file_mgt)
			}
		}
	}
}