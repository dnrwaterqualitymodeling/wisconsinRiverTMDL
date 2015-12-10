dirs = c(
	"C:/TEMP/WRB.Sufi2.SwatCup",
	"C:/TEMP/WRB.Sufi2.SwatCup/Backup"
)
for (dir in dirs) {
	setwd(dir)	
	fs = list.files(pattern="\\.mgt$")
	for (f in fs) {
		mgt = readLines(f)
		lu = strsplit(mgt[1], "\\s|:")[[1]][12]
		if (lu %in% c("PAST","FESC")) {
			substr(mgt[31], 22, 23) = " 5"
			writeLines(mgt, f)
		}
	}
}