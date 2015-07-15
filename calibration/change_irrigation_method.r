dir = "C:/Users/ruesca/Desktop/WRB.Sufi2.SwatCup"

dirs = c(dir, paste(dir, "Backup", sep="/"))

for (d in dirs) {
	print(d)
	setwd(d)
	files = list.files(pattern="^0.*\\.mgt$")
	for (f in files) {
		mgt = readLines(f, warn=F)
		flag = mgt[19] == "               3    | IRRSC: irrigation code"
		if (flag) {
			print(f)
			# mgt[19] = "               3    | IRRSC: irrigation code"
			# writeLines(mgt, f)
		}
	}
}