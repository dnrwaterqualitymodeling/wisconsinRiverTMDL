sc_dir = "C:/Users/ruesca/Desktop/WRB_generalized.Sufi2.SwatCup"
setwd(sc_dir)

pnd_files = list.files(pattern="\\.pnd$")

for (pnd_file in pnd_files) {
	pnd = readLines(pnd_file)
	pnd[14] = 
		"              15    | NDTARG: Number of days needed to reach target storage from current pond storage"
	writeLines(pnd, pnd_file)
}

setwd("Backup")


for (pnd_file in pnd_files) {
	pnd = readLines(pnd_file)
	pnd[14] = 
		"              15    | NDTARG: Number of days needed to reach target storage from current pond storage"
	writeLines(pnd, pnd_file)
}
