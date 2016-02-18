dir = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut"
dir_out = "C:/Users/ruesca/Desktop/grass_mgt_files"

files_mgt = list.files(dir, pattern="\\.mgt$", full.names=T)
for (file_mgt in files_mgt) {
	ln1 = readLines(file_mgt, n=1)
	ln1 = strsplit(ln1, "\\s+|:")[[1]]
	ln1 = ln1[ln1 != ""]
	lu = ln1[11]
	if (lu %in% c("PAST","CRRT")) {
		print(file_mgt)
		file_out = paste(dir_out, "/", basename(file_mgt), sep="")
		file.copy(file_mgt, file_out)
	}
}