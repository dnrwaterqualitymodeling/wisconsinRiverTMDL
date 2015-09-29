txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut_cnop_adjust"
dir_out = "C:/Users/ruesca/Desktop/fescue_change"

setwd(txtinout)

fs = list.files(pattern="\\.mgt$")

for (f in fs) {
	file_out = paste(dir_out, f, sep="/")
	mgt = readLines(f)
	lu = strsplit(mgt[1], "\\s|:")[[1]][12]
	if (lu == "PAST") {
		substr(mgt[31], 22, 23) = "38"
		print(file_out)
		writeLines(mgt, file_out)
	}
}