dirs = c("C:/TEMP/WRB.Sufi2.SwatCup", "C:/TEMP/WRB.Sufi2.SwatCup/Backup")

for (dir in dirs) {
	files_mgt = list.files(dir, pattern="\\.mgt", full.names=T)
	for (file_mgt in files_mgt) {
		mgt = readLines(file_mgt)
		mgt_op = mgt[-(1:30)]
		op_code = substr(mgt_op, 17, 18)
		fert_code = substr(mgt_op, 22, 23)
		app_rate = substr(mgt_op, 34, 43)
		op_info = cbind(op_code, fert_code, app_rate)
		del_lines = apply(
			op_info,
			1,
			function(x) {
				if (x[1] == " 3" & x[2] == "55" & as.numeric(x[3]) > 1000) {
					return(T)
				} else {
					return(F)
				}
			}
		)
		if (all(!del_lines)) {
			next
		}
		print(file_mgt)
#		break
		mgt_op = mgt_op[-which(del_lines)]
		mgt = c(mgt[1:30], mgt_op)
		writeLines(mgt, file_mgt)
	}
}