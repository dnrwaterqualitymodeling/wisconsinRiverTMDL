dir_swat_prj = "C:/TEMP/WRB.Sufi2.SwatCup"

files_ps = list.files(dir_swat_prj, pattern="^[0-9]+p\\.dat$", full.names=T)
files_ps = files_ps[file.info(files_ps)$size > 1000]

sums = c(0,0,0)
for (file_ps in files_ps) {
	print(file_ps)
	data_ps = read.table(file_ps, skip=5, header=T)
	data_ps = subset(data_ps, YEAR >= 2002)
	data_ann = aggregate(
		cbind(FLODAY, SEDDAY, PDAY = ORGPDAY + MINPDAY) ~ YEAR,
		data_ps,
		sum,
		na.rm=T
	)
	data_ann = aggregate(
		cbind(FLODAY, SEDDAY, PDAY) ~ 1,
		data_ann,
		mean,
		na.rm=T
	)
	sums = sums + data_ann
}

