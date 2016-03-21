library(dplyr)
library(stringr)

dir_swat = "C:/TEMP/WRB.Sufi2.SwatCup"
file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"
setwd(dir_swat)

files_ps = list.files(pattern="^[0-9]+p\\.dat$")
files_ps = file.info(files_ps)
files_ps = files_ps %>%
	mutate(filename=row.names(files_ps)) %>%
	filter(size > 1e3) %>%
	select(filename)	
files_ps = files_ps$filename

out_d = NULL
for (file_ps in files_ps) {
	print(file_ps)
	d = read.table(file_ps, skip=5, header=T)
	d = d %>% 
		rename(YR = YEAR) %>%
		filter(YR >= 2002) %>%
		mutate(
			DATE = as.Date(
				paste(YR, DAY),
				format="%Y %j"
			),
			MON = as.integer(format(DATE, "%m"))
		) %>%
		select(MON, YR, FLODAY, SEDDAY, ORGPDAY, MINPDAY) %>%
		group_by(MON, YR) %>%
		summarise(
			FLOW_OUT = mean(FLODAY, na.rm=T),
			SED_OUT = sum(SEDDAY, na.rm=T),
			ORGP_OUT = sum(ORGPDAY, na.rm=T),
			MINP_OUT = sum(MINPDAY, na.rm=T),
			TOT_P = sum(ORGPDAY + MINPDAY, na.rm=T)
		) %>%
		mutate(RCH=as.integer(str_extract(file_ps, "^[0-9]{1,3}"))) %>%
		select(RCH,MON,YR,FLOW_OUT,SED_OUT,ORGP_OUT,MINP_OUT,TOT_P)
	out_d = rbind(out_d, d)	
}

con = src_sqlite(file_db)
out_d = copy_to(con, out_d, "point_source_ms4_monthly", temporary=FALSE,
	indexes=list("RCH", c("RCH", "MON", "YR")))
rm(con)
