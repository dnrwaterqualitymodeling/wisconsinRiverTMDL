library(foreign)
library(dplyr)

file_tpl = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/hydro.dbf"
file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"

tpl = read.dbf(file_tpl) %>%
	select(FROM_NODE, TO_NODE) %>%
	mutate(from=FROM_NODE, to=TO_NODE) %>%
	select(from, to)

out = NULL
for (s in 1:337) {
	up = s
	repeat {
		st_l = length(up)
		up_drct = tpl$from[tpl$to %in% up]
		up = c(up, up_drct)
		up = unique(up)
		end_l = length(up)
		if (st_l == end_l) { break }
	}
	out_rows = data.frame(
		to_rch = s,
		from_rch = sort(up)
	)
	out = rbind(out, out_rows)
}

con = src_sqlite(file_db)
out = copy_to(con, out, "rch_topology", temporary=FALSE)
rm(con)
