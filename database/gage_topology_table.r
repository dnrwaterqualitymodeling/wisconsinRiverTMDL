library(dplyr)

db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")

n_up = tbl(db, "rch_topology") %>%
	group_by(to_rch) %>%
	summarize(n_up=n())

gage_tpl = tbl(db, "rch_topology") %>%
	left_join(n_up)

gages = tbl(db, sql("SELECT DISTINCT rch FROM observed_vs_simulated")) %>%
	collect() %>%
	filter(!(rch %in% c(138, 167)))

gage_tpl = gage_tpl %>%
	filter(to_rch %in% gages$rch)

min_n_up = gage_tpl %>%
	group_by(from_rch) %>%
	summarize(min_n_up=min(n_up))

gage_tpl = gage_tpl %>%
	left_join(min_n_up) %>%
	filter(n_up == min_n_up) %>%
	select(to_rch, from_rch)

gage_tpl = gage_tpl %>%
	collect() %>%
	mutate(
		to_rch = replace(
			to_rch,
			from_rch %in% c(87:89,327,91),
			as.integer(152)
		)
	) %>%
	mutate(
		to_rch = replace(
			to_rch,
			from_rch == 111,
			as.integer(162)
		)
	)
	
copy_to(db, gage_tpl, "gage_topology", temporary=FALSE)
