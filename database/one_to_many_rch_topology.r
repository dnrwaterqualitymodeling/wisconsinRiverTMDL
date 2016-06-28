library(foreign)
library(dplyr)

file_tpl = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/hydro.dbf"
file_db = "C:/Users/ruesca/Documents/tmdl_db/wrb_swat_spatial_db.sqlite"

tpl = read.dbf(file_tpl) %>%
	select(FROM_NODE, TO_NODE) %>%
	mutate(from=FROM_NODE, to=TO_NODE) %>%
	select(from, to)

# Upstream
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

# Downstream
out = NULL
for (s in 1:337) {
	down = s
	repeat {
		down_drct = tpl$to[tpl$from == down[length(down)]]
		if (down_drct == 0) {
			break
		} else {
			down = c(down, down_drct)
		}
	}
	out_rows = data.frame(
		from_rch = s,
		to_rch = sort(down)
	)
	out = rbind(out, out_rows)
}

con = src_sqlite(file_db)
out = copy_to(con, out, "rch_topology_down", temporary=FALSE)
rm(con)
