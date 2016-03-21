library(stringr)
library(dplyr)

file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"
file_obs = "C:/TEMP/WRB.Sufi2.SwatCup/Iterations/draft2/Sufi2.In/observed.txt"

obs_raw = readLines(file_obs)
obs_raw = obs_raw[grepl("_", obs_raw)]


out_tbl = NULL
i = 1
repeat {
	print(i)
	if (!str_detect(obs_raw[i], "^[0-9]")) {
		v = str_split(obs_raw[i], "\\t")[[1]][1]
		s = as.integer(str_extract(v, "[0-9]+"))
		v = str_extract(v, "([A-Z]|_)+")
		v = str_replace(v, "_$", "")
		i = i + 1
	} else {
		yrs = NULL
		repeat {
			if (i > length(obs_raw)) { break }
			if (str_detect(obs_raw[i], "^[0-9]")) {
				yr = str_split(obs_raw[i], "\\t")[[1]][2]
				yr = as.integer(str_extract(yr, "20[0-9]{2}$"))
				yrs = unique(c(yrs, yr))
				i = i + 1
			} else { break }
		}
		out_rows = data.frame(
			rch=s,
			variable=v,
			calibration_years=yrs
		)
		out_tbl = rbind(out_tbl, out_rows)
	}
	if (i > length(obs_raw)) { break }
}

con = src_sqlite(file_db)
out_tbl = copy_to(con, out_tbl, "calibration_data", temporary=F)
