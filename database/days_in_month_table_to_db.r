library(dplyr)
options(stringsAsFactors = FALSE)

db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")

all_days = data.frame(
	day = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")
)

days_in_mo = data.frame(
	yr = as.integer(format(seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month"), "%Y")),
	mon = as.integer(format(seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month"), "%m")),
	n_days = NA
)

n_days = all_days %>%
	mutate(mon = format(day, "%Y-%m")) %>%
	group_by(mon) %>%
	summarise(n_days = n()) %>%
	select(n_days)

days_in_mo$n_days = as.numeric(n_days$n_days)

days_in_mo = copy_to(db, days_in_mo, "days_in_mo", temporary=FALSE)
rm(db)
