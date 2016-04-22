library(dplyr)

db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_spatial_db.sqlite")

rchs = sort((tbl(db, "station_id_rch_xref") %>% collect())$rch)

out = data.frame()
for (r in rchs) {
	print(r)
	o = tbl(db, "rch_topology") %>%
#	o = tbl(db, "rch_topology") %>%
#		collect() %>%
		filter(to_rch == r) %>%
		inner_join(tbl(db, "output_sub_monthly"), c("from_rch" = "sub")) %>%
#		inner_join(d, c("from_rch" = "SUB")) %>%
		collect() %>%
		group_by(YR, MON) %>%
		summarize(
			pcp = sum(PRECIP),
			gw_q = sum(GW_Q),
			wyld = sum(WYLD)
		) %>%
		mutate(date=as.Date(paste(YR,MON,1,sep="-")))
	pcp_trend = lm(pcp ~ date, data=o)
	gw_trend = lm(gw_q ~ date, data=o)
	wyld_trend = lm(wyld ~ date, data=o)
	row = data.frame(
		rch = r,
		pcp_p_value = summary(pcp_trend)$coefficients[2,4],
		gw_p_value = summary(gw_trend)$coefficients[2,4],
		wyld_p_value = summary(wyld_trend)$coefficients[2,4]
	)
	out = rbind(out, row)
}

