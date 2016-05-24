library(dplyr)
library(tidyr)

db = src_sqlite("C:/Users/ruesca/Documents/tmdl_db/wrb_swat_spatial_db.sqlite")

d = collect(tbl(db, "bias_corr_observed_vs_simulated"))

d_tp = gather(d, src, tp, tp_sim:tp_obs) %>%
	mutate(tp = as.numeric(tp))
	filter(rch %in% c(1, ))

boxplot(tp ~ src * rch, data=d_tp, log="y", col=c("red", "blue"))



for (rch_i in unique(d$rch)) {
	d_rch = filter(d, rch == rch_i)
	
}

