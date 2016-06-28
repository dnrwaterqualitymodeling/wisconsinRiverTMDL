library(dplyr)
library(tidyr)

db = src_sqlite("~/Documents/WRB.Sufi2.SwatCup/wrb_swat_spatial_db.sqlite")

ovs = tbl(db, "bias_corr_observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, flow_sim, flow_obs, sed_sim, sed_obs, tp_sim, tp_obs) %>%
	rename(flow_adj = flow_sim, sed_adj = sed_sim, tp_adj = tp_sim) %>%
	inner_join(
		tbl(db, "observed_vs_simulated") %>%
			select(rch, mon, yr, flow_sim, sed_sim, tp_sim),
	) %>%
	collect() %>%
	gather(p, value, flow_adj:tp_sim) %>%
	separate(p, c("p", "src"), sep="_") %>%
	spread(src, value) %>%
	mutate(
		adj = as.numeric(adj),
		obs = as.numeric(obs),
		sim = as.numeric(sim)
	) %>%
	filter(!is.na(obs))


summ_stat = ovs %>%
	group_by(rch, station_name, p) %>%
	summarize(
		pbias_sim = (sum(sim, na.rm=T) - sum(obs, na.rm=T)) / sum(obs, na.rm=T),
		pbias_adj = (sum(adj, na.rm=T) - sum(obs, na.rm=T)) / sum(obs, na.rm=T),
		rmse_sim = sqrt(sum((sim - obs)^2, na.rm=T)),
		rmse_adj = sqrt(sum((adj - obs)^2, na.rm=T)),
		nse_sim = 1 - (sum((obs - sim)^2, na.rm=T) / sum((obs - mean(obs, na.rm=T))^2, na.rm=T)),
		nse_adj = 1 - (sum((obs - adj)^2, na.rm=T) / sum((obs - mean(obs, na.rm=T))^2, na.rm=T))
	) %>%
	left_join(
		ovs %>%
		group_by(rch, station_name, p) %>%
		do(
			mod_sim = lm(sim ~ obs, data=.),
			mod_adj = lm(adj ~ obs, data=.)
		) %>%
		mutate(
			r2_sim = summary(mod_sim)$r.squared,
			r2_adj = summary(mod_adj)$r.squared
		) %>%
		select(-mod_sim, -mod_adj)
	)
	
copy_to(db, summ_stat, "bias_corr_summ_stat", temporary=F)


ovs = tbl(db, "bias_corr2_observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, flow_sim, flow_obs, sed_sim, sed_obs, tp_sim, tp_obs) %>%
	rename(flow_adj = flow_sim, sed_adj = sed_sim, tp_adj = tp_sim) %>%
	inner_join(
		tbl(db, "observed_vs_simulated") %>%
			select(rch, mon, yr, flow_sim, sed_sim, tp_sim),
	) %>%
	collect() %>%
	gather(p, value, flow_adj:tp_sim) %>%
	separate(p, c("p", "src"), sep="_") %>%
	spread(src, value) %>%
	mutate(
		adj = as.numeric(adj),
		obs = as.numeric(obs),
		sim = as.numeric(sim)
	) %>%
	filter(!is.na(obs))


summ_stat = ovs %>%
	group_by(rch, station_name, p) %>%
	summarize(
		pbias_sim = (sum(sim, na.rm=T) - sum(obs, na.rm=T)) / sum(obs, na.rm=T),
		pbias_adj = (sum(adj, na.rm=T) - sum(obs, na.rm=T)) / sum(obs, na.rm=T),
		rmse_sim = sqrt(sum((sim - obs)^2, na.rm=T)),
		rmse_adj = sqrt(sum((adj - obs)^2, na.rm=T)),
		nse_sim = 1 - (sum((obs - sim)^2, na.rm=T) / sum((obs - mean(obs, na.rm=T))^2, na.rm=T)),
		nse_adj = 1 - (sum((obs - adj)^2, na.rm=T) / sum((obs - mean(obs, na.rm=T))^2, na.rm=T))
	) %>%
	left_join(
		ovs %>%
			group_by(rch, station_name, p) %>%
			do(
				mod_sim = lm(sim ~ obs, data=.),
				mod_adj = lm(adj ~ obs, data=.)
			) %>%
			mutate(
				r2_sim = summary(mod_sim)$r.squared,
				r2_adj = summary(mod_adj)$r.squared
			) %>%
			select(-mod_sim, -mod_adj)
	)

copy_to(db, summ_stat, "bias_corr2_summ_stat", temporary=F)