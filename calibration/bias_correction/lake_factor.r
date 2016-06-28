library(dplyr)
library(stringr)
library(tidyr)
library(RODBC)

db = src_sqlite("C:/Users/ruesca/Documents/tmdl_db/wrb_swat_spatial_db.sqlite")

bc = collect(
		tbl(db, "bias_corr_cum") %>%
		select(-sed) %>%
		rename(flow_sim = flow, tp_sim = tp)
	) %>%
	left_join(
		collect(
			tbl(db, "bias_corr_observed_vs_simulated")) %>%
			select(rch, station_name, mon, yr, tp_obs)
	) %>%
	left_join(collect(tbl(db, "days_in_mo")), by = c("mon"="MON", "yr"="YR")) %>%
	mutate(
		date = as.Date(paste(yr, mon, "1", sep="-")),
		tp_obs = as.numeric(tp_obs)
	) %>%
	rename(n_days = N_DAYS) %>%
	select(rch, station_name, mon, yr, date, n_days, flow_sim, tp_sim, tp_obs) %>%
	arrange(rch, date)
tpl_down = collect(tbl(db, "rch_topology_down"))

lks = list(
	list(
		lk = "Lake Wausau",
		rch_id = 154,
		md = 2.145356537,
		v = 16652000
	),
	list(
		lk = "Big Eau Pleine Reservoir",
		rch_id = 87,
		md = 4.56528739,
		v = 126185000
	),
	list(
		lk = "Lake Du Bay",
		rch_id = 81,
		md = 2.538081549,
		v = 68334800
	),
	list(
		lk = "Wisconsin R Fl C3-Stevens Pt",
		rch_id = 148,
		md = 1.732970645,
		v = 14678400
	),
	list(
		lk = "Biron Flowage",
		rch_id = 145,
		md = 2.336888426,
		v = 20105700
	),
	list(
		lk = "Petenwell Lake",
		rch_id = 74,
		md = 4.303435125,
		v = 401251000
	),
	list(
		lk = "Castle Rock Lake",
		rch_id = 59,
		md = 3.756491817,
		v = 212220000
	),
	list(
		lk = "Lake Wisconsin",
		rch_id = 1,
		md = 4.062303517,
		v = 147956000
	)
)

lk_mod = function (x, bc, lks, tpl_down, lk_method = c("vol", "cb1", "cb2"), optim=T) {
	bc$tp_adj = bc$tp_sim
	for (lk in lks) {
		print(lk$lk)
		bc_lk = filter(bc, rch == lk$rch_id) %>%
			arrange(yr, mon)
		downs = tpl_down$to_rch[tpl_down$from_rch == lk$rch_id]
		sa = (lk$v / lk$md)
		L = (bc_lk$tp_adj * 1e6) / sa
		z = lk$md
		v = lk$v
		q = bc_lk$flow_sim * 86400 * bc_lk$n_days # m3 per mo
		rho = q / v
		if (lk_method == "vol") {
			sigma = (x[1]/z)
			tp_conc = L / (z*(sigma + rho))
		} else if (lk_method == "cb1") {
			sigma = x[1] * (L/z)^x[2]
			tp_conc = L / (z*(sigma + rho))
		} else if (lk_method == "cb2") {
			sigma = x[1] * (L/z)^x[2]
			tp_conc = (x[3] * L) / (z*(sigma + rho))
		}
		tp_load = tp_conc * bc_lk$flow_sim * bc_lk$n_days * 0.0864
		tp_d = bc_lk$tp_adj - tp_load
		bc$tp_adj[bc$rch %in% downs] = 
			bc$tp_adj[bc$rch %in% downs] - tp_d
	}
	rchs = unlist(lapply(lks, function(x) {x$rch_id}))
	if (optim) {
		bc = bc %>%
			filter(!is.na(bc$tp_obs), rch %in% rchs)
		if (any(bc$tp_adj <= 0)) {return(1e99)}
		resid_log = log(bc$tp_adj) - log(bc$tp_obs)
		sse = sum(resid_log^2)
		rmse = sqrt(sse / nrow(bc))
		resid = bc$tp_adj - bc$tp_obs
		pbias = abs(sum(resid, na.rm=T)) / sum(bc$tp_obs)
		return((1 + pbias) * rmse)
	} else {
		return(bc)
	}
}

priors_cb1 = c(0.114, 0.589)
priors_cb2 = c(0.114, 0.589, 0.8)

o_cb1 = optim(
	priors_cb1,
	lk_mod,
	bc=bc,
	lks=lks,
	tpl_down=tpl_down,
	lk_method="cb1",
	optim=T
)

o_cb2 = optim(
	priors_cb2,
	lk_mod,
	bc=bc,
	lks=lks,
	tpl_down=tpl_down,
	lk_method="cb2",
	optim=T
)

o_vol = optimize(
	lk_mod,
	lower=0.5,
	upper=20,
	bc=bc,
	lks=lks,
	tpl_down=tpl_down,
	lk_method="vol",
	optim=T
)

adj_cb1 = lk_mod(
	o_cb1$par,
	bc=bc,
	lks=lks,
	tpl_down=tpl_down,
	lk_method="cb1",
	optim=F
)

adj_cb2 = lk_mod(
	o_cb2$par,
	bc=bc,
	lks=lks,
	tpl_down=tpl_down,
	lk_method="cb2",
	optim=F
)

adj_vol = lk_mod(
	o_vol$minimum,
	bc=bc,
	lks=lks,
	tpl_down=tpl_down,
	lk_method="vol",
	optim=F
)

bc$tp_cb1 = adj_cb1$tp_adj
bc$tp_cb2 = adj_cb2$tp_adj
bc$tp_vol = adj_vol$tp_adj

routing_adjust = function (
	x,
	bc,
	lks=lks,
	optim=T,
	lag=T,
	season=T,
	start_yr=2002
) {
	
	bc = rbind(bc[1:7,], bc)
	bc[1:7,"mon"] = 6:12
	bc[1:7,"yr"] = start_yr - 1
	
	fill_vals = bc %>%
		filter(mon %in% 6:12) %>%
		group_by(mon) %>%
		summarise(mn = mean(sim))
	bc[1:7, "sim"] = fill_vals$mn
	bc$adj = NA
	if (lag) {
		for (i in 8:nrow(bc)) {
			bc$adj[i] = sum(bc[i:(i-7),"sim"] * exp(x[1] + x[2]*1:8))
		}
	}
	if (season) {
		bc = bc %>%
			mutate(
				w = x[3] + x[4] * sin(((2*pi)/x[5])*mon + x[6]),
				adj = adj * w
			)
	}
	bc = bc %>%
		filter(yr >= start_yr) %>%
		select(rch,mon,yr,obs,sim,adj) %>%
		arrange(yr,mon)
	if (optim) {
		bc = bc %>%
			filter(!is.na(bc$obs), rch %in% rchs)
		if (any(bc$adj <= 0)) {return(1e99)}
		resid_log = log(bc$adj) - log(bc$obs)
		sse = sum(resid_log^2)
		rmse = sqrt(sse / nrow(bc))
		resid = bc$adj - bc$obs
		pbias = abs(sum(resid, na.rm=T)) / sum(bc$obs)
		return((1 + pbias) * rmse)
	} else {
		return(bc)
	}	
}

bc_res = bc %>%
	rename(sim=tp_sim, obs=tp_obs) %>%
	select(rch, station_name, mon, yr, sim, obs)

rchs = unlist(lapply(lks, function(x) {x$rch_id}))

priors=c(-0.4, -0.7, 1.0, 0.3, 11, 1)
diagnostics = NULL
ovs_adj = NULL
for (lk in lks) {
	print(lk$lk)
	bc_lk = filter(bc_res, rch == lk$rch_id) %>%
		arrange(yr, mon)
	o = optim(
		priors,
		routing_adjust,
		bc=bc_lk,
		lks=lks,
		optim=T,
		lag=T,
		season=T,
		start_yr=2002
	)
	adj = routing_adjust(
		o$par,
		bc=bc_lk,
		lks=lks,
		optim=F,
		lag=T,
		season=T,
		start_yr=2002
	)
	tp_d = adj$sim - adj$adj
	downs = tpl_down$to_rch[tpl_down$from_rch == lk$rch_id]
	bc_res$sim[bc_res$rch %in% downs] = 
		bc_res$sim[bc_res$rch %in% downs] - tp_d
	
	ovs_adj = rbind(ovs_adj, adj)
	
	adj = adj %>%
		filter(!is.na(sim), !is.na(obs))
	resid = cbind(
		adj$sim - adj$obs,
		adj$adj - adj$obs
	)
	sse = c(sum(resid[,1]^2), sum(resid[,2]^2))
	
	diagnostics_row = data.frame(
		rch=bc_lk$rch[1],
		station_name=bc_lk$station_name[1],
		sample_size=nrow(adj),
		df=nrow(adj) - length(o$par),
		p1=o$par[1],
		p2=o$par[2],
		p3=o$par[3],
		p4=o$par[4],
		p5=o$par[5],
		p6=o$par[6],
		rmse=sqrt(sse[1] / nrow(adj)),
		rmse_adj=sqrt(sse[2] / nrow(adj)),
		pbias=sum(resid[,1], na.rm=T) / sum(adj$obs),
		pbias_adj=sum(resid[,2], na.rm=T) / sum(adj$obs),
		r2=summary(
			lm(adj$sim ~ adj$obs)
		)$r.squared,
		r2_adj=summary(
			lm(adj$adj ~ adj$obs)
		)$r.squared
	)
	diagnostics = rbind(diagnostics, diagnostics_row)
}

#copy_to(db, ovs_adj, "bias_corr_reservoir_observed_vs_simulated", temporary=F)
copy_to(db, diagnostics, "routing_coefficients_reservoir", temporary=F)

bc$tp_rd = bc_res$sim
bc_db = bc %>%
	select(rch, mon, yr, tp_cb1, tp_cb2, tp_vol, tp_rd)
copy_to(db, bc_db, "bias_corr_reservoir", temporary=F)

pdf("test.pdf", width = 11, height = 8.5)
for (rch_i in rchs) {
	plot_data = bc %>%
		filter(rch == rch_i)
	ylim = with(
		plot_data,
		range(c(tp_sim, tp_obs, tp_cb1, tp_cb2, tp_vol, tp_rd), na.rm=T)
	)
	plot(
		tp_sim ~ date
		,data=plot_data
		,ylim=ylim
		,type="l"
		,ylab="TP (kg/mo)"
		,main=plot_data$station_name[1]
		,lwd=2
		,log="y"
	)
	lines(tp_cb1 ~ date, data=plot_data, lwd=2, col="#ff7f00")
	lines(tp_cb2 ~ date, data=plot_data, lwd=2, col="#377eb8")
	lines(tp_vol ~ date, data=plot_data, lwd=2, col="#4daf4a")
	lines(tp_rd ~ date, data=plot_data, lwd=2, col="#a65628")
	lines(tp_obs ~ date, data=plot_data, lwd=2, col="#984ea3")
	legend(
		"topright",
		legend=c(
			"Original bias-corr",
			"LOADEST",
			"Canfield-Bachmann 1",
			"Canfield-Bachmann 2",
			"Vollenweider",
			"Ruesch-Diebel"
		),
		col=c("black", "#984ea3", "#ff7f00", "#377eb8", "#4daf4a", "#a65628"),
		lwd=2
	)
}
dev.off()

bc = bc %>%
	select(rch, station_name, mon, yr, tp_sim, tp_obs, tp_cb1, tp_cb2, tp_vol, tp_rd)

