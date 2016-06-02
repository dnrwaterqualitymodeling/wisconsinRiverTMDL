library(dplyr)
library(stringr)
library(tidyr)
library(RODBC)

db = src_sqlite("C:/Users/ruesca/Documents/tmdl_db/wrb_swat_spatial_db.sqlite")

bc = collect(tbl(db, "bias_corr_observed_vs_simulated")) %>%
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

lk_mod = function (x, bc, lks, lk_method = c("vol", "cb1", "cb2"), optim=T) {
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
	lk_method="cb1",
	optim=T
)

o_cb2 = optim(
	priors_cb2,
	lk_mod,
	bc=bc,
	lks=lks,
	lk_method="cb2",
	optim=T
)

o_vol = optimize(
	lk_mod,
	lower=0.5,
	upper=20,
	bc=bc,
	lks=lks,
	lk_method="vol",
	optim=T
)

adj_cb1 = lk_mod(
	o_cb1$par,
	bc=bc,
	lks=lks,
	lk_method="cb1",
	optim=F
)

adj_cb2 = lk_mod(
	o_cb2$par,
	bc=bc,
	lks=lks,
	lk_method="cb2",
	optim=F
)

adj_vol = lk_mod(
	o_vol$minimum,
	bc=bc,
	lks=lks,
	lk_method="vol",
	optim=F
)

bc$tp_cb1 = adj_cb1$tp_adj
bc$tp_cb2 = adj_cb2$tp_adj
bc$tp_vol = adj_vol$tp_adj

rchs = unlist(lapply(lks, function(x) {x$rch_id}))

pdf("test.pdf", width = 11, height = 8.5)
for (rch_i in rchs) {
	plot_data = bc %>%
		filter(rch == rch_i)
	plot(
		tp_sim ~ date
		,data=plot_data
		,type="l"
		,ylab="TP (kg/mo)"
		,main=plot_data$station_name[1]
		,lwd=2
#		,log="y"
	)
	lines(tp_cb1 ~ date, data=plot_data, lwd=2, col="#ff7f00")
	lines(tp_cb2 ~ date, data=plot_data, lwd=2, col="#377eb8")
	lines(tp_vol ~ date, data=plot_data, lwd=2, col="#4daf4a")
	lines(tp_obs ~ date, data=plot_data, lwd=2, col="#984ea3")
	legend(
		"topright",
		legend=c(
			"Original bias-corr",
			"LOADEST",
			"Canfield-Bachmann 1",
			"Canfield-Bachmann 2",
			"Vollenweider"
		),
		col=c("black", "#984ea3", "#ff7f00", "#377eb8", "#4daf4a"),
		lwd=2
	)
}
dev.off()
