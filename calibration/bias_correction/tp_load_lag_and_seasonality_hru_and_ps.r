library(dplyr)

db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")

ovs = tbl(db, "tp_hru_urb_cumulative") %>%
	left_join(tbl(db, "tp_observed_minus_point_src_and_perm_ms4")) %>%
	select(rch, station_name, mon, yr, tp_obs, tp_hru_urb_kg) %>%
	collect() %>%
	mutate(tp_obs = as.numeric(tp_obs)) %>%
	mutate(date=as.Date(paste(yr,mon,"1",sep="-")))

lag = function (x, ovs, optim) {
	for (i in 8:nrow(ovs)) {
		ovs$tp_adj[i] = sum(
			ovs$tp_hru_urb_kg[i] * exp(x[1] + x[2]*1),
			ovs$tp_hru_urb_kg[i-1] * exp(x[1] + x[2]*2),
			ovs$tp_hru_urb_kg[i-2] * exp(x[1] + x[2]*3),
			ovs$tp_hru_urb_kg[i-3] * exp(x[1] + x[2]*4),
			ovs$tp_hru_urb_kg[i-4] * exp(x[1] + x[2]*5),
			ovs$tp_hru_urb_kg[i-5] * exp(x[1] + x[2]*6),
			ovs$tp_hru_urb_kg[i-6] * exp(x[1] + x[2]*7),
			ovs$tp_hru_urb_kg[i-7] * exp(x[1] + x[2]*8)
		)
	}
	ovs = ovs %>%
		mutate(
			w = x[3] + x[4] * sin(((2*pi)/x[5])*mon + x[6]),
			tp_adj = tp_adj * w
		)
	if (optim) {
		ovs = ovs %>%
			filter(!is.na(tp_adj), !is.na(tp_obs))
		if (any(ovs$tp_adj <= 0)) {return(1e99)}
		resid_log = log(ovs$tp_adj) - log(ovs$tp_obs)
		sse = sum(resid_log^2)
		rmse = sqrt(sse / nrow(ovs))
		resid = ovs$tp_adj - ovs$tp_obs
		pbias = abs(sum(resid, na.rm=T)) / sum(ovs$tp_obs)
		return((1 + pbias) * rmse)
	} else {
		return(
			ovs %>%
				select(rch, station_name, mon, yr, tp_obs, tp_hru_urb_kg, tp_adj)
		)
	}		
}

adjust_rch = function(ovs_rch, priors) {
	if (sum(!is.na(ovs_rch$tp_obs)) < 10) { return(NULL) }
	o = optim(
		priors,
		lag,
		ovs=ovs_rch,
		optim=T
	)
	out_ovs_adj = lag(o$par, ovs_rch, optim=F)
	ovs_adj_rows = out_ovs_adj %>%
		filter(!is.na(tp_hru_urb_kg), !is.na(tp_obs))
	resid = cbind(
		ovs_adj_rows$tp_hru_urb_kg - ovs_adj_rows$tp_obs,
		ovs_adj_rows$tp_adj - ovs_adj_rows$tp_obs
	)
	sse = c(sum(resid[,1]^2), sum(resid[,2]^2))
	diagnostics = data.frame(
		rch=rch_i,
		station_name=id,
		sample_size=nrow(ovs_adj_rows),
		df=nrow(ovs_adj_rows) - length(o$par),
		p1=o$par[1],
		p2=o$par[2],
		p3=o$par[3],
		p4=o$par[4],
		p5=o$par[5],
		p6=o$par[6],
		rmse=sqrt(sse[1] / nrow(ovs_adj_rows)),
		rmse_adj=sqrt(sse[2] / nrow(ovs_adj_rows)),
		pbias=sum(resid[,1], na.rm=T) / sum(ovs_adj_rows$tp_obs),
		pbias_adj=sum(resid[,2], na.rm=T) / sum(ovs_adj_rows$tp_obs),
		r2=summary(
			lm(
				ovs_adj_rows$tp_hru_urb_kg ~ ovs_adj_rows$tp_obs,
				data=ovs_rch
			)
		)$r.squared,
		r2_adj=summary(
			lm(
				ovs_adj_rows$tp_adj ~ ovs_adj_rows$tp_obs,
				data=ovs_rch
			)
		)$r.squared
	)
	return(list(ovs_adj=out_ovs_adj, diagnostics=diagnostics))
}

# Start by optimizing at gage sites without any sites above
rchs = unique(ovs[c("rch", "station_name")])
rchs = rchs[!is.na(rchs$station_name),]

rchs1 = c(78,140,141,142,149,150,151,152,155,157,158,159,162,184,195,268,326)

priors = c(-0.4, -0.7, 1.0, 0.3, 11, 1)
ovs_adj = NULL
diagnostics = NULL
for (rch_i in rchs1) {
	ovs_rch = ovs %>%
		filter(rch == rch_i) %>%
		arrange(yr, mon)
	ovs_rch$tp_hru_urb_kg[ovs_rch$tp_hru_urb_kg == 0] = 1
	id = ovs_rch$station_name[!is.na(ovs_rch$station_name)][1]
	print(id)
	ovs_adj_rows = adjust_rch(ovs_rch, priors)
	ovs_adj = rbind(ovs_adj, ovs_adj_rows[[1]])
	diagnostics = rbind(diagnostics, ovs_adj_rows[[2]])
}


tp_hru_urb = tbl(db, "tp_hru_urb") %>%
	mutate(tp_hru_kg_adj)
	

# Deal with BEP by applying coefficients fitted at Stratford to all subbasins
# that drain to the reservoir below Stratford (except for Fenwood and Freeman)

subs_above_bep = tbl(db, "rch_topology") %>%
	filter(to_rch == 87) %>%
	select(from_rch)




ovs_adj = copy_to(
	db,
	ovs_adj,
	"bias_corr_tp_observed_vs_simulated_hru_urb",
	temporary=F
)
diagnostics = copy_to(
	db,
	diagnostics,
	"bias_corr_tp_diagnostics_hru_urb",
	temporary=F
)













