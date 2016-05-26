library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(parallel)

db = src_sqlite("C:/Users/ruesca/Documents/tmdl_db/wrb_swat_spatial_db.sqlite")
n_cores = 8
start_yr = 2002

ovs = tbl(db, "sub_cum") %>%
	left_join(tbl(db, "observed_minus_point_src_and_perm_ms4")) %>%
	select(rch, station_name, mon, yr, flow_obs, flow, sed_obs, sed, tp_obs, tp) %>%
	collect() %>%
	mutate(tp_obs = as.numeric(tp_obs)) %>%
	mutate(date=as.Date(paste(yr,mon,"1",sep="-")))

routing_adjust = function (
	x,
	ovs_rch,
	optim=T,
	lag=T,
	season=T,
	start_yr=2002
	) {
	
	ovs_rch = rbind(ovs_rch[1:7,], ovs_rch)
	ovs_rch[1:7,"mon"] = 6:12
	ovs_rch[1:7,"yr"] = start_yr - 1
	
	fill_vals = ovs_rch %>%
		filter(mon %in% 6:12) %>%
		group_by(mon) %>%
		summarise(mn = mean(sim))
	ovs_rch[1:7, "sim"] = fill_vals$mn
	ovs_rch$adj = NA
	if (lag) {
		for (i in 8:nrow(ovs_rch)) {
			ovs_rch$adj[i] = sum(ovs_rch[i:(i-7),"sim"] * exp(x[1] + x[2]*1:8))
		}
	}
	if (season) {
		ovs_rch = ovs_rch %>%
			mutate(
				w = x[3] + x[4] * sin(((2*pi)/x[5])*mon + x[6]),
				adj = adj * w
			)
	}
	ovs_rch = ovs_rch %>%
		filter(yr >= start_yr) %>%
		select(rch,mon,yr,obs,sim,adj)
	if (optim) {
		ovs_rch = ovs_rch %>%
			filter(!is.na(adj), !is.na(obs))
		if (any(ovs_rch$adj <= 0)) {return(1e99)}
		resid_log = log(ovs_rch$adj) - log(ovs_rch$obs)
		sse = sum(resid_log^2)
		rmse = sqrt(sse / nrow(ovs_rch))
		resid = ovs_rch$adj - ovs_rch$obs
		pbias = abs(sum(resid, na.rm=T)) / sum(ovs_rch$obs)
		return((1 + pbias) * rmse)
	} else {
		return(ovs_rch)
	}		
}

adjust_rch = function(
	ovs_rch,
	priors,
	fields=c("obs","sim"),
	var="flow",
	station_name,
	start_yr=2002
	) {
	ovs_rch = ovs_rch %>%
		mutate(
			obs = as.numeric(ovs_rch[[fields[1]]]),
			sim = as.numeric(ovs_rch[[fields[2]]])
		) %>%
		select(rch,mon,yr,obs,sim)
	ovs_rch$obs[ovs_rch$obs <= 0] = NA
	if (sum(!is.na(ovs_rch$obs)) < 10) { return(NULL) }
	if (var == "flow") {
		lag=T
		season=F
	} else if (var == "sed") {
		lag=T
		season=T
	} else if (var == "tp") {
		lag=T
		season=T
	}
	o = optim(
		priors,
		routing_adjust,
		ovs=ovs_rch,
		optim=T,
		lag=lag,
		season=season,
		start_yr=start_yr
	)
	out_ovs_adj = routing_adjust(
		o$par,
		ovs_rch,
		optim=F,
		lag=lag,
		season=season,
		start_yr=start_yr
	)
	ovs_adj_rows = out_ovs_adj %>%
		left_join(
			ovs_rch %>%
				select(rch, mon, yr)
		) %>%
		filter(!is.na(sim), !is.na(obs))
	resid = cbind(
		ovs_adj_rows$sim - ovs_adj_rows$obs,
		ovs_adj_rows$adj - ovs_adj_rows$obs
	)
	sse = c(sum(resid[,1]^2), sum(resid[,2]^2))
	diagnostics = data.frame(
		rch=ovs_adj_rows$rch[1],
		station_name=station_name,
		variable=var,
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
		pbias=sum(resid[,1], na.rm=T) / sum(ovs_adj_rows$obs),
		pbias_adj=sum(resid[,2], na.rm=T) / sum(ovs_adj_rows$obs),
		r2=summary(
			lm(
				ovs_adj_rows$sim ~ ovs_adj_rows$obs,
				data=ovs_rch
			)
		)$r.squared,
		r2_adj=summary(
			lm(
				ovs_adj_rows$adj ~ ovs_adj_rows$obs,
				data=ovs_rch
			)
		)$r.squared
	)
	return(list(ovs_adj=out_ovs_adj, diagnostics=diagnostics))
}


################################################################################
# Start by optimizing at gage sites without any sites above
################################################################################
rchs1 = c(
#	78,
	137,
	140,
	141,
	142,
	149,
	150,
	151,
	152,
	155,
	157,
	158,
#	159,
#	162,
#	184,
	195,
	268,
	326
)

var_l = list(
	flow=list(
		fields=c("flow_obs", "flow"),
		priors=c(-0.4, -0.7)),
	sed=list(
		fields=c("sed_obs", "sed"),
		priors=c(-0.4, -0.7, 1.0, 0.3, 11, 1)),
	tp=list(
		fields=c("tp_obs", "tp"),
		priors=c(-0.4, -0.7, 1.0, 0.3, 11, 1))
)

cl = makeCluster(n_cores)
clusterExport(cl, c("ovs", "var_l", "start_yr", "adjust_rch", "routing_adjust"))
out = parLapply(
	cl,
	rchs1,
	function(x, ovs, var_l, start_yr) {
		print(x)
		library(dplyr)
		ovs_rch = ovs %>%
			filter(rch == x) %>%
			arrange(yr, mon)
		station_name = ovs_rch$station_name[!is.na(ovs_rch$station_name)][1]
		print(station_name)
		ovs_adj = NULL
		diagnostics = NULL
		for (v in names(var_l)) {
			print(v)
			ovs_adj_rows = adjust_rch(
				ovs_rch,
				priors=var_l[[v]][["priors"]],
				fields=var_l[[v]][["fields"]],
				var=v,
				station_name=station_name,
				start_yr=start_yr)
			ovs_adj_rows[[1]] = cbind(
				data.frame(variable=rep(v,nrow(ovs_adj_rows[[1]]))),
				ovs_adj_rows[[1]]
			)
			ovs_adj = rbind(ovs_adj, ovs_adj_rows[[1]])
			diagnostics = rbind(diagnostics, ovs_adj_rows[[2]])
		}
		return(list(ovs_adj=ovs_adj, diagnostics=diagnostics))
	},
	ovs=ovs,
	var_l=var_l,
	start_yr=start_yr
)
stopCluster(cl)

ovs_adj = NULL
diagnostics = NULL
for (i in 1:length(out)) {
	ovs_adj = rbind(ovs_adj, out[[i]]$ovs_adj)
	diagnostics = rbind(diagnostics, out[[i]]$diagnostics)
}

ave_pars = diagnostics %>%
	group_by(variable) %>%
	summarize(
		p1 = mean(p1),
		p2 = mean(p2),
		p3 = mean(p3),
		p4 = mean(p4),
		p5 = mean(p5),
		p6 = mean(p6)) %>%
	mutate(
		rch=as.integer(0),
		station_name = "Basin",
		sample_size = NA,
		df = NA,
		rmse = NA,
		rmse_adj = NA,
		pbias = NA,
		pbias_adj = NA,
		r2 = NA,
		r2_adj = NA
	) %>%
	select(rch, station_name, variable, sample_size, df, p1:p6, rmse:r2_adj)
diagnostics = rbind(diagnostics, ave_pars)	

copy_to(db, diagnostics, "routing_coefficients", temporary=F)

#ovs_adj = copy_to(
#	db,
#	ovs_adj,
#	"bias_corr_obs_vs_sim",
#	temporary=F
#)

## Adjust hru loads

hru = collect(tbl(db, "output_hru_monthly")) %>%
	left_join(collect(tbl(db, "days_in_mo"))) %>%
	select(HRU, AREA, SUB, MON, YR, N_DAYS, WYLD, SYLD, ORGP, SEDP, SOLP, P_GW) %>%
	mutate(
		TP = (ORGP + SEDP + SOLP + P_GW) * AREA * 100,
		WYLD = WYLD * (1/N_DAYS) * AREA * 0.01157407,
		SYLD = SYLD * AREA * 100
	) %>%
	select(HRU, SUB, MON, YR, WYLD, SYLD, TP) %>%
	rename(hru=HRU,sub=SUB,mon=MON,yr=YR,flow=WYLD,sed=SYLD,tp=TP)
routing_coefficients = collect(tbl(db, "routing_coefficients"))
gage_association = collect(tbl(db, "gage_association"))

d = hru %>%
	union(
		hru %>%
		group_by(hru, sub, mon) %>%
		summarize(
			yr = as.integer(start_yr - 1),
			flow = mean(flow),
			sed = mean(sed),
			tp = mean(tp)
		) %>%
		select(hru, sub, mon, yr, flow, sed, tp) %>%
		arrange(hru, yr, mon)
	) %>%
	left_join(
		gage_association,
		by=c("sub" = "Subbasin")
	) %>%
	left_join(
		routing_coefficients %>%
			filter(variable == "flow"),
		by=c("gageid_man" = "rch")
	) %>%
	mutate(
		p1_flow = p1,
		p2_flow = p2
	) %>%
	select(
		hru,
		sub,
		gageid_man,
		mon,
		yr,
		flow,
		sed,
		tp,
		p1_flow,
		p2_flow) %>%
	left_join(
		routing_coefficients %>%
			filter(variable == "sed"),
		by=c("gageid_man" = "rch")
	) %>%
	mutate(
		p1_sed = p1,
		p2_sed = p2
	) %>%
	select(
		hru,
		sub,
		gageid_man,
		mon,
		yr,
		flow,
		sed,
		tp,
		p1_flow,
		p2_flow,
		p1_sed,
		p2_sed) %>%
	left_join(
		routing_coefficients %>%
			filter(variable == "tp"),
		by=c("gageid_man" = "rch")
	) %>%
	mutate(
		p1_tp = p1,
		p2_tp = p2,
		p3_tp = p3,
		p4_tp = p4,
		p5_tp = p5,
		p6_tp = p6
	) %>%
	select(
		hru,
		sub,
		mon,
		yr,
		flow,
		sed,
		tp,
		p1_flow,
		p2_flow,
		p1_sed,
		p2_sed,
		p1_tp,
		p2_tp,
		p3_tp,
		p4_tp,
		p5_tp,
		p6_tp
	) %>%
	arrange(hru,yr,mon)
	
routing_coefficients = collect(tbl(db, "routing_coefficients"))
gage_association = collect(tbl(db, "gage_association"))
cl = makeCluster(n_cores)
clusterExport(
	cl,
	c(
		"hru",
		"var_l",
		"routing_coefficients",
		"gage_association",
		"start_yr",
		"routing_adjust"
	)
)
out = parLapply(
	cl,
	unique(d$hru),
	function (x, hru, var_l, gage_association, routing_coefficients) {
		library(dplyr)
		print(x)
		d_hru = hru %>%
			filter(hru == x) %>%
			mutate(obs = NA)
		sub = d_hru$sub[1]
		gage = gage_association %>%
			filter(Subbasin == d_hru$sub[1]) %>%
			select(gageid_man)
		out_adj = data.frame()
		for (v in names(var_l)) {
			d_hru_v = d_hru[c("hru", "mon", "yr", "obs", v)]
			names(d_hru_v)[c(1,5)] = c("rch", "sim")
			priors = routing_coefficients %>%
				filter(variable == v, rch == gage[[1]]) %>%
				select(p1:p6)
			if (v == "flow") {
				lag=T
				season=F
			} else if (v == "sed") {
				lag=T
				season=F
			} else if (v == "tp") {
				lag=T
				season=T
			}
			out_v = routing_adjust(
				as.matrix(priors)[1,],
				d_hru_v,
				optim=F,
				lag=lag,
				season=season,
				start_yr=start_yr
			)
			out_v = out_v %>%
				mutate(sub=sub, variable = v, yr = as.integer(yr)) %>%
				select(rch, sub, variable, mon, yr, adj) %>%
				rename(hru = rch)
			out_adj = rbind(out_adj, out_v)
		}
		return(out_adj)
	},
	hru=d,
	var_l=var_l,
	gage_association=gage_association,
	routing_coefficients=routing_coefficients
)
stopCluster(cl)

out_df = ldply(out, data.frame)
out_df = spread(out_df, variable, adj)

out = copy_to(db, out_df, "bias_corr_hru", temporary=F)



## Correct non-permitted MS4

urb = collect(tbl(db, "non_perm_urb")) # %>%
routing_coefficients = collect(tbl(db, "routing_coefficients"))
gage_association = collect(tbl(db, "gage_association"))

d = urb %>%
	union(
		urb %>%
			group_by(rch, mo) %>%
			summarize(
				yr = as.integer(start_yr - 1),
				flow = mean(flow),
				sed = mean(sed),
				tp = mean(tp)
			) %>%
			select(rch, mo, yr, flow, sed, tp) %>%
			arrange(yr, mo)
	) %>%
	left_join(
		gage_association,
		by=c("rch" = "Subbasin")
	) %>%
	left_join(
		routing_coefficients %>%
			filter(variable == "flow"),
		by=c("gageid_man" = "rch")
	) %>%
	mutate(
		p1_flow = p1,
		p2_flow = p2
	) %>%
	select(
		rch,
		gageid_man,
		mo,
		yr,
		flow,
		sed,
		tp,
		p1_flow,
		p2_flow) %>%
	left_join(
		routing_coefficients %>%
			filter(variable == "sed"),
		by=c("gageid_man" = "rch")
	) %>%
	mutate(
		p1_sed = p1,
		p2_sed = p2
	) %>%
	select(
		rch,
		gageid_man,
		mo,
		yr,
		flow,
		sed,
		tp,
		p1_flow,
		p2_flow,
		p1_sed,
		p2_sed) %>%
	left_join(
		routing_coefficients %>%
			filter(variable == "tp"),
		by=c("gageid_man" = "rch")
	) %>%
	mutate(
		p1_tp = p1,
		p2_tp = p2,
		p3_tp = p3,
		p4_tp = p4,
		p5_tp = p5,
		p6_tp = p6
	) %>%
	select(
		rch,
		mo,
		yr,
		flow,
		sed,
		tp,
		p1_flow,
		p2_flow,
		p1_sed,
		p2_sed,
		p1_tp,
		p2_tp,
		p3_tp,
		p4_tp,
		p5_tp,
		p6_tp
	) %>%
	arrange(rch,yr,mo)

d = d %>%
	rename(mon=mo)

routing_coefficients = collect(tbl(db, "routing_coefficients"))
gage_association = collect(tbl(db, "gage_association"))
cl = makeCluster(n_cores)
clusterExport(
	cl,
	c(
		"urb",
		"var_l",
		"routing_coefficients",
		"gage_association",
		"start_yr",
		"routing_adjust"
	)
)
out = parLapply(
	cl,
	unique(d$rch),
	function (x, d, var_l, gage_association, routing_coefficients) {
		library(dplyr)
		print(x)
		d_rch = d %>%
			filter(rch == x) %>%
			mutate(obs = NA)
		sub = d_rch$rch[1]
		gage = gage_association %>%
			filter(Subbasin == sub) %>%
			select(gageid_man)
		out_adj = data.frame()
		for (v in names(var_l)) {
			d_rch_v = d_rch[c("rch", "mon", "yr", "obs", v)]
			names(d_rch_v)[5] = "sim"
			priors = routing_coefficients %>%
				filter(variable == v, rch == gage[[1]]) %>%
				select(p1:p6)
			if (v == "flow") {
				lag=T
				season=F
			} else if (v == "sed") {
				lag=T
				season=T
			} else if (v == "tp") {
				lag=T
				season=T
			}
			out_v = routing_adjust(
				as.matrix(priors)[1,],
				d_rch_v,
				optim=F,
				lag=lag,
				season=season,
				start_yr=start_yr
			)
			out_v = out_v %>%
				mutate(variable = v, yr = as.integer(yr)) %>%
				select(rch, variable, mon, yr, adj)
			out_adj = rbind(out_adj, out_v)
		}
		return(out_adj)
	},
	d=d,
	var_l=var_l,
	gage_association=gage_association,
	routing_coefficients=routing_coefficients
)
stopCluster(cl)

out_df = ldply(out, data.frame)
out_df = spread(out_df, variable, adj)
out_df = out_df %>%
	select(rch, mon, yr, flow, sed, tp) %>%
	group_by(rch, mon, yr) %>%
	summarize(
		flow = sum(flow, na.rm=T),
		sed = sum(sed, na.rm=T),
		tp = sum(tp, na.rm=T)
	)

out = copy_to(db, out_df, "bias_corr_urb", temporary=F)

