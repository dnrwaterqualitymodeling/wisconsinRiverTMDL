library(dplyr)

db = src_sqlite("~/Documents/tmdl_db/wrb_swat_spatial_db.sqlite")
dir_plots = "T:/Projects/Wisconsin_River/Model_Outputs/plots/bias_correction"

ovs_adj = tbl(db, "bias_corr_observed_vs_simulated") %>%
	rename(flow_adj=flow_sim, sed_adj=sed_sim, tp_adj=tp_sim) %>%
	left_join(tbl(db, "station_id_rch_xref")) %>%
	left_join(tbl(db, "usgs_station_xref")) %>%
	left_join(
		tbl(db, "observed_vs_simulated") %>%
			select(rch, mon, yr, flow_sim, sed_sim, tp_sim)
	) %>%
	collect() %>%
	mutate(date=as.Date(paste(yr,mon,"1",sep="-"))) %>%
	gather(
		variable,
		value,
		c(flow_sim,flow_adj,flow_obs,sed_sim,sed_adj,sed_obs,tp_sim,tp_adj,tp_obs)) %>%
	separate(variable, into=c("variable","source"), sep="_") %>%
	spread(source, value) %>%
	select(rch, station_name, date, mon, yr, variable, obs, sim, adj) %>%
	mutate(
		obs = as.numeric(obs),
		sim = as.numeric(sim),
		adj = as.numeric(adj)
	)

labs = list(
	flow = list(
		name = "Streamflow",
		units = "cms"
	),
	tp = list(
		name = "Total Phosphorus",
		units = "kg"
	),
	sed = list(
		name = "Total Suspended Sediment",
		units = "metric tons"
	)
)

setwd(dir_plots)
for (v in c("flow", "sed", "tp")) {
	pdf(paste("diagnostics_", v, ".pdf", sep=""), height=6.5, width=9)
	par(mar=c(4.1,4.1,4.1,2.1))
	for (rch_i in unique(ovs_adj$rch)) {
		ovs_adj_rch = ovs_adj %>%
			filter(rch == rch_i, variable==v) %>%
			arrange(yr, mon)
		ovs_adj_rch$sim[ovs_adj_rch$sim == 0] = NA
			
	#	id = ovs_adj_rch$station_name[1]
		id = ovs_adj_rch$station_name[!is.na(ovs_adj_rch$station_name)][1]
		ylim = with(ovs_adj_rch, range(c(sim, adj, obs), na.rm=T))
		# Reporting
		# temporal plots
		layout(matrix(c(1,2), nrow=2, byrow=T))
		ylab = paste(labs[[v]]$name, labs[[v]]$units, sep=", ")
		plot(
			sim ~ date,
			data=ovs_adj_rch,
			type="n",
			ylab=ylab,
			xlab=NA,
			log="y",
			main=id,
			ylim=ylim
		)
		grid()
		lines(
			sim ~ date,
			data=ovs_adj_rch,
			type="o",
			pch=15,
			col="#377eb8"
		)
		lines(
			obs ~ date,
			data=ovs_adj_rch,
			type="o",
			lty=2,
			pch=17,
			col="#4daf4a"
		)
		plot(
			sim ~ date,
			data=ovs_adj_rch,
			type="n",
			ylab=ylab,
			xlab="Month",
			log="y",
			ylim=ylim
		)
		grid()
		lines(
			adj ~ date,
			data=ovs_adj_rch,
			type="o",
	#		lty=2,
			pch=15,
			col="#377eb8"
		)
		lines(
			obs ~ date,
			data=ovs_adj_rch,
			type="o",
			lty=2,
			pch=17,
			col="#4daf4a"
		)
		# scatterplots and QQ plots
		ovs_adj_rch = ovs_adj_rch %>%
			filter(!is.na(sim), !is.na(obs))
		if (nrow(ovs_adj_rch) == 0) {next}
		layout(matrix(1:4, nrow=2, byrow=T))
		ylab = paste(labs[[v]]$name, " simulated, ", labs[[v]]$units, sep="")
		xlab = paste(labs[[v]]$name, " observed, ", labs[[v]]$units, sep="")
		plot(
			sim ~ obs,
			data=ovs_adj_rch,
			type="n",
			ylab=ylab,
			xlab=xlab,
			xlim=ylim,
			ylim=ylim,
			log="xy",
			main="Observed vs. Simulated"
		)
		grid()
		points(
			sim ~ obs,
			data=ovs_adj_rch
		)
		abline(0,1)
		abline(
			lm(log(sim) ~ log(obs), data=ovs_adj_rch),
			lty=2,
			untf=T
		)
		plot(
			adj ~ obs,
			data=ovs_adj_rch,
			type="n",
			ylab=ylab,
			xlab=xlab,
			xlim=ylim,
			ylim=ylim,
			log="xy",
			main="Observed vs. Adjusted"
		)
		grid()
		points(
			adj ~ obs,
			data=ovs_adj_rch
		)
		abline(0,1)
		abline(
			lm(log(adj) ~ log(obs), data=ovs_adj_rch),
			lty=2,
			untf=T
		)
		# QQ plots
		ylab = paste(labs[[v]]$name, labs[[v]]$units, sep=", ")
		ovs_adj_rch = ovs_adj_rch %>%
			mutate(			
				obs_q = rank(obs) / nrow(ovs_adj_rch),
				sim_q = rank(sim) / nrow(ovs_adj_rch),
				adj_q = rank(adj) / nrow(ovs_adj_rch)
			)
		plot(
			obs ~ obs_q,
			data=ovs_adj_rch,
			type="n",
			ylab=ylab,
			xlab="Quantile",
			ylim=ylim,
			log="y",
			main="Observed vs. Simulated"
		)
		grid()
		points(
			obs ~ obs_q,
			data=ovs_adj_rch,
			pch=21,
			bg="#4daf4a"
		)	
		points(
			sim ~ sim_q,
			data=ovs_adj_rch,
			pch=24,
			bg="#377eb8"
		)
		plot(
			obs ~ obs_q,
			data=ovs_adj_rch,
			type="n",
			ylab=ylab,
			xlab="Quantile",
			ylim=ylim,
			log="y",
			main="Observed vs. Adjusted"
		)
		grid()
		points(
			obs ~ obs_q,
			data=ovs_adj_rch,
			pch=21,
			bg="#4daf4a"
		)	
		points(
			adj ~ adj_q,
			data=ovs_adj_rch,
			pch=24,
			bg="#377eb8"
		)
	#	{next}
		o_par = tbl(db, "routing_coefficients") %>%
			filter(rch == rch_i, variable==v) %>%
			select(p1:p6) %>%
			collect()
		if (nrow(o_par) == 0) {next}
		# Optim plots
		layout(matrix(c(1,2,0,0), nrow=2, byrow=T))
		x = 1:8
		lag_fact = exp(o_par$p1 + o_par$p2*x)
		plot(x,lag_fact,type="l",ylab="Lag factor",xlab="Months Prior")
		x = 1:12
		seas_fact = o_par$p3 + o_par$p4 * sin(((2*pi)/o_par$p5)*x + o_par$p6)
		if (all(is.na(seas_fact))) {next}
		plot(x,seas_fact,type="l",ylab="Season factor",xlab="Month",ylim=c(0,max(seas_fact)), xlim=c(1,12))
		o_par = NULL
	}
	dev.off()
}
