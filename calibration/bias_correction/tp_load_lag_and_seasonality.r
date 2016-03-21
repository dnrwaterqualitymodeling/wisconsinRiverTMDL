library(dplyr)
library(rgdal)
library(rgeos)
library(classInt)
library(RColorBrewer)

db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")
file_hydro = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/hydro.shp"

ovs = tbl(db, "observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, tp_sim, tp_obs) %>%
	collect() %>%
	mutate(tp_obs = as.numeric(tp_obs)) %>%
	mutate(date=as.Date(paste(yr,mon,"1",sep="-")))

lag = function (x, ovs, optim) {
	for (i in 8:nrow(ovs)) {
		ovs$tp_adj[i] = sum(
			ovs$tp_sim[i] * exp(x[1] + x[2]*1),
			ovs$tp_sim[i-1] * exp(x[1] + x[2]*2),
			ovs$tp_sim[i-2] * exp(x[1] + x[2]*3),
			ovs$tp_sim[i-3] * exp(x[1] + x[2]*4),
			ovs$tp_sim[i-4] * exp(x[1] + x[2]*5),
			ovs$tp_sim[i-5] * exp(x[1] + x[2]*6),
			ovs$tp_sim[i-6] * exp(x[1] + x[2]*7),
			ovs$tp_sim[i-7] * exp(x[1] + x[2]*8)
		)
	}
	ovs = ovs %>%
		mutate(
			w = x[3] + x[4] * sin(((2*pi)/x[5])*mon + x[6]),
			tp_adj = tp_adj * w
		)
	if (optim) {
		ovs = ovs %>%
			filter(!is.na(tp_sim), !is.na(tp_obs))
		if (any(ovs$tp_adj <= 0)) {return(1e99)}
		resid_log = log(ovs$tp_adj) - log(ovs$tp_obs)
		sse = sum(resid_log^2)
		rmse = sqrt(sse / nrow(ovs))
		resid = ovs$tp_adj - ovs$tp_obs
		pbias = abs(sum(resid, na.rm=T)) / sum(ovs$tp_obs)
		return((1 + pbias) * rmse)
	} else {
		return(ovs)
	}		
}


priors = c(-0.4, -0.7, 1.0, 0.3, 11, 1)
ovs_adj = NULL
diagnostics = NULL
for (rch_i in unique(ovs$rch)) {
	ovs_rch = ovs %>%
		filter(rch == rch_i)
	if (sum(!is.na(ovs_rch$tp_obs)) < 10) { next }
	id = ovs_rch$station_name[1]
	print(id)
	o = optim(
		priors,
		lag,
		ovs=ovs_rch,
		optim=T
	)
	ovs_adj_rows = lag(o$par, ovs_rch, optim=F)
	ovs_adj = rbind(ovs_adj, ovs_adj_rows)
	ovs_adj_rows = ovs_adj_rows %>%
		filter(!is.na(tp_sim), !is.na(tp_obs))
	resid = cbind(
		ovs_adj_rows$tp_sim - ovs_adj_rows$tp_obs,
		ovs_adj_rows$tp_adj - ovs_adj_rows$tp_obs
	)
	sse = c(sum(resid[,1]^2), sum(resid[,2]^2))
	diagnostics = rbind(diagnostics,
		data.frame(
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
					ovs_adj_rows$tp_sim ~ ovs_adj_rows$tp_obs,
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
	)
}

ovs_adj = copy_to(
	db,
	ovs_adj,
	"bias_corr_tp_observed_vs_simulated",
	temporary=F
)
diagnostics = copy_to(
	db,
	diagnostics,
	"bias_corr_tp_diagnostics",
	temporary=F
)













