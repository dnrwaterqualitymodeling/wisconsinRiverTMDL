library(dplyr)

file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_spatial_db.sqlite"
db_con = src_sqlite(file_db)
df_cal = tbl(db_con, "calibration_data") %>% collect()

pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_streamflow.pdf",
	width=6.5,
	height=9
)

rchs_tbl = tbl(db_con, "observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, flow_sim, flow_obs) %>%
	arrange(yr,mon) %>%
	collect() %>%
	mutate(date = as.Date(paste(yr,mon,1,sep="-")))
rchs = unique(rchs_tbl$rch)

par(mfrow=c(3,1))
for (rch_i in rchs) {
	rch_tbl = rchs_tbl %>%
		filter(rch == rch_i)
	print(rch_tbl$station_name[1])
	rch_tbl = mutate(rch_tbl, flow_obs = as.numeric(flow_obs))
	ylim = with(rch_tbl, range(c(flow_obs, flow_sim), na.rm=T))
	ylim[1] = 0
	plot(
		flow_sim ~ date,
		rch_tbl,
		type="n",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Streamflow (cms)",
		xlab="Month"
	)
	grid()
	lines(
		flow_sim ~ date,
		rch_tbl,
		lwd=2,
		col="#377eb8"
	)
	lines(flow_obs ~ date, data=rch_tbl, col="#e41a1c", lwd=2)
	if (rch_i %in% df_cal$rch) {
		cal_yrs = df_cal %>%
			filter(
				rch == rch_i,
				variable %in% c("RES_FLOW_OUT", "FLOW_OUT")) %>%
			select(calibration_years)
		rch_tbl[!(rch_tbl$yr %in% cal_yrs$calibration_years), "flow_obs"] = NA
		lines(flow_obs ~ date, data=rch_tbl, col="#4daf4a", lwd=2)
		legend_nms = c("observed (cal.)", "observed (val.)", "simulated")
		legend_col = c("#4daf4a", "#e41a1c", "#377eb8")
	} else {
		legend_nms = c("observed (not used in calibration)", "simulated")
		legend_col =c("#e41a1c", "#377eb8")
	}
	legend(
		"topright",
		legend=legend_nms,
		lwd=1,
		col=legend_col)
}
dev.off()


pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_tss.pdf",
	width=6.5,
	height=9
)

rchs_tbl = tbl(db_con, "observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, sed_sim, sed_obs) %>%
	arrange(yr,mon) %>%
	collect() %>%
	mutate(
		date = as.Date(paste(yr,mon,1,sep="-")),
		sed_obs = as.numeric(sed_obs)
	)
rchs = unique(rchs_tbl$rch)

par(mfrow=c(3,1))
for (rch_i in rchs) {
	rch_tbl = rchs_tbl %>%
		filter(rch == rch_i)
	print(rch_tbl$station_name[1])
	ylim = with(rch_tbl, range(c(sed_obs, sed_sim), na.rm=T))
	ylim[1] = 0
	plot(
		sed_sim ~ date,
		rch_tbl,
		type="n",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Total Suspended Solids (metric tons)",
		xlab="Month"
	)
	grid()
	lines(
		sed_sim ~ date,
		rch_tbl,
		lwd=2,
		col="#377eb8"
	)
	lines(sed_obs ~ date, data=rch_tbl, col="#e41a1c", lwd=2)
	if (rch_i %in% df_cal$rch) {
		cal_yrs = df_cal %>%
			filter(
				rch == rch_i,
				variable == "SED_OUT") %>%
			select(calibration_years)
		rch_tbl[!(rch_tbl$yr %in% cal_yrs$calibration_years), "sed_obs"] = NA
		lines(sed_obs ~ date, data=rch_tbl, col="#4daf4a", lwd=2)
		legend_nms = c("observed (cal.)", "observed (val.)", "simulated")
		legend_col = c("#4daf4a", "#e41a1c", "#377eb8")
	} else {
		legend_nms = c("observed (not used in calibration)", "simulated")
		legend_col =c("#e41a1c", "#377eb8")
	}
	legend(
		"topright",
		legend=legend_nms,
		lwd=1,
		col=legend_col)
}
dev.off()


pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_tp.pdf",
	width=6.5,
	height=9
)

rchs_tbl = tbl(db_con, "observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, tp_sim, tp_obs) %>%
	arrange(yr,mon) %>%
	collect() %>%
	mutate(
		date = as.Date(paste(yr,mon,1,sep="-")),
		tp_obs = as.numeric(tp_obs)
	)
rchs = unique(rchs_tbl$rch)

par(mfrow=c(3,1))
for (rch_i in rchs) {
	rch_tbl = rchs_tbl %>%
		filter(rch == rch_i)
	print(rch_tbl$station_name[1])
	ylim = with(rch_tbl, range(c(tp_obs, tp_sim), na.rm=T))
	ylim[1] = 0
	plot(
		tp_sim ~ date,
		rch_tbl,
		type="n",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Total Phosphorus (kilograms)",
		xlab="Month"
	)
	lines(
		tp_sim ~ date,
		rch_tbl,
		lwd=2
	)
	lines(tp_obs ~ date, data=rch_tbl, col="#e41a1c", lwd=2)
	if (rch_i %in% df_cal$rch) {
		cal_yrs = df_cal %>%
			filter(
				rch == rch_i,
				variable == "TOT_P") %>%
			select(calibration_years)
		rch_tbl[!(rch_tbl$yr %in% cal_yrs$calibration_years), "tp_obs"] = NA
		lines(tp_obs ~ date, data=rch_tbl, col="#4daf4a", lwd=2)
		legend_nms = c("observed (cal.)", "observed (val.)", "simulated")
		legend_col = c("#4daf4a", "#e41a1c", "#377eb8")
	} else {
		legend_nms = c("observed (not used in calibration)", "simulated")
		legend_col =c("#e41a1c", "#377eb8")
	}
	legend(
		"topright",
		legend=legend_nms,
		lwd=1,
		col=legend_col)
}
dev.off()


pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_tp_fwmc.pdf",
	width=6.5,
	height=9
)

rchs_tbl = tbl(db_con, "observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, tp_fwmc_sim, tp_fwmc_obs) %>%
	arrange(yr,mon) %>%
	collect() %>%
	mutate(
		date = as.Date(paste(yr,mon,1,sep="-")),
		tp_fwmc_obs = as.numeric(tp_fwmc_obs)
	)
rchs = unique(rchs_tbl$rch)

par(mfrow=c(3,1))
for (rch_i in rchs) {
	rch_tbl = rchs_tbl %>%
		filter(rch == rch_i)
	print(rch_tbl$station_name[1])
	ylim = with(rch_tbl, range(c(tp_fwmc_obs, tp_fwmc_sim), na.rm=T))
	ylim[1] = 0
	plot(
		tp_fwmc_sim ~ date,
		rch_tbl,
		type="n",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="TP Flow-Weighted Mean Concentration (ug/L)",
		xlab="Month"
	)
	grid()
	lines(
		tp_fwmc_sim ~ date,
		rch_tbl,
		lwd=2
	)
	lines(tp_fwmc_obs ~ date, data=rch_tbl, col="blue", lwd=2)
	legend(
		"topright",
		legend=c("simulated", "observed"),
		lwd=1,
		col=c("black", "blue"))
}
dev.off()


pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_dop.pdf",
	width=6.5,
	height=9
)

rchs_tbl = tbl(db_con, "observed_vs_simulated") %>%
	select(rch, station_name, mon, yr, dop_sim, dop_obs) %>%
	arrange(yr,mon) %>%
	collect() %>%
	mutate(
		date = as.Date(paste(yr,mon,1,sep="-")),
		dop_obs = as.numeric(dop_obs)
	)
rchs = unique(rchs_tbl$rch)

par(mfrow=c(3,1))
for (rch_i in rchs) {
	rch_tbl = rchs_tbl %>%
		filter(rch == rch_i)
	print(rch_tbl$station_name[1])
	ylim = with(rch_tbl, range(c(dop_obs, dop_sim), na.rm=T))
	ylim[1] = 0
	plot(
		dop_sim ~ date,
		rch_tbl,
		type="n",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Dissolve Orthophosphate (kilograms)",
		xlab="Month"
	)
	grid()
	lines(
		dop_sim ~ date,
		rch_tbl,
		lwd=2
	)
	lines(dop_obs ~ date, data=rch_tbl, col="blue", lwd=2)
	legend(
		"topright",
		legend=c("simulated", "observed"),
		lwd=1,
		col=c("black", "blue"))
}
dev.off()
