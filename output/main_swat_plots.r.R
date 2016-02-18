library(dplyr)

file_db = "C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3"
db_con = src_sqlite(file_db)

pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_streamflow.pdf",
	width=8.5,
	height=11
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
	ylim = with(rch_tbl, range(c(flow_obs, flow_sim), na.rm=T))
	ylim[1] = 0
	plot(
		flow_sim ~ date,
		rch_tbl,
		type="l",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Streamflow (cms)",
		xlab="Month"
	)
	lines(flow_obs ~ date, data=rch_tbl, col="blue")
	legend(
		"topright",
		legend=c("simulated", "observed"),
		lwd=1,
		col=c("black", "blue"))
}
dev.off()


pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_tss.pdf",
	width=8.5,
	height=11
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
		type="l",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Total Suspended Solids (metric tons)",
		xlab="Month"
	)
	lines(sed_obs ~ date, data=rch_tbl, col="blue")
	legend(
		"topright",
		legend=c("simulated", "observed"),
		lwd=1,
		col=c("black", "blue"))
}
dev.off()


pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_tp.pdf",
	width=8.5,
	height=11
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
		type="l",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="Total Phosphorus (kilograms)",
		xlab="Month"
	)
	lines(tp_obs ~ date, data=rch_tbl, col="blue")
	legend(
		"topright",
		legend=c("simulated", "observed"),
		lwd=1,
		col=c("black", "blue"))
}
dev.off()

pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/time_series/obs_sim_tp_fwmc.pdf",
	width=8.5,
	height=11
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
		type="l",
		ylim=ylim,
		main=rch_tbl$station_name[1],
		ylab="TP Flow-Weighted Mean Concentration (ug/L)",
		xlab="Month"
	)
	lines(tp_fwmc_obs ~ date, data=rch_tbl, col="blue")
	legend(
		"topright",
		legend=c("simulated", "observed"),
		lwd=1,
		col=c("black", "blue"))
}
dev.off()
