

ylim = with(ovs_adj, range(c(tp_sim, tp_adj, tp_obs), na.rm=T))
# Reporting
# temporal plots
layout(matrix(c(1,2), nrow=2, byrow=T))	
plot(
	tp_sim ~ date,
	data=ovs_adj,
	type="n",
	ylab="Total Phosphorus, kg",
	xlab=NA,
	log="y",
	main=id,
	ylim=ylim
)
grid()
lines(
	tp_sim ~ date,
	data=ovs_adj,
	type="o",
	pch=15,
	col="grey40"
)
lines(
	tp_obs ~ date,
	data=ovs_adj,
	type="o",
	lty=2,
	pch=17
)
plot(
	tp_sim ~ date,
	data=ovs_adj,
	type="n",
	ylab="Total Phosphorus, kg",
	xlab="Month",
	log="y",
	ylim=ylim
)
grid()
lines(
	tp_adj ~ date,
	data=ovs_adj,
	type="o",
	lty=2,
	pch=15,
	col="grey40"
)
lines(
	tp_obs ~ date,
	data=ovs_adj,
	type="o",
	lty=3,
	pch=17
)
# scatterplots and QQ plots
ovs_adj = ovs_adj %>%
	filter(!is.na(tp_sim), !is.na(tp_obs))
layout(matrix(1:4, nrow=2, byrow=T))
plot(
	tp_sim ~ tp_obs,
	data=ovs_adj,
	type="n",
	ylab="TP simulated, kg",
	xlab="TP observed, kg",
	xlim=ylim,
	ylim=ylim,
	log="xy",
	main="Observed vs. Simulated"
)
grid()
points(
	tp_sim ~ tp_obs,
	data=ovs_adj
)
abline(0,1)
abline(
	lm(log(tp_sim) ~ log(tp_obs), data=ovs_adj),
	lty=2,
	untf=T
)
plot(
	tp_adj ~ tp_obs,
	data=ovs_adj,
	type="n",
	ylab="TP adjusted, kg",
	xlab="TP observed, kg",
	xlim=ylim,
	ylim=ylim,
	log="xy",
	main="Observed vs. Adjusted"
)
grid()
points(
	tp_adj ~ tp_obs,
	data=ovs_adj
)
abline(0,1)
abline(
	lm(log(tp_adj) ~ log(tp_obs), data=ovs_adj),
	lty=2,
	untf=T
)
# QQ plots
ovs_adj = ovs_adj %>%
	mutate(			
		obs_q = rank(tp_obs) / nrow(ovs_adj),
		sim_q = rank(tp_sim) / nrow(ovs_adj),
		adj_q = rank(tp_adj) / nrow(ovs_adj)
	)
plot(
	tp_obs ~ obs_q,
	data=ovs_adj,
	type="n",
	ylab="Total phosphorus, kg",
	xlab="Quantile",
	ylim=ylim,
	log="y",
	main="Observed vs. Simulated"
)
grid()
points(
	tp_obs ~ obs_q,
	data=ovs_adj
)	
points(
	tp_sim ~ sim_q,
	data=ovs_adj,
	pch=2
)
plot(
	tp_obs ~ obs_q,
	data=ovs_adj,
	type="n",
	ylab="Total phosphorus, kg",
	xlab="Quantile",
	ylim=ylim,
	log="y",
	main="Observed vs. Adjusted"
)
grid()
points(
	tp_obs ~ obs_q,
	data=ovs_adj
)	
points(
	tp_adj ~ adj_q,
	data=ovs_adj,
	pch=2
)

# Optim plots
layout(matrix(c(1,2,0,0), nrow=2, byrow=T))
x = 1:8
lag_fact = exp(o$par[1] + o$par[2]*x)
plot(x,lag_fact,type="l",ylab="Lag factor",xlab="Months Prior")
x = 1:12
seas_fact = o$par[3] + o$par[4] * sin(((2*pi)/11)*x + o$par[5])
plot(x,seas_fact,type="l",ylab="Season factor",xlab="Month",ylim=c(0,max(seas_fact)), xlim=c(1,12))
}
dev.off()
#
#lapply(
#	diagnostics,
#	function (x, file) {
#		x
#		for (ln in 1:length(x)) {
#			write(paste(names(x[ln]), x[[ln]]), file, append=T)
#		}
#	},
#	file="test.txt"
#)
#	

hydro = readOGR(
	dirname(file_hydro),
	strsplit(basename(file_hydro), "\\.")[[1]][1]
)
pdf(
	"T:/Projects/Wisconsin_River/Model_Outputs/plots/bias_correction/parameter_plots.pdf",
	width=11,
	height=8.5
)
par_df = data.frame(
	rch = sapply(diagnostics, function (x) { x$rch }),
	p1 = sapply(diagnostics, function (x) { x$par[1] }),
	p2 = sapply(diagnostics, function (x) { x$par[2] }),
	p3 = sapply(diagnostics, function (x) { x$par[3] }),
	p4 = sapply(diagnostics, function (x) { x$par[4] }),
	p5 = sapply(diagnostics, function (x) { x$par[5] }),
	p6 = sapply(diagnostics, function (x) { x$par[6] })
)
layout(matrix(1:6, nrow=2, byrow=T))
hydro_c = gCentroid(hydro, byid=T, id=1:337)
hydro_c = hydro_c[par_df$rch]
hydro_c = SpatialPointsDataFrame(hydro_c, par_df)

pal = brewer.pal(5, "Reds")
for (p in paste("p", 1:6, sep="")) {
	ci = classIntervals(hydro_c@data[[p]],5,"quantile")
	col = findColours(ci, pal)
	plot(hydro, col="grey")
	points(hydro_c, bg=col, pch=21, cex=2)
}
dev.off()
