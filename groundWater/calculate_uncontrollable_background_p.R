file_data = "C:/TEMP/background_phosphorus_for_ce_qual.txt"

d = read.table(file_data, sep="\t", header=T)

# Wisconsin River and Yellow River

rchs = data.frame(
	rch=c(143, 60),
	desc=c("Wisconsin River", "Yellow River")
)


for (r in 1:nrow(rchs)) {
	sel = rchs$rch[r]
	attach(d)
	repeat {
		l1 = length(sel)
		from = FROM_NODE[TO_NODE %in% sel]
		sel = unique(c(sel, from))
		l2 = length(sel)
		if (l1 == l2) {break}
	}
	detach(d)
	data_up = subset(
		d,
		Subbasin %in% sel,
		select=c(Area, tp_mg_L)
	)
	attach(data_up)
	mean_tp = sum(Area*tp_mg_L) / sum(Area)
	detach(data_up)
	rchs$tp_mg_L[r] = mean_tp
}

direct_subs = c(59,73:75,77,141,142,251:255,311)
sub_d = subset(d, Subbasin %in% direct_subs)
attach(sub_d)
direct_row = data.frame(
	rch = direct_subs,
	desc = "Sands and direct drainages",
	tp_mg_L = sum(Area*tp_mg_L) / sum(Area)
)
detach(sub_d)
data_out = rbind(rchs, direct_row)
names(data_out) = c("Reach", "Description", "Background TP mg/L")

write.table(data_out,
	"C:/Users/ruesca/Desktop/uncontrollable_tp_concentrations.txt",
	row.names=F
)