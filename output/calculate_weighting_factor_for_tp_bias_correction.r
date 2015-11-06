library(RODBC)

file_swat_prj = "K:/WRB/WRB.mdb"
subbasins = c(78,127,137,140,141,142,149,150,151,152,155,157,158,159,162,184,195,199,226,268,326)
file_out = "T:/Projects/Wisconsin_River/Model_Outputs/tp_bias_crrection/weighting_factors.txt"

con = odbcConnectAccess(file_swat_prj)
rch = sqlQuery(con, "select from_node,to_node,areac from reach")
close(con)

adj_tbl = data.frame()
for (reach in rch$from_node) {
	print(reach)
	adj_tbl_rows = data.frame()
	wsa = subset(rch, from_node==reach, select=areac)[1,1]
	if (reach %in% subbasins) {
		adj_tbl_rows = data.frame(
			reach=reach,
			calibration_reach=reach,
			direction="on",
			reach_wsa=wsa,
			calibration_reach_wsa=wsa,
			raw_adj=1
		)
		adj_tbl = rbind(adj_tbl, adj_tbl_rows)
		next
	}
	downstream = reach
	repeat {
		to_node = subset(rch, from_node == downstream, select=to_node)
		if (nrow(to_node) == 0) {
			downstream_site = NULL
			break
		}
		if (to_node %in% subbasins) {
			downstream_site = to_node[1,1]
			calibration_reach_wsa = subset(
				rch,
				from_node==downstream_site,
				select=areac)[1,1]
			adj_tbl_rows = data.frame(
				reach=reach,
				calibration_reach=downstream_site,
				direction="downstream",
				reach_wsa=wsa,
				calibration_reach_wsa=calibration_reach_wsa,
				raw_adj = wsa / calibration_reach_wsa
			)
			adj_tbl = rbind(adj_tbl, adj_tbl_rows)
			break
		} else {
			downstream = to_node[1,1]
		}
	}
	upstream = reach
	repeat {
		upstream_count = length(upstream)
		from_nodes = subset(rch, to_node %in% upstream, select=from_node)[,1]
		upstream = unique(c(upstream, from_nodes))
		if (upstream_count == length(upstream)) {
			break
		}
	}
	upstream = upstream[upstream %in% subbasins]
	if (length(upstream) > 0) {
		calibration_reach_wsa = subset(
			rch,
			from_node %in% upstream,
			select=c(from_node, areac)
		)
		adj_tbl_rows = data.frame(
			reach=reach,
			calibration_reach=calibration_reach_wsa$from_node,
			direction="upstream",
			reach_wsa=wsa,
			calibration_reach_wsa=calibration_reach_wsa$areac,
			raw_adj = calibration_reach_wsa$areac / wsa
		)
		adj_tbl = rbind(adj_tbl, adj_tbl_rows)
	}
	if (nrow(adj_tbl_rows) == 0) {
		adj_tbl_rows = data.frame(
			reach=reach,
			calibration_reach=reach,
			direction="none",
			reach_wsa=wsa,
			calibration_reach_wsa=wsa,
			raw_adj=1
		)
		adj_tbl = rbind(adj_tbl, adj_tbl_rows)
	}
}

norm_tbl = data.frame()
for (reach in 1:337) {
	adj_tbl_rows = adj_tbl[adj_tbl$reach == reach,]
	if (all(adj_tbl_rows$direction %in% c("on", "none"))) {
		adj_tbl_rows$norm_adj = 1
		norm_tbl = rbind(norm_tbl, adj_tbl_rows)
		next
	}
	ratios = apply(adj_tbl_rows,
		1,
		function(x) {
			if (x[3] == "downstream") {
				return(as.numeric(x[4]) / as.numeric(x[5]))
			} else if (x[3] == "upstream") {
				return(as.numeric(x[5]) / as.numeric(x[4]))
			}
		}
	)
	adj_tbl_rows$norm_adj = ratios / sum(ratios)
	norm_tbl = rbind(norm_tbl, adj_tbl_rows)
}

write.table(norm_tbl, file_out, row.names=F, sep="\t")

