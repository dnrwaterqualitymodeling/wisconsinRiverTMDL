library(dplyr)

file_out = "T:/Projects/Wisconsin_River/Model_Outputs/data_requests/rb_index_wrb_matt.txt"

inputs = list(
	WR = list(
		prj="Wisconsin River",
		file="C:/Users/ruesca/Documents/WRB.Sufi2.SwatCup/output.rch",
		rchs=c(78,137,140,141,142,149,150,151,152,155,157,159,195,199,268,326)
	))
#	,
#	UFW = list(
#		prj="Upper Fox/Wolf",
#		file="C:/Users/ruesca/Documents/UFWB/UFWB_NLCDwetlands.Sufi2.SwatCup/output.rch",
#		rchs=c(90,131,143,167,169,172,177)
#	)
#)

summ_tbl = data.frame()
for (i in inputs) {
	flow = read.table(i$file, header=F, skip=9) %>%
		rename(rch=V2, day=V4, flow=V6) %>%
		select(rch, day, flow)
#		filter(rch %in% i$rchs)
	for (r in unique(flow$rch)) {
		print(paste(i$prj, r))
		flow_r = flow %>%
			filter(rch == r) %>%
			mutate(pl = NA)
		for (d in 2:nrow(flow_r)) {
			flow_r$pl[d] = abs(flow_r$flow[d]-flow_r$flow[d-1])
		}
		flow_r = flow_r[-1,]
		rb_index = sum(flow_r$pl) / sum(flow_r$flow)
		summ_row = data.frame(
			prj=i$prj,
			rch=r,
			rb_index=rb_index
		)
		summ_tbl = rbind(summ_tbl, summ_row)
	}
}

write.table(summ_tbl, file_out, sep="\t", row.names=F)