#dir_out = "T:/Projects/Wisconsin_River/Model_Documents/LimnoTech/swat_data_transfer"
dir_out = "C:/Users/ruesca/Desktop/Limnotech"

output.rch = readLines("C:/Users/ruesca/Desktop/sub_and_reach/output.rch")
output.sub = readLines("C:/Users/ruesca/Desktop/sub_and_reach/output.sub")

sub_codes = c(143, 60, 73, 74, 59)
rch_codes = c(333, 255, 76, 254, 253, 251, 252, 60)
#hru_codes = c(1312, 987)


pat = paste(rch_codes, collapse="|")
output.rch = c(
	output.rch[1:9],
	output.rch[grep(pat, substr(output.rch, 6, 10))]
)
writeLines(output.rch, paste(dir_out, "output.rch", sep="/"))

pat = paste(sub_codes, collapse="|")
output.sub = c(
	output.sub[1:9],
	output.sub[grep(pat, substr(output.sub, 7, 10))]
)
writeLines(output.sub, paste(dir_out, "output.sub", sep="/"))


all_sub = data.frame()
for (sub_code in sub_codes) {
	print(sub_code)
	sub_output.sub = output.sub[grep(
		paste("\\s", sub_code, sep=""),
		substr(output.sub, 7, 10))]
	area = as.numeric(substr(sub_output.sub[1], 25, 34))
	sub_output.sub = read.table(
		text=paste(
			substr(sub_output.sub, 25, nchar(sub_output.sub)),
			collapse="\n"
		)
	)
	all_sub = rbind(
		all_sub,
		data.frame(
			subbasin = sub_code,
			area_sq_km = area,
			date = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by="1 day"),
			water_yield_mm = sub_output.sub[,10],
			sediment_yield_metric_tons_per_ha = sub_output.sub[,11],
			organic_p_yield_kg_per_ha = sub_output.sub[,13],
			soluble_p_yield_kg_per_ha = sub_output.sub[,15],
			mineral_p_yield_kg_per_ha = sub_output.sub[,16]
		)
	)
}

all_rch = data.frame()
for (rch_code in rch_codes) {
	print(rch_code)
	rch_output.rch = output.rch[
		grep(
			paste("\\s", rch_code, sep=""),
			substr(output.rch, 6, 10))]
	rch_output.rch = read.table(
		text=paste(rch_output.rch, collapse="\n")
	)
	all_rch = rbind(
		all_rch,
		data.frame(
			reach = rch_code,
			date = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by="1 day"),
			flow_cms = rch_output.rch[,7],
			sediment_metric_tons = rch_output.rch[,11],
			organic_p_kg = rch_output.rch[,16],
			mineral_p_kg = rch_output.rch[,24]
		)
	)
}

write.table(all_sub, paste(dir_out, "output.sub_clean.txt", sep="/"), sep="\t", row.names=F)
write.table(all_rch, paste(dir_out, "output.rch_clean.txt", sep="/"), sep="\t", row.names=F)

