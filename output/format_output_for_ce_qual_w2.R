library(dplyr)
library(stringr)

#dir_out = "T:/Projects/Wisconsin_River/Model_Documents/LimnoTech/swat_data_transfer"
dir_out = "C:/Users/ruesca/Desktop/Limnotech"

output.hru = readLines("C:/Users/ruesca/Desktop/sub_and_reach/output.hru")
output.rch = readLines("C:/Users/ruesca/Desktop/sub_and_reach/output.rch")
output.sub = readLines("C:/Users/ruesca/Desktop/sub_and_reach/output.sub")

sub_codes = c(143, 60, 73, 74, 59)
rch_codes = c(333, 255, 76, 254, 253, 251, 252, 60)
#sub_codes = format(sub_codes, width=3)
rch_codes = format(rch_codes, width=5)
#hru_codes = c(1312, 987)

sub_codes = str_pad(sub_codes, 5, pad="0")
pat = paste(sub_codes, collapse="|")
output.hru = c(
	output.hru[1:9],
	output.hru[grep(pat, substr(output.hru, 11, 15))]
)
writeLines(output.hru, paste(dir_out, "output.hru", sep="/"))

pat = paste(rch_codes, collapse="|")
output.rch = c(
	output.rch[1:9],
	output.rch[grep(pat, substr(output.rch, 6, 10))]
)
writeLines(output.rch, paste(dir_out, "output.rch", sep="/"))
#
#pat = paste(sub_codes, collapse="|")
#output.sub = c(
#	output.sub[1:9],
#	output.sub[grep(pat, substr(output.sub, 7, 10))]
#)
#writeLines(output.sub, paste(dir_out, "output.sub", sep="/"))


all_sub = data.frame()
for (sub_code in as.numeric(sub_codes)) {
	print(sub_code)
	sub_output.hru = output.hru[
		grep(
			str_pad(sub_code, 5, pad="0"),
			substr(output.hru, 11, 15))]
	sub_output.hru = c(output.hru[9], sub_output.hru)
	sub_output.hru = read.table(
		text=paste(sub_output.hru, collapse="\n"),
		header=T
	)
	area = sum(
		sub_output.hru %>%
		group_by(HRU) %>%
		summarise(area_sq_km = AREAkm2[1]) %>%
		select(area_sq_km))
	sub_output.hru = sub_output.hru %>%
		mutate(TPkg.ha=ORGPkg.ha + SEDPkg.ha + SOLPkg.ha + P_GWkg.ha) %>%
		select(
			MO,
			DA,
			YR,
			AREAkm2,
			WYLDmm,
			SYLDt.ha,
			ORGPkg.ha,
			SEDPkg.ha,
			SOLPkg.ha,
			P_GWkg.ha,
			TPkg.ha) %>%
		group_by(MO,DA,YR) %>%
		summarise(
			WYLDmm=weighted.mean(WYLDmm, AREAkm2),
			SYLDt.ha=weighted.mean(SYLDt.ha, AREAkm2),
			ORGPkg.ha=weighted.mean(ORGPkg.ha, AREAkm2),
			SEDPkg.ha=weighted.mean(SEDPkg.ha, AREAkm2),
			SOLPkg.ha=weighted.mean(SOLPkg.ha, AREAkm2),
			P_GWkg.ha=weighted.mean(P_GWkg.ha, AREAkm2),
			TPkg.ha=weighted.mean(TPkg.ha, AREAkm2)
		) %>%
		ungroup() %>%
		arrange(YR,MO,DA)
	all_sub = rbind(
		all_sub,
		data.frame(
			subbasin = sub_code,
			area_sq_km = area,
			date = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by="1 day"),
			water_yield_mm = sub_output.hru$WYLDmm,
			sediment_yield_metric_tons_per_ha = sub_output.hru$SYLDt.ha,
			organic_p_yield_kg_per_ha = sub_output.hru$ORGPkg.ha,
			sediment_p_yield_kg_per_ha = sub_output.hru$SEDPkg.ha,
			soluble_p_yield_kg_per_ha = sub_output.hru$SOLPkg.ha,
			groundwater_p_yield_kg_per_ha = sub_output.hru$P_GWkg.ha,
			total_p_yield_kg_per_ha = sub_output.hru$P_GWkg.ha
		)
	)
}

all_sub %>%
	filter(as.numeric(format(date, "%Y")) >= 2010) %>%
	mutate(
		sediment_yield_metric_tons =
			sediment_yield_metric_tons_per_ha * area_sq_km * 100,
		tp_yield_kg = total_p_yield_kg_per_ha * area_sq_km * 100
	) %>%
	group_by(subbasin, format(date, "%Y")) %>%
	summarise(
		water_yield_mm = sum(water_yield_mm),
		sediment_yield_metric_tons = sum(sediment_yield_metric_tons),
		tp_yield_kg = sum(tp_yield_kg)
	) %>%
	group_by(subbasin) %>%
	summarise(
		water_yield_mm = mean(water_yield_mm),
		sediment_yield_metric_tons = mean(sediment_yield_metric_tons),
		tp_yield_kg = mean(tp_yield_kg)
	)

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

