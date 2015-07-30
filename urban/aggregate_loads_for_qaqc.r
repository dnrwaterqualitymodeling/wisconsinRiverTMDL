f = "C:/TEMP/subbasin_muni_loads.txt"
d = read.table(f, sep="\t", header=T)

by_muni_sub = aggregate(cbind(flow_m3,TSS_tons,P_filt_kg,P_part_kg) ~ muni + subbasin + date, data=d, FUN=sum)
by_muni_sub = by_muni_sub[order(by_muni_sub$muni,by_muni_sub$subbasin,by_muni_sub$date),]

write.table(
	by_muni_sub,
	"T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads_agg_soil.txt",
	sep="\t",
	row.names=F
)

by_muni_sub_yr = aggregate(cbind(flow_m3,TSS_tons,P_filt_kg,P_part_kg) ~ muni + subbasin, data=d, FUN=sum)
by_muni_sub_yr = by_muni_sub_yr[order(by_muni_sub_yr$muni,by_muni_sub_yr$subbasin),]
by_muni_sub_yr[,3:6] = by_muni_sub_yr[,3:6] / 12

write.table(
	by_muni_sub_yr,
	"T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads_agg_yr.txt",
	sep="\t",
	row.names=F
)