file_loads = "T:/Projects/Wisconsin_River/Model_Inputs/WinSLAMM_Inputs/subbasin_muni_loads.txt"
db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")

loads = read.table(file_loads, sep="\t", header=T) %>%
	mutate(
		date = as.Date(date),
		yr = as.integer(format(date, "%Y")),
		mo = as.integer(format(date, "%m")),
		da = as.integer(format(date, "%d"))
	) %>%
	select(muni, soil_type, subbasin, yr, mo, da, flow_m3, TSS_tons, P_filt_kg, P_part_kg)

loads = copy_to(
	db,
	loads,
	"urban_daily",
	indexes = list(
		"muni",
		"subbasin",
		c("muni", "mo", "yr"),
		c("subbasin", "mo", "yr")),
	temporary=F)
