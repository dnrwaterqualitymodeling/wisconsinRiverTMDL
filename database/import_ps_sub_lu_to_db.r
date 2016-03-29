library(dplyr)

file_lu = "T:/Projects/Wisconsin_River/Model_Documents/Point_Source_Info/FINAL_WASTEWATER_DATASET/WRB_Outfalls_FINAL_MAY2015.dbf"
db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")

lu = foreign::read.dbf(file_lu) %>%
	mutate(SAMPLE_ID = SAMPLE_PT, SUB=SUBBASIN) %>%
	select(SAMPLE_ID, SUB)

lu = copy_to(
	db,
	lu,
	"point_source_subbasin_xref",
	indexes = list("SAMPLE_ID"),
	temporary=F)
