library(dplyr)
library(tidyr)

db = src_sqlite("C:/TEMP/WRB.Sufi2.SwatCup/wrb_swat_db.sqlite3")

lulc_long = tbl(db, "eagle_river_lulc_composition") %>%
	mutate_each_(funs(cast,numeric), "AREA") %>%
	collect()


dat %>% mutate_each_(funs(factor), l1) %>% mutate_each_(funs(as.numeric), l2)
lulc_wide = spread(lulc_long, LULC, AREA)

lulc_wide = lulc_wide %>%
	mutate(
		AGRL=AGRL * 9e-4,
		FRSD=FRSD * 9e-4,
		FRSE=FRSE * 9e-4,
		FRST=FRST * 9e-4,
		PAST=PAST * 9e-4,
		URML=URML * 9e-4,
		WATR=WATR * 9e-4,
		WETN=WETN * 9e-4,
		WATERSHED_AREA_SQ_KM =
			AGRL + FRSD + FRSE + FRST + PAST + URML + WATR + WETN
	) %>%
	select(
		CATCHID,
		WATERSHED_AREA_SQ_KM,
		WATR,
		URML,
		PAST,
		FRSD,
		FRSE,
		FRST,
		AGRL,
		WETN
	)
