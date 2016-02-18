library(dplyr)

f = "C:/TEMP/WRB.Sufi2.SwatCup/output.hru"

hru = read.table(f, skip=9)
names(hru) = c("LULC", "HRU", "GIS", "SUB", "MGT", "MO", "DA", "YR", "AREAkm2",
	"SYLD", "NAUTO", "W_STRS", "TMP_STRS", "N_STRS", "P_STRS", "BIOM")

mean_biom = hru %>%
	filter(LULC == "BROS") %>%
	group_by(YR, MO, DA) %>%
	summarise(BIOM = mean(BIOM), NAUTO=mean(NAUTO)) %>%
	mutate(DATE=as.Date(paste(YR,MO,DA,sep="-")))

plot(BIOM ~ DATE, data=mean_biom)
