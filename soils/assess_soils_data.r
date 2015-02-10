options(stringsAsFactors=F)
library(ggplot2)

dir_net_soil = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"

file_agg_soil_units = "aggregated_soil_units.txt"
file_agg_profs = "aggregated_profiles.txt"

swat_soils = read.delim(paste(dir_net_soil, file_agg_soil_units, sep ='/'))
profs = read.delim(paste(dir_net_soil, file_agg_profs, sep ='/'))

profs["hydgrp"] = substr(profs$hru_grp, 1, 1)

tst = dcast(profs, )

txtrs = subset(profs, variable %in% c("sand", "silt", "clay",))
text = ggplot(profs, aes(x=))