options(stringsAsFactors=F)
setwd("T:/Projects/Wisconsin_River/GIS_Datasets/Soil_Phosphorus")

area_lu = read.csv("subbasin_county_union_area.txt")[,c(2,3,5)]
soil_tbl = read.csv("WRB_Counties_soil_P_org_C_1995.csv")
soil_tbl = soil_tbl[c("Variable", "County", "Mean")]
soil_tbl = dcast(soil_tbl, County ~ Variable, value.var="Mean")
soil_tbl = subset(soil_tbl, County != "all")
names(soil_tbl)[2:3] = c("OM", "P")

soil_area = merge(area_lu, soil_tbl, by.x="CTY_NAME", by.y="County")

sub_area = aggregate(Area ~ Subbasin, soil_area, sum)
OM = aggregate(Area * OM ~ Subbasin, soil_area, sum)[,2] / sub_area[,2]
P = aggregate(Area * P ~ Subbasin, soil_area, sum)[,2] / sub_area[,2]

subbasin_tbl = data.frame(
    Subbasin=sub_area$Subbasin,
    OM=OM * 10000, # convert % to ppm
    P=P)

subbasin_tbl$SOLP = subbasin_tbl$P * 0.5 # Vadas and White, 2010
prop_p = 1.3 / sum(140,10,1.3,1.3)
# based on C/N/P/S 140:10:1.3:1.3 from
# http://soils.wisc.edu/facstaff/barak/soilscience326/phosphorus.htm
subbasin_tbl$ORGP = subbasin_tbl$OM * prop_p
subbasin_tbl = subbasin_tbl[,c(1,4,5)]

write.table(subbasin_tbl, "soil_phosphorus_by_subbasin.txt", row.names=F, sep="\t")
