library(rgdal)
library(rgeos)
options(stringsAsFactors=F)

file_eco_land = "T:/Projects/Wisconsin_River/GIS_Datasets/Ecoregions/ecological_landscapes_dissolve.shp"
file_subbasins = 	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/subbasins.shp"
file_out = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/SB_by_ECO_lookup_ecological_landscapes.txt"
file_u = tempfile(fileext=".shp")


ln1 = "import arcpy"
ln2 = paste(
	"arcpy.Union_analysis(['",
	file_eco_land,
	"','",
	file_subbasins,
	"'],r'",
	file_u,
	"')",
	sep=""
)
py_script = paste(
	ln1,
	ln2,
	sep="\n"
)

file_py = tempfile(fileext=".py")
writeLines(py_script, file_py)
cmd = paste(
	"C:\\Python27\\ArcGIS10.1\\python.exe",
	file_py,
	sep=" "
)
system(cmd)

u = readOGR(
	dirname(file_u),
	strsplit(basename(file_u), "\\.")[[1]][1]
)

u@data$area = gArea(u, byid=T)

sub_by_eco = data.frame(
	subbasin = 1:337,
	eco_land = NA
)
for (s in 1:337) {
	sub_d = aggregate(
		area ~ ECO_LAND_G,
		data = subset(u@data, Subbasin == s),
		sum
	)
	eco_land = sub_d$ECO_LAND_G[which.max(sub_d$area)]
	sub_by_eco$eco_land[sub_by_eco$subbasin==s] = eco_land
}
write.table(sub_by_eco, file_out, row.names=F)