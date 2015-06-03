#Script to create ecoregion/subbasin lookup table
#Guy Hydrick, May 2015

import arcpy

#ecoregions = 'Database Connections\\Connection to sde.sde\\SDEDNR.EN_ECO_LANDSCAPE_REGN_AR_250K'
# omernik level 4 manually aggregated
ecoregions = "T:\\Projects\\Wisconsin_River\\GIS_Datasets\\Ecoregions\\ecoregions_level_iv_dissolve.shp"
subbasins = 'T:\\Projects\\Wisconsin_River\\Model_Inputs\\SWAT_Inputs\\hydro\\subbasins.shp'
FOIs = ["Subbasin", "wi_river"]
out_table = 'T:\\Projects\\Wisconsin_River\\Model_Inputs\\SWAT_Inputs\\hydro\\SB_by_ECO_lookup_level_4.txt'

arcpy.env.workspace = 'C:\\Users\\ruesca\\Desktop\\EcoRegions.gdb'

arcpy.Intersect_analysis([ecoregions, subbasins], 'Inters_SB_ECO')
arcpy.Dissolve_management('Inters_SB_ECO', 'SB_by_ECO', FOIs[0], [["SHAPE_Area","MAX"]])
arcpy.JoinField_management('SB_by_ECO', "MAX_SHAPE_Area", 'Inters_SB_ECO', "SHAPE_Area", FOIs[1])


f = open(out_table, "w")
f.write("\t".join(FOIs) + "\n")
with arcpy.da.SearchCursor('SB_by_ECO', FOIs) as cursor:
	for row in cursor:
		f.write("\t".join(map(str, row)) + "\n")
del cursor
f.close()