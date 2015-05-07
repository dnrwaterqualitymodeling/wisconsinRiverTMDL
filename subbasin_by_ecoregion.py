#Script to create ecoregion/subbasin lookup table
#Guy Hydrick, May 2015

import arcpy

#ecoregions = 'Database Connections\\Connection to sde.sde\\SDEDNR.EN_ECO_LANDSCAPE_REGN_AR_250K'
ecoregions = 'C:\\Users\\hydrig\\Desktop\\us_eco_l3_no_st.shp'	#omernik level 3
subbasins = 'T:\\Projects\\Wisconsin_River\\Model_Inputs\\SWAT_Inputs\\hydro\\subbasins_minus_urban_boundaries.shp'
FOIs = ["Subbasin", "US_L3NAME"]
out_table = 'T:\\Projects\\Wisconsin_River\\Model_Inputs\\SWAT_Inputs\\hydro\\SB_by_ECO_lookup.txt'
arcpy.env.workspace = 'C:\\Users\\hydrig\\Desktop\\EcoRegions.gdb'

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