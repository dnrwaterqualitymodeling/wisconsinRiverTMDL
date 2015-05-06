#Script to create ecoregion/subbasin lookup table
#Guy Hydrick, May 2015

import arcpy

ecoregions = 'Database Connections\\Connection to sde.sde\\SDEDNR.EN_ECO_LANDSCAPE_REGN_AR_250K'
subbasins = 'T:\\Projects\\Wisconsin_River\\Model_Inputs\\SWAT_Inputs\\hydro\\subbasins_minus_urban_boundaries.shp'
arcpy.env.workspace = 'C:\\Users\\hydrig\\Desktop\\EcoRegions.gdb'


arcpy.Intersect_analysis([ecoregions, subbasins], 'Inters_SB_ECO')
arcpy.Dissolve_management('Inters_SB_ECO', 'SB_by_ECO', ["Subbasin"], [["SHAPE_Area","MAX"]])
arcpy.JoinField_management('SB_by_ECO', "MAX_SHAPE_Area", 'Inters_SB_ECO', "SHAPE_Area", ["ECO_LANDSCAPE_NAME"])
arcpy.TableSelect_analysis('SB_by_ECO', 'SB_by_ECO_lookup')
arcpy.DeleteField_management('SB_by_ECO_lookup',["MAX_SHAPE_Area","SHAPE_Area","SHAPE_Length"])