import arcpy, time
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
env.overwriteOutput = True

landcoverDir = 'T:/Projects/Wisconsin_River/GIS_Datasets/landcover'
outLc = landcoverDir + '/WiRiverTmdlLandCoverDefinition_fullForestDef.img'

cdl = landcoverDir + '/Crop_Data_Layers_2008_2012/WTM_Reprojection/wrb_cdl_2011'

crp = landcoverDir + '/CRP/CRP_Parcels_WRB_Clip.shp'
wwi = landcoverDir + '/Wetland/wwi.tif'
cran = landcoverDir + '/Cranberries/Cranberries_Hand_Digitized_WRB_Clip.shp'

crpRaster = landcoverDir + '/CRP/CRP_Parcels_WRB_Clip_raster.img'
cranDissolve = landcoverDir + '/Cranberries/Cranberries_Hand_Digitized_WRB_Clip_dissolve.shp'
cranRaster = landcoverDir + '/Cranberries/Cranberries_Hand_Digitized_WRB_Clip_raster.img'

env.extent = cdl
env.snapRaster = cdl
env.cellsize = cdl

lcs = [\
	"Agriculture",\
	"Open water",\
	"Developed",\
	"Barren",\
	"Deciduous Forest",\
	"Evergreen Forest",\
	"Mixed Forest",\
	"Shrubland",\
	"Grassland / herbaceous",\
	"Woody wetlands",\
	"Herbaceous wetlands",\
	"Cranberries"\
]
# CDL reclassification
cdlRemap = RemapRange(
	[
		[1,71,1], # Agriculture
		[111,111,111], # Open Water
		[121,124,121], # Developed
		[131,131,131], # Barren
		[141,141,141], # Deciduous Forest
		[142,142,142], # Evergreen Forest
		[143,143,143], # Mixed Forest
		[152,152,152], # Shrubland
		[171,171,171], # Grassland / herbaceous
		[181,181,1], # Agriculture
		[190,190,190], # Woody wetland
		[195,195,195], # Herbaceous wetland
		[205,243,1], # Agriculture
		[250,250,250] # Cranberries
	]
)
cdlReclass = Reclassify(cdl, "Value", cdlRemap)

arcpy.AddField_management(crp, "VALUE", "SHORT")
arcpy.CalculateField_management(crp, "VALUE", 171)
arcpy.PolygonToRaster_conversion(crp, "VALUE", crpRaster, "MAXIMUM_COMBINED_AREA", '', cdl)

arcpy.AddField_management(cran, "VALUE", "SHORT")
arcpy.CalculateField_management(cran, "VALUE", 250)
arcpy.Dissolve_management(cran, cranDissolve, "VALUE", "", "SINGLE_PART")
arcpy.PolygonToRaster_conversion(cranDissolve, "VALUE", cranRaster, "MAXIMUM_COMBINED_AREA", '', cdl)

cdlOverlay = Con(IsNull(crpRaster), cdlReclass, crpRaster)
wwiOverlay = Con(IsNull(wwi), cdlOverlay, wwi)
cranOverlay = Con(IsNull(cranRaster), wwiOverlay, cranRaster)

cranOverlay.save(outLc)

arcpy.AddField_management(outLc, "CLASS", "TEXT", "", "", "25")
rows = arcpy.UpdateCursor(outLc)
for i,row in enumerate(rows):
	row.Class = lcs[i]
	rows.updateRow(row)
del row, rows





