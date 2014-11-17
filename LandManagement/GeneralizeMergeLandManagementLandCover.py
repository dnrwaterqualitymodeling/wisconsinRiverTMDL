#This python script first merges the 1/4 PLSS section agricultural land management information provided by the WRB counties, with the generalized rotations identified by the previously developed Cropland Data Layer (2008-2012) rules set - by CLU. It then merges the CLU agricultural land management with the 2011 land cover extent, as defined by the WI River TMDL modeling group.

#STEP 1
#Convert polygon features to a raster dataset - County-specific land management by PLSS 1/4 section. These polygons already have the appropriate randomizations of crop rotations by county (Sauk, Marathon, Wood, and Juneau dairies). This was done within ArcMap using the "random" function in the field calculator.

#In the raster "CropCodeRaster" note that the column "Crop_Code" was the original crop code, then the column "Crop_Code_2" is what was created after the crops were randomized using random number associations and the percentage distributions of certain crop type as given by the counties.

# Import system modules
import arcpy
import shutil
import os
import numpy as np
from arcpy import env
from arcpy.sa import *

# Set local variables
inCDLRasterCLU = "CDLRotations_2mileBuffer_MajorityPerCLU.img"
inCDLRasterPixels = "WRB_2mileBuffer_Rotations.img"
inLandCover = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WiRiverTmdlLandCoverDefinition.img"
inCDL2MileBuffer = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Cropland/CLU/CLU_WRB_2MileBuffer.shp"
crop_codes = "CropCodes_CountyApproved.shp"
remap1_txt = "remap_from_county_codes_to_numeric_code.txt"
remap2_txt = "remap_from_numeric_codes_to_swat_codes.txt"

# Copy files locally for processing speed
overlay_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Finalizing_Land_Management/Overlay_Process"
rotation_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations"
local_dir = env.scratchFolder + '/land_mgt_overlay'
if os.path.exists(local_dir):
	shutil.rmtree(local_dir)
os.makedirs(local_dir)
input_files = []
for dir in [overlay_dir, rotation_dir]:
	for f in os.listdir(dir):
		input_files.append(dir + '/' + f)
		if not os.path.exists(local_dir + '/' + f):
			shutil.copyfile(dir + '/' + f, local_dir + '/' + f)
for other_file in [inLandCover, inCDL2MileBuffer]:
	input_files.append(local_dir + os.path.basename(other_file))
	if not os.path.exists(local_dir + '/' + os.path.basename(other_file)):
		arcpy.Copy_management(other_file, local_dir + '/' + os.path.basename(other_file))
inLandCover = local_dir + '/' + os.path.basename(inLandCover)
inCDL2MileBuffer = local_dir + '/' + os.path.basename(inCDL2MileBuffer)

# Set environment settings
arcpy.CheckOutExtension("Spatial")
os.chdir(local_dir)
env.workspace = local_dir
env.snapRaster = inCDLRasterPixels
env.extent = inCDLRasterPixels
env.mask = inCDLRasterPixels
env.outputCoordinateSystem = inCDLRasterPixels
env.cellSize = 30

# inFeatures is the 1/4 section land management shapefile as defined by the counties.
# As such, it's county specific but only includes information for counties with a significant agricultural presence in the WRB
CropCodeRaster = "CountyRotationsRaster.img"
arcpy.PolygonToRaster_conversion(crop_codes, "Crp_Code_2", CropCodeRaster, "MAXIMUM_AREA", '', 30)

# Define the Remap Values.
# The original crop codes are the county specific codes used to identify the original rotation classification.
# They are coded to reflect the county they came from.
# So, a crop code beginning with "Co" for instance is a rotation in Columbia County,
#	crop code beginning with "CL" is a rotation in Clark County,
#	starting with "R" is Richland County, etc.
# See "remap_from_county_codes_to_numeric_code.txt" for details

remap1_list = eval(open(remap1_txt, 'r').read())
GeneralizedValues = RemapValue(remap1_list)
outReclassRV = Reclassify(CropCodeRaster, "Crp_Code_2", GeneralizedValues)

# Save the output to the Overlay Process folder.
# This raster is provides the generalized rotation categories:
#	300 values are dairy rotations
#	400 values are cash grain rotations
#	500 are potato/vegetable rotations
#	10 are non-agricultural
#	600 are pasture/hay.
# However, the only rotation values that are relevant at this point are 300, 400, and 500 values.
# Those will get merged with the CDL rotation analysis in the next steps. 

outReclassRV.save("LumpedRotationsRaster.img")

#STEP 2
#Expand land management types across entire WRB in individual rasters. 

#THE FIRST RUN IS EXECUTED FOR DAIRY ROTATIONS. 

# Variables for Step 3.
# These are the variables that the Expand toolbox is drawing from.
# The zone values are the generalized crop rotations from the land management raster dataset.
# The number of cells is set to 1000 because it really just needs to extend beyond the furthest distance of NoData cells.
# Note that zone values will have to be changed if land management generalizations are adjusted.

numberCells = 1000
zoneValues = [300, 301, 302, 303, 304, 305, 306, 307, 308, 309,310]
outExpand = Expand("LumpedRotationsRaster.img", numberCells, zoneValues)
# expandDairy = arcpy.RasterToNumPyArray(outExpand)
outExpand.save("expandDairy.img")

#THE SECOND RUN IS EXECUTED FOR CASH GRAIN ROTATIONS. 
zoneValues = [400, 401, 402]
outExpand = Expand("LumpedRotationsRaster.img", numberCells, zoneValues)
# expandCashGrain = arcpy.RasterToNumPyArray(outExpand)
outExpand.save("expandCashGrain.img")

#THE THIRD RUN IS EXECUTED FOR ALL LAND COVER TYPES FOR APPLICATION TO AG VALUES THAT FALL OUTSIDE OF CLU PARCELS
numberCells = 50
zoneValues = [111, 121, 131, 141, 152, 171, 190, 195, 250]
outExpand = Expand(inLandCover, numberCells, zoneValues)
# expandAllLandCover = arcpy.RasterToNumPyArray(outExpand)
outExpand.save("expandAllLandCover.img")

# Note that there is no expansion for potato/vegetable rotations because there is only one generalized class for them - value "500"
 
#STEP 3

# Create random raster of values 0 through 100.
# These will be used to apportion continuous corn rotations into 50% dairy and 50% cash grain
#	from the nearest expanded management schemes of those respective rotations types.
# Previous field-scale analysis in Pleasant Valley as well as conversation with county/regional conservation staff
#	showed that dairy was misclassified as continuous corn approximately 50% of the time and as cash grain 50% of the time.

# This step creates the random raster

arcpy.CreateRandomRaster_management(local_dir, "random.img", "UNIFORM 0.0 100.0", CropCodeRaster, env.cellSize)
random = Raster(local_dir + '/random.img')

# This step splits the continuous corn CDL analysis pixels into 50% dairy pixels and 50% cash grain pixels using uniform random values
step1 = Con(BooleanAnd(inCDLRasterCLU == 1, random <= 50), 3, inCDLRasterCLU)
step2 = Con(step1 == 1, 2, step1)
step2.save("step2.img")

#STEP 4

# Merge individual land management raster files into their respective CDL defined rotation categories.

# First the function applies non-ag values from the 2011 fixed extent land cover to the ""null"" values in the CDLRasterCLU.
# Then, it applies a value of 800 to all pasture/hay, as identified by the CDL rotation analysis.
# Following that, it applies generalized cash grain values 400-402 to all of the cash grain rotations identified by the CDL rotation analysis.
# Next, it applies generalized dairy rotation values 300-310 to all of the dairy rotations identified by the CDL rotation analysis - values 3 and 4.
# Then, it applies generalized potato/vegetable rotation value 500 to the potato/vegetable rotations identified by the CDL rotation analysis.
# Finally, if there are still random "ag" pixels in the land cover that were misidentified by the CDL years,
#	it applies the expanded non-ag land cover types to the leftover pixels.  

step_2a = SetNull(step2 == 7, step2)
step_2a = SetNull(BooleanAnd(step_2a >=1, step_2a <= 7), step2)
step_2b = Con(BooleanAnd(step_2a >=1, step_2a <= 7), step_2a, "expandAllLandCover.img")
step_2c = Con(step_2b == 6, 800, step_2b)
step_2d = Con(step_2c == 2, "expandCashGrain.img", step_2c)
step_2e = Con(BooleanOr(step_2d == 3, step_2d == 4), "expandDairy.img", step_2d)
step_2f = Con(step_2e == 5, 500, step_2e)
step_2g = Con(IsNull(step_2f), "expandAllLandCover.img", step_2f)


Con(inLandCover == 1, "expandAllLandCover.img", ste)


lmlc = Con(IsNull(step2), "expandAllLandCover.img",
	Con(step2 == 6, 800,
		Con(step2 == 2, "expandCashGrain.img",
			Con(step2 == 3, "expandDairy.img",
				Con(step2 == 4, "expandDairy.img",
					Con(step2 == 5, 500,
						Con(inLandCover == 1, "expandAllLandCover.img", "expandAllLandCover.img")))))))
# Save the outputs
lmlc = Raster(lmlc * 10)
#STEP 5

# Here we randomize ALL CROP ROTATIONS into three subcategories to offset the crop sequence.
# So, for instance, a rotations of Cg-Cs-OA/A-A-A-A will be subdivided into Cg-Cs-O/A-A-A-A, Cs-O/A-A-A-A-Cg, O/A-A-A-A-Cg-Cs.
# This will represent itself as 300, for example, going to 300a, 300b, and 300c.

# "random" is still saved in the scratch Arc database. Here we split the randomized raster "random" into thirds. 

split1 = random <= 33.33
split2 = BooleanAnd(random > 33.33, random <= 66.66)
split3 = random > 66.66

# Now create three variatios of each crop ROTATION, excluding static land cover types
step4 = Con(BooleanAnd(BooleanAnd(step3 >= 3000, step3 < 8000), split1), lmlc + 1, lmlc)
step5 = Con(BooleanAnd(BooleanAnd(step4 >= 3000, step4 < 8000), split2), step4 + 2, step4)
step6 = Con(BooleanAnd(BooleanAnd(step5 >= 3000, step5 < 8000), split3), step5 + 3, step5)
step6.save("randomizeAllRotations.img")

# Finally, run zonal statistics for crop rotations to expand them once again to the CLU boundaries,
#	otherwise the fields look like fuzzy television screens since it was randomized by pixel. 
outZonalStatistics = ZonalStatistics(inCDL2MileBuffer, "CLUID", step6, "MAJORITY", "NODATA")
outZonalStatistics.save("ZonalStats_RandomizedWRBRotations.img")

# FINAL STEPS. Now, merge the zonal statistics randomized crop rotations per CLU with other landcover types (i.e. those outside the CLU boundaries).

outLandCoverManagementCombined = Con(outZonalStatistics == 0,
	"expandAllLandCover.img",
	outZonalStatistics)
outLandCoverManagementCombined.save("LandCoverLandManagementWithAllRandomizationsAndZonalStatistics.img")

# outLandCoverManagementCombined = Con("LandCoverLandManagementWithAllRandomizationsAndZonalStatistics3.img" == 0,
	# inNonAgLandCoverRaster,
	# "LandCoverLandManagementWithAllRandomizationsAndZonalStatistics3.img")
# outLandCoverManagementCombined.save("LandCoverLandManagementWithAllRandomizationsAndZonalStatistics.img")

# REMAP ROTATION/LAND COVER VALUES TO 55 categories (45 rotations and 10 land cover types).
remap2_list = eval(open(remap2_txt, 'r').read())
SWATreadyValues = RemapValue(remap2_list)
# outRandomizedRasterReclass = Reclassify("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/asciitoRaster_noZeros", "VALUE", SWATreadyValues)
outRandomizedRasterReclass = Reclassify(outLandCoverManagementCombined, "VALUE", SWATreadyValues)


# Save the output to the Overlay Process folder. 

outRandomizedRasterReclass.save("LandCoverLandManagement_FinalProduct.img")
