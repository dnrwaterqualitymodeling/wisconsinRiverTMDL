#This python script first merges the 1/4 PLSS section agricultural land management information provided by the WRB counties, with the generalized rotations identified by the previously developed Cropland Data Layer (2008-2012) rules set - by CLU. It then merges the CLU agricultural land management with the 2011 land cover extent, as defined by the WI River TMDL modeling group.

#STEP 1
#Convert polygon features to a raster dataset - County-specific land management by PLSS 1/4 section. These polygons already have the appropriate randomizations of crop rotations by county (Sauk, Marathon, Wood, and Juneau dairies). This was done within ArcMap using the "random" function in the field calculator.

#In the raster "CropCodeRaster" note that the column "Crop_Code" was the original crop code, then the column "Crop_Code_2" is what was created after the crops were randomized using random number associations and the percentage distributions of certain crop type as given by the counties.

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Finalizing_Land_Management/Overlay_Process"
# Set local variables

#inFeatures is the 1/4 section land management shapefile as defined by the counties. As such, it's county specific but only includes information for counties with a significant agricultural presence in the WRB
inFeatures = "CropCodes_CountyApproved.shp"
valField = "Crp_Code_2"
CropCodeRaster = "CountyRotationsRaster.img"
assignmentType = "MAXIMUM_AREA"
priorityField = ""
env.cellSize = 30
env.snapRaster = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/WRB_2mileBuffer_Rotations.img"
env.extent = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/WRB_2mileBuffer_Rotations.img"
env.mask = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/WRB_2mileBuffer_Rotations.img"
env.outputCoordinateSystem = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/WRB_2mileBuffer_Rotations.img"
inCDLRasterCLU = Raster("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/CDLRotations_2mileBuffer_MajorityPerCLU.img")
inCDLRasterCLUnoContCorn = "CDLRotations_2mileBuffer_MajorityPerCLU_NoContinuousCorn"
inCDLRasterPixels = Raster("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/WRB_2mileBuffer_Rotations.img")
inDairyRaster = Raster("expandDairy.img")
inCashGrainRaster = Raster("expandCashGrain.img")
inNonAgLandCoverRaster = Raster("expandAllLandCover.img")
inLandCover = Raster("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WiRiverTmdlLandCoverDefinition.img")
inCDLnoContCorn = Raster("T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Finalizing_Land_Management/Overlay_Process/CDLRotations_2mileBuffer_MajorityPerCLU_NoContinuousCorn.img")
outLandManagementCover = ("FinalLandManagmentLandCover_04032014.img")
inCDL2MileBuffer = ("T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Cropland/CLU/CLU_WRB_2MileBuffer.shp")
RandomizedWRBRotations = ("randomizeAllRotations_04042014.img")
MajorityFilterRandomizedRotations = ("ZonalStats_RandomizedWRBRotations.img")
RandomizedRotationsIN = ("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/landcoverlandmanagementconversion_fixNas.img")

# Execute PolygonToRaster
arcpy.PolygonToRaster_conversion(inFeatures, valField, CropCodeRaster, assignmentType, priorityField, env.cellSize)

# Define the Remap Values. The original crop codes are the county specific codes used to identify the original rotation classification. They are coded to reflect the county they came from. So, a crop code beginning with "Co" for instance is a rotation in Columbia County, a crop code beginning with "CL" is a rotation in Clark County, starting with "R" is Richland County, etc. 

GeneralizedValues = RemapValue(
	[
		["Co4",300],
		["Sa7",300],
		["Co3",301],
		["Sa2",301],
		["Sa1",302],
		["D1",303],
		["R1",303],
		["La2",303],
		["T1",303],
		["Wo1/Wo2",303],
		["J2",303],
		["D3",304],
		["R2",304],
		["Mo2",304],
		["La1",304],
		["Wa3",304],
		["Wa7",304],
		["Po2",304],
		["Wo8",304],
		["J6",304],
		["V1",305],
		["V2",305],
		["A1",305],
		["Mo5",305],
		["Li2",305],
		["V4",306],
		["O2",306],
		["Pr2",306],
		["Vi2",306],
		["Ja2",306],
		["CL1",307],
		["CL3",307],
		["Ma10",307],
		["CL2",308],
		["CL4",308],
		["Ma9",308],
		["Ma2",309],
		["Ma4",309],
		["Ma1",310],
		["Ma3",310],
		["Co1",400],
		["Ma6",400],
		["Po3",400],
		["Li3",400],
		["Wa4",400],
		["Mo7",400],
		["La4",400],
		["Wa8",400],
		["Wo3/Wo4",400],
		["D4",401],
		["V6",401],
		["CL5",401],
		["R3",401],
		["T2",401],
		["Sa4",401],
		["A3", 401],
		["O1",401],
		["Vi1",401],
		["Ja1",401],
		["A2",402],
		["Sa3",402],
		["Wa2",500],
		["Po1",500],
		["Ma5",500],
		["Wa6",500],
		["J1",500],
		["O3",500],
		["Vi3",500],
		["Wo6",500],
		["A4",500],
		["La3",500],
		["Wa1",500],
		["Li5",500],
		["Wa5",500],
		["No Ag",10],
		["V7",600],
		["V5",600],
		["Cranberries",600],
		["Mo8",600],
		["D5",600],
		["Co5",600],
		["R4",600],
		["R5",600],
		["R6",600],
		["J5",600],
		["J3",600],
		["J4",600],
		["Wo7",600],
		["Po6",600],
		["Po4",600],
		["Ma8",600],
		["Ma7",600],
		["CL6",600],
		["T3",600],
		["Li6",600],
		])

# Execute RemapValue
outReclassRV = Reclassify(CropCodeRaster, valField, GeneralizedValues)

# Save the output to the Overlay Process folder. This raster is provides the generalized rotation categories where 300 values are dairy rotations, 400 values are cash grain rotations, 500 are potato/vegetable rotations, 10 are non-agricultural, and 600 are pasture/hay. However, the only rotation values that are relevant at this point are 300, 400, and 500 values as those will get merged with the CDL rotation analysis in the next steps. 

outReclassRV.save("LumpedRotationsRaster.img")

#STEP 2
#Expand land management types across entire WRB in individual rasters. 

#THE FIRST RUN IS EXECUTED FOR DAIRY ROTATIONS. 

#Variables for Step 3. These are the variables that the Expand toolbox is drawing from. The zone values are the generalized crop rotations from the land management raster dataset. The number of cells is set to 1000 because it really just needs to extend beyond the furthest distance of NoData cells. Note that zone values will have to be changed if land management generalizations are adjusted.
numberCells = 1000
zoneValues = [300, 301, 302, 303, 304, 305, 306, 307, 308, 309,310]

#Execute expand tool. This is running the expansion. 
outExpand = Expand("LumpedRotationsRaster.img", numberCells, zoneValues)

#Save the output of the expand tool run.
outExpand.save("expandDairy.img")

#THE SECOND RUN IS EXECUTED FOR CASH GRAIN ROTATIONS. 

#Variables
numberCells = 1000
zoneValues = [400, 401, 402]

#Execute expand tool. 
outExpand = Expand("LumpedRotationsRaster.img", numberCells, zoneValues)

#Save the output of the expand tool run.
outExpand.save("expandCashGrain.img")

#THE THIRD RUN IS EXECUTED FOR ALL LAND COVER TYPES FOR APPLICATION TO AG VALUES THAT FALL OUTSIDE OF CLU PARCELS

#Variables 
inLandCoverRaster = "WIRiverTmdlLandCoverDefinition.img"
numberCells = 50
zoneValues = [111, 121, 131, 141, 152, 171, 190, 195, 250]

#Execute expand tool. 
outExpand = Expand(inRaster, numberCells, zoneValues)

#Save the output of the expand tool run.
outExpand.save("expandAllLandCover.img")

# Note that there is no expansion for potato/vegetable rotations because there is only one generalized class for them - value "500"
 
#STEP 3

# Create random raster of values 0 through 100 for apportioning continuous corn rotations into 50% dairy and 50% cash grain from the nearest expanded management schemes of those respective rotations types. Previous field-scale analysis in Pleasant Valley as welk as conversation with county/regional conservation staff showed that dairy was misclassified as continuous corn approximately 50% of the time and as cash grain 50% of the time.

# This step creates the random raster

arcpy.CreateRandomRaster_management(env.scratchFolder, "random", "UNIFORM 0.0 100.0", CropCodeRaster, env.cellSize)
arcpy.CopyRaster_management(env.scratchFolder + '/random', env.scratchGDB + '/random')
random = Raster(env.scratchGDB + '/random')

# This step splits the continuous corn CDL analysis pixels into 50% dairy pixels and 50% cash grain pixels using uniform random values
step1 = Con(BooleanAnd(inCDLRasterCLU == 1, random1 <= 50), 3, inCDLRasterCLU)
step2 = Con(step1 == 1, 2, step1)

step2.save(inCDLRasterCLUnoContCorn)

#STEP 4

#Merge individual land management raster files into their respective CDL defined rotation categories.

#Execute Con Function. Used "if, else" statements to create pixel-based land cover with land management incorporated.

#First the function applies non-ag values from the 2011 fixed extent land cover to the ""null"" values in the CDLRasterCLU. Then, it applies a value of 800 to all pasture/hay, as identified by the CDL rotation analysis. Following that, it applies generalized cash grain values 400-402 to all of the cash grain rotations identified by the CDL rotation analysis. Next, it applies generalized dairy rotation values 300-310 to all of the dairy rotations identified by the CDL rotation analysis - values 3 and 4. Then, it applies generalized potato/vegetable rotation value 500 to the potato/vegetable rotations identified by the CDL rotation analysis. Finally, if there are still random ""ag"" pixels in the land cover that were misidentified by the CDL years, it applies the expanded non-ag land cover types to the leftover pixels.  

outCon = Con(IsNull(inCDLRasterCLUnoContCorn), inNonAgLandCoverRaster,
	Con(inCDLRasterCLUnoContCorn == 6, 800,
			Con(inCDLRasterCLUnoContCorn == 2, inCashGrainRaster,
				Con(inCDLRasterCLUnoContCorn == 3, inDairyRaster,
					Con(inCDLRasterCLUnoContCorn == 4, inDairyRaster,
						Con(inCDLRasterCLUnoContCorn == 5, 500,
							Con(inLandCover == 1, inNonAgLandCoverRaster, inNonAgLandCoverRaster)))))))
#Save the outputs 
outCon.save(outLandManagementCover)

#STEP 5

# Here we randomize ALL CROP ROTATIONS into three subcategories to offset the crop sequence. So, for instance, a rotations of Cg-Cs-OA/A-A-A-A will be subdivided into Cg-Cs-O/A-A-A-A, Cs-O/A-A-A-A-Cg, O/A-A-A-A-Cg-Cs. This will represent itself as 300, for example, going to 300a, 300b, and 300c.

# Multiply the landcover/land Management dataset by 10 to get unique values for boolean addition in next steps.

step3 = Raster(outLandManagementCover)*10

# "random" is still saved in the scratch Arc database. Here we split the randomized raster "random" into thirds. 

split1 = random <= 33.33
split2 = BooleanAnd(random > 33.33, random <= 66.66)
split3 = random > 66.66

# Now create three variatios of each crop ROTATION, excluding static land cover types. 

step4 = Con(BooleanAnd(BooleanAnd(step3 >= 3000, step3 < 8000), split1), step3 + 1, step3)
step5 = Con(BooleanAnd(BooleanAnd(step4 >= 3000, step4 < 8000), split2), step4 + 2, step4)
step6 = Con(BooleanAnd(BooleanAnd(step5 >= 3000, step5 < 8000), split3), step5 + 3, step5)

step6.save("randomizeAllRotations_04042014.img")

# Finally, run zonal statistics for crop rotations to expand them once again to the CLU boundaries, otherwise the fields look like fuzzy television screens since it was randomized by pixel. 

# Set local variable, zones are CLU boundaries for WRB 2 mile buffer.
zoneField = "CLUID"

# Execute ZonalStatistics
outZonalStatistics = ZonalStatistics(inCDL2MileBuffer, zoneField, RandomizedWRBRotations,
                                     "MAJORITY", "NODATA")

# Save the output 
outZonalStatistics.save("ZonalStats_RandomizedWRBRotations.img")

# FINAL STEPSSSSSSSSSSS. Now, merge the zonal statistics randomized crop rotations per CLU with other landcover types (i.e. those outside the CLU boundaries).


outLandCoverManagementCombined = Con("LandCoverLandManagementWithAllRandomizationsAndZonalStatistics3.img" == 0, inNonAgLandCoverRaster, "LandCoverLandManagementWithAllRandomizationsAndZonalStatistics3.img")

outLandCoverManagementCombined.save("LandCoverLandManagementWithAllRandomizationsAndZonalStatistics.img")



 Define the Remap Values. The original crop codes are the county specific codes used to identify the original rotation classification. They are coded to reflect the county they came from. So, a crop code beginning with "Co" for instance is a rotation in Columbia County, a crop code beginning with "CL" is a rotation in Clark County, starting with "R" is Richland County, etc. 

 # REMAP ROTATION/LAND COVER VALUES TO 55 categories (45 rotations and 10 land cover types). 
 
SWATreadyValues = RemapValue(
	[
		["111",1],
		["121",2],
		["131",3],
		["141",4],
		["152",5],
		["171",6],
		["190",7],
		["195",8],
		["250",9],
		["3001",10],
		["3002",11],
		["3003",12],
		["3011",13],
		["3012",14],
		["3013",15],
		["3021",16],
		["3022",17],
		["3023",18],
		["3031",19],
		["3032",20],
		["3033",21],
		["3041",22],
		["3042",23],
		["3043",24],
		["3051",25],
		["3052",26],
		["3053",27],
		["3061",28],
		["3062",29],
		["3063",30],
		["3071",31],
		["3072",32],
		["3073",33],
		["3081",34],
		["3082",35],
		["3083",36],
		["3091",37],
		["3092",38],
		["3093",39],
		["3101",40],
		["3102",41],
		["3103",42],
		["4001",43],
		["4002",44],
		["4003",45],
		["4011",46],
		["4012",47],
		["4013",48],
		["4021",49],
		["4022",50],
		["4023",51],
		["5001",52],
		["5002",53],
		["5003",54],
		["8000",55],
		])

# Execute RemapValue
outRandomizedRasterReclass = Reclassify("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/asciitoRaster_noZeros", "VALUE", SWATreadyValues)

# Save the output to the Overlay Process folder. 

outReclassRV.save("T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Finalizing_Land_Management/LandCoverLandManagement_FinalProduct.img")






























