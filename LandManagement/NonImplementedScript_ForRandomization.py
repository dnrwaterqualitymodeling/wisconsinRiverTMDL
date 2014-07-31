#Name: RandomPointsRandomValues.py
#Purpose: create random points with random values

# Import system modules
import arcpy, os, random
from arcpy import env

# Create random points in the features of a constraining feature class
# Number of points for each feature determined by the value in the field specified
outGDB = "C:/data/county.gdb"
outName = "randpeople"
conFC = "C:/data/county.gdb/blocks"
numField = "POP2000"
arcpy.CreateRandomPoints_management(outGDB, outName, conFC, "", numField)

# set workspace
env.workspace = "C:/data/county.gdb"

# Create fields for random values
fieldInt = "fieldInt"
fieldFlt = "fieldFlt"
arcpy.AddField_management(outName, fieldInt, "LONG") # add long integer field
arcpy.AddField_management(outName, fieldFlt, "FLOAT") # add float field

# Calculate random values between 1-100 in the new fields
arcpy.CalculateField_management(outName, fieldInt, "random.randint(1,100)","PYTHON","import random")
arcpy.CalculateField_management(outName, fieldFlt, "random.uniform(1,100)","PYTHON","import random")

#Step 2
#Remaps the county-specific raster values into generalized rotations.

arcpy.CreateRandomRaster_management(env.scratchFolder, "random", "UNIFORM 0.0 100.0", outRaster, env.cellSize)
arcpy.CopyRaster_management(env.scratchFolder + '/random', env.scratchGDB + '/random')
random = Raster(env.scratchGDB + '/random')

arcpy.CopyRaster_management(outRaster, countyRotFile)
countyRot = Raster(countyRotFile)

# Give 40$ of Marathon county Ma9/Ma10 dairies a code value of 90
step1 = Con(BooleanAnd(countyRot == 64, random <= 40), 90, countyRot)
# Give 50% of Juneau county dairies a code value of 91
step2 = Con(BooleanAnd(step1 == 35, random <= 50), 91, step1)
# Give 40% of Wood county dairies a code value of 92
step3 = Con(BooleanAnd(step2 == 53, random <= 40), 92, step2)
# Give 85% of Sauk county dairies a code value of 93
step4 = Con(BooleanAnd(step3 == 22, random <= 85), 93, step3)
# Give 40% of Marathon county Ma1/Ma2 dairies a code value of 94
step5 = Con(BooleanAnd(step4 == 71, random <= 40), 94, step4)
# Give 40% of Marathon county Ma3/Ma4 dairies a code value of 95
step6 = Con(BooleanAnd(step5 == 61, random <= 40), 95, step5)

step6.save(env.scratchGDB + '/randomizedDairyManagement')
step6.save(env.scratchGDB + '/rdm')

# arcpy.MakeRasterLayer_management(step6, "step6_layer")
# arcpy.JoinField_management(step6, 'Value', countyRot, 'Value', ['Value', 'Crop_Code'])

# Set local variables for Step 3. Uses previously created raster to make generalizations among the county-specific crop categories. The ""County_Codes" are in the left side of the brackets and their generalized rotation values are in the right side of the brackets.