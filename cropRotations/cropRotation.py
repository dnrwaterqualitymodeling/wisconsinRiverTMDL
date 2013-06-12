import arcpy
from arcpy import env
from arcpy.sa import *
import os
arcpy.ImportToolbox("S:/Diebel/DGEL/DGEL_toolbox/hydrographyAttributionToolbox/~HydroAttribution.tbx", "ha")
arcpy.CheckOutExtension("Spatial")

doResample = True
doZonal = True

resampleDir = "D:/NASS/cropRotation/resampledNASS_dtrsq" # All files within saved as .img
areaBoundary = "D:/NASS/cropRotation/dtrsq.img"
zonalDir = "D:/NASS/cropRotation/crosstab_dtrsq"

if doResample:
	arcpy.rasterResampling_ha("D:/NASS/cropRotation/rawNASS", areaBoundary, areaBoundary, resampleDir)


if doZonal:
	Rscript = '"C:\\r\\bin\\x64\\Rscript.exe" D:\\NASS\\cropRotation\\crosstabCalledFromToolbox_cropRotation.r D:/NASS/cropRotation/resampledNASS_dtrsq D:/NASS/cropRotation/dtrsq.img D:/NASS/cropRotation/crosstab_dtrsq D:/TEMP lm W img'
	os.system(Rscript)
