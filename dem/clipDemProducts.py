import arcpy, os, time
from arcpy import env

demFile = "G:/10m_NED/native10mNED/raw_prj.img"
fdrFile = "G:/10m_NED/native10mNED/fdr.img"
fillFile = "G:/10m_NED/native10mNED/fill.img"
wrbBufferFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Watersheds/WRB_Basin_2mile_Buffer.shp"

outDem = "T:/Projects/Wisconsin_River/GIS_Datasets/DEM/WRB_dem"
outFdr = "T:/Projects/Wisconsin_River/GIS_Datasets/DEM/WRB_fdr"
outFill = "T:/Projects/Wisconsin_River/GIS_Datasets/DEM/WRB_fill"

arcpy.Clip_management(demFile, '', outDem, wrbBufferFile, '', True)
arcpy.Clip_management(fdrFile, '', outFdr, wrbBufferFile, '', True)
arcpy.Clip_management(fillFile, '', outFill, wrbBufferFile, '', True)

# Create metadata
metadataFile = "T:/Projects/Wisconsin_River/GIS_Datasets/DEM/readme.txt"
timeNow = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
scriptFile = '"/code/dem/clipDemProducts.py"'
metadataOut = os.path.basename(outDem) + ', ' + os.path.basename(outFdr) + ', and '\
	+ os.path.basename(outFill) + ' were created/changed by Aaron Ruesch on ' + timeNow\
	+ ' using ' + scriptFile + '\n\n'
f = open(metadataFile, 'a+')
f.write(metadataOut)
f.close()
