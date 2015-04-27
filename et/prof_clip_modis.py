# RasterClipper.py - clip a geospatial image using a shapefile
import os
import subprocess

# Polygon shapefile used to clip
shp = "H:/wrb_basin.shp"

inDir = "H:/et_data/"
outDir = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/et_data/monthly_et_data/"
tmpDir = "H:/temp_directory/"

if not os.path.exists(outDir):
	os.mkdir(outDir)

if not os.path.exists(tmpDir):
	os.mkdir(tmpDir)

gdalwarp = "gdalwarp "
wtm = "-t_srs EPSG:3070 "
dstnodata = "-dstnodata -9999 "
srcnodata = "-srcnodata -9999 "
ctlne = "-cutline " + shp + " "
crp2ctlne = "-crop_to_cutline "

tifs = os.listdir(inDir)

for tif in tifs:
	if not ".tif" == tif[-4:]:
		continue
	print "Beginning", tif
	srcFile = inDir + tif
	dstFile = tmpDir + "wtm_" + tif
	srcFile = srcFile.replace("/", "\\")
	dstFile = dstFile.replace("/", "\\")
	cmd_proj = gdalwarp +\
		wtm +\
		srcFile + \
		" " + dstFile
	p = subprocess.Popen(cmd_proj)
	p.wait()
	print "Old dst file:", dstFile
	srcFile = dstFile
	del dstFile
	
	dstFile = outDir + "wrb_" + tif
	dstFile = dstFile.replace("/", "\\")
	print "New dst file:", dstFile
	cmd_clip = gdalwarp +\
		srcnodata +\
		dstnodata +\
		crp2ctlne +\
		ctlne +\
		srcFile + " " +\
		dstFile
	p = subprocess.Popen(cmd_clip)
	p.wait()

