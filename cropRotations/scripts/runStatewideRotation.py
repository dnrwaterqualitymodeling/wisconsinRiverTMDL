import arcpy, os, random, urllib, xml, sys, subprocess
# sys.path.insert(1, os.path.dirname(os.path.realpath(__file__)))
# import setupTemp as tmp
import numpy as np
from arcpy import env
arcpy.CheckOutExtension("Spatial")
from arcpy.sa import *
env.overwriteOutput = True
env.pyramid = 'NONE'
env.rasterStatistics = 'NONE'

import subprocess
from subprocess import Popen
startupinfo = subprocess.STARTUPINFO()
startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW

outDir = 'C:/Users/ruesca/Desktop/rotationTiles'

downloadBool = 'false'
localCdlList = 'K:/NASS/rawCdl/2008_30m_cdls.img;' +\
	'K:/NASS/rawCdl/2009_56m_cdls.img;' +\
	'K:/NASS/rawCdl/2010_30m_cdls.img;' +\
	'K:/NASS/rawCdl/2011_30m_cdls.img;' +\
	'K:/NASS/rawCdl/2012_30m_cdls.img'

watershedFile = 'T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WI_Statewide_Fishnet.shp'

rasterTemplateFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/WiRiverTmdlLandCoverDefinition.img"
yrStart = 2008
yrEnd = 2012

legendFile = 'W:/beneke/wiRiverCdlRotations/etc/cdlLegend.csv'
cFactorXwalkFile = 'W:/beneke/wiRiverCdlRotations/etc/cFactorLookup.csv'

tempDir = 'D:/TEMP'
tempGdb = tempDir + '/temp.gdb'

outHigh = ''
outLow = ''
os.environ['ARCTMPDIR'] = tempDir

env.scratchWorkspace = tempDir
env.workspace = tempDir

sr = arcpy.SpatialReference(102039)

# Set up your AOIs, then loop through each and save AOI rotations to some output directory
arcpy.MakeFeatureLayer_management(watershedFile, 'fishnet')

# FIDs = [130, 131, 132, 133, 134, 149, 150, 151, 152, 153, 168, 169, 170 , 171, 172]

for FID in range(0, 279):
# for FID in range(17, 18):
# for FID in FIDs:
	outRotation = outDir + '/tile_' + str(FID)
	print outRotation
	if arcpy.Exists(outRotation):
		continue
	arcpy.SelectLayerByAttribute_management('fishnet', 'NEW_SELECTION', '"FID" = ' + str(FID))

	rid = str(random.randint(10000,99999))
	watershedCdlPrj = tempGdb + '/watershedCdlPrj_' + rid
	samplePts = tempGdb + '/samplePts_' + rid
	outRotation1 = tempGdb + '/outRotation1_' + rid
	outHigh1 = tempGdb + '/outHigh1_' + rid
	outLow1 = tempGdb + '/outLow1_' + rid
	cdlUrl = r'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?'

	arcpy.AddMessage("Projecting Area Of Interest to Cropland Data Layer projection...")
	print watershedCdlPrj
	arcpy.Project_management('fishnet', watershedCdlPrj, sr)

	ext = arcpy.Describe(watershedCdlPrj).extent

	if downloadBool == 'true':
		ping = Popen(['ping', '-n', '1', 'nassgeodata.gmu.edu'], startupinfo=startupinfo)
		ping.wait()
		if ping == 1:
			arcpy.AddError('The CropScape server is down. Please try again later, or download local Cropland Data Layers at http://www.nass.usda.gov/research/Cropland/Release/index.htm')
		arcpy.AddMessage("Downloading Cropland Data Layers...")
		years = range(int(yrStart), int(yrEnd) + 1)
		cdlTiffs = []
		for year in years:
			year = str(year)
			clipUrl = cdlUrl\
				+ r'year='\
				+ year + r'&'\
				+ r'bbox='\
				+ str(ext.XMin) + '%2C'\
				+ str(ext.YMin) + '%2C'\
				+ str(ext.XMax) + '%2C'\
				+ str(ext.YMax)
			try:
				downloadLocXml = tempDir + '/download_' + year + '_' + rid + '.xml'
				urllib.urlretrieve(clipUrl, downloadLocXml)
				tiffUrl = xml.etree.ElementTree.parse(downloadLocXml).getroot()[0].text
				downloadTiff = tempDir + '/cdl_' + year + '_' + rid + '.tif'
				urllib.urlretrieve(tiffUrl, downloadTiff)
			except:
				arcpy.AddError("The CropScape server failed. Please download the layers to your hard drive at http://www.nass.usda.gov/research/Cropland/Release/index.htm")
			cdlTiffs.append(downloadTiff)
	else:
		arcpy.AddMessage("Clipping Cropland Data Layers to watershed extent...")
		localCdlSplit = localCdlList.split(';')
		cdlTiffs = []
		years = []
		for i,localCdl in enumerate(localCdlSplit):
			clipCdl = tempDir + '/cdl_' + str(i) + '_' + rid + '.tif'
			arcpy.Clip_management(localCdl, '', clipCdl, watershedCdlPrj)
			cdlTiffs.append(clipCdl)
			years.append(i)
			
	resolutions = []
	for cdlTiff in cdlTiffs:
		print cdlTiff
		res = float(arcpy.GetRasterProperties_management(cdlTiff, 'CELLSIZEX').getOutput(0))
		resolutions.append(res)

	minResCdlTiff = np.array(cdlTiffs)[resolutions == np.min(resolutions)][0]
	
	arcpy.AddMessage("Converting Cropland Data Layer grid to points. If your watershed is larger than a HUC12, this may take awhile...")
	arcpy.RasterToPoint_conversion(minResCdlTiff, samplePts)

	cdlList = []
	yrCols = []
	for i,year in enumerate(years):
		yrCol = 'lc_' + str(year)
		yrCols.append(yrCol)
		cdlList.append([cdlTiffs[i], yrCol])

	arcpy.AddMessage("Pulling crop sequence from Cropland Data Layers...")
	ExtractMultiValuesToPoints(samplePts, cdlList, 'NONE')

	nonRotCropVals = [0] + range(63,181) + range(182,204)
	corn = np.array([1])
	alfalfa = np.array([28, 36, 37, 58])
	pasture = np.array([62, 181])
	soyAndGrain = np.array([4,5,21,22,23,24,25,27,28,29,30,39,205])
	potatoes = np.array([43])
	veggies = np.array([12,42,47,49,50,53,206,216])

	# Read in C-factor crosswalk table and CDL legend file
	cFactorXwalk = np.loadtxt(cFactorXwalkFile \
		, dtype=[('LAND_COVER', 'S40'), ('COVER_LEVEL', 'S10'), ('C_FACTOR', 'f4')] \
		, delimiter=',', skiprows=1)

	cdlLegend = np.loadtxt(legendFile \
		, dtype=[('VALUE', 'u1'), ('CLASS_NAME', 'S30')] \
		, delimiter=',', skiprows=1)

	arcpy.AddField_management(samplePts, 'rotation', 'TEXT')
	arcpy.AddField_management(samplePts, 'rotationCode', 'SHORT')
	arcpy.AddField_management(samplePts, 'cFactorLow', 'FLOAT')
	arcpy.AddField_management(samplePts, 'cFactorHigh', 'FLOAT')
	
	ptCount = int(arcpy.GetCount_management(samplePts).getOutput(0))
	msg = "Generalizing rotation from crop sequence, and applying a C-factor..."
	arcpy.SetProgressor("step", msg, 0, ptCount, 1)
	rows = arcpy.UpdateCursor(samplePts)
	for i,row in enumerate(rows):
		lcs = []
		for yrCol in yrCols:
			if row.getValue(yrCol) is None:
				lcs.append(0)
			else:
				lcs.append(row.getValue(yrCol))
		lcs = np.array(lcs)
		nYr = float(len(lcs))
		# Crop proportions
		pNas = float(len(np.where(lcs == 0)[0])) / nYr
		pCorn = float(len(np.where(np.in1d(lcs,corn))[0])) / nYr
		pAlfalfa = float(len(np.where(np.in1d(lcs,alfalfa))[0])) / nYr
		pPasture = float(len(np.where(np.in1d(lcs,pasture))[0])) / nYr
		pSoyAndGrain = float(len(np.where(np.in1d(lcs,soyAndGrain))[0])) / nYr
		pPotato = float(len(np.where(np.in1d(lcs,potatoes))[0])) / nYr
		pVeggies = float(len(np.where(np.in1d(lcs,veggies))[0])) / nYr
		
		noDataBool = pNas == 1.
		contCornBool = pCorn >= 3./5 and \
			(pSoyAndGrain + pPotato + pVeggies + pAlfalfa + pPasture) == 0.
		cashGrainBool = (pCorn + pSoyAndGrain) >= 2./5 and \
			(pPotato + pVeggies + pAlfalfa + pPasture) == 0.
		dairyBool1 = pAlfalfa >= 1./5 and \
			(pCorn + pSoyAndGrain) >= 1./5
		dairyPotatoBool = pPotato >= 1./5 and \
			pAlfalfa >= 1./5 and \
			pVeggies == 0.
		potGrnVegBool = (pPotato + pVeggies) >= 1./5 # and \
			# (pSoyAndGrain + pCorn) >= 2./5
		pastureBool = (pPasture + pAlfalfa) >= 2./5 and \
			(pCorn + pSoyAndGrain + pPotato + pVeggies) == 0.
		dairyBool2 = (pAlfalfa + pPasture) >= 1./5 # and \
			# (pCorn + pSoyAndGrain) >= 1./5
		if noDataBool:
			rot = "No Data"
			rotCode = 0
		elif contCornBool:
			rot = "Continuous Corn"
			rotCode = 1
		elif cashGrainBool:
			rot = "Cash Grain"
			rotCode = 2
		elif dairyBool1:
			rot = "Dairy Rotation"
			rotCode = 3
		elif dairyPotatoBool:
			rot = "Dairy Potato Year"
			rotCode = 4
		elif potGrnVegBool:
			rot = "Potato/Grain/Veggie Rotation"
			rotCode = 5
		elif pastureBool:
			rot = "Pasture/Hay/Grassland"
			rotCode = 6
		elif dairyBool2:
			rot = "Dairy Rotation"
			rotCode = 3
		else:
			rot = "No agriculture"
			rotCode = 7
			c_s = np.empty(len(lcs))
			for j,lc in enumerate(lcs):
				c = np.extract(cFactorXwalk['LAND_COVER'] == str(lc) \
					, cFactorXwalk['C_FACTOR'])
				if len(c) > 0:
					c_s[j] = c
				else:
					c_s[j] = np.nan
			c_ave = np.nansum(c_s) / np.sum(np.isfinite(c_s))
			if np.isnan(c_ave):
				c_high = None
				c_low = None
			else:
				c_high = float(c_ave)
				c_low = float(c_ave)
		if rot != "No agriculture":
			rotBool = cFactorXwalk['LAND_COVER'] == rot
			highBool = np.in1d(cFactorXwalk['COVER_LEVEL'], np.array(['High', '']))
			lowBool = np.in1d(cFactorXwalk['COVER_LEVEL'], np.array(['Low', '']))
			c_high = np.extract(np.logical_and(rotBool, highBool), cFactorXwalk['C_FACTOR'])
			c_low = np.extract(np.logical_and(rotBool, lowBool), cFactorXwalk['C_FACTOR'])
			c_high = float(c_high)
			c_low = float(c_low)
		row.cFactorHigh = c_high
		row.cFactorLow = c_low
		row.rotation = rot
		row.rotationCode = rotCode
		rows.updateRow(row)
		arcpy.SetProgressorPosition()
	arcpy.ResetProgressor()
	del row, rows
	
	arcpy.AddMessage("Converting points to raster...")
	arcpy.PointToRaster_conversion(samplePts, "rotationCode", outRotation1, 'MOST_FREQUENT', \
		'', minResCdlTiff)
	# arcpy.PointToRaster_conversion(samplePts, "cFactorHigh", outHigh1, 'MEAN', \
		# '', minResCdlTiff)
	# arcpy.PointToRaster_conversion(samplePts, "cFactorLow", outLow1, 'MEAN', \
		# '', minResCdlTiff)
	
	env.snapRaster = rasterTemplateFile
	wtm = arcpy.Describe(rasterTemplateFile).spatialReference	
	outRes = int(arcpy.GetRasterProperties_management(rasterTemplateFile, 'CELLSIZEX').getOutput(0))	
	arcpy.ProjectRaster_management(outRotation1, outRotation, wtm, 'NEAREST', outRes)
	# arcpy.ProjectRaster_management(outHigh1, outHigh, wtm, 'BILINEAR', outRes)
	# arcpy.ProjectRaster_management(outLow1, outLow, wtm, 'BILINEAR', outRes)
