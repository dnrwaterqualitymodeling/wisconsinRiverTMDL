import arcpy
import os
import imp
import tempfile
import uuid
import random
import subprocess
from arcpy import env
env.overwriteOutput = True

dir_evaal = "C:/Users/ruesca/Documents/EVAAL"
file_swat_reaches = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/hydro.shp"
file_all_reaches = "K:/24kAttribution/Spatial24k03142013.gdb/reaches" 
file_rip_buffs = "K:/24kAttribution/Spatial24k03142013.gdb/riparianBuffers"
#===============================================================================
# evaal = imp.load_source(
#     "evaal",
#     dir_evaal + "/__EVAAL__.pyt"
# )
#===============================================================================

td = tempfile.mkdtemp()
env.scratchWorkspace = td

def setupTemp(tempDir, tempGdb):
    env.workspace = tempGdb
    env.scratchWorkspace = os.path.dirname(tempDir)
    tempDir = env.scratchFolder
    tempGdb = env.scratchGDB
    arcpy.AddMessage(' ')
    arcpy.AddMessage('#################')
    arcpy.AddMessage('Cleaning scratch space...')
    arcpy.Compact_management(tempGdb)
    tempFiles = arcpy.ListDatasets() + arcpy.ListTables() + arcpy.ListFeatureClasses()
    for tempFile in tempFiles:
        arcpy.AddMessage('Deleting ' + tempFile + '...')
        arcpy.Delete_management(tempFile)
        arcpy.Compact_management(tempGdb)    
    os.chdir(tempDir)
    fileList = os.listdir('.')
    for f in fileList:
        if os.path.isdir(f):
            arcpy.AddMessage('Deleting ' + f + '...')
            shutil.rmtree(f)
        else:
            arcpy.AddMessage('Deleting ' + f + '...')
            os.remove(f)
    arcpy.AddMessage('#################')
    arcpy.AddMessage(' ')

def downloadCroplandDataLayer(yrStart, yrEnd, tempDir, watershedCdlPrj, rid):
    years = range(int(yrStart), int(yrEnd) + 1)
    cdlUrl = r'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?'
    ext = arcpy.Describe(watershedCdlPrj).extent
    ping = subprocess.call(['ping', '-n', '1', 'nassgeodata.gmu.edu'])
    if ping == 1:
        arcpy.AddError('The CropScape server is down. Please try again later, or download local \
            Cropland Data Layers at http://www.nass.usda.gov/research/Cropland/Release/index.htm')
    #unclipped
    cdlTiffs_fl = []
    for year in years:
        year = str(year)
        clipUrl = cdlUrl\
            + r'year='\
            + year + r'&'\
            + r'bbox='\
            + str(ext.XMin) + ','\
            + str(ext.YMin) + ','\
            + str(ext.XMax) + ','\
            + str(ext.YMax)
        try:
            downloadLocXml = tempDir + '/download_' + year + '_' + rid + '.xml'
            print clipUrl
            urllib.urlretrieve(clipUrl, downloadLocXml)
            print downloadLocXml
            tiffUrl = xml.etree.ElementTree.parse(downloadLocXml).getroot()[0].text
            downloadTiff = tempDir + '/cdl_' + year + '_' + rid + '.tif'
            urllib.urlretrieve(tiffUrl, downloadTiff)
        except:
            print 'something happened'
            arcpy.AddError("The CropScape server failed. Please download the layers to your hard drive at http://www.nass.usda.gov/research/Cropland/Release/index.htm")
        cdlTiffs_fl.append(downloadTiff)

    # For clipping to watershed extent
    cdlTiffs = []
    for i,fullCdl in enumerate(cdlTiffs_fl):
            clipCdl = tempDir + '/cdl_' + str(i) + '_' + rid + '.tif'
                    #testing the ClippingGeometry option..
            arcpy.Clip_management(fullCdl, '', clipCdl, watershedCdlPrj, '#', 'ClippingGeometry')
            cdlTiffs.append(clipCdl)

    return cdlTiffs

def calculateCFactor(downloadBool, localCdlList, watershedFile, rasterTemplateFile, yrStart, yrEnd,\
    outRotation, outHigh, outLow, legendFile, cFactorXwalkFile, tempDir, tempGdb):

    setupTemp(tempDir,tempGdb)

    env.workspace = tempGdb
    os.environ['ARCTMPDIR'] = tempDir

    rid = str(random.randint(10000,99999))
    watershedCdlPrj = tempGdb + '/watershedCdlPrj_' + rid
    samplePts = tempGdb + '/samplePts_' + rid
    outRotation1 = tempGdb + '/outRotation1_' + rid
    outHigh1 = tempGdb + '/outHigh1_' + rid
    outLow1 = tempGdb + '/outLow1_' + rid
    cdlUrl = r'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?'

    arcpy.AddMessage("Projecting Area Of Interest to Cropland Data Layer projection...")
    sr = arcpy.SpatialReference(102039)
    arcpy.Project_management(watershedFile, watershedCdlPrj, sr)
    if downloadBool == 'true':
        print 'download'
        arcpy.AddMessage("Downloading Cropland Data Layers...")
        cdlTiffs = downloadCroplandDataLayer(yrStart, yrEnd, tempDir, watershedCdlPrj, rid)
        print cdlTiffs
        years = range(int(yrStart), int(yrEnd) + 1)
    else:
        arcpy.AddMessage("Clipping Cropland Data Layers to watershed extent...")
        localCdlList = localCdlList.split(';')
        cdlTiffs = []
        years = []
        for i,localCdl in enumerate(localCdlList):
            clipCdl = tempDir + '/cdl_' + str(i) + '_' + rid + '.tif'
            print localCdl, clipCdl, watershedCdlPrj
            arcpy.Clip_management(localCdl, '', clipCdl, watershedCdlPrj, '#', 'ClippingGeometry')
            cdlTiffs.append(clipCdl)
            years.append(i)

    resolutions = []
    for cdlTiff in cdlTiffs:
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
    pasture = np.array([62, 181, 176])
    soyAndGrain = np.array([4,5,21,22,23,24,25,27,28,29,30,39,205])
    potatoes = np.array([43])
    veggies = np.array([12,42,47,49,50,53,206,216])

    # Read in C-factor crosswalk table and CDL legend file
    cFactorXwalk = np.loadtxt(cFactorXwalkFile \
        , dtype=[('LAND_COVER', 'S40'), ('SCENARIO', 'S10'), ('C_FACTOR', 'f4')] \
        , delimiter=',', skiprows=1)

    cdlLegend = np.loadtxt(legendFile \
        , dtype=[('VALUE', 'u1'), ('CLASS_NAME', 'S30')] \
        , delimiter=',', skiprows=1)

    arcpy.AddField_management(samplePts, 'rotation', 'TEXT')
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
        potGrnVegBool = (pPotato + pVeggies) >= 1./5
        pastureBool = (pPasture + pAlfalfa) >= 2./5 and \
            (pCorn + pSoyAndGrain + pPotato + pVeggies) == 0.
        dairyBool2 = (pAlfalfa + pPasture) >= 1./5
        if noDataBool:
            rot = "No Data"
        elif contCornBool:
            rot = "Continuous Corn"
        elif cashGrainBool:
            rot = "Cash Grain"
        elif dairyBool1:
            rot = "Dairy Rotation"
        elif dairyPotatoBool:
            rot = "Dairy Potato Year"
        elif potGrnVegBool:
            rot = "Potato/Grain/Veggie Rotation"
        elif pastureBool:
            rot = "Pasture/Hay/Grassland"
        elif dairyBool2:
            rot = "Dairy Rotation"
        else:
            rot = "No agriculture"
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
            highBool = np.in1d(cFactorXwalk['SCENARIO'], np.array(['High', '']))
            lowBool = np.in1d(cFactorXwalk['SCENARIO'], np.array(['Low', '']))
            c_high = np.extract(np.logical_and(rotBool, highBool), cFactorXwalk['C_FACTOR'])
            c_low = np.extract(np.logical_and(rotBool, lowBool), cFactorXwalk['C_FACTOR'])
            c_high = float(c_high)
            c_low = float(c_low)
        row.cFactorHigh = c_high
        row.cFactorLow = c_low
        row.rotation = rot
        rows.updateRow(row)
        arcpy.SetProgressorPosition()
    arcpy.ResetProgressor()
    del row, rows

    arcpy.AddMessage("Converting points to raster...")
    arcpy.PointToRaster_conversion(samplePts, "rotation", outRotation1, 'MOST_FREQUENT', \
        '', minResCdlTiff)
    arcpy.PointToRaster_conversion(samplePts, "cFactorHigh", outHigh1, 'MEAN', \
        '', minResCdlTiff)
    arcpy.PointToRaster_conversion(samplePts, "cFactorLow", outLow1, 'MEAN', \
        '', minResCdlTiff)

    wtm = arcpy.Describe(rasterTemplateFile).spatialReference
    outRes = int(arcpy.GetRasterProperties_management(rasterTemplateFile, 'CELLSIZEX').getOutput(0))
    env.mask = rasterTemplateFile
    env.snapRaster = rasterTemplateFile
    env.extent = rasterTemplateFile
    arcpy.ProjectRaster_management(outRotation1, outRotation, wtm, 'NEAREST', outRes)
    arcpy.ProjectRaster_management(outHigh1, outHigh, wtm, 'BILINEAR', outRes)
    arcpy.ProjectRaster_management(outLow1, outLow, wtm, 'BILINEAR', outRes)

all_reaches_clip = td + "\\" + str(uuid.uuid4()).replace("-","") + ".img"
all_reaches_poly = td + "\\" + str(uuid.uuid4()).replace("-","") + ".shp"
rip_buffs_clip = td + "\\" + str(uuid.uuid4()).replace("-","") + ".img"
rip_buffs_poly = td + "\\" + str(uuid.uuid4()).replace("-","") + ".shp"
rip_sel = td + "\\" + str(uuid.uuid4()).replace("-","") + ".shp"
rotation = td + "\\" + str(uuid.uuid4()).replace("-","") + ".shp"
low_c = td + "\\" + str(uuid.uuid4()).replace("-","") + ".shp"
high_c = td + "\\" + str(uuid.uuid4()).replace("-","") + ".shp"
for reach in range(1,338):
    arcpy.MakeFeatureLayer_management(
        file_swat_reaches,
        "reach_line",
        "Subbasin = " + str(reach)
    )
    arcpy.Clip_management(
        file_all_reaches,
        "#",
        all_reaches_clip,
        "reach_line",
        "#",
        "NONE"
    )
    arcpy.RasterToPolygon_conversion(
        all_reaches_clip,
        all_reaches_poly,
        "NO_SIMPLIFY",
        "VALUE"
    )
    arcpy.MakeFeatureLayer_management(
        all_reaches_poly,
        "all_reaches_poly"
    )
    arcpy.SelectLayerByLocation_management(
        "all_reaches_poly",
        "INTERSECT",
        "reach_line",
        "#",
        "NEW_SELECTION"
    )
    arcpy.Clip_management(
        file_rip_buffs,
        "#",
        rip_buffs_clip,
        "reach_line",
        "#",
        "NONE"
    )
    arcpy.RasterToPolygon_conversion(
        rip_buffs_clip,
        rip_buffs_poly,
        "NO_SIMPLIFY",
        "Value"
    )
    arcpy.MakeFeatureLayer_management(
        rip_buffs_poly,
        "rip_buffs_poly"
    )
    n_sel = int(arcpy.GetCount_management("all_reaches_poly").getOutput(0))
    hydroids = [0] * n_sel
    with arcpy.da.SearchCursor("all_reaches_poly", "GRIDCODE") as cursor:
        i = -1
        for row in cursor:
            i += 1
            hydroids[i] = int(row[0])
    arcpy.SelectLayerByAttribute_management(
        "rip_buffs_poly", 
        "NEW_SELECTION",
        "GRIDCODE IN " + str(tuple(hydroids))
    )
    arcpy.CopyFeatures_management("rip_buffs_poly", rip_sel)
    calculateCFactor(
        'true',
        '',
        rip_sel,
        file_all_reaches,
        2008,
        2012,
        rotation,
        high_c,
        low_c,
        dir_evaal + '/etc/cdlLegend.csv',
        dir_evaal + "/etc/cFactorLookup.csv",
        env.scratchFolder,
        env.scratchGDB
    )
    calculateCFactor(downloadBool, localCdlList, watershedFile, rasterTemplateFile, yrStart, yrEnd,\
    outRotation, outHigh, outLow, legendFile, cFactorXwalkFile, tempDir, tempGdb)