import arcpy, urllib, ftplib
import numpy as np
from arcpy import env
env.overwriteOutput = True
noaaFtp = 'ftp.ncdc.noaa.gov'
ftp = ftplib.FTP(noaaFtp)
ftp.login()
climateDataLink = 'pub/data/gsod'
ftp.cwd(climateDataLink)
stationLocations = 'C:/TEMP/stationLocations.shp'
climateDataDir = 'C:/TEMP/climateData'
years = np.arange(1995, 2014)


# read in climate station data/station code
        #ftp--> list of years --> each year has station code
# for stationCode in stationCodes
        # for year in years
                #find file
                #download it

dirStr = {}
for year in years:
        print year
        ftp.cwd(str(year))
        filelist = []
        ftp.retrlines('NLST', filelist.append)
        ftp.cwd("..")
        dirStr[str(year)] = filelist
ftp.close()

rows = arcpy.SearchCursor(stationLocations)
for row in rows:
        usafId = str(int(row.USAF))
        wbanId = str(int(row.WBAN))
        beginYear = int(str(int(row.BEGIN))[0:4])
        endYear = int(str(int(row.END))[0:4])
        # Prepend leading zeros onto IDs
        nZeros = 6 - len(usafId)
        if nZeros > 0:
                usafId = '0'*nZeros + usafId
        nZeros = 5 - len(wbanId)
        if nZeros > 0:
                wbanId = '0'*nZeros + wbanId
        print str(usafId) + ' ' + str(wbanId)
        for year in years:
                print '      ' + str(year)
                filename = usafId + '-' + wbanId + '-' + str(year) + '.op.gz'
                if filename in dirStr[str(year)]:                
                        inPath = 'ftp://' + noaaFtp + '/' + climateDataLink + '/' + str(year) + '/'\
                                 + usafId + '-' + wbanId + '-' + str(year) + '.op.gz'
                        print '    ' + inPath
                        outPath = climateDataDir + '/' + usafId + '-' + wbanId + '-' + str(year) + '.op.gz'
                        urllib.urlretrieve (inPath, outPath)
                        d = np.genfromtxt(outPath)
del row, rows


                # read contents
                # append data to master table


