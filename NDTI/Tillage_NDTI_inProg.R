#create directories
createDirectories <- function(x) {
        dir.create("D:/Backups/RS_Tillage/All_Images/extra", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/Original", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/WarpProject", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/CloudMasks", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/Clipped_CLU", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/Projected_ZeroRemoved", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/ClearBands", recursive = TRUE)
        dir.create("D:/Backups/RS_Tillage/All_Images/extra/NDTI", recursive = TRUE)
}

library(raster)
library(rgdal)
library(gdalUtils)

#Global Variables
NADproj <- "+init=epsg:3071 +proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

resolution <- c(30,30)

pathStart <- "D:/Backups/RS_Tillage/All_Images/"

#set the working directories based on input year <- "x"
x <- "extra"
owd <- paste(pathStart, x, "/Original", sep='')
projwd <- paste(pathStart, x, "/WarpProject", sep='')
cloudwd <- paste(pathStart, x, "/CloudMasks", sep='')
clrwd <- paste(pathStart, x, "/ClearBands", sep='')
NDTIwd <- paste(pathStart, x, "/NDTI", sep='')



#Preprocessing: project and clip images to Marathon County CLU boundaries
setwd(owd)
#assign band alias lists based on filename
band_list <- list.files(pattern="*band..tif$")
cloud_list <- list.files(pattern="*cfmask+.*tif$")

#---PROJECT BANDS
count <- 1
projectBands <- function(a) {
        for (item in band_list) {
                #filenames of rasters being written
                name <- substr(item, 1, 30)
                nameProject <- paste(name, "_project.tif", sep='')
        
                #reprojects raster to EPSG 3071 and writes new file, datatype keeps raster at 16 bit, otherwise it defaults to 64 bit
                gdalwarp(srcfile=item, dstfile=nameProject, t_srs=NADproj, tr=resolution, r="near", output_Raster = TRUE, overwrite=TRUE)
                
                print (  paste(nameProject, "completed", " (", count, "of", length(band_list), ")")  )
                if (count==length(band_list)) print("DONE")
                count <- count + 1
                #switch to models saved to tillage.tbx in arc to get clipped
        }
                
}

projectClouds <- function(b) {
        for (item in cloud_list) {
                
                #filenames of rasters being written
                name <- substr(item, 1, 28)
                nameProject <- paste(name, "_project.tif", sep='')
                
                #reprojects raster to EPSG 3071 and writes new file
                gdalwarp(srcfile=item, dstfile=nameProject, t_srs=NADproj, tr=resolution, r="near", output_Raster = TRUE, overwrite=TRUE)
                #switch to models saved to tillage.tbx in arc to get clipped
                
                print (  paste(nameProject, "completed", " (", count, "of", length(cloud_list), ")")  )
                
                if (count==length(cloud_list)) print("DONE")
                count <- count + 1
                
        }
}

# copy new bands over to WarpProject directory

#----SWITCH to Arc to iteratively 'strip' excess 0 values from beyond footprint and clip to CLU with masks
# 1a) "Build Raster Attribute Table"
# copy new cfmask bands over to CloudMasks directory

# in Clipped_CLU directory
# 1b) "Extract, CLU clip" in Tillage.tbx -> masked..._project.tif

#----STAY in Arc to Strip cloud bands without removing real 0 values, then clip to CLUs
# in CloudMasks directory
# 2a) "Strip Clouds" -> strip..._cfmask.tif
# 2b) "Extract Clear" -> clear..._cfmask.tif
# 2c) "Reclass Clear" -> reclass..._cfmask.tif

# in ClearBands directory
# 3) "Multiply" -> clearmasked...project.tif


#should start with all of the projected and clipped to CLU bands
setwd(clrwd)
calculateNDVI <- function(c) {
        #FOR LANDSAT8
        redList <- list.files(pattern="*_band4_project.tif$")
        nirList <- list.files(pattern="*_band5_project.tif$")
        
        for (i in 1:length(redList)) {
                red <- raster(redList[i])
                nir <- raster(nirList[i])
                
                ndvi <- (nir-red)/(nir+red)
                
                imageName <- substr(redList[i], 12, 27)
                ndviName <- paste(imageName, "_NDVI.tif", sep='')
                
                writeRaster(ndvi, filename = ndviName, format="GTiff", datatype = "FLT4S", overwrite=TRUE)
                
                print (  paste(ndviName, "completed", " (", i, "of", length(redList), ")")  )
                if (i==length(redList)) print("DONE")
        }
        
}
calculateNDVI_LE7 <- function(c) { #!!! fill in list of correct bands
        redList <- list.files(pattern="*_band3_project.tif$")
        nirList <- list.files(pattern="*_band4_project.tif$")
        
        for (i in 1:length(redList)) {
                red <- raster(redList[i])
                nir <- raster(nirList[i])
                
                ndvi <- (nir-red)/(nir+red)
                
                imageName <- substr(redList[i], 12, 27)
                ndviName <- paste(imageName, "_NDVI.tif", sep='')
                
                writeRaster(ndvi, filename = ndviName, format="GTiff", datatype = "FLT4S", overwrite=TRUE)
                
                print (  paste(ndviName, "completed", " (", i, "of", length(redList), ")")  )
                if (i==length(redList)) print("DONE")
        }
        
}


extractNDVI <- function(d) {
        ndviList <- list.files(pattern="*NDVI.tif$")
        for (i in 1:length(ndviList)) {
                ndviFile <- raster(ndviList[i])
                ndviFile[ndviFile <= -1] <- NA
                ndviFile[ndviFile >= 0.3] <- NA
                extractName <- paste(substr(ndviList[i], 1, 21),"extract.tif", sep='')
                writeRaster(ndviFile, filename = extractName, format="GTiff", datatype = "FLT4S", overwrite=TRUE)
                
                print (  paste(extractName, "completed", " (", i, "of", length(ndviList), ")")  )
                if (i==length(ndviList)) print("DONE")
                
        }
}


calculateNDTI <- function(e) {
        
        swir1_List <- list.files(pattern = "*_band6_project.tif$")
        swir2_List <- list.files(pattern = "*_band7_project.tif$")
        
        for (i in 1:length(swir1_List)) {
                swir1 <- raster(swir1_List[i])
                swir2 <- raster(swir2_List[i])
                
                ndti <- (swir1 - swir2) / (swir1 + swir2)
                
                imageName <- substr(swir1_List[i], 12, 27)
                ndtiName <- paste(imageName, "_NDTI.tif", sep='')
                
                writeRaster(ndti, filename = ndtiName, format="GTiff", datatype="FLT4S", overwrite=TRUE)
                
                print (  paste(ndtiName, "completed", " (", i, "of", length(swir1_List), ")")  )
                if (i==length(swir1_List)) print("DONE")
                
        }
}
calculateNDTI_LE7 <- function(e) {
        swir1_List <- list.files(pattern = "*_band5_project.tif$")
        swir2_List <- list.files(pattern = "*_band7_project.tif$")
        
        for (i in 1:length(swir1_List)) {
                swir1 <- raster(swir1_List[i])
                swir2 <- raster(swir2_List[i])
                
                ndti <- (swir1 - swir2) / (swir1 + swir2)
                
                imageName <- substr(swir1_List[i], 12, 27)
                ndtiName <- paste(imageName, "_NDTI.tif", sep='')
                
                writeRaster(ndti, filename = ndtiName, format="GTiff", datatype="FLT4S", overwrite=TRUE)
                
                print (  paste(ndtiName, "completed", " (", i, "of", length(swir1_List), ")")  )
                if (i==length(swir1_List)) print("DONE")
                
        }
}

# copy and paste _NDTI.tif to NDTI direcotry

setwd(NDTIwd)
extractNDTI <- function(f) {
        ndtiList <- list.files(pattern="*NDTI.tif$")
        
        for (i in 1:length(ndtiList)) {
                ndtiFile <- raster(ndtiList[i])
                ndtiFile[ndtiFile <= 0] <- NA
                ndtiFile[ndtiFile >= 0.3] <- NA
                extractName <- paste(substr(ndtiList[i], 1, 21),"extract.tif", sep='')
                writeRaster(ndtiFile, filename = extractName, format="GTiff", datatype = "FLT4S", overwrite=TRUE)
                
                print (  paste(extractName, "completed", " (", i, "of", length(ndtiList), ")")  )
                if (i==length(ndtiList)) print("DONE")
                
        }
}


#SWITCH to Arc to mask out occluded, high NDVI areas and push out final correct projection
# in ClearBands directory
# 4) "Reclass NDVI Extract" -> reclass...NDVIextract.tif 
# in NDTI directory
# 5) "Final NDTI Extract" -> imageday_finalndti
# 6) "Project to EPSG 3071" -> proj_imageday_ndti (projects strange R projection to EPSG 3071 without creating extra values)

calculate_minNDTI <- function(g) {
        ndtiprojList <- list.files(pattern="*proj+.*tif$")
        ndtiRasters <- list()
        for (item in ndtiprojList) {
                ras <- raster(item)
                ndtiRasters <- append(ndtiRasters, ras, after = length(ndtiRasters))        
        }
        ndti_stack <- stack(ndtiRasters)
        ndti_min <- calc(ndti_stack, fun=min, na.rm=T)
        minName <- paste(x, "_Marathon", "_minNDTI.tif", sep='')
        
        writeRaster(ndti_min, filename=minName, format="GTiff", datatype="FLT4S", overwrite=TRUE)
        
        
}

# Project minNDTI rasters in Arc to EPSG 3071 using 6) again and *minNDTI.tif wildcard expression

