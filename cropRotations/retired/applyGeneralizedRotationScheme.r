
rotationFrequencyFile = "D:/NASS/cropRotation/rotationByQuarterSection_relumped.csv"
rotationTable = read.csv(rotationFrequencyFile)

rule = function(rotation) {
    nNas = length(which(is.na(rotation)))
    nCorn = length(which(rotation == 'Co'))
    nSoy = length(which(rotation == 'So'))
    nForage = length(which(rotation == 'Fo'))
    nPotato = length(which(rotation == 'Po'))
    nVeggies = length(which(rotation == 'Vg'))
    nDryBeans = length(which(rotation == 'Db'))
    if (nNas == 5) {
        rot = "No Agriculture"
    } else if (nCorn >= 1 & nSoy >= 1 & (nCorn + nSoy) >= 3) {
        rot = "Cash Grain"
    } else if (nCorn >= 4) {
        rot = "Continuous Corn"
    } else if (nCorn >= 2 & nDryBeans >= 2) {
        rot = "Corn / Dry Beans"
    } else if (nCorn == 1 & nSoy == 1 & nForage >= 1
               & all(c(nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, 1 Year Corn, 1 Year Soybean"
    } else if (nCorn == 1 & nSoy >= 2 & nForage >= 1
               & all(c(nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, 1 Year Corn, 2 Year Soybean"
    } else if (nCorn %in% c(1,2) & nForage >= 1
               & all(c(nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, 2 Years Corn"
    } else if (nCorn >= 1 & nSoy >= 1 & (nCorn + nSoy >= 2) & nForage >= 1
               & all(c(nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, 2 Years Corn, 1 Year Soybean"
    } else if (nCorn == 3 & nForage >= 1
               & all(c(nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, 3 Years Corn"
    } else if (nCorn >= 1 & nPotato >= 1 & nForage >= 1
               & all(c(nSoy, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, Corn / Potato"
    } else if (nCorn == 0 & nSoy >= 1 & nForage >= 1 
               & all(c(nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Dairy, Soybean"
    } else if (nDryBeans >= 2 & nForage >= 2 
               & all(c(nCorn, nSoy, nPotato, nVeggies) == 0)) {
        rot = "Dry Beans"
    } else if (nForage >= 4
               & all(c(nCorn, nSoy, nPotato, nVeggies, nDryBeans) == 0)) {
        rot = "Forage"
    } else if (nPotato >= 2
               & all(c(nCorn, nSoy, nDryBeans, nVeggies) == 0)) {
        rot = "Potatoes"
    } else if (nCorn >= 1 & nPotato >= 1 & (nCorn + nPotato >= 4)) {
        rot = "Potatoes / Corn"
    } else if (nCorn >= 1 & nPotato >= 1 & nDryBeans >= 1
               & all(c(nSoy, nVeggies) == 0)) {
        rot = "Potatoes / Corn / Dry Beans"
    } else if (nCorn >= 1 & nPotato >= 1 & nSoy >= 1) {
        rot = "Potatoes / Corn / Soybean"
    } else if (nCorn >= 1 & nPotato >= 1 & nVeggies >= 1) {
        rot = "Potatoes / Corn / Vegetable"
    } else if (nPotato >= 1 & nDryBeans >= 1 & nForage >= 1
               & all(c(nCorn, nSoy, nVeggies) == 0)) {
        rot = "Potatoes / Dry Beans"
    } else {
        rot = "Insufficient"
    }
    return(rot)
}

rotation = rotationTable[,4:8]    
rotAgg = apply(X=rotation, MARGIN=1, rule)
rotationTable$ROTATION = rotAgg 
table(rotationTable$ROTATION)

rotationTable$ROTATION_CODE = as.integer(as.factor(rotationTable$ROTATION))
write.csv(rotationTable, file = "T:/Projects/Wisconsin_River/GIS_Datasets/Crop_Rotations/aggregatedRotationByQuarterSection_061313.csv", row.names=F, na='')



