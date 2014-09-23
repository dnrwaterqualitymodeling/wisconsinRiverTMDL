
rotationFrequencyFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Transects/Wood_Cty_RawData/Wood_CropYears_Translated.csv"
aggRotFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Transects/Wood_Cty_RawData/Wood_Cty_Rotations.csv"
rotationTable = read.csv(rotationFrequencyFile, stringsAsFactors=F)
cropCols = 5:9

rule = function(rotation) {
	# nNas = length(which(is.na(rotation)))
	nNas = length(which(rotation == ''))
	nCorn = length(which(rotation == 'Co'))
	nAlfalfa = length(which(rotation == 'Al'))
	nPasture = length(which(rotation == 'Pa'))
	nSoy = length(which(rotation == 'So'))
	nPotato = length(which(rotation == 'Po'))
	nVeggies = length(which(rotation == 'Vg'))
	nDryBeans = length(which(rotation == 'Db'))
	if (nNas == 5) {
		rot = "No Agriculture"
	} else if (nCorn >= 1 & nSoy >= 1 & sum(nPotato, nVeggies, nDryBeans) == 0 & (nPasture + nAlfalfa) <= 1) {
		rot = "Cash Grain"
	} else if ((nPasture + nAlfalfa) >= 2 & sum(nCorn, nSoy, nPotato, nVeggies, nDryBeans) == 0) {
		rot = "Pasture/Hay/Grassland"
	} else if (nCorn >= 1 & (nPasture + nAlfalfa) >= 1 & sum(nPotato, nVeggies, nDryBeans) == 0) {
		rot = "Dairy Rotation"
	} else if (nCorn >= 1 & nPotato >= 1 & (nPasture + nAlfalfa) >= 1 & sum(nVeggies, nDryBeans) == 0) {
		rot = "Dairy, Potato Year"
	} else if (nPotato >= 1 & sum(nVeggies, nDryBeans, nSoy, nCorn, nPasture, nAlfalfa) >= 2) {
		rot = "Potato/Grain/Veggie Rotation"
	} else if (nCorn >= 3 & sum(nSoy, nPotato, nVeggies, nDryBeans) == 0) {
		rot = "Continuous Corn"
	} else {
		rot = "Insufficient"
	}
	return(rot)
}

rotation = rotationTable[,cropCols]
rotAgg = apply(X=rotation, MARGIN=1, rule)
rotationTable$ROTATION = rotAgg 
table(rotationTable$ROTATION)

rotationTable$ROTATION_CODE = as.integer(as.factor(rotationTable$ROTATION))
write.csv(rotationTable, file = aggRotFile, row.names=F, na='')

