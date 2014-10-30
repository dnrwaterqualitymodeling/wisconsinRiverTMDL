library(foreign)
library(rgdal)
library(reshape2)
library(aqp)
library(soiltexture)
library(stringr)
library(mclust)
options(stringsAsFactors = F)
net_soil_dir <- "T:/WD_Projects/DNR/Water/TMDL/Projects/Wisconsin_River/GIS_Datasets/Soils/"
net_soil_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets/Soils/"
soil_tbl = read.table(paste(net_soil_dir, "SWAT_US_SSURGO_Soils_wrb.txt",sep=''), header=T)
agg_prof_tbl = paste(net_soil_dir, 'aggregated_profiles.txt', sep = '')
agg_soil_unit_tbl = paste(net_soil_dir, "aggregated_soil_units.txt", sep = '')
agg_unit_mukey_lu_tbl = paste(net_soil_dir, "agg_unit_mukey_lu.txt", sep = '')
mupolygon = readOGR(paste(net_soil_dir, "WRB_Soils_2mile_Buffer_gSSURGO.gdb", sep=""),
	"MUPOLYGON_with_michigan_2_mile_buffer")
wrb_mukeys <- read.csv(paste(net_soil_dir, "wrb_mukeys.txt",sep = ''))
mupolygon = subset(mupolygon, MUKEY %in% wrb_mukeys$MUKEY)

excld = c("Dam concrete",
	"Dam, concrete",
	"Landfill",
	"Psammaquents, nearly level",
	"Rock land",
	"Rock outcrop",
	"Stony and rocky land",
	"Sandy land",
	"Miscellaneous water",
	"Water",
	"Steep sandy land",
	"Pits",
	"Pits, quarry, hard bedrock",  
	"Pits, quarries",
	"Pits, quarry, soft bedrock",
	"Pits, quarry, hard bedrock",
	"Pits, siliceous sand", 
	"Pits, gravel", 
	"Coal pit", 
	"Udorthents, earthen dams",
	"Udorthents")

# Average specific characteristics across horizons.
depth_cols = grep("*_Z[0-9]$", names(soil_tbl), value=T)
sand_cols = grep("SAND[0-9]$", names(soil_tbl), value=T)
silt_cols = grep("SILT[0-9]$", names(soil_tbl), value=T)
clay_cols = grep("CLAY[0-9]$", names(soil_tbl), value=T)
bd_cols = grep("*_BD[0-9]$", names(soil_tbl), value=T)
k_cols = grep("SOL_K[0-9]$", names(soil_tbl), value=T)
awc_cols = grep("*_AWC[0-9]$", names(soil_tbl), value=T)
cbn_cols = grep("*_CBN[0-9]$", names(soil_tbl), value=T)
usle_k_cols = grep("USLE_K[0-9]$", names(soil_tbl), value=T)
#   Do we need pH, Rock fragments, albedo, ec, CaCO3?
ph_cols = grep("PH[0-9]$", names(soil_tbl), value=T)
rock_cols = grep("ROCK[0-9]$", names(soil_tbl), value=T)
albedo_cols = grep("SOL_ALB[0-9]$", names(soil_tbl), value=T)
salinity_cols = grep("SOL_EC[0-9]$", names(soil_tbl), value=T)
#   all CaCO3 values are zero

# Calculate depth-weighted mean of hydrologic characteristics used in SWAT
agg_tbl = array(NA, dim=c(nrow(soil_tbl),9)) # or 13 if all included
mukeys = array(NA, dim=nrow(soil_tbl))
for (i in 1:nrow(soil_tbl)) {
	d = soil_tbl[i,]
	mukeys[i] = d$MUID
	n = d$NLAYERS
	depths = as.integer(d[depth_cols][1:n])
	depths = depths - c(0, depths[1:(n-1)])
	sand = sum(d[sand_cols][1:n] * depths) / sum(depths)
	silt = sum(d[silt_cols][1:n] * depths) / sum(depths)
	clay = sum(d[clay_cols][1:n] * depths) / sum(depths)
	bd = sum(d[bd_cols][1:n] * depths) / sum(depths)
	k = sum(d[k_cols][1:n] * depths) / sum(depths)
	awc = sum(d[awc_cols][1:n])
	cbn = sum(d[cbn_cols][1:n] * depths) / sum(depths)
	usle_k = d$USLE_K1
	tot_depth = sum(depths)
	agg_tbl[i,] = as.numeric(c(tot_depth,
		sand,
		silt,
		clay,
		bd,
		k,
		awc,
		cbn,
		usle_k))
}

# Loop across HSG A,B,C,D
# set.seed(2014) # set seed for reproducibility, (in case kMeans is variable)
for (hsg in LETTERS[1:4]) {
	ind = which(soil_tbl$HYDGRP == hsg & !(soil_tbl$SNAM %in% excld))
	clus_d = agg_tbl[ind,]
	clus_d_scld <- scale(clus_d)
	# For each HSG, find clusters
	# clusters = kmeans(clus_d, centers = 3)
	clusters = Mclust(clus_d_scld, G=3)
	print(table(clusters$classification))
	soil_tbl[ind, "hru_grp"] = paste(hsg,clusters$classification, sep="")
}
q = soil_tbl$SNAM %in% c("Miscellaneous water", "Water")
soil_tbl$hru_grp[q] = "W"
q = soil_tbl$SNAM %in% excld[!(excld %in% c("Miscellaneous water", "Water"))]
soil_tbl$hru_grp[q] = "X"
soil_tbl$MUID = as.character(soil_tbl$MUID)
write.table(soil_tbl[,c("MUID", "hru_grp", "SNAM")], agg_unit_mukey_lu_tbl, sep="\t", row.names=F)

###############################
# Reformatting soil data so it can be used by aqp
###############################
soil_pr = data.frame()
for (i in 1:nrow(soil_tbl)){
#	 i <- 1
	d = soil_tbl[i,]
	n = d$NLAYERS
	bottom = as.integer(d[depth_cols][1:n])
	top = c(0, bottom[1:n-1])
	sand = as.integer(d[sand_cols][1:n])
	silt = as.integer(d[silt_cols][1:n])
	clay = as.integer(d[clay_cols][1:n])
	bd = as.numeric(d[bd_cols][1:n])
	k = as.numeric(d[k_cols][1:n])
	awc = as.numeric(d[awc_cols][1:n])
	cbn = as.numeric(d[cbn_cols][1:n])
	usle_k = as.numeric(d[usle_k_cols][1:n])
	ph <- sum(d[ph_cols][1:n] * depths) / sum(depths)
	rock <- sum(d[rock_cols][1:n] * depths) / sum(depths)
	albedo <- sum(d[albedo_cols][1:n] * depths) / sum(depths)
	salinity <- sum(d[salinity_cols][1:n] * depths) / sum(depths)
	rows = data.frame(
		id=rep(d$MUID, n),
		name=paste(rep(d$MUID, n), 1:n, sep="_"),
		group=rep(d$hru_grp, n),
		top=top,
		bottom=bottom,
		sand=sand,
		silt=silt,
		clay=clay,
		sol_bd=bd,
		sol_k = k,
		sol_awc=awc,
		sol_cbn=cbn,
		usle_k=usle_k,
		sol_ph = ph,
		rock = rock,
		sol_alb = albedo,
		sol_ec = salinity)
	soil_pr = rbind(soil_pr, rows)
}
max_depths = aggregate(SOL_ZMX ~ hru_grp, data=soil_tbl, FUN=mean)
# promote to soil profile collection object
depths(soil_pr) = id ~ top + bottom
# make the object aware of the different groups (sites in aqp terms)
site(soil_pr) <- ~ group

agg_profs <- data.frame()
###############################
# Merge grouped soils together using aqp
# this loop takes 30 to 40 minutes to run
###############################
for (grp in unique(soil_tbl$hru_grp)) {
	qry <- paste("group == '",grp, "'",sep='')
	print(paste("Working on ", grp, "...")) 
	grp_tbl <- subsetProfiles(soil_pr, s=qry)
	print(paste(grp," contains ", length(grp_tbl), " profiles...")) 
	max_depth = max_depths[max_depths$hru_grp == grp, "SOL_ZMX"]
	# setting those horizons with percentages that sum to less than 80
	txtSums <- rowSums(grp_tbl@horizons[,c('sand','silt','clay')])
	grp_tbl@horizons[which(txtSums < 80),c('sand','silt','clay')] <- NA
	grp_slab = slab(grp_tbl, fm = ~ sand + silt + clay + sol_bd + sol_k + 
		sol_awc + sol_cbn + usle_k + sol_ph + 
		rock + sol_alb + sol_ec,
		slab.structure=seq(0,max_depth,length.out=6),
		slab.fun=mean,
		na.rm=TRUE)
	grp_slab$hru_grp <- grp
	# assuming 5 horizons
	grp_slab$hrz_num <- rep(1:5, times = 12)
	agg_profs <- rbind(agg_profs, grp_slab)
}
agg_profs$variable <- as.character(agg_profs$variable)
write.table(agg_profs, agg_prof_tbl, sep = '\t', row.names = F)

agg_profs = read.table(agg_prof_tbl, sep="\t", header=T)
agg_soil_data <- data.frame(soil_tbl[1,])
agg_soil_data[1,] <- 0
for (rw in 1:length(unique(soil_tbl$hru_grp))){
	hruGrp <- unique(soil_tbl$hru_grp)[rw]
	if (is.na(hruGrp)) {next}
	sbst <- subset(agg_profs, hru_grp == hruGrp)
	nlayers <- length(unique(sbst$top))
	agg_soil_data[rw, 'NLAYERS'] <- nlayers
	agg_soil_data[rw, 'SOL_ZMX'] <- max_depths[max_depths$hru_grp == hruGrp, "SOL_ZMX"]
	agg_soil_data[rw, 'ANION_EXCL'] <- 0.5
	agg_soil_data[rw, 'SOL_CRK'] <- 0.5
	agg_soil_data[rw, 'HYDGRP'] <- strsplit(hruGrp, '[0-9]')[[1]]
	agg_soil_data[rw, c('SNAM','MUID','SEQN','S5ID')] <- hruGrp
	# grabbing hz depths
	dpths <- unique(sbst$bottom)
	dpths <- dpths - c(0, dpths[-length(dpths)])
	solZ <- paste('SOL_Z',1:nlayers,sep = '')
	agg_soil_data[rw,solZ] <- dpths
	# creating texture class
	txt<- dcast(sbst, top + bottom ~ variable)[c('sand','silt','clay')]
	names(txt) <- toupper(names(txt))
	for (h in 1:5) {
		err <- try(
			txt$TxtClass[h] <- TT.points.in.classes(tri.data = txt[h,1:3],
				class.sys ="USDA.TT",
				PiC.type = 't',
				text.tol = 3), silent=T) # tolerance, may need to be changed
		if(class(err)=="try-error") {
			print(paste("There is no texture data for", hruGrp))
			txt$TxtClass[h] <- 'Non-mineral'
		} else {
			txt$TxtClass[h] <- strsplit(txt$TxtClass[h], ',')[[1]][1]
			txt$TxtClass[h] <-str_replace(txt$TxtClass[h], "[a]", '')
			txt$TxtClass[h] <-str_replace(txt$TxtClass[h], "[o]", '')
			txt$TxtClass[h] <-str_replace(txt$TxtClass[h], "[l]", '')
		}
	}
	agg_soil_data[rw, 'TEXTURE'] <- paste(txt$TxtClass[1:5], collapse= '-')
	# for the horizon level variables
	for (vrb in unique(sbst$variable)){
		for (hz in 1:nlayers){
			hrzID <- paste(vrb,hz,sep = '')
			agg_soil_data[rw,toupper(hrzID)] <- sbst[which(sbst$variable == vrb),'value'][hz]
		}
	}
}

agg_soil_data$OBJECTID = 1:nrow(agg_soil_data)
agg_soil_data$CMPPCT <- 100
agg_soil_data = agg_soil_data[,1:(ncol(agg_soil_data)-2)]
agg_soil_data[is.na(agg_soil_data)] = 0
agg_soil_data$MUID = as.character(agg_soil_data$MUID)
write.table(agg_soil_data, agg_soil_unit_tbl, sep="\t", row.names = F)

mupolygon_remap_mukey = merge(mupolygon, soil_tbl[,c("MUID", "hru_grp")], by.x="MUKEY", by.y="MUID")
mupolygon_remap_mukey@data$MUKEY = mupolygon_remap_mukey@data$hru_grp
writeOGR(mupolygon_remap_mukey, "C:/Users/ruesca/Documents",
	"MUPOLYGON_remap_mukey", driver = "ESRI Shapefile")
# writeOGR(mupolygon_remap_mukey, "C:/Users/ruesca/Documents/WRB_Soils_2mile_Buffer_gSSURGO_with_michigan.mdb",
	# "MUPOLYGON_remap_mukey", driver = "ODBC")