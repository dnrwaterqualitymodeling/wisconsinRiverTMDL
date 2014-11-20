library(rgdal)
library(reshape2)
library(aqp)
library(soiltexture)
library(stringr)
library(mclust)
# library(RODBC)
options(stringsAsFactors = F)
net_soil_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils/"
soil_tbl = read.table(paste(net_soil_dir, "aggregated_soils_to_mapunit.txt",sep=''), header=T)
agg_prof_tbl = paste(net_soil_dir, 'aggregated_profiles.txt', sep = '')
agg_soil_unit_tbl = paste(net_soil_dir, "aggregated_soil_units.txt", sep = '')
agg_unit_mukey_lu_tbl = paste(net_soil_dir, "agg_unit_mukey_lu.txt", sep = '')
# mupolygon = readOGR(paste(net_soil_dir, "WRB_Soils_2mile_Buffer_gSSURGO.gdb", sep=""),
# 	"MUPOLYGON__2mile_buffer_wMich_2014")
mupolygon = readOGR(
    gsub('/$','',net_soil_dir), 
    "MUPOLYGON__2mile_buffer_wMich_2014")
wrb_mukeys = read.csv(paste(net_soil_dir, "wrb_mukeys.txt",sep = ''))
mupolygon = subset(mupolygon, MUKEY %in% wrb_mukeys$MUKEY)

# wtm CRS
wtm = "+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +units=m +no_defs"

# for updating swat soils db
f_swat_soils_db = "C:/SWAT/ArcSWAT/Databases/SWAT_US_SSURGO_Soils.mdb"
# These will need to be changed to reflect the new trash baskets.
excld = c(
    "Aquents",
#     "Psammaquents",
    "Fluvaquents, wet",
    "Alluvial land, wet",
    
    "Marsh",
    
	"Water",
    "Water greater than 40 acres",

    "Rock land",
	"Rock outcrop",
	"Stony and rocky land",
	"Sandy land",
    "Steep sandy land",
	"Dam concrete",
    "Dam, concrete",
	"Landfill",
    "Pits",  
	"Pits, quarries",
    "Pits, quarry",
	"Pits, quarry, soft bedrock",
	"Pits, quarry, hard bedrock",
    "Pits, sand and gravel",
	"Pits, siliceous sand", 
	"Pits, gravel", 
	"Coal pit", 
	"Udorthents, earthen dams",
	"Udorthents",
    "Urban land")

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
# Instantiating columns, but not being used in clustering
ph_cols = grep("PH[0-9]$", names(soil_tbl), value=T)
rock_cols = grep("ROCK[0-9]$", names(soil_tbl), value=T)
albedo_cols = grep("SOL_ALB[0-9]$", names(soil_tbl), value=T)
salinity_cols = grep("SOL_EC[0-9]$", names(soil_tbl), value=T)
caco3_cols = grep("SOL_CAL[0-9]$", names(soil_tbl), value=T)

# Calculate depth-weighted mean of hydrologic characteristics,
# and collect into numeric array for mclust processing
agg_tbl = array(NA, dim=c(nrow(soil_tbl),9)) # or 13 if all included
mukeys = array(NA, dim=nrow(soil_tbl))
pb = txtProgressBar(0,1)
for (i in 1:nrow(soil_tbl)) {
    setTxtProgressBar(pb, i/nrow(soil_tbl))
	d = soil_tbl[i,]
	mukeys[i] = d$MUID
	n = d$NLAYERS
    if (is.na(n) | n == 1) {
        depths = 1
    } else {
	    depths = sort(as.integer(d[depth_cols][1:n]))
	    depths = depths - c(0, depths[1:(n-1)])
    }
    sand = sum(d[sand_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	silt = sum(d[silt_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	clay = sum(d[clay_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	bd = sum(d[bd_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	k = sum(d[k_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	awc = sum(d[awc_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	cbn = sum(d[cbn_cols][1:n] * depths, na.rm=T) / sum(depths, na.rm=T)
	usle_k = d$USLE_K1
	tot_depth = d$SOL_ZMX
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
close(pb)

agg_tbl[is.na(agg_tbl)] = 0 # Conforms to SWAT SSURGO database
# Loop across HSG A,B,C,D

for (hsg in LETTERS[1:4]) {
	ind = which(soil_tbl$HYDGRP == hsg & !(soil_tbl$SNAM %in% excld))
	clus_d = agg_tbl[ind,]
	clus_d_scld = scale(clus_d)
	# For each HSG, find clusters
	# using default cluster settings: 1 to 9.
	clusters = Mclust(clus_d_scld) #, G=3
	print(table(clusters$classification))
	soil_tbl[ind, "hru_grp"] = paste(hsg, clusters$classification, sep="")
}
q = soil_tbl$SNAM %in% c("Water", "Water greater than 40 acres")
soil_tbl$hru_grp[q] = "W"
q = soil_tbl$SNAM %in% excld[!(excld %in% c("Water", "Water greater than 40 acres"))]
soil_tbl$hru_grp[q] = "X"
soil_tbl$MUID = as.character(soil_tbl$MUID)
write.table(soil_tbl[,c("MUID", "hru_grp", "SNAM")], agg_unit_mukey_lu_tbl, sep="\t", row.names=F)
# Hell yeah.
water_mus <- subset(soil_tbl, hru_grp == 'W')
soil_tbl <- subset(soil_tbl, hru_grp != "W")

###############################
# Reformatting soil data so it can be used by aqp
###############################
soil_pr = data.frame()
pb = txtProgressBar(0,1)
for (i in 1:nrow(soil_tbl)){
    setTxtProgressBar(pb, i/nrow(soil_tbl))
	d = soil_tbl[i,]
	n = d$NLAYERS
	bottom = as.integer(d[depth_cols][1:n])
	top = c(0, bottom[1:n-1])
	sand = as.integer(d[sand_cols][1:n])
	silt = as.integer(d[silt_cols][1:n])
	clay = as.integer(d[clay_cols][1:n])
	bd = as.numeric(d[bd_cols][1:n])
	k   = as.numeric(d[k_cols][1:n])
	awc = as.numeric(d[awc_cols][1:n])
	cbn = as.numeric(d[cbn_cols][1:n])
	usle_k = as.numeric(d[usle_k_cols][1:n])
	ph = sum(d[ph_cols][1:n] * depths) / sum(depths)
	rock = sum(d[rock_cols][1:n] * depths) / sum(depths)
	albedo = sum(d[albedo_cols][1:n] * depths) / sum(depths)
	salinity = sum(d[salinity_cols][1:n] * depths) / sum(depths)
    caco3 = sum(d[caco3_cols][1:n] * depths) / sum(depths)
	rows = data.frame(
		id = rep(d$MUID, n),
		name = paste(rep(d$MUID, n), 1:n, sep="_"),
		group = rep(d$hru_grp, n),
		top = top,
		bottom = bottom,
		sand = sand,
		silt = silt,
		clay = clay,
		sol_bd = bd,
		sol_k = k,
		sol_awc = awc,
		sol_cbn = cbn,
		usle_k = usle_k,
		sol_ph = ph,
		rock = rock,
		sol_alb = albedo,
		sol_ec = salinity,
        sol_cal = caco3)
	soil_pr = rbind(soil_pr, rows)
}
close(pb)
soil_pr$top[is.na(soil_pr$top)] = 0
soil_pr$bottom[is.na(soil_pr$bottom)] = 0
max_depths = aggregate(SOL_ZMX ~ hru_grp, data=soil_tbl, FUN=mean)
# promote to soil profile collection object
depths(soil_pr) = id ~ top + bottom
# make the object aware of the different groups (sites in aqp terms)
site(soil_pr) = ~ group

agg_profs = data.frame()
###############################
# Merge grouped soils together using aqp
# this loop takes 30 to 40 minutes to run
###############################
pb = txtProgressBar(0,1)
i = 0
for (grp in unique(soil_tbl$hru_grp)) {
    i = i + 1
    setTxtProgressBar(pb, i/length(unique(soil_tbl$hru_grp)))
	qry = paste("group == '",grp, "'",sep='')
	print(paste("Working on ", grp, "...")) 
	grp_tbl = subsetProfiles(soil_pr, s=qry)
	print(paste(grp," contains ", length(grp_tbl), " profiles...")) 
	max_depth = max_depths[max_depths$hru_grp == grp, "SOL_ZMX"]
	# setting those horizons with percentages that sum to less than 80
# 	txtSums = rowSums(grp_tbl@horizons[,c('sand','silt','clay')])
# 	grp_tbl@horizons[which(txtSums < 80),c('sand','silt','clay')] = NA
    
	grp_slab = slab(grp_tbl,
        fm = ~ sand +
            silt +
            clay +
            sol_bd +
            sol_k + 
            sol_awc +
            sol_cbn +
            usle_k +
            sol_ph +
            rock +
            sol_alb +
            sol_ec +
            sol_cal,
		slab.structure=seq(0,max_depth,length.out=6),
		slab.fun=mean,
		na.rm=TRUE)
	grp_slab$hru_grp = grp
	# assuming 5 horizons
	grp_slab$hrz_num = rep(1:5, times = 13)
	agg_profs = rbind(agg_profs, grp_slab)
}
close(pb)
agg_profs$variable = as.character(agg_profs$variable)
write.table(agg_profs, agg_prof_tbl, sep = '\t', row.names = F)

# hrucode look up table. probably necessary for arcswat
# ADD W and 1
hru_grp_code_lu = data.frame()
for (i in unique(agg_profs$hru_grp)){
    if (i == 'X'){
        hru_grp_code_lu = rbind(hru_grp_code_lu, c(i, "99"))
        next
    }
    indx = which(LETTERS == strsplit(i, split = '')[[1]][1])
    cde = gsub("[A-Z]", indx, i)
    rw = c(i, cde)
    hru_grp_code_lu = rbind(hru_grp_code_lu, rw)
}
hru_grp_code_lu = rbind(hru_grp_code_lu, c("W", "1"))
names(hru_grp_code_lu) = c("hru_grp", "hru_code")

agg_profs = read.table(agg_prof_tbl, sep="\t", header=T)
agg_soil_data = data.frame(soil_tbl[1,])
agg_soil_data[1,] = 0
for (rw in 1:length(unique(soil_tbl$hru_grp))){
	hruGrp = unique(soil_tbl$hru_grp)[rw]
	if (is.na(hruGrp)) {next}
	sbst = subset(agg_profs, hru_grp == hruGrp)
	nlayers = length(unique(sbst$top))
	agg_soil_data[rw, 'NLAYERS'] = nlayers
	agg_soil_data[rw, 'SOL_ZMX'] = max_depths[max_depths$hru_grp == hruGrp, "SOL_ZMX"] * 10
	agg_soil_data[rw, 'ANION_EXCL'] = 0.5
	agg_soil_data[rw, 'SOL_CRK'] = 0.5
	agg_soil_data[rw, 'HYDGRP'] = strsplit(hruGrp, '[0-9]')[[1]]
	agg_soil_data[rw, c('SNAM','SEQN','S5ID')] = hruGrp
    agg_soil_data[rw, 'MUID'] = hru_grp_code_lu[which(hru_grp_code_lu$hru_grp == hruGrp), 'hru_code']
	# grabbing hz depths
	dpths = unique(sbst$bottom) * 10
	dpths = dpths - c(0, dpths[-length(dpths)])
	solZ = paste('SOL_Z', 1:nlayers,sep = '')
	agg_soil_data[rw,solZ] = cumsum(dpths)
	# creating texture class
	txt= dcast(sbst, top + bottom ~ variable)[c('sand','silt','clay')]
	names(txt) = toupper(names(txt))
	for (h in 1:5) {
		err = try({
			txt$TxtClass[h] = TT.points.in.classes(tri.data = txt[h,1:3],
				class.sys ="USDA.TT",
				PiC.type = 't',
				text.tol = 3)}, silent=T) # tolerance, may need to be changed
		if(class(err)=="try-error") {
			print(paste("There is no texture data for", hruGrp))
			txt$TxtClass[h] = 'Non-mineral'
		} else {
			txt$TxtClass[h] = strsplit(txt$TxtClass[h], ',')[[1]][1]
			txt$TxtClass[h] = str_replace(txt$TxtClass[h], "[a]", '')
			txt$TxtClass[h] = str_replace(txt$TxtClass[h], "[o]", '')
			txt$TxtClass[h] = str_replace(txt$TxtClass[h], "[l]", '')
		}
	}
	agg_soil_data[rw, 'TEXTURE'] = paste(txt$TxtClass[1:5], collapse= '-')
	# for the horizon level variables
	for (vrb in unique(sbst$variable)){
		for (hz in 1:nlayers){
			hrzID = paste(vrb,hz,sep = '')
			agg_soil_data[rw,toupper(hrzID)] = sbst[which(sbst$variable == vrb),'value'][hz]
		}
	}
}
# converting from SSURGO's ksat units of um/sec to SWAT's mm/hr (3.6)
agg_soil_data[,k_cols] = agg_soil_data[,k_cols] * 3
# water's MUID/MUKEY is now 1
agg_soil_data[nrow(agg_soil_data) + 1, c('SNAM','SEQN', 'S5ID',"TEXTURE")] <- 'W'
update_num_cols = c('MUID',
	'CMPPCT',
	'SOL_ALB1',
	"ANION_EXCL",
	"SOL_CRK",
	"SOL_ZMX",
	"SOL_Z1",
	"NLAYERS",
	"SOL_K1")
agg_soil_data[agg_soil_data$SNAM == "W", update_num_cols] <- c(1, 100, 0.23, 0.5, 0.5, 25, 25, 1, 600)
# givin' water a D
agg_soil_data$HYDGRP[agg_soil_data$SNAM == "W"] <- "D"

agg_soil_data$OBJECTID = 1:nrow(agg_soil_data)
agg_soil_data$CMPPCT = 100

agg_soil_data[is.na(agg_soil_data)] = 0

agg_soil_data$MUID = as.integer(agg_soil_data$MUID)
# agg_soil_data$MUID = as.character(agg_soil_data$MUID)

x_hydgrps = subset(soil_tbl, hru_grp == "X")$HYDGRP

# Average the HSGs together for MUID = X
x_hydgrp = LETTERS[round(mean(unlist(lapply(x_hydgrps, function (x) {which(LETTERS == x)}))))]
agg_soil_data$HYDGRP[agg_soil_data$SNAM == "X"] = x_hydgrp

soil_tbl = rbind(soil_tbl, water_mus)
# need to recode the soils polygon layer so mukey is now a numeric identifier of the hru_grp
soil_tbl$hru_code <- NA
for (grp in 1:nrow(hru_grp_code_lu)){
    hru_grp <- hru_grp_code_lu[grp, 'hru_grp']
    hru_code <- hru_grp_code_lu[grp, 'hru_code']
    soil_tbl[which(soil_tbl$hru_grp==hru_grp),'hru_code'] <- hru_code 
}
agg_soil_data <- subset(agg_soil_data, select = -hru_grp)
write.table(agg_soil_data, agg_soil_unit_tbl, sep="\t", row.names = F)

#----
mupolygon = spTransform(mupolygon, CRS(wtm))
        
mupolygon_remap_mukey = merge(mupolygon, 
    soil_tbl[,c("MUID", "hru_grp","hru_code")], 
    by.x="MUKEY", 
    by.y="MUID")

# I (DE) don't believe its necessary to have a numeric id for mukey,
#   in fact I think it makes it more difficult as it gets read as long type and cannot be
#   joined to the shapefile
mupolygon_remap_mukey@data$MUKEY = mupolygon_remap_mukey@data$hru_code
writeOGR(mupolygon_remap_mukey, gsub("/$", "", net_soil_dir),
	"MUPOLYGON_remap_mukey", driver = "ESRI Shapefile")
