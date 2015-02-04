library(reshape2)
library(aqp)
# library(soilDB)
# library(RCurl)
# library(SSOAP)
library(plyr)
library(foreign)
options(stringsAsFactors = F, warn = 1)

# Inputs

net_soil_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"
aggregated_to_mapunit_file = paste(net_soil_dir, 'aggregated_soils_to_mapunit.txt', sep="/")
tmplate = paste(net_soil_dir, "SWAT_US_SSURGO_Soils_wrb.txt", sep="/")
wrb_mukeys_file = paste(net_soil_dir, "wrb_mukeys.txt", sep="/")
mukeys_file <- paste(net_soil_dir, "mupolygon_2mile_buffer_wMich_wDrained_2014.dbf", sep="/")
comp_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/component.txt", sep="/")
hrzn_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chorizon.txt", sep="/")
chfr_file = paste(net_soil_dir, "SSURGO_wi_mi_2014/chfrags.txt", sep="/")
check_on_these_file = paste(net_soil_dir, 'check_on_these.txt', sep='/')

#########
# template output table
agg_mky_tbl = read.table(tmplate, nrows=1, header=T)
# mukeys only within the WRB
mukeys = read.dbf(mukeys_file, as.is=T)
wrb_mukeys = unique(read.table(wrb_mukeys_file, header=T, sep=',')[["MUKEY"]])
wrb_mukeys = wrb_mukeys[!is.na(wrb_mukeys)]
mukeys = unique(subset(mukeys, MUKEY %in% wrb_mukeys))
# read in gSSURGO tables and process
##############
dat_cols = c(
	"dbovendry_r",
	"awc_r",
	"ksat_r",
	"cbn",
	"claytotal_r",
	"silttotal_r",
	"sandtotal_r",
	"fragvol_r", 
	"albedodry_r",
	"kwfact",
	"ec_r", 
	"caco3_r", 
	"ph1to1h2o_r",
	"hydgrp")
variables = c(
	"SOL_BD",
	"SOL_AWC",
	"SOL_K",
	"SOL_CBN",
	"CLAY",
	"SILT",
	"SAND",
	"ROCK",
	"SOL_ALB",
	"USLE_K",
	"SOL_EC",
	"SOL_CAL",
	"SOL_PH")
comp_cols = c("mukey",
	"cokey",
	"compname",
	"comppct_r",
	"hydgrp",
	"albedodry_r")
hrzn_cols = c("cokey",
	"chkey",
	"hzdept_r",
	"hzdepb_r",
	"dbovendry_r",
	"awc_r",
	"om_r",
	"ksat_r",
	"claytotal_r",
	"silttotal_r",
	"sandtotal_r",
	"kwfact",
	"ec_r",
	"caco3_r",
	"ph1to1h2o_r")
chfr_cols = c("chkey", "fragvol_r")
print("Reading component, horizon, and chfrags tables. Please wait...")
comp = read.table(comp_file, header=T, sep="\t")[comp_cols]
hrzn = read.table(hrzn_file, header=T, sep="\t")[hrzn_cols]
chfr = read.table(chfr_file, header=T, sep="\t")[chfr_cols]
comp = subset(comp, mukey %in% wrb_mukeys) # Only mukeys in WRB
comp = merge(comp, hrzn, by="cokey", all.x=T)
chfr = aggregate(fragvol_r ~ chkey, chfr, sum, na.rm=T) # Sum rock volumes by hrzn
comp = merge(comp, chfr, by="chkey", all.x=T) # Join component/horizon with chfrags
comp$fragvol_r[is.na(comp$fragvol_r)] = 0 # Force NA rock fragments to zero
# There are currently 6% NA HSGs in comp

# Bring in drained information
comp = merge(mukeys, comp, all.x=T, by.x="MUKEY", by.y="mukey")
drained = which(comp$DRAINED == 1)
not_drained = which(comp$DRAINED == 0)
comp$hydgrp[drained] = with(comp[drained,], substr(hydgrp, 1, 1))
comp$hydgrp[not_drained] = with(comp[not_drained,], substr(hydgrp, nchar(hydgrp), nchar(hydgrp)))
comp$MUKEY[drained] = with(comp[drained,], paste(MUKEY, hydgrp, sep=""))

comp$hydgrp[comp$hydgrp == "A"] = 1
comp$hydgrp[comp$hydgrp == "B"] = 2
comp$hydgrp[comp$hydgrp == "C"] = 3
comp$hydgrp[comp$hydgrp == "D"] = 4
comp$hydgrp[comp$hydgrp == ""] = NA
comp$hydgrp = as.integer(comp$hydgrp)

# ASSUMING here that 50% of organic matter is Carbon
comp$cbn = comp$om_r * 0.50 # Brady and Weil, 
comp$comppct_r = comp$comppct_r / 100
##############

# Functions for aggregating soil horizons using slab function in aqp
scale_to_1 = function(pcts) {
	scaled = pcts / (sum(pcts, na.rm=T))
	return(scaled)
}
sum.that.works = function(values, hrz.height, comppct) {
	d = matrix(values, nrow=hrz.height)
	pct_bool = matrix(!is.na(d), nrow=hrz.height, ncol=ncol(d))
	pct_bool[!pct_bool] = NA
	pcts = t(apply(pct_bool, 1, function(X) {X * comppct}))
	scaled_pcts = t(apply(pcts, 1, scale_to_1))
	ss = rowSums(d * scaled_pcts, na.rm=T)
	s = mean(ss, na.rm=T)
	return(c(s=s))
}

check.on.these = NULL
rw = 0
for (m in unique(comp$MUKEY)){
	rw = rw + 1
	agg_mky_tbl[] = NA
	print("-----------------------------------")
	print(paste('Working on ', m,'...',sep = ''))
	print(paste(round(rw / length(unique(comp$MUKEY))*100, 2), "% complete", sep=""))
	mc = subset(comp, MUKEY==m)
	# for id purposes, selecting the component name with 
	# the greatest comp pct
	SNAM = mc$compname[which.max(mc$comppct_r)][1]
	if (length(unique(mc$cokey))==1 | all(is.na(mc$hzdept_r))) {
		print("One component or non-slabbable...")
		mc = mc[,c('MUKEY',
			'chkey',
			"comppct_r",
			"hzdept_r", 
			"hzdepb_r",
			dat_cols)]
		max_depth = max(mc$hzdepb_r, is.na=T)
		# need to calculate a component-weighted mean of all data columns
		if (length(unique(mc$cokey)) > 1) { # If there is more than one component
			w.aves = colSums(mc$comppct_r * mc[dat_cols], na.rm=T)
			mc = data.frame(
				mukey=mc$mukey[1],
				chkey=mc$chkey[1],
				comppct_r=sum(mc$comppct_r, na.rm=T),
				hzdept_r=NA,
				hzdepb_r=NA)
			mc = data.frame(c(mc, w.aves))
		}
		mky_data = melt(mc, id=c("hzdept_r", "hzdepb_r"))
		names(mky_data)[1:2] = c("top", "bottom")
		mky_data = subset(mky_data, !(variable %in% c("MUKEY", "chkey")))
		mky_data$value = as.numeric(mky_data$value)
		# check query says for any single-component map unit, compenent percentages add to less than half,
		# or there are no horizons,
		# all hydrologic soil groups are NA (probably water, and some pits, and other trash basket items).
		check_query = any(mc$comppct_r < 0.5) | any(is.na(mc$chkey)) | all(is.na(mc$hydgrp))
		if (check_query) {
			check.on.these = rbind(check.on.these, mc)
		}
	} else {
		print("Slabber...")
		max_depths = aggregate(cbind(hzdepb_r, comppct_r) ~ cokey, mc, max, na.rm=T)
		max_depth = with(max_depths, weighted.mean(hzdepb_r, w = comppct_r))
		depths(mc) = cokey ~ hzdept_r + hzdepb_r
		# slab to the MU level		   
		slab.structure = seq(0,round(max_depth),length.out=6)
		hrz.height = floor(slab.structure[2])
		comppct = with(mc@horizons, unique(cbind(cokey, comppct_r)))[,2]
		mky_data = slab(mc, fm = 
			~ hydgrp +
			sandtotal_r +
			silttotal_r + 
			claytotal_r +
			dbovendry_r + 
			kwfact + 
			awc_r + 
			cbn + 
			ksat_r + 
			ph1to1h2o_r + 
			fragvol_r +
			albedodry_r + 
			ec_r +
			caco3_r,
			slab.structure=slab.structure,
			slab.fun=sum.that.works,
			hrz.height=hrz.height,
			comppct=comppct
		)
	}
	nlayers = length(unique(mky_data$top))
	mky_data$value[mky_data$variable == "hydgrp" & mky_data$value == 0] = NA
	hydgrps = mky_data$value[mky_data$variable == "hydgrp"]
	if (all(is.na(hydgrps))) {
		hydgrp = NA
	} else {
		hydgrp = as.integer(round(mean(hydgrps, na.rm=T)))
		hydgrp = LETTERS[hydgrp]
	}
	print(paste('Mukey', m, 'has a hydgrp of', hydgrp))
	
	# grabbing hz depths
	mky_data = mky_data[order(mky_data$variable, mky_data$top),]
	dpths = unique(mky_data$bottom)
	dpths = dpths - c(0, dpths[-length(dpths)])
	solZ_cols = paste('SOL_Z',1:nlayers,sep = '')
	agg_mky_tbl[1, 'OBJECTID'] = rw
	agg_mky_tbl[1, 'MUID'] = m
	agg_mky_tbl[1, 'SEQN'] = m
	agg_mky_tbl[1, 'SNAM'] = SNAM
	agg_mky_tbl[1, 'S5ID'] = m
	agg_mky_tbl[1, 'CMPPCT'] = 100
	agg_mky_tbl[1, 'NLAYERS'] = nlayers
	agg_mky_tbl[1, 'HYDGRP'] = hydgrp
	agg_mky_tbl[1, 'HYDGRP_ORIG'] = hydgrp
	agg_mky_tbl[1, 'SOL_ZMX'] = max_depth
	agg_mky_tbl[1, 'S5ID'] = m
	agg_mky_tbl[1, 'ANION_EXCL'] = 0.5
	agg_mky_tbl[1, 'SOL_CRK'] = 0.5
	agg_mky_tbl[1, 'TEXTURE'] = 'TBD'
	agg_mky_tbl[1, solZ_cols] = cumsum(dpths)
	for (vrb in 1:length(variables)) {
		orig_col = dat_cols[vrb]
		var_data = subset(mky_data, variable == orig_col)
		var_data = var_data[order(var_data$top),]
		for (hz in 1:nrow(var_data)) {
			hrz_col_name = toupper(paste(variables[vrb], hz, sep = ''))
			prop_val = var_data[hz, "value"]
			agg_mky_tbl[1,hrz_col_name] = prop_val
		}
	}
	print("Writing to file...")
	if (rw == 1) {
		append = F
		col.names = T
	} else {
		append = T
		col.names = F
	}
	write.table(agg_mky_tbl, 
		aggregated_to_mapunit_file,
		append=append,
		row.names = F,
		col.names = col.names,
		sep = '\t')
}

write.table(check.on.these, 
	check_on_these_file,
	row.names = F, 
	sep='\t')
