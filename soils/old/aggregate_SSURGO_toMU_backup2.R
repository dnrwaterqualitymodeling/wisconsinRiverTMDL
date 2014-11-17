library(foreign)
library(rgdal)
library(reshape2)
library(rgeos)
library(aqp)
library(soiltexture)
library(stringr)
# library(mclust)
library(Hmisc)
library(soilDB)
# install.packages('soilDB', dep=TRUE)
# install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source")
library(plyr)
library(raster)
library(RCurl)
library(XML)
library(SSOAP)
library(XMLSchema)
library(plyr)
options(stringsAsFactors = F, warn = 1)
# my file names
net_soil_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets/Soils"
aggregated_to_mapunit_file <- 'aggregated_soils_to_mapunit.txt'
# mukey_table <- "wrb_mukeys.txt"
tmplate <- "SWAT_US_SSURGO_Soils_wrb.txt"
fish_file = "WRB_2_mile_buffer_fishnet"

# wrb_mukeys <- read.csv(paste(net_soil_dir, mukey_table,sep = '/'))
# to collect aggregated data
agg_mky_tbl <- read.table(paste(net_soil_dir, tmplate,sep='/'), header=T)[1,] 

fishnet = readOGR(net_soil_dir, fish_file)
sda_crs = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
fishnet = spTransform(fishnet, sda_crs)


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

file.remove(list.files(tempdir(), pattern="ssurgo", full.names=T))
all_mukeys = NULL
check.on.these=NULL
for (id in 1:2) {#length(fishnet)) {
    f = subset(fishnet, Id == id)
    e = extent(f)
    m = mapunit_geom_by_ll_bbox(c(e@xmin, e@ymin, e@xmax, e@ymax))
    m@data$mukey = as.character(m@data$mukey)
    m = gIntersection(m, f, byid=T, id=m@data$mukey)
    mukeys = row.names(m)
    row.names(m) = as.character(1:length(m))
    m = SpatialPolygonsDataFrame(m,
        data=data.frame(mukey=mukeys,
            row.names=as.character(1:length(m))))
    tile_file = tempfile(pattern="ssurgo_")
    writeOGR(m, dirname(tile_file), basename(tile_file), "ESRI Shapefile")
    mukeys = unique(mukeys[!(mukeys %in% all_mukeys)])
    all_mukeys = unique(c(all_mukeys, mukeys))
    for (rw in 1:length(mukeys)){
    #     rw <- 1
        agg_mky_tbl[] = 0
    	mky <- mukeys[rw]
        print(paste('Working on ', mky,'...',sep = ''))
    #     print(paste(round(rw / length(unique(wrb_mukeys$MUKEY))), "% complete", sep=""))
    	in.statement <- format_SQL_in_statement(mky)
    
    	# sand, silt, clay, awc, usle_k, ksat, Db, rock%, EC, C, pH, albedo, EC
    	# format query in SQL- raw data are returned
        exists_q = paste(
            "select count(1)
            from component as c
            where c.mukey = '", mky, "'", sep="")
    	q <- paste(
    		"SELECT 
    		c.mukey, 
    		c.cokey,
    		c.chkey,
    		c.compname, 
    		c.albedodry_r, 
    		c.hydgrp,
    		c.comppct_r,
    		c.hzdept_r,
    		c.hzdepb_r,
    		c.hzname,
    		c.awc_r, 
    		c.sandtotal_r,
    		c.claytotal_r,
    		c.silttotal_r,
    		c.dbovendry_r,
    		c.ksat_r,
    		c.caco3_r,
    		c.kwfact,
    		c.ec_r,
    		c.om_r,
    		c.ph1to1h2o_r,
    		cf.fragvol_r
    		FROM
    		(SELECT 
    		 component.mukey, 
    		 component.cokey,
    		 chkey,
    		 compname, 
    		 albedodry_r, 
    		 hydgrp,
    		 comppct_r,
    		 hzdept_r,
    		 hzdepb_r,
    		 hzname,
    		 awc_r, 
    		 sandtotal_r,
    		 claytotal_r,
    		 silttotal_r,
    		 dbovendry_r,
    		 ksat_r,
    		 caco3_r,
    		 kwfact,
    		 ec_r,
    		 om_r,
    		 ph1to1h2o_r
    		 FROM component JOIN chorizon
    		 ON component.cokey = chorizon.cokey AND mukey IN ", in.statement, ") AS c
    		LEFT JOIN chfrags AS cf
    		ON c.chkey = cf.chkey
    		ORDER BY mukey, comppct_r DESC, hzdept_r ASC", sep="")
    
    
    	# now get component and horizon-level data for these map unit keys
    	comp_data <- SDA_query(q)
    	# write.table(comp_data, paste(net_soil_dir, 'wrb_hz_level_soil_data.txt',sep=''),sep='\t',row.names = F)
    	# processing soil properties
    		#ASSUMING here that 50% of organic matter is Carbon
    	agg_cols <- names(comp_data)[names(comp_data) != 'fragvol_r']
    	#     agg_cols = paste(agg_cols, collapse = " + ")
    
    	comp_data_no_frag <- comp_data[,agg_cols]
    	comp_data_no_frag <- unique(comp_data_no_frag)
        comp_data[is.na(comp_data$fragvol_r), 'fragvol_r'] <- 0
            
    	frags <- aggregate(fragvol_r ~ chkey, data=comp_data, sum, na.rm = T)
    	comp_data <- merge(comp_data_no_frag, frags, by = 'chkey',all.x = T)
    	# Convert hydrologic soil group to integer
    	# A/D, B/D, and C/D will all get values of 4
    	# This is consistent with what we found when overlaying soils with landuse
    	# There was very little agriculture on A/D, B/D, and C/D soils
    	# This leads us to assume very little of these soils are drained in Wisconsin
    	hsgs = c(LETTERS[1:4], "A/D", "B/D", "C/D")
    	hsgs_num = c(1:4, rep(4,3))
    	for (i in 1:7) {
    		comp_data$hydgrp[comp_data$hydgrp == hsgs[i]] = hsgs_num[i]
    	}
    	comp_data$hydgrp = as.integer(comp_data$hydgrp)
        comp_data$cbn <- comp_data$om_r * 0.50
     	comp_data$comppct_r <- comp_data$comppct_r / 100
        comp_data[dat_cols] = as.numeric(unlist(comp_data[dat_cols]))
    #     comp_data[,dat_cols] <- comp_data[,dat_cols] * comp_data[,'comppct_r']
        max_depths = aggregate(cbind(hzdepb_r, comppct_r) ~ compname, data=comp_data, FUN=max)
    	max_depth <- with(max_depths, weighted.mean(hzdepb_r, w = comppct_r))
        
        if (length(unique(comp_data$cokey))==1) {
            comp_data = comp_data[c(dat_cols,"hzdept_r", "hzdepb_r")]
            mky_data = melt(comp_data, id=c("hzdept_r", "hzdepb_r"))
            names(mky_data)[1:2] = c("top", "bottom")
            if (any(is.na(comp_data))) {
                check.on.these = rbind(check.on.these, comp_data)
                next
            }
        } else {
        	depths(comp_data) <- cokey ~ hzdept_r + hzdepb_r
        	# slab to the MU level           
            slab.structure = seq(0,round(max_depth),length.out=6)
        	hrz.height = floor(slab.structure[2])
            comppct = with(comp_data@horizons, unique(cbind(cokey, comppct_r)))[,2]
        	mky_data <- slab(comp_data, fm = 
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
    	nlayers <- length(unique(mky_data$top))
    	hydgrp = round(mean(mky_data$value[mky_data$variable == "hydgrp"]))
    	if (hydgrp == 1) {hydgrp = "A"} 
    	if (hydgrp == 2) {hydgrp = "B"}
    	if (hydgrp == 3) {hydgrp = "C"}
    	if (hydgrp == 4) {hydgrp = "D"}
    	print(paste('Mukey', mky, 'has a hydgrp of',hydgrp))
        
        # grabbing hz depths
        dpths <- unique(mky_data$bottom)
        dpths <- dpths - c(0, dpths[-length(dpths)])
        solZ <- paste('SOL_Z',1:nlayers,sep = '')
        agg_mky_tbl[1, 'OBJECTID'] <- rw
        agg_mky_tbl[1, 'MUID'] <- mky
        agg_mky_tbl[1, 'SEQN'] <- mky
        agg_mky_tbl[1, 'SNAM'] <- mky
        agg_mky_tbl[1, 'S5ID'] <- mky
        agg_mky_tbl[1, 'CMPPCT'] <- 100
        agg_mky_tbl[1, 'NLAYERS'] <- 5
        agg_mky_tbl[1, 'HYDGRP'] <- hydgrp
        agg_mky_tbl[1, 'HYDGRP_ORIG'] <- hydgrp
        agg_mky_tbl[1, 'SOL_ZMX'] <- max_depth
        agg_mky_tbl[1, 'S5ID'] <- mky
        agg_mky_tbl[1, 'ANION_EXCL'] <- 0.5
        agg_mky_tbl[1, 'SOL_CRK'] <- 0.5
        agg_mky_tbl[1, 'TEXTURE'] <- 'TBD'
        agg_mky_tbl[1, solZ] <- dpths
        
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
        for (vrb in 1:length(variables)){
        	for (hz in 1:5){
        		hrzID <- paste(variables[vrb],hz,sep = '')
                prop_val <- mky_data[which(mky_data$variable == dat_cols[vrb]),'value'][hz]
        		agg_mky_tbl[1,toupper(hrzID)] <- prop_val
        	}
        }
        if (rw == 1) {
            write.table(agg_mky_tbl, 
                        paste(net_soil_dir, aggregated_to_mapunit_file, sep = '/'), 
                        row.names =F,
                        col.names = T,
                        sep = '\t')
        } else {
            write.table(agg_mky_tbl, 
                        paste(net_soil_dir, aggregated_to_mapunit_file, sep = '/'), 
                        row.names = F,
                        append = T,
                        col.names = F,
                        sep = '\t')
        }
    }
}
