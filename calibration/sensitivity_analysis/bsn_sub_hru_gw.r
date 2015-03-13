# to run a sensitivity analysis for basin parameters
arguments = commandArgs(trailingOnly = T)
txtinout = arguments[1]
dir_out = arguments[2]
temp_dir = arguments[3]
p = arguments[4]
ext = arguments[5]
mn = as.numeric(arguments[6])
mx = as.numeric(arguments[7])
method = arguments[8]
iter = as.integer(arguments[9])
# run = as.integer(arguments[9])
##
# txtinout = "H:/WRB/Scenarios/Default/TxtInOut"
# dir_out = "H:/WRB_sensitivity"
# temp_dir = "H:/temp_directory"
# p = "CN2"
# ext = "mgt"
# mn = -0.5
# mx = 0.5
# method = "r"
# iter = 2

# Potential argument, hard code for now
horizon_number = c(1)#c(1,2,3,4,5)

logfile = paste(dir_out, '/',p,'.log',sep='')
write(
	paste("Log file for", p),
	logfile
)

library(ncdf)
options(stringsAsFactors=F)

# these are all the codes in SWAT_lookup.csv that are above 9, assuming 9 = cranberries
ag_codes = c(
		"SWHT",
		"WWHT",
		"DWHT",
		"RYE",
		"BARL",
		"OATS",
		"RICE",
		"PMIL",
		"TIMO",
		"BROS",
		"BROM",
		"FESC",
		"BLUG",
		"BERM",
		"CWGR",
		"WWGR",
		"SWGR",
		"RYEG",
		"RYER",
		"RYEA",
		"SIDE",
		"BBLS",
		"LBLS",
		"SWCH",
		"INDN",
		"ALFA",
		"CLVS",
		"CLVR",
		"CLVA",
		"SOYB",
		"CWPS",
		"MUNG",
		"LIMA",
		"LENT",
		"PNUT",
		"FPEA",
		"PEAS",
		"SESB",
		"COTS",
		"COTP",
		"SGBT",
		"POTA",
		"SPOT")

# matrix of begining and ending values for where values are in text files
# for
# 	bsn, hru, gw, rte
# 
place_vals = data.frame(
	file_ext = c('bsn', 'hru', 'gw','rte','mgt'),
	beginnings = c(9,8,8,7,12),
	endings = c(16,16,16,14,16))

# Delete outputs from txtinout if they exist
output.files = list.files(txtinout, pattern="^output", full.names=T)
unlink(output.files)

# Move txintout to a parameter-specific folder

if (!file.exists(temp_dir)){dir.create(temp_dir)}

td = paste(temp_dir, "\\", p, "_", ext, sep="")
# td = "C:/Users/evansdm/AppData/Local/Temp/Rtmpm45Zcz/SLSUBBSN"
if (!file.exists(td)) {dir.create(td)} 
wd = paste(td, basename(txtinout), sep="\\")
# wd = "C:/Users/evansdm/AppData/Local/Temp/Rtmpm45Zcz/SLSUBBSN/TxtInOut"
print("Beginning to copy files...")
file.copy(txtinout, td, recursive=T)
print("Copying complete.")

setwd(wd)
# move swat executable, already in TxtInout...not needed
# file.copy("D:/TxtInOut/SWAT_64rel.exe", "swat.exe")

########### 
# Format file.cio
# setting to daily
# Write file.cio
file.cio.dat = readLines("file.cio")

file.cio.dat[59] = "               1    | IPRINT: print code (month, day, year)"

##### Reach output variables
file.cio.dat[65] = "   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
##### Subbasin output variables
file.cio.dat[67] = "   9  10  12  14  15   0   0   0   0   0   0   0   0   0   0"
##### HRU output variables
file.cio.dat[69] = "   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
##### HRU data to be printed
file.cio.dat[71] = "   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
writeLines(file.cio.dat, "file.cio")
###########
# format parameter file(s)
# List files with extension associated with parameter (e.g., bsn, gw)
 ## remember to delete outfiles !!!!
ext.re = paste("\\.", ext, "$",sep='')
p.files = list.files(pattern = ext.re)

# Grab original parameter value for each file, hold in memory
p.file.list = list()
for (p.file in p.files){
	pf = readLines(p.file)
	if (ext == 'mgt'){
		strting = regexpr("Luse", pf[1])[1]
		strting = strting + 5
		luse=substr(pf[1], strting, strting+3)
		if (!luse %in% ag_codes){next}
	}
	p.file.list[[p.file]] = pf
}

# processing to find which line parameter of interest is on
###### For soil files
if (ext == 'sol'){
	sol_par_lu = data.frame(
		par.name=c("SOL_BD","SOL_AWC","SOL_K","SOL_OC","SOL_ALB","USLE_K", "SOL_Z"),
		#name.in.sol=c(),
		# the line at which it occurs in the .sol file
		par.indx=c(9,10,11,12,17,18,4))
	#find index
	p.ind = sol_par_lu$par.indx[which(sol_par_lu$par.name == p)]
	
	# finding beginning and ending location for values
	# horizon_number = c(1,2,3,4,5)
	horz_col_position = data.frame(
		horz.num = 1:5,
		ending.poz = c(39,51,63,75,88))
	
	endin_loc = horz_col_position$ending.poz[which(horz_col_position$horz.num %in% horizon_number)]
	begin_loc = endin_loc - 6
	# begin_loc = 33
	# endin_loc = 39
	
	dec.places = 2
	
	#collecting parameter values for soils files
	if (method == "r"){
		dflts = lapply(p.file.list, 
			FUN = function(x, p.ind){
				ln = x[p.ind]
				val = as.numeric(substr(ln, begin_loc, endin_loc))
				return(val)
			},
			p.ind = p.ind)
		dflts = unlist(dflts)
		p.mn = dflts * (1 + mn)
		p.mx = dflts * (1 + mx)
	} else {
		p.mn = rep(mn, length(p.file.list))
		p.mx = rep(mx, length(p.file.list))
	}
#################################
#####    for not soil parameters
} else { 
	p.ind = strsplit(p.file.list[[1]], '\\||:')
	p.ind = lapply(p.ind, function(x,p){grepl(p, x[2])}, p=p)
	p.ind = unlist(p.ind)
	p.ind = which(p.ind)

	# Finding out how many characters in param value
	#	 only necessary for relative adjustment...right?
	vl = p.file.list[[1]][p.ind]
		# grabbing only the places where the values might exist
	begin_loc = place_vals$beginnings[which(place_vals$file_ext == ext)]
	endin_loc = place_vals$endings[which(place_vals$file_ext == ext)]
	vl = substr(vl, begin_loc, endin_loc)
	vl = strsplit(vl, split = '.', fixed = T)[[1]][2]

	if (is.na(vl)){
		dec.places = 0
	} else { 
		dec.places = nchar(vl)
	}

# Scale parameters according to range and save as a matrix
	# Need to do differently for each method (i.e., relative, absolute)
	if (method == "r"){
		dflts = lapply(p.file.list, 
			FUN = function(x, p.ind){
				ln = x[p.ind]
				val = as.numeric(strsplit(ln, split = "\\||:")[[1]][1])
				return(val)
			},
			p.ind = p.ind)
		dflts = unlist(dflts)
		p.mn = dflts * (1 + mn)
		p.mx = dflts * (1 + mx)
	} else {
		p.mn = rep(mn, length(p.file.list))
		p.mx = rep(mx, length(p.file.list))
	}
}

p.rg = cbind(p.mn, p.mx)
p.mat = apply(p.rg, 1, function(x,iter) {seq(x[1], x[2], length.out=iter)}, iter=iter)
p.mat = t(p.mat)

dimI = dim.def.ncdf( "iteration", "unitless", 1:iter)
dimS = dim.def.ncdf( "subbasin", "ID", 1:337)
dimT = dim.def.ncdf( "Time", "days since 2001-12-31", 1:4383)

# Make varables of various dimensionality, for illustration purposes
mv = 1.e30 # missing value to use

q.var = var.def.ncdf( "water_yield", "mm", list(dimT,dimS,dimI), mv)
s.var = var.def.ncdf( "sediment", "metric tons", list(dimT,dimS,dimI), mv)
org.p.var = var.def.ncdf( "org_phosphorus", "kilograms", list(dimT,dimS,dimI), mv)
sol.p.var = var.def.ncdf( "sol_phosphorus", "kilograms", list(dimT,dimS,dimI), mv)
min.p.var = var.def.ncdf( "sed_phosphorus", "kilograms", list(dimT,dimS,dimI), mv)



# q.in.var = var.def.ncdf( "streamflow_in", "cms", list(dimT,dimS,dimI), mv)
# q.out.var = var.def.ncdf( "streamflow_out", "cms", list(dimT,dimS,dimI), mv)
# s.var = var.def.ncdf( "sediment", "metric tons", list(dimT,dimS,dimI), mv)
# p.var = var.def.ncdf( "phosphorus", "kilograms", list(dimT,dimS,dimI), mv)

nc = create.ncdf(
	paste(dir_out, "/", p, "_", ext, ".nc", sep=""),
	list(q.var,s.var,org.p.var,sol.p.var,min.p.var))

# For each iteration, rewrite all necessary files,
#	then run swat, and collect the data from each run.
for (i in 1:iter){
	# rewrite input files with new params
	for (fl in 1:length(p.file.list)) {
		new.val = p.mat[fl, i]
		new.val = formatC(
			new.val,
			digits = dec.places,
			# to find how wide the data val needs to be
			width = endin_loc-begin_loc+1,
			format = 'f')
		substr(p.file.list[[fl]][p.ind], begin_loc, endin_loc) = new.val
		writeLines(p.file.list[[fl]], names(p.file.list)[fl])
	}
	bat = tempfile(pattern="runswat_", fileext=".bat")
	writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
	system(bat)
	print("Processing SWAT output...")
	dat = readLines(paste(wd, "output.sub", sep="\\"))
	dat = dat[10:length(dat)]
	dat = gsub("\\s+", ",", dat)
	dat = gsub("BIGSUB,", "", dat)
	dat = strsplit(dat, ",")
	nrows = length(dat)
	ncols = length(dat[[1]])
	dat = unlist(dat)
	dat = matrix(dat, nrow=nrows, ncol=ncols, byrow=T)
	dat = apply(dat, 2, as.numeric)
	dat = subset(dat, select=c(1,4:8))
	colnames(dat) = c("sub","wat_yld", "sed", "org_p", "sol_p", "min_p")
	
	dat = dat[order(dat[,1]),]
	
	q = matrix(
		dat[,2], 
		nrow=4383, 
		ncol=337)
	sed = matrix(
		dat[,3], 
		nrow=4383, 
		ncol=337)
	org.pho = matrix(
		dat[,4], 
		nrow=4383, 
		ncol=337)
	sol.pho = matrix(
		dat[,5], 
		nrow=4383, 
		ncol=337)
	min.pho = matrix(
		dat[,6], 
		nrow=4383, 
		ncol=337)
	
	dim(q) = c(4383, 337, 1)
	dim(sed) = c(4383, 337, 1)
	dim(org.pho) = c(4383, 337, 1)
	dim(sol.pho) = c(4383, 337, 1)
	dim(min.pho) = c(4383, 337, 1)
	print("Writing output to netCDF...")
	put.var.ncdf(nc, q.var, q, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, s.var, sed, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, org.p.var, org.pho, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, sol.p.var, sol.pho, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, min.p.var, min.pho, start=c(1,1,i), count=c(-1,-1,1))
	
	write(
		paste("Completed iteration", i, "at", Sys.time()),
		logfile,
		append=T)
}
close.ncdf(nc)


