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
# operation = arguments[10]
# collect_reach_data = as.logical(arguments[11])
# run = as.integer(arguments[9])
##
#
# txtinout = "H:/WRB/Scenarios/Default/TxtInOut"
# dir_out = "H:/wrb_calibration_7iters"
# temp_dir = "H:/temp_directory"
# txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut"
# dir_out = "C:/Users/ruesca/Desktop/WRB_sensitivity"
# temp_dir = "C:/Users/ruesca/Desktop/temp_directory"
# p = "WET_MXVOL"
# ext = "pnd"
# mn = -0.5
# mx = 3
# method = "r"
# iter = 7

just_calibration = TRUE
file_output = paste(dir_out, paste(p,"_param_values.txt",sep=''), sep="/")

horizon_number = c(1)#c(1,2,3,4,5)
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
place_vals = data.frame(
	file_ext = c('bsn','hru','gw','rte','mgt','pnd'),
	beginnings = c(9,8,8,7,12,5),
	endings = c(16,16,16,14,16,16))

wd = txtinout
setwd(wd)
# format parameter file(s)
# List files with extension associated with parameter (e.g., bsn, gw)

ext.re = paste("\\.", ext, "$",sep='')
p.files = list.files(pattern = ext.re)
if (just_calibration & ext != "bsn"){
	calib_basins = c(127,162,159,268,157,155,152,326,151,150,149,78,142,141,140,199,195,138,184,137)
	# calib_search = paste("00",calib_basins,sep='')
	
	pfs = NULL
	for (sb in calib_basins){
		if (nchar(sb) == 3){
			srch = paste("^00", sb,sep='')
		} else if (nchar(sb) == 2){
			srch = paste("^000", sb,sep='')
		} else if (nchar(sb) == 1){
			srch = paste("^0000", sb,sep='')
		} 
		pfs = c(pfs, p.files[grepl(srch, p.files)])
	}
	p.files = pfs
}
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
defaults_files = p.file.list
#### processing for CNOP
#### currently trying to set up for calibrating planting CNOP differently from tilling
if (substr(p,1,4) == "CNOP"){
	operation = substr(p, 6, nchar(p))
	### for tillage
	if (operation == "planting"){
		opnum = " 1"
		pstion = c(76, 80)
		dgts = 2
		frmting = "%2.2f"
	} else if (operation == "tillage"){
		opnum = " 6"
		pstion = c(32, 43)
		dgts = 5
		frmting = "%5.5f"
	}
	dflts = lapply(p.file.list,
		FUN=function(x, opnum){
			indx = which(substr(x, 17, 18) == opnum)
			vals = substr(x[indx], pstion[1], pstion[2])
			return(vals)
		}, opnum=opnum
	)
	
	p.mat.list = lapply(dflts,
		FUN=function(x){
			x = as.numeric(x)
			p.mn = x * (1 + mn)
			p.mx = x * (1 + mx)
			p.rg = cbind(p.mn, p.mx)
			p.mat = apply(p.rg, 1, function(x,iter) {seq(x[1], x[2], length.out=iter)}, iter=iter)
			p.mat = t(p.mat)
		}
	)
} else if (ext == 'sol'){
# processing to find which line parameter of interest is on
###### For soil files
	sol_par_lu = data.frame(
		par.name=c("SOL_BD","SOL_AWC","SOL_K","SOL_OC","SOL_ALB","USLE_K","SOL_Z"),
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
	
	dec.places = 2
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
}
# Scale parameters according to range and save as a matrix
	# Need to do differently for each method (i.e., relative, absolute)
if (substr(p,1,4) != "CNOP"){
	if (method == "r"){
		dflts = lapply(p.file.list, 
			FUN = function(x, p.ind){
				ln = x[p.ind]
				if (ext == ".sol"){
					val = as.numeric(substr(ln, begin_loc, endin_loc)) 
				} else {
					val = as.numeric(strsplit(ln, split = "\\||:")[[1]][1])
				}
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

	p.rg = cbind(p.mn, p.mx)
	p.mat = apply(p.rg, 1,
		function(x,iter) {
			seq(x[1], x[2], length.out=iter)
		},
		iter=iter)
	p.mat = t(p.mat)
}

hdr = c("Subbasin", paste("iter", seq(1, iter, 1), sep=""))
hdr = paste(hdr, collapse="\t")
write(hdr, file_output)
prev_fl = "00000"
if (ext == "bsn"){
	write(paste(c(0, paste(p.mat,collapse="\t")),collapse='\t'),file_output,append=T)
} else if (method == "a"){
	write(paste(c(0, paste(p.mat[1,],collapse="\t")),collapse='\t'),file_output,append=T)
} else {
	for (fl in dimnames(p.mat)[[1]]){
		if (substr(fl, 1, 5) == substr(prev_fl, 1, 5)){
			prev_fl = fl
			next
		}
		vals = p.mat[which(dimnames(p.mat)[[1]]==fl),]
		info = substr(fl, 1, 5)
		info = paste(
			info, 
			paste(vals, collapse="\t"),
			sep="\t")
		write(info, file_output, append=T)
		prev_fl = fl
	}
}


Sys.sleep(5)