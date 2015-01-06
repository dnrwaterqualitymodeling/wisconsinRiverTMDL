# to run a sensitivity analysis for basin parameters
arguments = commandArgs(trailingOnly = T)
txtinout = arguments[1]
dir_out = arguments[2]
p = arguments[3]
ext = arguments[4]
mn = as.numeric(arguments[5])
mx = as.numeric(arguments[6])
method = arguments[7]
iter = as.integer(arguments[8])
####

txtinout = "H:\\WRB\\Scenarios\\Default\\TxtInOut"
dir_out = "H:\\WRB_sensitivity"
p = "SLSUBBSN"
ext = "hru"
mn = -0.5
mx = 0.5
method = "r"
iter = 2

options(stringsAsFactors=F)

# Delete outputs from txtinout if they exist
output.files = list.files(txtinout, pattern="^output", full.names=T)
unlink(output.files)

# Move txintout to a parameter-specific folder
td = paste(tempdir(), p, sep="\\")
if (!file.exists(td)) {dir.create(td)} # else {unlink(td, recursive=T)}
wd = paste(td, basename(txtinout), sep="\\")
file.copy(txtinout, td, recursive=T)

setwd(wd)
# move swat executable
file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", "swat.exe")

########### 
# Format file.cio
# setting to daily
# Write file.cio
file.cio.dat = readLines("file.cio")

file.cio.dat[59] = "               1    | IPRINT: print code (month, day, year)"

file.cio.dat[65] = "   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[67] = "   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[69] = "   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
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
	p.file.list[[p.file]] = pf
}

p.ind = strsplit(p.file.list[[1]], '\\||:')
p.ind = lapply(p.ind, function(x,p){grepl(p, x[2])}, p=p)
p.ind = unlist(p.ind)
p.ind = which(p.ind)
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
	p.mn = mn
	p.mx = mx
}
p.rg = cbind(p.mn, p.mx)
p.mat = apply(p.rg, 1, function(x,iter) {seq(x[1], x[2], length.out=iter)}, iter=iter)
p.mat = t(p.mat)

for (i in 1:iter){
	for (fl in 1:length(p.file.list)) {
		new.val = p.mat[fl, i]
		substr(p.file.list[[fl]][p.ind], 16-nchar(new.val), 16)
	}
}
	# grab column with scaled values and replace input files with them
	par.ind = which(substr(basins.bsn, 23, 27) == p)
	substr(basins.bsn[par.ind], 11, 16) = 
	format(val, digits=3, nsmall=3, width = 6)
				
	writeLines(basins.bsn, paste(wd, "basins.bsn", sep="\\"))

	
    # Run swat
	bat = tempfile(pattern="runswat_", fileext=".bat")
	writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
	system(bat)
	# collect results
	dat = readLines(paste(wd, "output.rch", sep="\\"))
	dat = dat[10:length(dat)]
	dat = gsub("\\s+", ",", dat)
	dat = gsub("REACH,", "", dat)
	dat = strsplit(dat, ",")
	nrows = length(dat)
	ncols = length(dat[[1]])
	dat = unlist(dat)
	dat = matrix(dat, nrow=nrows, ncol=ncols, byrow=T)
	dat = apply(dat, 2, as.numeric)
	
	tf = tempfile(fileext=".csv")
	writeLines(dat, tf)
	dat = read.table(tf, sep=",")
	dat = dat[,c(1,3,5,6,7)]
	names(dat) = c("sub", "mon", "flow", "sed", "totp")
	
	# add data to netcdf file
}
# Collect result from each iteration and save




