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
# txtinout = "H:/WRB/Scenarios/Default/TxtInOut"
# dir_out = "H:/WRB_sensitivity"
# p = "SFTMP"
# ext = "bsn"
# mn = -5
# mx = 5
# method = "a"
# iter = 2




logfile = paste(dir_out, '/',p,'.log',sep='')
write(
	paste("Log file for", p),
	logfile
)


library(ncdf)

options(stringsAsFactors=F)

# Delete outputs from txtinout if they exist
output.files = list.files(txtinout, pattern="^output", full.names=T)
unlink(output.files)

# Move txintout to a parameter-specific folder
td = paste(tempdir(), p, sep="\\")
# td = "C:/Users/evansdm/AppData/Local/Temp/Rtmpm45Zcz/SLSUBBSN"
if (!file.exists(td)) {dir.create(td)} # else {unlink(td, recursive=T)}
wd = paste(td, basename(txtinout), sep="\\")
# wd = "C:/Users/evansdm/AppData/Local/Temp/Rtmpm45Zcz/SLSUBBSN/TxtInOut"
print("Beginning to copy files...")
file.copy(txtinout, td, recursive=T)
print("Copying complete.")

setwd(wd)
# move swat executable
# file.copy("D:/TxtInOut/SWAT_64rel.exe", "swat.exe")

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

# processing to find which line parameter of interest is on
p.ind = strsplit(p.file.list[[1]], '\\||:')
p.ind = lapply(p.ind, function(x,p){grepl(p, x[2])}, p=p)
p.ind = unlist(p.ind)
p.ind = which(p.ind)

# Finding out how many characters in param value
#	 only necessary for relative adjustment...right?
vl = p.file.list[[1]][p.ind]
	# grabbing only the places where the values are
vl = substr(vl, 9, 16)
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
	p.mn = mn
	p.mx = mx
}
p.rg = cbind(p.mn, p.mx)
p.mat = apply(p.rg, 1, function(x,iter) {seq(x[1], x[2], length.out=iter)}, iter=iter)
p.mat = t(p.mat)

dimI = dim.def.ncdf( "iteration", "unitless", 1:iter)
dimS = dim.def.ncdf( "subbasin", "ID", 1:338)
dimT = dim.def.ncdf( "Time", "days since 2001-12-31", 1:4383)

# Make varables of various dimensionality, for illustration purposes
mv = 1.e30 # missing value to use
q.var = var.def.ncdf( "streamflow", "cms", list(dimT,dimS,dimI), mv)
s.var = var.def.ncdf( "sediment", "metric tons", list(dimT,dimS,dimI), mv)
p.var = var.def.ncdf( "phosphorus", "kilograms", list(dimT,dimS,dimI), mv)

nc = create.ncdf(
	paste(dir_out, "/", p, ".nc", sep=""),
	list(q.var,s.var,p.var))

# For each iteration, rewrite all necessary files,
#	then run swat, and collect the data from each run.
for (i in 1:iter){
	# rewrite input files with new params
	for (fl in 1:length(p.file.list)) {
		new.val = p.mat[fl, i]
		new.val = formatC(
			new.val,
			digits = dec.places,
			width = 9,
			format = 'f')
		substr(p.file.list[[fl]][p.ind], 8, 16) = new.val
		writeLines(p.file.list[[fl]], names(p.file.list)[fl])
	}
	bat = tempfile(pattern="runswat_", fileext=".bat")
	writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
	system(bat)
	print("Processing SWAT output...")
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
	dat = subset(dat, select=c(1, 3, 5:7))
	colnames(dat) = c("sub", "mon", "flow", "sed", "totpkg")
	
	dat = dat[order(dat[,1]),]
	
	q = matrix(
		dat[,3], 
		nrow=4383, 
		ncol=338)
	sed = matrix(
		dat[,4], 
		nrow=4383, 
		ncol=338)
	pho = matrix(
		dat[,5], 
		nrow=4383, 
		ncol=338)
	
	dim(q) = c(4383, 338, 1)
	dim(sed) = c(4383, 338, 1)
	dim(pho) = c(4383, 338, 1)
	print("Writing output to netCDF...")
	put.var.ncdf(nc, q.var, q, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, s.var, sed, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, p.var, pho, start=c(1,1,i), count=c(-1,-1,1))
	write(
		paste("Completed iteration", i, "at", Sys.time()),
		logfile,
		append = T)
}
close.ncdf(nc)


