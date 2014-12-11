# to run a sensitivity analysis for basin parameters
arguments = commandArgs(trailingOnly = T)
wd = arguments[1]
dir_out = arguments[2]
p = arguments[3]
p_file = arguments[]
iter = arguments[4]
out_file = arguments[5]
####

wd = "H:\\WRB\\Scenarios\\Default\\TxtInOut"

options(stringsAsFactors=F)
setwd(wd)
file.cio = paste(wd,
    "/file.cio",
    sep="")

# Move txintout to a parameter-specific folder

# move swat executable
file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))

########### 
# Format file.cio
# setting to daily
# Write file.cio
file.cio.dat = readLines(file.cio)

file.cio.dat[59] = "               1    | IPRINT: print code (month, day, year)"

file.cio.dat[65] = "   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[67] = "   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[69] = "   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[71] = "   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
writeLines(file.cio.dat, file.cio)
###########
# format parameter file(s)


# List files with extension associated with parameter (e.g., bsn, gw)
# Grab original parameter value for each file, hold in memory
# Scale parameters according to range and save as a matrix
	# Need to do differently for each method (i.e., relative, absolute)



# for i in number of iterations
	# grab column with scaled values and replace input files with them
	par.ind = which(substr(basins.bsn, 23, 27) == param_name)
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




