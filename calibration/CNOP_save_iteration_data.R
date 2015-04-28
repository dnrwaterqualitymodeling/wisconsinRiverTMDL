library(ncdf)
txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/gw_and_runoff_calibrated"
dir_out = "C:/Users/ruesca/Desktop/WRB_sensitivity_CNOP"
temp_dir = "C:/Users/ruesca/Desktop/temp_directory"
p = "CNOP"
ext = "mgt"
mn = -0.1
mx = 0.1
method = "r"
iter = 50
 
logfile = paste(dir_out, '/',p,'.log',sep='')
write(
	paste("Log file for", p),
	logfile
)

# Move txintout to a parameter-specific folder
if (!file.exists(temp_dir)){dir.create(temp_dir)}

td = paste(temp_dir, "/", p, "_", ext, sep="")
if (!file.exists(td)) {dir.create(td)} 
wd = paste(td, basename(txtinout), sep="/")
print("Beginning to copy files...")
# system(paste("cp -r", txtinout, td))
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
file.cio.dat[67] = "   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
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
#	if (ext == 'mgt'){
#		strting = regexpr("Luse", pf[1])[1]
#		strting = strting + 5
#		luse=substr(pf[1], strting, strting+3)
#		if (!luse %in% ag_codes){next}
#	}
	p.file.list[[p.file]] = pf
}
#### processing for CNOP. Ok. 
op_types = list(
	c('planting', " 1", 76, 80),
	c('tillage', " 6", 32, 43)
)
dgts = 2
frmting = "%2.2f"

dflts = list()
p.mat.list = list()
for (op_type in op_types){
	dflts[[op_type[1]]] = lapply(
		p.file.list,
		FUN=function(x, op_type){
			indx = which(substr(x, 17, 18) == op_type[2])
			vals = substr(x[indx], op_type[3], op_type[4])
			return(vals)
		}, op_type=op_type
	)
	p.mat.list[[op_type[1]]] = lapply(
		dflts[[op_type[1]]],
		FUN=function(x){
			x = as.numeric(x)
			p.mn = x * (1 + mn)
			p.mx = x * (1 + mx)
			p.rg = cbind(p.mn, p.mx)
			p.mat = apply(p.rg, 1, function(x,iter) {seq(x[1], x[2], length.out=iter)}, iter=iter)
			p.mat = t(p.mat)
		}
	)
}

dimI = dim.def.ncdf( "iteration", "unitless", 1:iter)
dimS = dim.def.ncdf( "subbasin", "ID", 1:337)
dimT = dim.def.ncdf( "Time", "days since 2001-12-31", 1:4383)

# Make varables of various dimensionality, for illustration purposes
mv = 1.e30 # missing value to use

q.var = var.def.ncdf("streamflow", "cms", list(dimT,dimS,dimI), mv)
s.var = var.def.ncdf("sediment", "metric tons", list(dimT,dimS,dimI), mv)
p.var = var.def.ncdf("phosphorus", "kilograms", list(dimT,dimS,dimI), mv)

var_list = list(q.var, s.var, p.var)

output.file = "output.rch"
first_col = "REACH" 
col_nums = c(1,5:7)
col_names = c("sub", "flow_out", "sed", "totpkg")

nc = create.ncdf(
	paste(dir_out, "/", p, "_", ext, ".nc", sep=""),
	var_list)

# For each iteration, rewrite all necessary files,
#	then run swat, and collect the data from each run.
for (i in 1:iter){
	# rewrite input files with new params
	for (fl in 1:length(p.file.list)){	
#		print(paste("Hey its running file", names(p.file.list)[fl]))
		for (op_type in op_types){
			p.mat = p.mat.list[[op_type[1]]][[fl]]
			if (length(p.mat) == 0) { next }
			new.vals = p.mat[,i]
			new.vals[new.vals>100] = 100
			new.vals = formatC(
				new.vals,
				digits=dgts,
				width=as.integer(op_type[4])-as.integer(op_type[3]),
				format="f")
			indices = which(substr(p.file.list[[fl]], 17, 18) == op_type[2])
			for (indx in 1:length(indices)){
				substr(
					p.file.list[[fl]][indices[indx]],
					as.integer(op_type[3]),
					as.integer(op_type[4])) = new.vals[indx]
			}
		}
		writeLines(p.file.list[[fl]], names(p.file.list)[fl])
	}
	bat = tempfile(pattern="runswat_", fileext=".bat")
	writeLines(paste("cd ", wd, "\nswat.exe", sep=""), bat) 
	system2(bat)
	print("Processing SWAT output...")
	dat = readLines(paste(wd, output.file, sep="/"))
	dat = dat[10:length(dat)]
	dat = gsub("\\s+", ",", dat)
	dat = gsub(paste(first_col,",",sep=''), "", dat)
	dat = strsplit(dat, ",")
	nrows = length(dat)
	ncols = length(dat[[1]])
	dat = unlist(dat)
	dat = matrix(
			dat,
			nrow=nrows,
			ncol=ncols,
			byrow=T)
	dat = apply(dat, 2, as.numeric)
	dat = subset(dat, select=col_nums)
	colnames(dat) = col_names
	
	dat = dat[order(dat[,1]),]
	
	q = matrix(
		dat[,"flow_out"], 
		nrow=4383, 
		ncol=337)
	sed = matrix(
		dat[,"sed"], 
		nrow=4383, 
		ncol=337)
	pho = matrix(
		dat[,"totpkg"],
		nrow=4383,
		ncol=337)
	dim(q) = c(4383, 337, 1)
	dim(sed) = c(4383, 337, 1)
	dim(pho) = c(4383, 337, 1)
	print("Writing output to netCDF...")
	put.var.ncdf(nc, q.var, q, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, s.var, sed, start=c(1,1,i), count=c(-1,-1,1))
	put.var.ncdf(nc, p.var, pho, start=c(1,1,i), count=c(-1,-1,1))
	
	write(
		paste("Completed iteration", i, "at", Sys.time()),
		logfile,
		append=T)
}
close.ncdf(nc)


