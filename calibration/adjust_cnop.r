library(RODBC)

#txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut"
txtinout = "C:/Users/ruesca/Desktop/mgt_files"
db_prj = "C:/Users/ruesca/Desktop/WRB/WRB.mdb"
temp_dir = tempdir()
p = "CNOP"
ext = "mgt"
#	scalar = 0.2
#	subbasins = c(1,2)
#	hydgrp = c("A", "B")

options(stringsAsFactors=F)

# matrix of begining and ending values for where values are in text files
place_vals = data.frame(
	file_ext = c('bsn','hru','gw','rte','mgt','pnd'),
	beginnings = c(9,8,8,7,12,5),
	endings = c(16,16,16,14,16,16))

# Delete outputs from txtinout if they exist
output.files = list.files(txtinout, pattern="^output", full.names=T)
unlink(output.files)

# Move txintout to a parameter-specific folder
if (!file.exists(temp_dir)){dir.create(temp_dir)}

td = paste(temp_dir, "/", p, "_", ext, sep="")
if (!file.exists(td)) {dir.create(td)} 

print("Beginning to copy files...")
# system(paste("cp -r", txtinout, td))
file.copy(
	list.files(txtinout, pattern="\\.mgt$", full.names=T),
	td,
	recursive=T
)
print("Copying complete.")

setwd(td)

# Get soil/HSG lu table

con = odbcConnectAccess(db_prj)
sol = unique(sqlQuery(con, "SELECT SOIL,HYDGRP FROM sol"))
close(con)


adjust_cnop = function(
	scalar=0.2,
	subbasins=c(1,2),
	hydgrp=c("A","B"),
	dir_out="C:/Users/ruesca/Desktop/CNOP_adjust") {
	
	# List files with extension associated with parameter (e.g., bsn, gw)
	## remember to delete outfiles !!!!
	ext.re = paste("\\.", ext, "$",sep='')
	p.files = list.files(pattern = ext.re)
	p.files = p.files[as.integer(substr(p.files, 1,5)) %in% subbasins]
	
	
	print("Grabbing original parameter values")
	# Grab original parameter value for each file, hold in memory
	p.file.list = list()
	for (p.file in p.files){
		pf = readLines(p.file, warn=F)
		soil = as.integer(strsplit(pf[1], ":|\\s+")[[1]][15])
		hydgrp_hru = subset(sol, SOIL == soil, select=HYDGRP)
		if (!(hydgrp_hru %in% hydgrp)) {next}
		p.file.list[[p.file]] = pf
	}
	
	for (operation in c("planting", "tillage")) {
		print(operation)
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
		print("Grabbing defaults")
		dflts = lapply(p.file.list,
			FUN=function(x, opnum){
				ln1 = strsplit(x[1], "\\s+|:")[[1]]
				ln1 = ln1[ln1 != ""]
				lu = ln1[11]
#				print(lu)
				if (lu == "WATR") {
					adj = "no_adj"
				} else {
					adj = "adj"
				}
				indx = which(substr(x, 17, 18) == opnum)
				vals = substr(x[indx], pstion[1], pstion[2])
				return(list(vals=vals, adj=adj))
			}, opnum=opnum
		)
		print("Applying scalar")
		p.mat.list = lapply(dflts,
			FUN=function(x){
				if (x$adj == "adj") {
					x = as.numeric(x$vals)
					p.adj = x * (1 + scalar)
					p.mat = t(p.adj)
				} else {
					p.mat = t(as.numeric(x$vals))
				}
				return(p.mat)
			}
		)
		print("format output")
		for (fl in 1:length(p.file.list)){
			p.mat = p.mat.list[[fl]]
			new.vals = p.mat
			new.vals[new.vals > 95] = 95
			new.vals = formatC(new.vals, digits=dgts, width=pstion[2]-pstion[1],format="f")
			indices = which(substr(p.file.list[[fl]], 17, 18) == opnum)
			for (indx in 1:length(indices)){
				substr(p.file.list[[fl]][indices[indx]], pstion[1], pstion[2]) = new.vals[indx]
			}
		}
	}
	print("writing files to temporary directory")
	for (fl in 1:length(p.file.list)) {
		writeLines(p.file.list[[fl]], names(p.file.list)[fl])
	}
	
	print("Copying *.mgt files to output directory")
	if (!file.exists(dir_out)) { dir.create(dir_out) }
	file.copy(
		paste(td, "/", names(p.file.list), sep=""),
		dir_out,
		recursive=T,
		overwrite=T
	)
}
