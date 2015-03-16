<<<<<<< HEAD
extr_subs_fun <- function(subbasins, src_folder, dst_folder, git_bin="C:\\Users\\evansdm\\AppData\\Local\\Programs\\Git\\bin"){
=======
extr_subs_fun <- function(
	subbasins,
	src_folder,
	dst_folder,
	hru=T,
	rch=T,
	git_bin="C:\\Users\\evansdm\\AppData\\Local\\Programs\\Git\\bin"){
>>>>>>> 7d24adf0a78a22ccf0fc0935890e7afac9525984
	### Function to extract specific subbassins from output.hru and output.rch
	### This function requires GIT
	### subbasins is a vector of subbasin ids
	### src_folder is where output.hru is found					#
	### dst_folder is where subbasin files are output			#
	### FULL PATH NAMES!! I'm not sure it will work with relative paths...
	tmplt = readLines("~/Code/swat_output_assessment/template_grep.txt")
	dflt = "H:/WRB/Scenarios/Default/TxtInOut"
	tmpfiles = NULL
	for (sb in subbasins){
		print(sb)
		tmplt[1] = paste("SCEN=",dst_folder,sep="")#sca3_eff90_mx10_asq02
		tmplt[2] = paste("for SB in", sb)
		tmplt[19] = gsub(dflt, src_folder, tmplt[19])
		tmplt[22] = gsub(dflt, src_folder, tmplt[22])
<<<<<<< HEAD
=======
		if (!hru){
			tmplt[17] = paste("#", tmplt[17])
			tmplt[18] = paste("#", tmplt[18])
			tmplt[19] = paste("#", tmplt[19])
		}
		if (!rch){
			tmplt[20] = paste("#", tmplt[20])
			tmplt[21] = paste("#", tmplt[21])
			tmplt[22] = paste("#", tmplt[22])
		}
>>>>>>> 7d24adf0a78a22ccf0fc0935890e7afac9525984
		tmpf = tempfile(fileext=".sh")
		writeLines(tmplt, tmpf)
		tmpfiles = c(tmpfiles, tmpf)
	}
<<<<<<< HEAD
	# git_bin = "~\\AppData\\Local\\Programs\\Git\\bin"
	# git_bin = path.expand(git_bin)
=======

>>>>>>> 7d24adf0a78a22ccf0fc0935890e7afac9525984
	cwd = getwd()
	setwd(git_bin)
	lnes = paste("cd", git_bin) 
	for (fl in 1:length(tmpfiles)){
		ln = paste("START", '"',fl,'"', "sh.exe", tmpfiles[fl])
		lnes = c(lnes, ln)
	}

	tmpf_ctrl = tempfile("ctrl",fileext=".bat")

	writeLines(
		paste(lnes,sep="\n"),
		tmpf_ctrl
	)
	system2(tmpf_ctrl)
	setwd(cwd)
}