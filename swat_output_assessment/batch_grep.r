
extr_subs_fun <- function(
	subbasins,
	src_folder,
	dst_folder,
	hru=T,
	rch=T,
	wtr=F,
	git_bin="C:\\Users\\ruesca\\AppData\\Local\\Programs\\Git\\bin"){

	### Function to extract specific subbassins from output.hru and output.rch
	### This function requires GIT
	### subbasins is a vector of subbasin ids
	### src_folder is where output.hru is found					#
	### dst_folder is where subbasin files are output			#
	### FULL PATH NAMES!! I'm not sure it will work with relative paths...
	tmplt = readLines("T:/Projects/Wisconsin_River/Code/swat_output_assessment/template_grep.txt")
	dflt = "C:/Users/ruesch/Desktop/WRB/Scenarios/Default/TxtInOut"
	tmpfiles = NULL
	for (sb in subbasins){
		print(sb)
		tmplt[1] = paste("SCEN=",dst_folder,sep="")#sca3_eff90_mx10_asq02
		tmplt[2] = paste("for SB in", sb)
		tmplt[19] = gsub(dflt, src_folder, tmplt[19])
		tmplt[22] = gsub(dflt, src_folder, tmplt[22])

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
		if (!wtr){
			tmplt[23] = paste("#", tmplt[23])
			tmplt[24] = paste("#", tmplt[24])
			tmplt[25] = paste("#", tmplt[25])
		}

		tmpf = tempfile(fileext=".sh")
		writeLines(tmplt, tmpf)
		tmpfiles = c(tmpfiles, tmpf)
	}

	cwd = getwd()
	setwd(git_bin)
	lnes = paste("cd", git_bin) 
	for (fl in 1:length(tmpfiles)){
		ln = paste("START", '"Batch GREP',fl,'"', "sh.exe", tmpfiles[fl])
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