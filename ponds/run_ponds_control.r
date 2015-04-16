
user_path = path.expand("~")
rscript = paste(user_path, "R\\R-3.1.1\\bin\\x64\\Rscript.exe",sep="\\")
pndsscrpt = paste(user_path, "Code\\ponds\\calculatePondParameters.r",sep="\\")
pnds_cleanup = paste(user_path, "Code\\ponds\\run_ponds_cleanup.r",sep="\\")

wd <- "T:/Projects/Wisconsin_River/GIS_Datasets/ponds"
gd_dir <- "T:/Projects/Wisconsin_River/GIS_Datasets"
#### Number of processes to start
procs = 4

strt_stps = round(seq(1,337+337/procs,by=337/procs))
cmd = NULL
for (i in 1:(length(strt_stps)-1)){
	strt = strt_stps[i]
	stp = strt_stps[i+1]-1
	ln = paste(paste("START",'"',paste("POND", strt),'"', rscript, pndsscrpt, strt, stp))
	cmd = c(cmd, ln)
	print(ln)
}

tmpf = tempfile(fileext=".bat")
writeLines(
	paste(cmd, collapse="\n"),
	tmpf
)
system(tmpf)

cmd_clean = paste(rscript, pnds_cleanup)

run.clean.up = F
while (!run.clean.up) {
	ps = grep("POND", system('tasklist /v', intern=TRUE), value=TRUE)
	if (length(ps) < 1) {
		run.clean.up = T
		system(cmd_clean)
	} else {
		Sys.sleep(1)
		run.clean.up = F
	}
}