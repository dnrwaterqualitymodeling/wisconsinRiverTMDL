# wd = "C:/Users/hydrig/Documents/Projects/WinSLAMM"

# dirs = list.files(wd, pattern="pUS*", full.names=T)

# for (dir in dirs){
	# csvs = list.files(dir, pattern="*.csv", full.names=T)
	# for (csv in csvs){
		# fl = read.csv(csv)
		# rowNum = nrow(fl)
		# fl =  
	# }
# }

import Subprocess
import os

cleanCSV = “clean_csv.r”
#You’ll want to check your version here, you’re maybe 3.3.1…
rscript = os.path.expanduser(“~”)+”\\R\\R-3.1.1\\bin\\Rscript.exe” 
cmd = rscript +” “ +cleanCSV
p = Subprocess.Popen(cmd)
p.wait()
