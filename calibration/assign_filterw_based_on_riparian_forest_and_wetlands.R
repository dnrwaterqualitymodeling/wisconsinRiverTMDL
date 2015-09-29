txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut_cnop_adjust"
rip_lc = read.table(
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/land_cover_area_by_subbasin.txt",
	sep=",",
	header=T
)
col = "WETN_FRS"
rip_lc[col] = rowSums(rip_lc[c("WETN","FRSE","FRSD","FRST")]) /
	rowSums(rip_lc[,3:ncol(rip_lc)])
#txtinout = "D:/WRB_generalized/WRB.Sufi2.SwatCup/Backup"
#rip_lc = read.table(
#	"D:/area_forest_and_wetland_riparian.txt",
#	sep="\t",
#	header=T
#)
#col = "WETN"

reset = T

# Map the range of percent forest and wetland from 0 to 2 instead of 0 to 1
# This should inflate the effect when used as a proxy for filter strips
r = 2 / (max(rip_lc[[col]], na.rm=T) - min(rip_lc[[col]], na.rm=T))
y = (rip_lc[[col]] - min(rip_lc[[col]])) * r
rip_lc[col] = y

setwd(txtinout)
mgt_files = list.files(pattern="\\.mgt$")
for (s in 1:337) {
	s_mgt_files = mgt_files[which(as.integer(substr(mgt_files, 3, 5)) == s)]
	s_rip_fw = subset(rip_lc, SUBBASIN == s, select = col)	
	for (s_mgt_file in s_mgt_files) {
		print(s_mgt_file)
		mgt = readLines(s_mgt_file)
		if (reset) {
			val = 0
		} else if (dim(s_rip_fw)[1] == 0) {
			val = 1
		} else {
			val = s_rip_fw[1,1]
		}		
		substr(mgt[14], 1, 16) = sprintf("% 16.3f", val)
		writeLines(mgt, s_mgt_file)
	}
}

