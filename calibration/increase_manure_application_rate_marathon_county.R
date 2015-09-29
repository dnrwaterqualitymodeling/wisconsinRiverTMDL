#txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut_cnop_adjust"
txtinout = "C:/Users/ruesca/Desktop/manure_increase"
dir_out = "C:/Users/ruesca/Desktop/manure_decrease"
eco_sub = read.table(
	"T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/hydro/ab_hsg_subbasins_in_forest_transition.txt",
	header=T
)
scalar = 0.4

eco_sub = subset(eco_sub, maj_hsg %in% c("C","D"))
ecos = eco_sub$subbasin

setwd(txtinout)

fs = list.files(pattern="\\.mgt$")

fs = fs[substr(fs, 1, 5) %in% sprintf("%05.f", ecos)]

for (f in fs) {
	print(f)
	mgt = readLines(f)
	l = 30
	repeat {
		l = l + 1
		if (l > length(mgt)) { break }
		if (substr(mgt[l], 17, 18) == " 3" & substr(mgt[l], 22, 23) == "44") {
			file_new = paste(dir_out, f, sep="/")
			orig = as.numeric(substr(mgt[l], 34, 43))
			new = sprintf("%9.5f", orig * scalar)
			substr(mgt[l], 34, 43) = new
			writeLines(mgt, file_new)
		} else {
			next
		}
	}
}