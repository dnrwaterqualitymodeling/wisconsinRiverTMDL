
library(xts)
wd = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"

# first line
first_line = "19900101"
solar_han = read.csv(paste(wd, "solar_han.txt",sep='/'),skip =1)
solar_spg = read.csv(paste(wd, "solar_spg.txt",sep='/'),skip =1)

# setting early 2004 and an anomalous value in 2007 in hancock to missing 
solar_han[5119:5132,1] = -99
solar_han[6435,1] = -99


# setting anamolous values in spring green 2005 to missing
solar_spg[which(solar_spg[,1] > 35),1] = -99


write(first_line,
	"solar_han.txt")
write(
	as.matrix(solar_han),
	"solar_han.txt",
	ncolumns=1,
	append = TRUE)

write(first_line,
	"solar_spg.txt")
write(
	as.matrix(solar_spg),
	"solar_spg.txt",
	ncolumns=1,
	append = TRUE)