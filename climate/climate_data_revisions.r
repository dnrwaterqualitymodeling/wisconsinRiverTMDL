
wd = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"

# first line
first_line = "19900101"
file_han = paste(wd, "solar_han.txt",sep='/')
file_spg =  paste(wd, "solar_spg.txt",sep='/')

solar_han = readLines(file_han, skip=0)
solar_spg = readLines(file_spg, skip=0)

# setting early 2004 and an anomalous value in 2007 in hancock to missing 
solar_han[5121:5131] = "-99"
solar_han[6436] = "-99"


# setting anamolous values in spring green 2005 to missing
solar_spg[5604:5625] = "-99"


write(first_line,
	file_han)
write(
	as.matrix(solar_han),
	file_han,
	ncolumns=1,
	append = TRUE)

write(first_line,
	file_spg)
write(
	as.matrix(solar_spg),
	file_spg,
	ncolumns=1,
	append = TRUE)