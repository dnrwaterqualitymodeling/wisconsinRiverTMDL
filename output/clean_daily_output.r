library(RSQLite)

file_db = "C:/Users/ruesca/Desktop/test_wrb_db.sqlite"
dir_swat = "C:/TEMP/swat_daily_output"
exe_sh = "C:/Users/ruesca/AppData/Local/Programs/Git/bin/sh.exe"


# Reformat SWAT output tables as tab-delimited
td = tempdir()
td = gsub("\\\\", "/", td)
for (of in c("output.rch", "output.sub", "output.rsv", "output.hru")) {
	tmp_out = paste(td, "/")
	sh_script = paste(
		"cd ", dir_swat, "\n",
		"sed s/\\s\\+/\\t/g <output.rch >"
	)
	file_grep = tempfile(fileext=".sh")
}
setwd(dir_swat)

db = dbConnect(SQLite(), file_db)
dbWriteTable(
	db,
	"test",
	"output.rsv",
	skip=8
)

rch = read.table("output.rch", skip=9, header=F)


hru = readLines("output.hru")


