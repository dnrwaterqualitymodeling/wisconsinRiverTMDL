
cols = c("Month","Year","Flomon","Sedmon","Orgnmon","Orgpmon","No3mon","Nh3mon","No2mon","Minpmon","Cbodmon","Disoxmon","Chlamon","Solpstmon","Srbpstmon","Bactpmon","Bactlpmon","Cmtl1mon","Cmtl2mon","Cmtl3mon")

ts = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 month")

df = matrix(0, ncol = length(cols), nrow=length(ts))
df = as.data.frame(df)
names(df) = cols

df$Month = as.integer(format(ts, "%m"))
df$Year = as.integer(format(ts, "%Y"))

df$Flomon = rnorm(length(ts), 0, 1) + 10
df$Sedmon = rnorm(length(ts), 0, 1) + 10
df$Orgpmon = rnorm(length(ts), 0, 1) + 10
df$Minpmon = rnorm(length(ts), 0, 1) + 10

write.csv(df,
	"T:/Projects/Wisconsin_River/GIS_Datasets/Mill_Creek_Betatest/pointSources/ps1.txt",
	row.names=F)
write.csv(df,
	"T:/Projects/Wisconsin_River/GIS_Datasets/Mill_Creek_Betatest/pointSources/ps2.txt",
	row.names=F)

 