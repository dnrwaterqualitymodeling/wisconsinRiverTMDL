
library(xts)
wd = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"

# list.files(wd, pattern = "^pUS[*]txt$") 

files_precip = list.files(wd, pattern = "^pUS")
files_temp = list.files(wd, pattern = "^tUS")
files_wind = list.files(wd, pattern = "wind_")
files_solar = list.files(wd, pattern = "solar_")
files_relhum = list.files(wd, pattern = "rel_hum_")


vars = list(
	c("Precip","^pUS", "mm"),
	c("Avg Temp", "^tUS", "deg C"),
	c("Wind", "wind_", "m/sec"),
	c("Solar radiation", "solar_", "MJ/M2/day"),
	c("Relativity Humidity", "rel_hum_", "")
)
mod_years = c(
	"2002",
	"2003",
	"2004",
	"2005",
	"2006",
	"2007",
	"2008",
	"2009",
	"2010",
	"2011",
	"2012",
	"2013")
tseries = seq(
	as.Date("1990-01-01", "%Y-%m-%d"),
	by='day',
	length.out = 8765)
# as.Date("1990-01-01", "%Y-%m-%d"),
			# as.Date("2013-12-31", "%Y-%m-%d")
for (v in vars){

	files_clim = list.files(wd, pattern = v[2])
	print(paste("Plotting", v[1]))
	pdf(paste(v[1], "_series.pdf",sep=''), width = 11, height = 8)
	for (fl in files_clim){
		
		dat = read.csv(paste(wd, fl,sep='/'),skip=1)
		dat = xts(
			dat, 
			tseries)
		dat[which(dat[,1] == -99),1] = NA 
		dat = dat["2002/"]
		if (v[1] == "Avg Temp") {
			dat[,1] = rowMeans(dat)
			dat = dat[,1]
		}
		plot(dat,
			main = paste(fl, "Entire Record"),
			sub = v[1],
			auto.grid = F,
			minor.ticks = F,
			ylab = v[3])
		for (yr in mod_years){
			dat_sb = dat[paste(yr)]
			if (nrow(dat_sb) == sum(is.na(dat_sb))){next}
			plot(dat_sb,
				main = yr,
				sub = paste(fl, v[1]),
				auto.grid = F,
				minor.ticks = F,
				ylab = v[3])
		}
	}
	dev.off()
}
		
		