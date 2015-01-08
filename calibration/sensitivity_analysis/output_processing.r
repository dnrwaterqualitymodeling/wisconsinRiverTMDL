library(ncdf)
library(zoo)
library(RColorBrewer)
dir_out = "H:/WRB_sensitivity"
esco = open.ncdf(paste(dir_out, "/ESCO.nc", sep =''))

# creates a 3d array, 4383 x 338 x 25
flow = get.var.ncdf(esco, 'streamflow')

# grab just baraboo
baraboo = flow[,137,]

baraboo = zoo(baraboo, 
	seq(
		as.Date("2002-01-01"), 
		as.Date("2013-12-31"), 
		by = 'day')
)

pal = brewer.pal(5, 'YlOrRd')
for (i in 1:5){
	iter = seq(1,25,length.out=5)[i]
	colr = pal[i]
	plot(baraboo[,i], 
		type = 'l', 
		col = colr,
		main = i)
	dev.new()
}