library(ncdf)
library(zoo)
library(RColorBrewer)
dir_out = "H:/WRB_sensitivity"
timp = open.ncdf(paste(dir_out, "/TIMP.nc", sep =''))

# creates a 3d array, 4383 x 338 x 25
flow = get.var.ncdf(timp, 'streamflow')

# grab just baraboo
baraboo = flow[,1,]

baraboo = zoo(baraboo, 
	seq(
		as.Date("2002-01-01"), 
		as.Date("2013-12-31"), 
		by = 'day')
)
iters = NULL
pal = brewer.pal(5, 'Dark2')
for (i in 1:5){
	iter = seq(1,25,length.out=5)[i]
	colr = pal[i]
	if (i == 1){
		plot(baraboo[,i], 
			type = 'l', 
			col = colr)
	} else {
		lines(baraboo[,i],
			col = colr)
	}
	iters = c(iters, iter)
}
legend('topright',
		fill = pal,
		legend = iters)
