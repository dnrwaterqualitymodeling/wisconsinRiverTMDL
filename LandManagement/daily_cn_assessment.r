library(ggplot2)
library(reshape)
options(stringsAsFactors=F)

dir_txtinout = "H:/WRB/Scenarios/Default/TxtInOut"
file_output.hru = paste(dir_txtinout, "output.hru", sep="/")

colm.names = c("LULC", "HRU", "subHRU", "SUBBASIN", "mgt", "MON", "AREA", "PRECIP", "DAILYCN", "SURQ_GEN")
wdths = c(4, 5, 10, 5, 5, 5, 10, 10, 10, 10)

output.hru = read.fwf(
	file_output.hru,
	widths=wdths,
	skip=9,
	col.names=colm.names
)

mod_per = data.frame(
	DATE = seq(
		as.Date("2002-01-01", "%Y-%m-%d"),
		as.Date("2013-12-31", "%Y-%m-%d"),
		by = 'day'
		),
	HRU = rep(c(853, 1411, 1419), each = 4383))

dates = seq(
	as.Date("2002-01-01"),
	as.Date("2013-12-31"),
	by = '1 day')

col_mat = matrix(c(
	"CORN", "#7fc97f80",
	"SOYB", "#beaed480",
	"CSIL", "#fdc08680",
	"ALFA", "#ffff9980"),
	4,2,byrow=T)
	
df_lndcvr = data.frame(
	hru = c(
		853,
		1411,
		1419),
	lndcode = c(
		"COTS, No Till",
		"BBLS, Dairy",
		"FPEA, Cash Grain"))

tillage_dates = list(
		c(as.Date("2002-05-01"),as.Date("2003-05-01"),as.Date("2005-05-01"),as.Date("2007-05-01"),
			as.Date("2008-05-01"),as.Date("2009-05-01"),as.Date("2011-05-01"),as.Date("2013-05-01")),
		c(as.Date("2002-05-01"),as.Date("2002-10-18"),as.Date("2003-05-01"),as.Date("2003-10-18"),as.Date("2004-04-17"),as.Date("2007-10-15"),
			as.Date("2008-05-01"),as.Date("2008-10-18"),as.Date("2009-05-01"),as.Date("2009-10-18"),as.Date("2010-04-17"),as.Date("2013-10-15")),
		c(as.Date("2002-05-01"),as.Date("2002-11-20"),as.Date("2003-05-01"),as.Date("2003-11-01"),as.Date("2004-05-01"),as.Date("2003-11-20"),
			as.Date("2005-05-01"),as.Date("2005-11-01"),as.Date("2006-05-01"),as.Date("2006-11-20"),as.Date("2007-05-01"),as.Date("2007-11-01"),
			as.Date("2008-05-01"),as.Date("2008-11-20"),as.Date("2009-05-01"),as.Date("2009-11-01"),as.Date("2010-05-01"),as.Date("2010-11-20"),
			as.Date("2011-05-01"),as.Date("2011-11-01"),as.Date("2012-05-01"),as.Date("2012-11-20"),as.Date("2013-05-01"),as.Date("2013-11-01"))
		)
names(tillage_dates) = c(853,1411,1419)

for (hru in unique(output.hru$HRU)) {
	file_pdf = paste("cn_hru_", hru, ".pdf", sep="")
	pdf(file_pdf, height=8.5, width=11)
	d = subset(output.hru, HRU == hru)
	d$date = dates
	
	lndcvr_class = df_lndcvr[which(df_lndcvr[,1] == hru),2]
	plot(DAILYCN ~ date,
		data=d,
		type="l",
		main=paste("Landcover Class:", lndcvr_class),
		sub=paste("HRU:", hru))
	abline(v=tillage_dates[[as.character(hru)]], col="#BEBEBE80")
	for (lu in unique(d$LULC)){
		colr = col_mat[which(col_mat[,1] == lu),2]
		d_sbst = subset(d, LULC == lu)
		d_sbst = merge(d_sbst, data.frame(date = dates), all.x = T, all.y=T)
		lines(DAILYCN ~ date,
			data=d_sbst,
			type="l",
			col=colr,
			lwd=5)
	}
	legend(
		"bottomleft",
		legend=col_mat[,1],
		fill=col_mat[,2],
		bty='white',
		bg='white'
		)
	####
	# precip
	par(new=TRUE)
	##layout(matrix(c(1,2,3,4), 2,2,byrow=T))
	plot(
		PRECIP~date,
		axes=F,
		data=d,
		col="#386cb080",
		type="l",
		ylab='',
		xlab='',
		lwd = 2
	)
	axis(4,col="#386cb080",line=-1)
	
	## yearly
	for (yr in 2002:2013) {
		d_yr = subset(d, format(date, "%Y") == yr)
		plot(DAILYCN ~ date,
			data=d_yr,
			type="l",
			main=paste("Landcover Class:", lndcvr_class),
			sub = paste("HRU:",hru, ";",yr))
		for (lu in unique(d_yr$LULC)){
			colr = col_mat[which(col_mat[,1] == lu),2]
			d_yr_sbst = subset(d_yr, LULC == lu)
			d_yr_sbst = merge(d_yr_sbst, data.frame(date = dates), all.x = T, all.y=T)
			lines(DAILYCN ~ date,
				data=d_yr_sbst,
				type="l",
				col=colr,
				lwd=5)
			abline(v=tillage_dates[[as.character(hru)]], col="#BEBEBE80")
			legend(
				"bottomleft",
				legend=col_mat[,1],
				fill=col_mat[,2],
				bty='white',
				bg='white'
			)
		}
		# precip
		par(new=TRUE)
		##layout(matrix(c(1,2,3,4), 2,2,byrow=T))
		plot(
			PRECIP~date,
			data=d_yr,
			axes=F,
			col="#386cb099",
			type="l",
			ylab='',
			xlab='',
			lwd = 2
		)
		axis(4,col="#386cb080",line=-1)
	}
	dev.off()
}














# colmn_names = output.hru[9]
# output.hru = output.hru[10:length(output.hru)]
# output.hru = gsub("\\s+", ",", output.hru)
# output.hru = gsub("REACH,", "", output.hru)
# output.hru = strsplit(output.hru, ",")
# dealing with the separation betwixt area and MON
# output.hru = lapply(output.hru, function(x){
	# area = unlist(strsplit(x[6], ".", fixed=T))[2]
	# area = paste(".", area, sep='')
	# x[6] = area
	# return(x)
	# }
# )
# nrows = length(output.hru)
# ncols = length(output.hru[[1]])
# output.hru = unlist(output.hru)
# output.hru = matrix(output.hru, nrow=nrows, ncol=ncols, byrow=T)
# output.hru = cbind(output.hru[,1], as.data.frame(apply(output.hru[,2:ncol(output.hru)], 2, as.numeric)))

# colors = as.character(d$LULC)
	# colors = replace(colors, colors == "CORN", "#7fc97f80")
	# colors = replace(colors, colors == "SOYB", "#beaed480")
	# colors = replace(colors, colors == "CSIL", "#fdc08680")
	# colors = replace(colors, colors == "ALFA", "#ffff9980")

	# barplot(rep(100, length(dates)),
		# space=0,
		# col=colors,
		# border=NA)