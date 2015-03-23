library(ncdf)
# library(rgdal)
# library(raster)
library(RColorBrewer)
library(classInt)
# library(rgeos)
library(zoo)

options(stringsAsFactors = FALSE)

# setwd("H:/WRB_sensitivity")
# only interested in streamflow currently...
vars = list(
	c("streamflow_out", "Annual Average streamflow (cms)")#,
	# c("sediment", "Average Daily Sediment Load (tons)"),
	# c("phosphorus", "Average Daily P Load (kg)")
)

pal = brewer.pal(6, 'Set1')
pal = paste(pal, "80",sep='')
files_nc = list.files("H:/WRB_sensitivity", "*.nc")

for (fl in files_nc){
	for (i in 1:6){
		var = vars[[i]][1]
		nc_file = paste("H:/WRB_sensitivity/", fl, sep='')
		# nc_file= paste("H:/WRB_sensitivity/CH_N2_rte.nc")
		nc = open.ncdf(nc_file)
		
		param = strsplit(nc_file, "\\.")[[1]][1]
		param = basename(param)
		d = get.var.ncdf(nc, varid=var, start=c(1,1,1), count=c(-1,-1,-1))
		pdf(paste(param,"_",var,".pdf",sep=""),width=11,height=6)
		print(paste("Beginning to plot", param))
		print("")
		print("###################################")
		pb = txtProgressBar(0,1)
		for (sub in 1:337){
			setTxtProgressBar(pb, sub/337)
			# print(paste("working on subbasin",sub))
			sb = d[,sub,]
			# file_name = paste(param,"subbasin",sub,sep='_')
			# pdf(paste(file_name, "_entire_record.pdf",sep = ''))
			for (i in 1:6){
				iter = c(1,5,10,15,20,25)[i]
				iter = sb[,iter]
				
				iter = zoo(iter, 
					seq(
						as.Date("2002-01-01"), 
						as.Date("2013-12-31"), 
						by = 'day'))
				colr = pal[i]
				if (i == 1){
					plot(iter,
						type='l',
						col=colr,
						ylab=vars[[i]][2],
						xlab='Date',
						main=paste("Subbasin", sub,"Entire Record"))
				} else {
					lines(iter,
						col=colr)
				}
				legend('topright',
					legend=c(
						"Iteration 1",
						"Iteration 5",
						"Iteration 10",
						"Iteration 15",
						"Iteration 20",
						"Iteration 25"),
					fill=pal)
			}
			# dev.off()

			strting = seq(from=as.Date("2002-01-01"),
				by='year',
				length.out=12)

			# pdf(paste(file_name, "_yearly.pdf", sep = ''))
			for (yr in strting){
				
				time_period = seq(from=as.Date(yr), by='day',length.out=365)
				for (i in 1:6){
					iter = c(1,5,10,15,20,25)[i]
					iter = sb[,iter]
					iter = iter = zoo(iter, 
						seq(as.Date("2002-01-01"), 
							as.Date("2013-12-31"), 
							by = 'day'))
					
					colr = pal[i]
					if (i == 1){
						plot(iter[time_period],
							type='l',
							col=colr,
							ylab=vars[[i]][2],
							xlab='Date',
							main=paste("Subbasin", sub,":", format(as.Date(yr),'%Y')))
					} else {
						lines(iter[time_period],
							col=colr)
					}
					legend('topright',
						legend=c(
							"Iteration 1",
							"Iteration 5",
							"Iteration 10",
							"Iteration 15",
							"Iteration 20",
							"Iteration 25"),
					fill=pal)
				}
			}
			# dev.off()
		}
		dev.off()
	}
}








# to assess how to classify the data
# pal.brew = brewer.pal(9,"RdYlGn")

# toClass = all_params[v[1]]
# toClass = as.matrix(toClass)

# reduced_toClass = sample(toClass, nrow(toClass)*0.4)
# classification methods: 
# class_methods = c("sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks") # skipping "fixed", 
# pdf("potential_classification_methods_streamflow.pdf")
# for (mth in class_methods){
	# print(paste("working on ",mth))
	# prop_int = classIntervals(reduced_toClass, 9, mth)
	# plot(prop_int, pal.brew,main=mth)
# }
# dev.off()




















