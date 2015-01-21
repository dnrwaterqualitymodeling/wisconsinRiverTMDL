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
	c("streamflow", "Annual Average streamflow (cms)")#,
	# c("sediment", "Average Daily Sediment Load (tons)"),
	# c("phosphorus", "Average Daily P Load (kg)")
)
pal = brewer.pal(2, 'Set1')
files_nc = list.files("H:/netCDF_files", "*.nc")

for (fl in files_nc){
	nc_file = paste("H:/netCDF_files/", fl, sep='')
	nc_file = "D:/WRB_sensitivity/SURLAG_bsn.nc"
	nc = open.ncdf(nc_file)
	param = strsplit(nc_file, "\\.")[[1]][1]
	param = basename(param)
	d = get.var.ncdf(nc, varid="streamflow", start=c(1,1,1), count=c(-1,-1,-1))
	pdf(paste("T:/Projects/Wisconsin_River/Model_Documents/TMDL_report/",param, "_entire_record.pdf",sep = ''))
	print(paste("Beginning to plot", param))
	print("")
	print("###################################")
	pb = txtProgressBar(0,1)
	for (sub in 1:338){
		setTxtProgressBar(pb, sub/338)
		print(paste("working on subbasin",sub))
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
					ylab='cumecs',
					xlab='time',
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
						ylab='cumecs',
						xlab='time',
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







sb_region_lu = read.delim(file_subbasin_region_lu)
out_subbasin_files = list.files("H:/sensitivity_text_files",pattern="^subbasin*",full.name=T)
out_regional_file = "H:/sensitivity_text_files/regional_sensitivity.txt"

all_params = NULL
for (sub_file in out_subbasin_files) {
	param = substr(basename(sub_file), 10, 20)

	sb_fl = read.delim(sub_file)
	sb_fl['param_name'] = param
	all_params = rbind(all_params, sb_fl)
}
# for easy merging
names(all_params)[1] = "Subbasin"
pal.brew = brewer.pal(9,"RdYlGn")

for (v in vars){
	toClass = all_params[v[1]]
	toClass = as.matrix(toClass)
	prop_int = classIntervals(toClass, 9, 'kmeans')
#     prop_colr = findColours(prop_int, pal)
    brks <- prop_int$brks
	pdf(paste(v[1],'_subbasin_maps.pdf',sep=''))
    for (param in unique(all_params$param_name)){
		print(paste("Working on ",param))
		ind_param = which(all_params$param_name == param)
		param_df = all_params[ind_param,]
		param_subbasins = merge(subbasins, param_df)

		plot(subbasins,
			col=pal.brew[findInterval(toClass[ind_param,v[1]], 
                brks,
                all.inside = T)],
			main = paste(param,v[1]),
			border = NA)

        txt <- properLegend(findColours(prop_int, pal.brew), sig_figs=4)
        legend('bottomright', 
			legend = txt, 
			bg = 'white', 
			bty = 'n',
            fill = pal.brew,
            title = paste(param))
	}
	dev.off()
}

#########################################################################
#### For regional and global plotting ###################################
#########################################################################
out_regional = read.delim(out_regional_file)
names(out_regional)[1] = "Name"
for (v in vars){
	df_regional = subset(out_regional, Variable == v[1])
	toClass = as.matrix(df_regional[,4:5])
	prop_int_mean = classIntervals(toClass[,1], 9, 'kmeans')
	prop_int_sd = classIntervals(toClass[,2], 9, 'kmeans')
	
	brks_mean <- prop_int_mean$brks
	brks_sd = prop_int_sd$brks
	
	stat_list = list(
		list("delta_mean", prop_int_mean, brks_mean),
		list("delta_sd", prop_int_sd, brks_sd))
	
	pdf(paste(v,"_testHists.pdf",sep=''),width=11,height=8)
	for (param in unique(df_regional$Parameter)){
		print(paste("Working on", param))
		ind_param = which(df_regional$Parameter == param)
		df_param = df_regional[ind_param,]
		
		param_regions = merge(regions, df_param)
		par(mfrow=c(1,2))
		for (stat in stat_list){
			hist(param_regions@data$delta_mean, main = paste(param, stat[[1]]))

			# plot(regions,
				# col=pal.brew[findInterval(toClass[ind_param, stat[[1]]], 
					# stat[[3]],
					# all.inside = T)],
				# main = paste(param,v[1], stat[[1]]),
				# border = NA)
			# txt <- properLegend(findColours(stat[[2]], pal.brew), sig_figs=4)
			# legend('bottomright', 
				# legend = txt, 
				# bg = 'white', 
				# bty = 'n',
				# fill = pal.brew,
				# title = paste(param, stat[[1]]))
		}
	}
	dev.off()
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




















