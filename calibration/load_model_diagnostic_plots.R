dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/Final Daily Loads"
lookup = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Chemistry/USGS_pollutant_load_estimates/StationID_StationName_lookup.txt"
dir_out = "C:/Users/ruesca/Desktop/wrb_swat_model_draft_release/plots"

stations=read.table(lookup, "\t",header=T)
include = rbind(
	c("SS_00530", 10012666),
	c("SS_00530", 573051),
	c("SS_00530", 723128),
	c("SS_00530", 10030199),
	c("SS_00530", 10012667),
	c("SS_00530", 503130),
	c("SS_00530", 10031106),
	c("SS_00530", 373183),
	c("SS_00530", 353068),
	c("SS_00530", 10017209),
	c("SS_00530", 573076),
	c("SS_00530", 293156),
	c("SS_00530", 10031103),
	c("TP_00665", 10012666),
	c("TP_00665", 5390680),
	c("TP_00665", 573051),
	c("TP_00665", 723128),
	c("TP_00665", 10030199),
	c("TP_00665", 10012667),
	c("TP_00665", 503130),
	c("TP_00665", 10031106),
	c("TP_00665", 373411),
	c("TP_00665", 373325),
	c("TP_00665", 373183),
	c("TP_00665", 10018128),
	c("TP_00665", 353068),
	c("TP_00665", 10017209),
	c("TP_00665", 353214),
	c("TP_00665", 573076),
	c("TP_00665", 293156),
	c("TP_00665", 10031103),
	c("TP_00665", 5392083),
	c("TP_00665", 353109),
	c("TP_00665", 373366)
)
include = data.frame(
	var_id = include[,1],
	station_id = as.character(include[,2])
)


out_pdf = paste(dir_out,"/load_model_diagnostic_plots.pdf", sep="")
pdf(out_pdf,width=6,height=6)
par(mar=c(5.1,5.1,4.1,2.1))
for (d in c("TP_00665", "SS_00530")) {	
	# create a variable that stores the directory to the output .gz files
	pol_dir = paste(dir, "/", d, "/Loads", sep="")
	pol_code = strsplit(d, "_")[[1]][2]
	# lists all the files in above directory 
	gz_files = list.files(pol_dir, pattern="\\.gz$", full.names=T) 
	
	for (f in gz_files) {
		print(f)
		code = strsplit(basename(f), "\\.")[[1]][1]
		incl_bool = any(include$var_id == d & include$station_id == code)
		if (!incl_bool) { next }
		
		loads=read.table(f, sep="\t", header=T)
		if (dim(loads)[2] < 5) { next }
		
		
		obs_col = paste("actual_", pol_code, sep="")
		obs_loads = loads[!is.na(loads[obs_col]),]
		max_val = max(c(obs_loads[,6], obs_loads[,5]), na.rm=T)
		min_val = min(c(obs_loads[,6], obs_loads[,5]), na.rm=T)
		plot(
			obs_loads[,6],
			obs_loads[,5],
			main=stations[stations[,1]==code,"station_name"],
			xlab=paste("Observed", d, "(kg)"),
			ylab=paste("Predicted", d, "(kg)"),
			xlim = c(min_val,max_val * 1.1),
			ylim = c(min_val,max_val * 1.1),
			pch=20
		)
		grid()
		# same with log transformation on pg 2 of pdf
		plot(
			obs_loads[,6],
			obs_loads[,5],
			log="xy",
			main=paste(stations[stations[,1]==code,"station_name"], "(log scale)"),
			xlab=paste("Observed", d, "(kg)"),
			ylab=paste("Predicted", d, "(kg)"),
			xlim = c(min_val, max_val * 1.1),
			ylim = c(min_val, max_val * 1.1),
			pch=20
		)
		grid(equilog=F)
		
		loads$date = as.Date(loads$date, format="%m/%d/%Y")
		first_day_i = min(which(!is.na(loads[,5])))
		last_day_i = max(which(!is.na(loads[,5])))
		inds = first_day_i:last_day_i
		loads = loads[inds,]

		max_val = max(c(loads[,6], loads[,5]), na.rm=T)
		min_val = min(c(loads[,6], loads[,5]), na.rm=T)
		plot(
			loads$date,
			loads[,6],
			type="l",
			col="grey",
			main=stations[stations[,1]==code,"station_name"],
			xlab="Date",
			ylab=paste(d, "(kg)"),
			ylim = c(min_val, max_val * 1.1),
		)
		points(
			loads$date,
			loads[,5],
			pch=20
		)
		grid()
		legend(
			"topright",
			c("Observed", "Predicted"),
			pch=c(20,NA),
			lty=c(NA,1),
			col=c("black","grey")
		)
		plot(
			loads$date,
			loads[,6],
			type="l",
			col="grey",
			log="xy",
			main=paste(stations[stations[,1]==code,"station_name"], "(log scale)"),
			xlab="Date",
			ylab=paste(d, "(kg)"),
			ylim = c(min_val, max_val * 1.1),
		)
		points(
			loads$date,
			loads[,5],
			pch=20
		)
		grid(equilog=F)
		legend(
			"topright",
			c("Observed", "Predicted"),
			pch=c(20,NA),
			lty=c(NA,1),
			col=c("black","grey")
		)
	}
}
dev.off()


