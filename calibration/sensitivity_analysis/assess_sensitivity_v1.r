options(stringsAsFactors=F)
library(ggplot2)
library(Hmisc)

dir_proj = "H:/WRB_sensitivity"
dir_sens = "T:/Projects/Wisconsin_River/GIS_Datasets/Sensitivity_Analysis"
file_reg_sens = paste(dir_sens, "regional_sensitivity.txt", sep="/")

dat = read.delim(file_reg_sens)
dat = subset(dat, region == "Global" & value > 0)

dat = subset(dat,  grepl("_annual_mean", variable) | grepl("_spring_mean", variable))
dat = subset(dat, !(grepl("org_p" , variable) | grepl("sed_p" , variable) | grepl("sol_p" , variable)))

dat$relative_rank = NA
for (var in unique(dat$variable)){
	indx = which(dat$variable == var)
	dat$relative_rank[indx] = dat$value[indx]/max(dat$value[indx])
}

dat$variable = capitalize(gsub("_", " ", dat$variable))

png("C:/Users/evansdm/Documents/miscellaneous/sensitivity_fig_for_symp.png",res=900,units='in',height=11,width=8)
ggplt = ggplot(dat, aes(x=reorder(parameter, relative_rank), y=relative_rank, fill = variable))
ggplt = ggplt + geom_bar(stat="identity", position = "dodge") + coord_flip() + theme_bw() + ggtitle("Global Sensitivity")#+ facet_grid(. ~ var)
ggplt = ggplt + theme(
	axis.text.x=element_text(size=15),
	axis.text.y=element_text(size=13, angle = 40),
	axis.title.x=element_text(size=20),
	axis.title.y=element_text(size=20),
	plot.title=element_text(size=22),
	legend.title=element_text(size=15),
	legend.text=element_text(size=12)) + 

	scale_fill_manual(name="Variable", values=c("#a6cee3", "#1f78b4","#fdbf6f","#ff7f00","#fb9a99", "#e31a1c")) +
	labs(y="Relative Rank", x="Parameter")
plot(ggplt)
dev.off()

# leaving out of the figure
# exclde = c("CHD_rte", "CHW2_rte", "HRU_SLP_hru", "SLSOIL_hru", "ADJ_PKR_bsn", "FFCB_bsn")
exclde = NULL
# var = c("streamflow_annual_mean", "phosphorus_annual_mean", "sediment_annual_mean", "streamflow_spring_mean", "phosphorus_spring_mean", "sediment_spring_mean")
# var_dat = subset(dat, (variable %in% var & region == "Global"))
# var_dat = subset(var_dat, value != 0)
# var_dat = subset(var_dat, !(parameter %in% exclde))

all_var = data.frame()
for (var in vars){
	tm_priod = paste(var, c("_annual_mean", "_spring_mean"),sep='')
	ann = subset(dat, (Variable %in% tm_priod[1] & Region == "Global"))
	spr = subset(dat, (Variable %in% tm_priod[2] & Region == "Global"))
	ann$rel_delta_mean = ann$delta_mean / max(ann$delta_mean)
	spr$rel_delta_mean = spr$delta_mean / max(spr$delta_mean)
	var_dat = rbind(ann, spr)
	
	# var_dat = subset(dat, (variable %in% tm_priod & region == "Global"))
	var_dat = subset(var_dat, delta_mean != 0)
	var_dat = subset(var_dat, !(Parameter %in% exclde))
	tming = strsplit(var_dat$Variable, paste(var, "_",sep=''))
	tming = unlist(lapply(tming, FUN=function(x){x[2]}))
	var_dat["Time_Period"] = tming
	var_dat["var"] = var
	all_var = rbind(all_var, var_dat)
	# var_dat = transform(var_dat, parameter=reorder(parameter, value))
}
gttle = paste("Global Sensititvity")
# png(paste(gttle,"absolute.png",sep=""), res=500, units='in', height=8,width=7)
ggvar = ggplot(all_var, aes(x=Parameter, y=delta_mean/max(delta_mean), fill = Variable))
# ggvar = ggplot(var_dat, aes(x=Parameter, y=delta_mean/max(delta_mean))) 
ggvar = ggvar + geom_bar(stat="identity", position = "dodge") + coord_flip() + theme_bw() + ggtitle(gttle)#+ facet_grid(. ~ var)
ggvar = ggvar + scale_fill_manual(name="Time Period", values=c("darkolivegreen4", "goldenrod1", "dodgerblue4"))
ggvar = ggvar + labs(x="Parameter", y="Sensitivity Rank")
plot(ggvar)
dev.off()

# strmf = strmf[order(strmf$value),]

# gstrmf = ggplot(strmf,aes(x=parameter, y = value/max(value),color = variable))
# gstrmf + geom_bar(stat="identity") + coord_flip() + theme_bw() + ggtitle(")#theme(axis.text.x=) #+ theme_bw()


# var_dat["variable"] = apply(
		# var_dat,
		# MARGIN=1,
		# FUN=function(x){
			##splt = sub(paste(var,"_",sep=''),"",x["variable"])
			# splt = paste(capitalize(strsplit(splt,"_")[[1]]), collapse=" ")
			# return(splt)
		# }
	# )
##########

	# var_dat["variable"] = apply(
		# var_dat,
		# MARGIN=1,
		# FUN=function(x){
			## splt = sub(paste(var,"_",sep=''),"",x["variable"])
			# splt = paste(capitalize(strsplit(splt,"_")[[1]]), collapse=" ")
			# return(splt)
		# }
	# )
	# var_dat["Parameter_Type"] = apply(
		# var_dat,
		# MARGIN=1,
		# FUN=function(x){
			# splt = strsplit(x["parameter"], "_")[[1]]
			# splt = splt[length(splt)]
			# return(splt)
		# }
	# )
	
	# var_dat["annual_val"] = apply(
		# var_dat,
		# MARGIN=1,
		# FUN=function(x){
			# if(grepl("annual",x["variable"])){
				# return(as.numeric(x["value"]))
			# } else {
				# return(NA)
			# }
		# }) 