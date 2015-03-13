options(stringsAsFactors=F)
library(ggplot2)
library(Hmisc)

dir_proj = "H:/WRB_sensitivity"
file_reg_sens = paste(dir_proj, "regional_sensitivity.txt", sep="/")

dat = read.delim(file_reg_sens)
dat = subset(dat, Region == "Global")

ggplt = ggplot(all_var, aes(x=Parameter, y=delta_mean/max(delta_mean), fill = Variable))
ggplt + geom_bar(stat="identity", position = "dodge") + coord_flip() + theme_bw() + ggtitle("Global Sensitivity")#+ facet_grid(. ~ var)
# leaving out of the figure
# exclde = c("CHD_rte", "CHW2_rte", "HRU_SLP_hru", "SLSOIL_hru", "ADJ_PKR_bsn", "FFCB_bsn")
exclde = NULL
# var = c("streamflow_annual_mean", "phosphorus_annual_mean", "sediment_annual_mean", "streamflow_spring_mean", "phosphorus_spring_mean", "sediment_spring_mean")
# var_dat = subset(dat, (variable %in% var & region == "Global"))
# var_dat = subset(var_dat, value != 0)
# var_dat = subset(var_dat, !(parameter %in% exclde))

all_var = data.frame()
for (var in c("streamflow", "phosphorus", "sediment")){
	
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