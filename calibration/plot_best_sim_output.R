dir_prj = "C:/TEMP/WRB.Sufi2.SwatCup"
dir_out = "C:/Users/ruesca/Desktop/Plots"

gage_subbasin_lu =
	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
	colClasses=c(rep("character", 5), "integer", "integer", "character"))

exclude = rbind(
	c("FLOW_OUT", 59),
	c("FLOW_OUT", 74),
	c("FLOW_OUT", 81),
	c("FLOW_OUT", 148),
	c("FLOW_OUT", 154),
	c("SED_OUT", 151),
	c("SED_OUT", 152),
	c("SED_OUT", 157),
	c("SED_OUT", 162),
	c("SED_OUT", 268),
	c("SED_OUT", 326),
	c("TOT_P", 74),
	c("TOT_P", 81),
	c("TOT_P", 148),
	c("TOT_P", 154),
	c("TOT_P", 138)
)

out_pdf = paste(dir_out,"/model_diagnostic_plots.pdf", sep="")
pdf(out_pdf, width=6, height=6)

best_sim = readLines(paste(dir_prj, "SUFI2.OUT", "best_sim.txt", sep="/"))
o_data = readLines(paste(dir_prj, "SUFI2.IN", "observed.txt", sep="/"))
#station_lu = read.table(station_lu_file, sep="\t", header=T)

data_lines = grep("[A-Z]+_[A-Z]+_[0-9]{2}_[0-9]{4}", o_data, value=T)
dates = lapply(
	strsplit(data_lines, "_|\t"),
	function (x) {
		paste(x[5], x[4], "1", sep="-")
	}
)
dates = as.Date(unlist(dates))

par(mar=c(5.1,5.1,4.1,2.1))
hdr_lns = grep("_", best_sim)
all = data.frame()
for (hdr_ln in hdr_lns) {
  ln = best_sim[hdr_ln]
  v_match = regexpr("[A-Z]+_[A-Z]+", ln)
  pos1 = v_match[1]
  pos2 = pos1 + attr(v_match, "match.length") - 1
  v = substr(ln, pos1, pos2)
  sub_match = regexpr("[0-9]+$", ln)
  pos1 = sub_match[1]
  pos2 = pos1 + attr(sub_match, "match.length") - 1
  sub = as.integer(substr(ln, pos1, pos2))
  ln_d = hdr_ln + 2
  all_data = as.numeric(strsplit(best_sim[ln_d], "\\s+")[[1]])
  repeat {
    ln_d = ln_d + 1
    if (nchar(best_sim[ln_d]) == 0 | ln_d > length(best_sim)) { break }
    d = as.numeric(strsplit(best_sim[ln_d], "\\s+")[[1]])
    all_data = rbind(all_data, d)
  }
  v_data = data.frame(
    variable = v,
    subbasin = sub,
    observed = all_data[,1],
    predicted = all_data[,2]
    
  )
  all = rbind(all, v_data)
}

all_date = cbind(all, dates)
all_date = subset(all_date, predicted > 0)

combos = unique(all[,1:2])
combos = subset(
	combos,
	!(paste(variable, subbasin) %in% paste(exclude[,1], exclude[,1]))
#	!(variable %in% exclude[,1]) | !(subbasin %in% exclude[,2]) 
)


unit_lu = data.frame(
	variable=c("TOT_P", "SED_OUT", "FLOW_OUT"),
	units=c("kg", "metric ~ tons", "m^3/s")
)

  
for (row in 1:nrow(combos)){
  sub = combos[row,"subbasin"]
  var = as.character(combos[row,"variable"])
  u = as.character(subset(unit_lu, variable == var, select="units")[1,1])
  lab = gage_subbasin_lu$Flow_Name[gage_subbasin_lu$WRB_SubbasinID == sub]
  output = subset(all_date, subbasin == sub & variable == var )
  
  max_val = max(c(output[,"observed"],output[,"predicted"]), na.rm=T)
  min_val = min(c(output[,"observed"],output[,"predicted"]), na.rm=T)
  plot(
	  predicted ~ observed,
	  data=output,
	  main=paste(var, lab),
	  xlab=parse(text=paste("Observed ~ (", u, ")", sep="")),
	  ylab=parse(text=paste("Predicted ~ (", u, ")", sep="")),
	  xlim = c(min_val, max_val * 1.1),
	  ylim = c(min_val, max_val * 1.1),
	  pch=20
  )
  grid()
 
  
  plot(
	  predicted ~ observed,
	  main=paste("Log", var, lab),
	  xlab=parse(text=paste("Observed ~ (", u, ")", sep="")),
	  ylab=parse(text=paste("Predicted ~ (", u, ")", sep="")),
	  xlim = c(min_val, max_val * 1.1),
	  ylim = c(min_val, max_val * 1.1),
	  log="xy",
	  data=output,
	  pch=20
  )
  grid(equilogs=F)
  
  out_all_dates = data.frame(
	  dates = seq(min(output$dates), max(output$dates), by="month")
  )
  out_all_dates = merge(
	  out_all_dates,
	  output,
	  all.x=T
  )
  ylim = with(out_all_dates, range(c(predicted, observed), na.rm=T))
  plot(
	  predicted ~ dates,
	  data=out_all_dates,
	  type="l",
	  ylim=ylim,
	  main=paste(var, lab),
	  xlab=paste("Date"),
	  ylab=parse(text=paste(var, " ~ (", u, ")", sep=""))
  )
  lines(observed ~ dates, out_all_dates, col="grey")
  legend("topright", c("Observed", "Predicted"), lwd=1, col=c("grey","black"))
  
  qo = sort(output[,"observed"])
  qp = sort(output[,"predicted"])
  max_val = max(c(qo, qp), na.rm=T)
  min_val = min(c(qo, qp), na.rm=T)
  plot(
	  qo,
	  qp,
	  pch=20,
	  main=paste("QQ Plot", lab),
	  xlab=parse(text=paste("Observed ~ (", u, ")", sep="")),
	  ylab=parse(text=paste("Predicted ~ (", u, ")", sep="")),
	  xlim = c(min_val,max_val * 1.1),
	  ylim = c(min_val,max_val * 1.1)
  )
  grid()
  abline(0,1)
  
  plot(
	  qo,
	  qp,
	  pch=20,
	  main=paste("Log QQ Plot", var, lab),
	  xlab=parse(text=paste("Observed ~ (", u, ")", sep="")),
	  ylab=parse(text=paste("Predicted ~ (", u, ")", sep="")),
	  xlim=c(min_val,max_val * 1.1),
	  ylim=c(min_val,max_val * 1.1),
	  log="xy"
  )
  grid(equilogs=F)
  abline(0,1)
  
  mos = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

  o_i = as.integer(format(output$date, format="%m")) - 0.25
  p_i = as.integer(format(output$date, format="%m")) + 0.25
  
  plot(
	  o_i,
	  output$observed,
	  col="grey",
	  pch=20,
	  main=paste("Annual Variablity in", var, "for", lab),
	  xaxt="n",
	  xlab="Month",
	  ylab=parse(text=paste(var, " ~ (", u, ")", sep="")),
	  xlim=c(0.5, 12.5),
	  ylim=c(0,max(output$observed))
	)
  points(p_i, output$predicted, col="black", pch=20)
  axis(1, at=1:12, labels=mos)
  abline(v=1:13 - 0.5,col="grey",lty=2)
  legend(9,max(output$observed), c("Observed", "Predicted"), pch=20, col=c("grey","black"))
}
dev.off()



