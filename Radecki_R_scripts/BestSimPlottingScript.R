
dir_out = ('C:/Users/radeca/Desktop/rcode')

out_pdf = paste(dir_out,"/Flow_Loads.pdf", sep="")
pdf(out_pdf,width=6, height=6)

best_sim = readLines('C:/Users/radeca/Desktop/WRB.Sufi2.SwatCup/SUFI2.OUT/best_sim.txt')
o_data=readLines('C:/Users/radeca/Desktop/WRB.SUFI2.SwatCup/SUFI2.IN/observed.txt')


aa=grep("_",o_data, value=T)
bb=sub("SED_OUT_","",aa)
cc=sub("TOT_P_","",bb)
dd=sub("FLOW_OUT_","",cc)
ee=sub("FLOW_IN_","",dd)
ff=paste(ee,"-01",sep="")
gg=gsub("\t","    ",ee)
hh=substr(gg,5,14)
jj=grep("_",hh,value=T)
kk=gsub(" ","",jj)
ll=paste(kk,"-01",sep="")
mm=sub("_","-",ll)
date=as.Date(mm,"%m-%Y-%d")



hdr_lns = grep("_", best_sim)
dt_lns=grep("_", aa)

all = data.frame()
for (dt_ln in dt_lns){
  dt=aa[dt_ln]
}

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

all_date=cbind(all,date)

combos=unique(all[,1:2])
unit=combos[,1]
unita=sub("TOT_P","kg",unit)
unitb=sub("SED_OUT","Tons",unita)
uu=sub("FLOW_OUT","m^3/s",unitb)
combos2=cbind(combos,uu)
  
  

for (row in 1:nrow(combos2)){
  sub=combos2[row,"subbasin"]
  var=combos2[row,"variable"]
  u=combos2[row,"uu"]
  
  output = subset (all_date, subbasin == sub & variable == var )
  
    max_val =max(c(output[,3],output[,4], na.rm=T))
  plot(predicted~observed, data=output,main=paste("Subbasin", sub, var), xlab=paste("Observed",u), ylab=paste("Predicted",u),  xlim = c(0,max_val * 1.1), ylim = c(0,max_val * 1.1),  pch=20)
 
  min_value=min(c(output[,3],output[,4], na.rm=T))
  min_val=log(min_value+0.01)
  max_val =log(max(c(output[,3],output[,4], na.rm=T)))
  plot(log(observed)~log(predicted), main=paste("Log Transformation of Subbasin", sub, var), xlab=paste("Ln Observed",u), ylab=paste("Ln Predicted",u), xlim = c(min_val, max_val * 1.1), ylim = c(min_val,max_val * 1.1), data=output, pch=20)
  
  plot(predicted~date, data=output, type="l",main=paste("Subbasin", sub, var), xlab=paste("Years of Observed Data"), ylab=paste( var,u) )
  points(observed~date, output)
  
  qo=sort(output[,3])
  qp=sort(output[,4])
  max_val =max(c(output[,3],output[,4], na.rm=T))
  plot(qo,qp, pch=20, main=paste("QQ Plot Subbasin", sub), xlab=paste(var,"Observed",u), ylab=paste(var,"Predicted",u),xlim = c(0,max_val * 1.1), ylim = c(0,max_val * 1.1),)
  abline(0,1)
  
  l_qo=log(sort(output[,3]))
  l_qp=log(sort(output[,4]))
  max_val =log(max(c(output[,3],output[,4], na.rm=T)))
  min_value=min(c(output[,3],output[,4], na.rm=T))
  min_val=log(min_value+0.01)
  plot(l_qo,l_qp, pch=20, main=paste("Ln of QQ Plot Subbasin", sub), xlab=paste("Ln",var,"Observed",u), ylab=paste("Ln",var,"Predicted",u),xlim = c(min_val,max_val * 1.1), ylim = c(min_val,max_val * 1.1),)
  abline(0,1)
  mos = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

  o_i = as.integer(format(output$date, format="%m")) - 0.25
  p_i = as.integer(format(output$date, format="%m")) + 0.25
  
  plot(o_i, output$observed, col="grey",pch=20, main=paste("Annual Variablity in", var, "for Subbasin",sub), xaxt="n",xlab="Month",ylab=paste(var,u), xlim=c(0, 12.5), ylim=c(0,max(output$observed)))
  points(p_i, output$predicted, col="black",pch=20)
  axis(1, at=1:12, labels=mos)
  abline(v=1:12,col="grey",lty=2)
  legend(9,max(output$observed), c("Observed", "Predicted"), pch=20, col=c("grey","black"))
}

dev.off()