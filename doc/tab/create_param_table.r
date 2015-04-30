options(stringsAsFactors=F)
library(xtable)
library(Hmisc)

param_table = "C:/Users/evansdm/Documents/Code/calibration/sensitivity_analysis/basin_sensitivity_parameters.csv"
parms = read.csv(param_table)

parms = subset(parms, run == 1, select=-c(note,run))
names(parms) = capitalize(names(parms))
parms[which(parms$Method == 'a'),"Method"] = "Absolute"
parms[which(parms$Method == 'r'),"Method"] = "Relative"

xparms = xtable(parms)
print.xtable(xparms, type='latex',file="C:/Users/evansdm/Documents/Code/doc/tab/param_table.tex",include.rownames=F)
