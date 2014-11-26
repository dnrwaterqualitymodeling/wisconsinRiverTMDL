# CHANGE THESE ###########
# SWAT project
projectDir = "D:/WRB.Sufi2.SwatCup"
simCount = 2001
subbasinCount = 338
startYr = 2002
endYr = 2013
objFuncCode = 8
# Observations -- variable name, column index in output.rch, subbasin ID, observed data
obsDir = "D:/usgs_raw"
gage_subbasin_lu = read.csv("D:/gauge_basin_lookup.csv",
    colClasses=c("character", "character", "integer", "integer", "character"))
monthly = T

# parameterization = rbind(
    # c("r__ALPHA_BF.gw", -0.99, -0.5),
    # c("r__CN2.mgt", -0.6, 0.2),
    # c("a__SOL_AWC().sol", -0.05, 0.05),
    # c("v__ESCO.hru", 0.9, 1),
    # c("v__GWQMN.gw", 500, 3000),
    # c("v__GW_REVAP.gw", 0.02, 0.2),
    # c("v__REVAPMN.gw", 0, 500)
# )
parameterization = rbind(
	c("v__SFTMP.bsn",-20,20),
	c("v__SMTMP.bsn",-20,20),
	c("v__SMFMX.bsn",0,20),
	c("v__SMFMN.bsn",0,20),
	c("v__TIMP.bsn",0,1)
)

# Don't change these
source("D:/wisconsinRiverTMDL/validation/functions_query_output.r")

gage_subbasin_lu = subset(gage_subbasin_lu, Keep == 1)
gage_subbasin_lu = gage_subbasin_lu[c("USGS_ID", "WRB_SubbasinID")]
observed_table = cbind(rep("FLOW_OUT", nrow(gage_subbasin_lu)),
    rep(6, nrow(gage_subbasin_lu)),
    gage_subbasin_lu$WRB_SubbasinID,
    paste(obsDir, "/", gage_subbasin_lu$USGS_ID, ".txt", sep="")
)
observed_table = observed_table[order(as.integer(observed_table[,3])),]

inDir = paste(projectDir,
    "/",
    toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
    ".IN",
    sep="")
file.cio = paste(projectDir,
    "/file.cio",
    sep="")
Par_inf_file = paste(inDir,
    "/par_inf.txt",
    sep="")
swEdit_file = paste(projectDir,
    "/",
    toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
    "_swEdit.def",
    sep="")
observed_rch_file = paste(inDir,
    "/observed_rch.txt",
    sep="")
extract_rch_file = paste(projectDir,
     "/",
     toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
     "_extract_rch.def",
     sep="")
observed_file = paste(inDir,
    "/Observed.txt",
    sep="")
var_file_name = paste(inDir,
    "/Var_file_name.txt",
    sep="")
var_file_rch = paste(inDir,
    "/Var_file_rch.txt",
    sep="")
if (monthly) {
    time_series = data.frame(DATE = seq(
        as.Date(paste(startYr, "-01-01", sep="")),
        as.Date(paste(endYr, "-12-31", sep="")),
        "1 months"))
    time_series = cbind(data.frame(i = 1:nrow(time_series)), time_series)
} else {
    time_series = data.frame(DATE = seq(
        as.Date(paste(startYr, "-01-01", sep="")),
        as.Date(paste(endYr, "-12-31", sep="")),
        1))
    time_series = cbind(data.frame(i = 1:nrow(time_series)), time_series)
}

# Write file.cio
file.cio.dat = readLines(file.cio)
if (monthly) {
	file.cio.dat[59] = "               0    | IPRINT: print code (month, day, year)"
} else {
	file.cio.dat[59] = "               1    | IPRINT: print code (month, day, year)"
}
file.cio.dat[65] = "   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[67] = "   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[69] = "   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[71] = "   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
writeLines(file.cio.dat, file.cio)

# Write par_inf file
l1 = paste(nrow(parameterization), "  : Number of Parameters (the program only reads the first 4 parameters or any number indicated here)", sep="")
l2 = paste(simCount, "  : number of simulations", sep="")
writeLines(paste(l1,"\n",l2,"\n\n",sep=""), Par_inf_file)
write.table(parameterization, Par_inf_file, row.name=F, col.names=F, quote=F, append=T)
# write swEdit file
l1 = "1\t: starting simulation number"
l2 = paste(simCount, ": ending simulation number", sep="\t")
writeLines(paste(l1, "\n", l2, sep=""), swEdit_file)
########################
# Write observed_rch and observed.txt file
########################
l1 = paste(nrow(observed_table), ": number of observed variables", sep="\t")
write(l1, observed_rch_file)
# write('\n', observed_rch_file, append=T)
write(paste(nrow(observed_table), ": number of observed variables", sep="\t"), observed_file)
write(paste(objFuncCode, ": Objective function type, 1=mult,2=sum,3=r2,4=chi2,5=NS,6=br2,7=ssqr,8=PBIAS,9=RSR", sep="\t"),
      observed_file, append=T)
write("0.5\t: min value of objective function threshold for the behavioral solutions", observed_file, append=T)
write("\n", observed_file, append=T)

for (obs_i in 1:nrow(observed_table)) {
    obsData = read.table(observed_table[obs_i,4], skip=2, sep="\t", header=T)
    obsData = obsData[-1,]
    obsData = obsData[obsData[,5] == "A",]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
        FLOW_OBSERVED=as.numeric(as.character(obsData[,4])))
    if (monthly) {
        months = as.POSIXlt(obsData$DATE)$mo + 1
        years = as.POSIXlt(obsData$DATE)$year + 1900
        date = paste(years, months, "01", sep="-")
        obsMonthly = aggregate(FLOW_OBSERVED ~ date, data=obsData, mean)
        obsData = data.frame(DATE=as.Date(obsMonthly[,1]),
            FLOW_OBSERVED=obsMonthly[,2])
        obsData = merge(time_series, obsData, all.y=T, all.x=F)
        obsData = obsData[order(obsData$i),]
        obsData$VARNAME_DATE = paste(
            observed_table[obs_i, 1],
            format(obsData$DATE, "%m"),
            format(obsData$DATE, "%Y"),
            sep="_")
    } else {
        obsData = data.frame(DATE=as.Date(obsData[,1]),
            FLOW_OBSERVED=obsData[,2])
        obsData = merge(time_series, obsData, all.y=T, all.x=F)
        obsData = obsData[order(obsData$i),]
        obsData$VARNAME_DATE = paste(
            observed_table[obs_i, 1],
            format(obsData$DATE, "%d"),
            format(obsData$DATE, "%m"),
            format(obsData$DATE, "%Y"),
            sep="_")
    }
    obsData = obsData[c("i","VARNAME_DATE", "FLOW_OBSERVED")]
    #converting from cf/s to cm/s
    obsData$FLOW_OBSERVED <- obsData$FLOW_OBSERVED * 0.0283168466
    l1 = paste(observed_table[obs_i,1], "_", observed_table[obs_i,3],
        "   : this is the name of the variable and the subbasin number to be included in the objective function",
        sep="")
    l2 = paste(nrow(obsData), "   : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
    l3 = "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
    
    write("\n", observed_rch_file, append=T)
    write(l1, observed_rch_file, append=T)
    write(l2, observed_rch_file, append=T)
    write(l3, observed_rch_file, append=T)
    write("\n", observed_rch_file, append=T)
    
    write.table(obsData, observed_rch_file, sep="\t", row.names=F, col.names=F, append=T, quote=F)
    # Observed.txt
    write(paste(observed_table[obs_i,1], "_", observed_table[obs_i,3],
                "\t: this is the name of the variable and the subbasin number to be included in the objective function",
                sep=""),
          observed_file, append=T)
    write("1     : weight of the variable in the objective function\n-1    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations\n-1    : constant flow separation, threshold value. (not considered if -1)\n1     : if separation of signal is considered, this is weight of the smaller values in the objective function\n1     : if separation of signal is considered, this is weight of the larger values in the objective function\n10    : percentage of measurement error",
          observed_file, append=T)
    write(paste(nrow(obsData), ": number of data points for this variable as it follows below. First column is a sequential number from beginning",
        sep="\t"),
        observed_file, append=T)
    write("      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value.",
        observed_file, append=T)
    write("\n", observed_file, append=T)
    write.table(obsData, observed_file, sep="\t", row.names=F, col.names=F, append=T, quote=F)
    write("\n", observed_file, append=T)   
}
# write extract_rch table
write("output.rch     : swat output file name", extract_rch_file)
write(paste(length(unique(observed_table[,1])), ": number of variables to get", sep="\t"),
      extract_rch_file,
      append=T)
write(
    paste(
        paste(unique(observed_table[,2]), sep=" "),
        ": variable column number(s) in the swat output file (as many as the above number)"
        , sep="\t"),
    extract_rch_file,
    append=T
)
write("", extract_rch_file, append=T)
write(paste(subbasinCount, ": total number of reaches (subbasins) in the project", sep="\t"),
      extract_rch_file,
      append=T)
write("", extract_rch_file, append=T)
for (variable in unique(observed_table[,2])) {
    reaches = unique(observed_table[observed_table[,2] == variable, 3]) 
    write(paste(
        length(reaches),
        ": number of reaches (subbasins) to get for variable"
        , sep="\t"),
        extract_rch_file,
        append=T)
    write(paste(paste(reaches, collapse=" "),
        ": reach (subbasin) numbers for variable (ordered)",
        sep="\t"),
        extract_rch_file,
        append=T)
}
write("", extract_rch_file, append=T)
write(paste(startYr, ": beginning year of simulation not including the warm up period", sep="\t"),
      extract_rch_file,
      append=T)
write(paste(endYr, ": end year of simulation", sep="\t"),
      extract_rch_file,
      append=T)
write("", extract_rch_file, append=T)
if (monthly) {
    write(paste(2, ": time step (1=daily, 2=monthly, 3=yearly)", sep="\t"),
          extract_rch_file,
          append=T)
} else {
    write(paste(1, ": time step (1=daily, 2=monthly, 3=yearly)", sep="\t"),
          extract_rch_file,
          append=T)
}
filenames = paste(observed_table[,1], "_", observed_table[,3], ".txt", sep="")
write(filenames, var_file_name)
write(filenames, var_file_rch)
