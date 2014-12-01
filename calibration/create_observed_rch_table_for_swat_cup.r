# CHANGE THESE ###########
# SWAT project
projectDir = "C:/SWAT/Wetlands_2/Sufi_Test1.Sufi2.SwatCup"
simCount = 5
subbasinCount = 337
startYr = 2002
endYr = 2013
objFuncCode = 9
# Observations -- variable name, column index in output.rch, subbasin ID, observed data
observed_table = rbind(
    c("FLOW_OUT", 7, 137, "T:/Projects/Wisconsin_River/GIS_Datasets/observed/baraboo/dv.txt")
)
monthly = T
parameterization = rbind(
    c("r__ALPHA_BF.gw",-1,80)
#     c("r__CN2.mgt",-0.2,0.2)
)





# Don't change these
inDir = paste(projectDir,
    "/",
    toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
    ".IN",
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
l1 = paste(length(unique(observed_table[,1])), ": number of observed variables", sep="\t")
write(l1, observed_rch_file)
write('\n', observed_rch_file, append=T)
write(paste(nrow(observed_table), ": number of observed variables", sep="\t"), observed_file)
write(paste(objFuncCode, ": Objective function type, 1=mult,2=sum,3=r2,4=chi2,5=NS,6=br2,7=ssqr,8=PBIAS,9=RSR", sep="\t"),
      observed_file, append=T)
write("0.5\t: min value of objective function threshold for the behavioral solutions", observed_file, append=T)
for (observed in 1:nrow(observed_table)) {
    obsData = read.table(observed_table[observed,4], skip=2, sep="\t", header=T)
    obsData = obsData[-1,]
    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
    	FLOW_OBSERVED=as.numeric(as.character(obsData[,4])))
    if (monthly) {
    	months = as.POSIXlt(obsData$DATE)$mo + 1
    	years = as.POSIXlt(obsData$DATE)$year + 1900
    	date = paste(years, months, "01", sep="-")
    	obsMonthly = aggregate(obsData$FLOW_OBSERVED, list(date), mean) 
    	obsData = data.frame(ID = NA,
            DATE=paste("FLOW_OUT", as.integer(format(as.Date(obsMonthly[,1]), "%m")),
                as.integer(format(as.Date(obsMonthly[,1]), "%Y")), sep="_"),
    		FLOW_OBSERVED=obsMonthly[,2]*0.0283168466)
    	obsData = obsData[order(as.Date(obsMonthly[,1])),]
		obsData$ID = 1:length(unique(date))
    }
    
    
    l1 = paste(observed_table[observed,1], "_", observed_table[observed,3],
        "   : this is the name of the variable and the subbasin number to be included in the objective function",
        sep="")
    l2 = paste(nrow(obsData), "   : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
    l3 = "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
    
    write(l1, observed_rch_file, append=T)
    write(l2, observed_rch_file, append=T)
    write(l3, observed_rch_file, append=T)
    write("\n", observed_rch_file, append=T)
    
    write.table(obsData, observed_rch_file, sep="\t", row.names=F, col.names=F, append=T, quote=F)
    # Observed.txt
    write(paste(observed_table[observed,1], "_", observed_table[observed,3],
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
    write.table(obsData, observed_file, sep="\t", row.names=F, col.names=F, append=T, quote=F)
        
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
    write(paste(paste(reaches, sep=" "),
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
write(paste(2, ": time step (1=daily, 2=monthly, 3=yearly)", sep="\t"),
      extract_rch_file,
      append=T)

filenames = paste(observed_table[,1], "_", observed_table[,3], ".txt", sep="")
write(filenames, var_file_name)
write(filenames, var_file_rch)
