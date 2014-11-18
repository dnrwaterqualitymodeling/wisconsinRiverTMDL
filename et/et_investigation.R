# library(sendmailR)
proj_dir <- "H:/"
wd = "H:\\WRB\\Scenarios\\Default\\TxtInOut"
setwd(wd)
file_basin <- "basins.bsn"
source("C:/Users/evansdm/Documents/Code/validation/functions_query_output.r")

file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))
rch_runs_dir <- "sim_flow"
if (!file.exists(paste(proj_dir, rch_runs_dir, sep = ''))){
    dir.create(paste(proj_dir, rch_runs_dir, sep = ''))
}
    
eq_name <- c('Priestly_Taylor', 'Penman_Monteith', "Hargreaves")

for (eq in 0:2) {
    print(paste("Running SWAT with the", eq_name[eq+1], "equation."))
    bsn.dat = readLines(paste(wd, file_basin, sep="\\"))
    # Replacing the ET code
    substr(bsn.dat[11], 16, 16) <- as.character(eq)
    writeLines(bsn.dat, paste(wd, file_basin, sep="\\"))
    

    bat = tempfile(pattern="runswat_", fileext=".bat")
    writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
    system(bat)
    
    print(paste("Reading reach table..."))
    dat = readLines(paste(wd, "output.rch", sep="\\"))
    dat = dat[10:length(dat)]
    
    select_cols = list(
        cols = c("RCH", "AREA", "MON", "FLOW_OUT", "SED_OUT", "TOT_P"),
        dtypes = c(as.integer, as.numeric, as.integer, as.numeric, as.numeric, as.numeric)
    )
    
    modData = matrix(NA, nrow=length(dat), ncol=length(select_cols$cols))
    modData = as.data.frame(modData)
    names(modData) = select_cols$cols
    for (row in 1:length(select_cols$cols)) {
        col_name = select_cols$cols[row]
        dtype = select_cols$dtypes[row][[1]]
        vals = query_output.rch(dat, col_name)
        vals = sapply(vals, dtype)
        modData[col_name] = data.frame(vals, stringsAsFactors=F)
    }
    
    write.csv(modData, 
        paste(proj_dir, rch_runs_dir,'/', eq_name[eq+1],"_rchData.csv",sep =''),
        row.names=F)
    rm(modData, dat)
}



