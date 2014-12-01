# run SWAT for different crops using a range of bio E
#   This should be run with MONTHLY output (code 0)
wd = "H:\\WRB\\Scenarios\\Default\\TxtInOut"
setwd(wd)

dir_out = "H:/sim_flow/hyd_params"

source("C:/Users/evansdm/Documents/Code/validation/functions_query_output.r")

file_obs_flow_lu = "T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv"
obs_flow_lu = read.csv(file_obs_flow_lu)
obs_flow_lu = subset(obs_flow_lu, 
    Keep == 1, 
    select = c("Flow_Name", "WRB_SubbasinID"))

# move swat executable
file.copy("C:/SWAT/ArcSWAT/SWAT_64rel.exe", paste(wd, "SWAT_64rel.exe", sep="\\"))


# surlag = 0.05 to 24;4.000
# esco   = 0 to 1; default: 0.950
# epco   = 0 to 1; default: 1.00
# sftmp  = -5 to 5; default: 1.000
# smtmp  = 05 to 5; defualt: 0.500
valTbl = list(seq(0, 1, length.out=10), 
    seq(0, 1, length.out=10), 
    seq(0.05, 24, length.out=10),
    seq(-5, 5, length.out=10),
    seq(-5, 5, length.out=10))
dflts = list(0.950,
    1.00,
    4.000,
    1.000,
    0.500)
#shortening and/or lengthening to make 5 chars
names(valTbl) = c("ESCO:", "EPCO:", "SURLA", "SFTMP", "SMTMP")
names(dflts) = c("ESCO:", "EPCO:", "SURLA", "SFTMP", "SMTMP")
# default alphaBF: 0.0480
for (abf in 1:2){
    # second time around input default alpha bf
    if (abf == 2){
        alphaBF = "Default"
        files.gw = list.files(wd, pattern = "*.gw")
        for (fl in files.gw){
            gw.txt = readLines(paste(wd, fl, sep = '/'))
            bf.ind = which(substr(gw.txt, 23, 31) == "ALPHA_BF ")
            substr(gw.txt[bf.ind], 11, 15) = format(0.500, digits = 4, nsmall = 3)
            writeLines(gw.txt, paste(wd, fl, sep = '/'))
        }
    } else {
        alphaBF = "Region"
    }
    for (param in names(valTbl)) {
        vals = valTbl[[param]]
        for (val in vals){
            print(paste("Running SWAT, changing:",param, "to", val))
            basins.bsn = readLines(paste(wd, "basins.bsn", sep="\\"))
            par.ind = which(substr(basins.bsn, 23, 27) == param)
            substr(basins.bsn[par.ind], 11, 16) = 
                format(val, digits=3, nsmall=3, width = 6)
                        
            writeLines(basins.bsn, paste(wd, "basins.bsn", sep="\\"))
            bat = tempfile(pattern="runswat_", fileext=".bat")
            writeLines(paste("cd ", wd, "\nSWAT_64rel.exe", sep=""), bat) 
            
            system(bat)
            
            dat = readLines(paste(wd, "output.rch", sep="\\"))
            dat = dat[10:length(dat)]
            
            select_cols = list(
                cols = c("RCH", "MON", "FLOW_OUT"),
                dtypes = c(as.integer, as.integer, as.numeric)
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
            modData = subset(modData, RCH %in% obs_flow_lu$WRB_SubbasinID & MON < 13)
            
            fl_name = paste(substr(param,1,4), val, alphaBF, "flowout.csv", sep = '_')
            write.csv(modData, paste(dir_out, fl_name, sep = '/'), row.names=F)
        }
        substr(basins.bsn[par.ind], 11, 16) = 
            format(dflts[[param]], digits=2, nsmall=3, width = 6)
        writeLines(basins.bsn, paste(wd, "basins.bsn", sep="\\"))
    }

}




