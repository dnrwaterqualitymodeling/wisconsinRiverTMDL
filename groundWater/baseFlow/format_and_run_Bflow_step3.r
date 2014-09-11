# This file is step three in stream flow processing
#   this script takes as input the .flw files produced from the
#   contiguous record finding script (step 2). These files are then 
#   input, one by one, into bflow.exe. One by one because for some unknown
#   reason bflow errors out on some of the files and won't continue to process 
#   the other files.
options(warn=0)
wd = "C:/evans/bflow/baseflow/"
setwd(wd)
# grab the all the .flw files in the folder and then 
#   pull them together to get them for input to bflow
flLst <- list.files(wd, pattern = "*.flw")
masterList <- NULL
for (f in flLst){
    print(f)
    masterList <- rbind(masterList, f, deparse.level=0) 
    print(paste('Appended', f))
}

# grab the input file required for bflow.exe
inputList <- data.frame(readLines("file.lst", n = 7), stringsAsFactors=FALSE)
masterList <- as.data.frame(masterList,stringsAsFactors=FALSE)
names(masterList) <- names(inputList)

# input each .flw one at a time into bflow.exe and catch it if it 
#   errors out.
#   This then grabs the bflow output file, strips the relevant info from it
#   and hangs onto it for output
outDat <- NULL; failed <- NULL
for (flw in 1:nrow(masterList)) {
    exportList <- rbind(inputList,as.character(masterList[flw,1]))
    write.table(exportList,"file.lst", quote = F, row.names =F,col.names = F)
    err <- system("bflow.exe")
    if (err == 0){
        out <- readLines("baseflow.dat", n = 5)[5]
        outDat <- c(outDat, out)
        print(paste("Completed",flw))
    } else {
        print(paste("bflow failed on",flw))
        failed <- c(failed, masterList[flw,1])
    }
}
writeLines(outDat, paste("c:/evans/bflow/wi_dailyValues/data_files/","bflow_OutPut.txt",sep=''))
# in case the failed files are of interest
writeLines(failed, paste("c:/evans/bflow/wi_dailyValues/data_files/","failed_in_bflow.txt",sep=''))
# # # # # # # ## # # # # # # ## # # # # # # ## # # # # # # ## # # # # # # #

