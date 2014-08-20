library(RODBC)

#project directories 

prjDb = "C:/SWAT/Reservoirs_2/Reservoirs_2.mdb"
netDir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement"
crosswalk_file = paste(netDir, "Landuse_Lookup.csv", sep="/")

# Read in all necessary tables

crosswalk = read.csv(crosswalk_file)

con_updates = odbcConnectAccess(paste(netDir, "OpSchedules_fert.mdb", sep="/"))
opSched = sqlFetch(con_updates, "OpSchedules")
fert = sqlFetch(con_updates, "fert")
close(con_updates)

con_mgt2 = odbcConnectAccess(prjDb)
mgt2 = sqlFetch(con_mgt2, "mgt2")
close(con_mgt2)

for (s in mgt2$SUBBASIN) {
    print(s)
    for (hru in mgt2$HRU) {
        print(hru)
        q = mgt2$HRU == hru & mgt2$SUBBASIN == s
        lc = as.character(unique(mgt2$LANDUSE[q]))
        opCode = as.character(crosswalk$KEY[crosswalk$LANDUSE == lc])
        operation = opSched[opSched$SID == opCode]
        break
    }
}
