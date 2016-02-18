library("stringr")

data = read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/dairy.txt", skip=1, header=F)
hru_data = read.table("C:/Users/radeca/Desktop/USLE_P/HRUouts/hru4.txt", header=T)
projectDir = "C:/Users/radeca/Desktop/USLE_P/WRB.Sufi2.SwatCup"
dir_out = paste(projectDir, "reduced_fert", sep="/")
lu_lookup = read.table("T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/LandCoverLandManagement/Landuse_Lookup_27012015.csv", sep=",", header=T)

if (!file.exists(dir_out)) {
  dir.create(dir_out)
}

yld_biomass = data.frame(
  LULC = c("ALFA", "CORN", "CSIL", "GRBN", "POTA", "SCRN", "SOYB"),
  Prc_yld = c(0.9, 0.5, 0.9, 0.65, 0.95, 0.65, 0.31))

fert_lookup = data.frame(
  TYPE = c("Potato/Vegetable", "Cash Grain", "Dairy"),
  P_conc = c(0.103, 0.044, 0.013)
)

files_mgt = list.files("C:/Users/radeca/Desktop/USLE_P/WRB.Sufi2.SwatCup", "*.mgt")
for (fl in files_mgt){
    hru_file = paste(projectDir, fl, sep="/" )
    file_out = paste(dir_out, fl, sep="/")
    # Calculate Plant uptake of P
      # Get HRU data  
        # Read HRU number from first line of .mgt file
    lne1 = readLines(hru_file, 1 )
    linesplit = strsplit(lne1, "\\s+|:")
    hru = linesplit[[1]][6]
     
    hru_d = subset(
      hru_data,
      HRU==hru & MON != 12,
      select = c(HRU, LULC, MON, PUPkg.ha)
    )
    hru_d = merge(hru_d, yld_biomass)
    # Average HRU PU data across years
    pup = mean(hru_d$PUPkg.ha * hru_d$Prc_yld)
    # Convert Plant uptake to fertilizer application rate
      # Get fertilizer [P]
    # Read LU ID from first line of .mgt file
    # Find the ag type associated with that LU from lookup table on T:/ drive
    landuse = linesplit[[1]][12] 
    ag_type = as.character(
      subset(
        lu_lookup,
        LANDUSE == landuse,
        select = TYPE)[1,1]
    )
    if (!(ag_type %in% fert_lookup$TYPE)) {
      next
    }
        # Find the fertilizer [P] associated with that ag type from pre-defined 3X2 data.frame
   fert_p = subset(fert_lookup, TYPE == ag_type, select=P_conc)[1,1]
    # Using above info divide average annual
    #    plant uptake P by fertilizer [P] to determine new fertilizer application rate
   new_ann_rate = pup / fert_p
   # Proportionally reduce fertilizer applications in .mgt file to match uptake
      # Read in fertilizer only operations from schedule in .mgt file
   mgt = readLines(hru_file)
   mgt_ops = mgt[31:length(mgt)]
   fert_ops = mgt_ops[as.integer(substr(mgt_ops, 17, 18)) == 3]
   fert_rates = as.numeric(substr(fert_ops, 34, 44))
      # Calculate average annual fertilizer application rate
   old_ann_rate = sum(fert_rates) / 6
      # calculate proportion of fertilizer applied compared to what is needed by plants
   fert_prop = new_ann_rate / old_ann_rate
   print(paste(hru_file, fert_prop))
      # Calculate each new fertilizer application by applying that proportional reduction
   new_rates = fert_rates * fert_prop
    # Write new fertilizer applications to file
   new_rates = sprintf("%10.5f", new_rates)
   substr(fert_ops, 34, 44) = new_rates
   mgt_ops[as.integer(substr(mgt_ops, 17, 18)) == 3] = fert_ops
   mgt[31:length(mgt)] = mgt_ops
   writeLines(mgt, file_out)
}

