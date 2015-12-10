rsv = readLines("output.rsv")
rsv[9] = paste(rsv[9], "   TP_OUTkg")
data_rsv = rsv[-(1:9)]
org_p = as.numeric(substr(data_rsv,176,187))
min_p = as.numeric(substr(data_rsv,320,331))
tp = format(org_p + min_p, digits=5, width=11, scientific=T)
data_rsv = paste(data_rsv, tp)
rsv = c(rsv[1:9], data_rsv)
writeLines(rsv, "output.rsv")