txtinout = "C:/Users/ruesca/Desktop/WRB/Scenarios/Default/TxtInOut_cnop_adjust_no_reservoirs"

fig.fig = readLines(paste(txtinout, "fig.fig", sep="/"))
adds = grep("^add", fig.fig)
routres = grep("^routres", fig.fig)
res_hsl = substr(fig.fig[routres], 19, 22)
rch_hsl = substr(fig.fig[routres], 31, 34)

for (i in 1:length(res_hsl)) {
	re = paste("^add.*", res_hsl[i], sep="")
	re = gsub(" ", "\\\\s", re)
	repl_lns = grep(re, fig.fig)
	for (ln in repl_lns) {
		pos = regexpr(res_hsl[i], fig.fig[ln])
		pos1 = pos[1]
		pos2 = pos1 + attr(pos, "match.length") - 1
		substr(fig.fig[ln], pos1, pos2) = rch_hsl[i]
	}
}

writeLines(fig.fig, paste(txtinout, "fig.fig", sep="/"))
