library(stringr)

file_obs = "C:/Users/ruesca/Desktop/observed.txt"
file_best_sim = "C:/Users/ruesca/Desktop/best_sim.txt"
file_out = "C:/Users/ruesca/Desktop/driftless_obs_sim.txt"

obs_raw = readLines(file_obs)
obs_raw = obs_raw[obs_raw != ""]
n_site = as.integer(str_extract(obs_raw[1], "^[0-9]+"))
obs_raw = obs_raw[-(1:3)]
best_sim_raw = readLines(file_best_sim)
best_sim_raw = best_sim_raw[best_sim_raw != ""]

len_df = length(best_sim_raw) - n_site*2

ts = data.frame(
	subbasin=rep(as.integer(NA),len_df),
	date=rep(as.Date(NA),len_df),
	observed=rep(as.numeric(NA),len_df),
	simulated=rep(as.numeric(NA),len_df)
)
i = 0
for (site in 1:n_site) {
	subbasin = str_extract(obs_raw[1], "[0-9]+")
	obs_raw = obs_raw[-(1:9)]
	best_sim_raw = best_sim_raw[-(1:2)]
	j = 0
	while (
		(length(grep("^F", obs_raw[j+1])) == 0) &
		(i < len_df)) {
		i = i + 1
		j = j + 1
		date = strsplit(obs_raw[j], "\\t")[[1]][2]
		date = sub("FLOW_OUT_", "", date)
		date = as.Date(date, "%m_%d_%Y")
		best_sim = strsplit(best_sim_raw[j], "\\s+|\\t+")[[1]]
		row = data.frame(
			subbasin = as.integer(subbasin),
			date = date,
			observed = as.numeric(best_sim[1]),
			simulated = as.numeric(best_sim[2])
		)
		ts[i,] = row
	}
	if (site != n_site) {
		obs_raw = obs_raw[-(1:j)]
		best_sim_raw = best_sim_raw[-(1:j)]
	}
}

write.table(ts, file_out, row.names=F, sep="\t")