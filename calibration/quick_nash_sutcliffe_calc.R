best_sim = readLines("clipboard")


nash_sutcliffe = function(Q_m, Q_o) {
	num = sum((Q_o - Q_m)^2)
	den = sum((Q_o - mean(Q_o))^2)
	ns = 1-(num/den)
	return(ns)
}

best_sim = best_sim[!(substr(best_sim,1,1) %in% c("F", "o", ""))]
best_sim = strsplit(best_sim, "\\s+")
d = NULL
for (i in 1:length(best_sim)) {
	d = rbind(d, c(best_sim[[i]][1], best_sim[[i]][2]))
}
num_mat = matrix(as.numeric(d), nrow = nrow(d))
num_mat = data.frame(
	observed = num_mat[,1],
	simulated = num_mat[,2]
)
plot(observed ~ simulated, data=num_mat, log="xy")
lm1 = lm(observed ~ simulated, data=num_mat)
ns = nash_sutcliffe(num_mat$simulated, num_mat$observed)