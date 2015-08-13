file_goal = "C:/Users/ruesca/Desktop/goal.txt"

cols = strsplit(readLines(file_goal, n=4)[4], "\\s+")[[1]]
cols = gsub("^[0-9]+:", "", cols)

obj_fun = readLines(file_goal)
obj_fun = obj_fun[5:length(obj_fun)]
obj_fun = gsub("\\s+", ",", obj_fun)
obj_fun = strsplit(obj_fun, ",")
obj_fun = as.numeric(unlist(obj_fun))
obj_fun = matrix(obj_fun, ncol=length(cols), byrow=T)
obj_fun = as.data.frame(obj_fun)
cols = gsub("\\{|\\}|\\(|\\)|\\.", "_", cols)
names(obj_fun) = cols

formula = formula(paste(
	"goal_value ~ ",
	paste(cols[2:(length(cols)-1)], collapse = " + ",sep=""),
	sep=""
))

summary(lm(formula, data=obj_fun))