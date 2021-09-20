setwd("/home/olga/MyProjects/Polikek/ML/Regression/datasets")

data = read.delim("cygage.txt")
f = lm(calAge ~ Depth, data, weights = data$Depth)
plot(data$Depth, data$calAge)
lines(data$Depth, predict(f), col = 'red')
summary(f)
