library(scatterplot3d)

setwd("/home/olga/MyProjects/Polikek/ML/Regression/datasets")

data = read.delim("reglab1.txt")
plot(data)
plot3d(x=data$x, y=data$y, z=data$z)
scatterplot3d(x = data$x, y = data$y, z = data$z)
fit = lm(z ~ x+y, data, subset = !is.na(x) & !is.na(y))
summary(fit)
fit = lm(z ~ x/y, data, subset = !is.na(y) & !is.na(y))
summary(fit)
fit = lm(z ~ (x+y)^2, data, subset = !is.na(x) & !is.na(y))
summary(fit)
fit = lm(z ~ x * y, data, subset = !is.na(x) & !is.na(y))
summary(fit)

plot(fit$residuals, ylab = "residuals", log = "")
abline(0, 0, col = "red")
