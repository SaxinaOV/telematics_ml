library(MASS)
library(glmnet)
library(lmridge)
library(ggplot2)
etwd("/home/olga/MyProjects/Polikek/ML/Regression/datasets")

plot(longley)
reg = lm(Employed ~ ., longley)
summary(reg)
data = subset(longley, select=-c(Population))
s = sample(seq(dim(data)[1]), dim(data)[1] * 0.5)
train = data[s,]
test = data[-s,]
test_er = c()
train_er = c()
lambda_seq = 10^(-3+0.2*(0:10))
j = 1
for (i in lambda_seq){
  reg = lm.ridge(Employed ~ ., train, lambda=i)
  pred.train = scale(train[1:5],center = TRUE, scale = reg$scales)%*% 
    reg$coef + reg$ym
  pred.test = scale(test[1:5],center = TRUE, scale = reg$scales)%*% 
    reg$coef + reg$ym
  test_er[j] = mean(sqrt((test$Employed - pred.test)^2))
  train_er[j] = mean(sqrt((train$Employed - pred.train)^2))
  j = j + 1
}

df = data.frame(lambda = lambda_seq, test = test_er, train = train_er)
ggplot(df, aes(x = lambda)) + 
  geom_line(aes(y = test, color = "test")) +
  geom_line(aes(y = train, color = "train")) +
  labs(y = "mean error")

plot(data)

library(car)
scatterplotMatrix(longley,spread = FALSE, lty.smooth = 2, main ='Соотношение переменных')


library(datasets)
air = airquality[, c("Ozone", "Solar.R", "Wind", "Temp")] 
f = lm.ridge(Ozone ~ ., data = air, subset = !is.na(Solar.R) & !is.na(Ozone), lambda = seq(1, 10000, by = 10))
plot(f)
