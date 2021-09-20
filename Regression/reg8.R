library(ggplot2)
library(dplyr)
library(ggpubr)
setwd("/home/olga/MyProjects/Polikek/ML/Regression/datasets")

data = read.csv("UKgas.csv")
data = data[ ,-1]
reg = lm(UKgas ~ time, data)
ggplot(data, aes(x=time, y=UKgas)) + geom_line() +
  stat_smooth(method = "lm", col = "red")

predict.lm(reg, list(time=2016.00))
predict.lm(reg, list(time=2016.25))
predict.lm(reg, list(time=2016.50))
predict.lm(reg, list(time=2016.75))

date_min = 0
date_max = 0
coef_max = 0
coef_min = 100
for (i in 1:(dim(data)[1]-1)){
  df = data.frame(time = c(data$time[i], data$time[i+1]),
                  UKgas = c(data$UKgas[i], data$UKgas[i+1]))
  reg = lm(UKgas ~ time, df)
  if (reg$coefficients["time"] < coef_min){
    coef_min = reg$coefficients["time"]
    date_min = data$time[i]
  }
  else{
    if (reg$coefficients["time"] > coef_max){
      coef_max = reg$coefficients["time"]
      date_max = data$time[i]
    }
  }
}

plot(data$time, data$UKgas)
lines(data$time, predict(reg), col = 'red')
quartals = data.frame(q1 = data[-nrow(data), ]$time,
                      q2 = data[-1, ]$time,
                      gas1 = data[-nrow(data), ]$UKgas,
                      gas2 = data[-1, ]$UKgas)
regs = c()
for (i in 1:214){
  regs[i] = lm(gas1[i]+gas2[i] ~ q1[i]+q2[i], quartals)
}

df = data.frame(time = c(quartals$q1[1], quartals$q2[1]),
                gas = c(quartals$gas1[1], quartals$gas2[1]))

p1 = ggplot(df, aes(x=time, y=gas)) +
  geom_line() +
  geom_point()

df = data.frame(time = c(quartals$q1[2], quartals$q2[2]),
                gas = c(quartals$gas1[2], quartals$gas2[2]))

p2 = ggplot(df, aes(x=time, y=gas)) +
  geom_line() +
  geom_point()

cp = list(p1, p2)

ggarrange(plotlist=cp)

