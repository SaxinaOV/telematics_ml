library(datasets)

data = data.frame(year=as.numeric(time(JohnsonJohnson)), profit=as.matrix(JohnsonJohnson))
ggplot(data, aes(x=year, y=profit)) + geom_line() +
  stat_smooth(method = "lm", col = "red")
reg = lm(profit ~ year, data)

mean(c(predict.lm(reg, list(year=2016.00)),
predict.lm(reg, list(year=2016.25)),
predict.lm(reg, list(year=2016.50)),
predict.lm(reg, list(year=2016.75))))

#plot(data$year, data$profit)
#lines(data$year, predict(reg), col = 'red')

regs = c()
date_min = 0
date_max = 0
coef_max = 0
coef_min = 100
for (i in 1:(dim(data)[1]-1)){
  df = data.frame(year = c(data$year[i], data$year[i+1]),
                  profit = c(data$profit[i], data$profit[i+1]))
  reg = lm(profit ~ year, df)
  if (reg$coefficients["year"] < coef_min){
    coef_min = reg$coefficients["year"]
    date_min = data$year[i]
  }
  else{
    if (reg$coefficients["year"] > coef_max){
      coef_max = reg$coefficients["year"]
      date_max = data$year[i]
    }
  }
}

ggarrange(plotlist = plots[1:16], nrow = 4, ncol = 4)




df = data.frame(year = c(data$year[1], data$year[2]),
                profit = c(data$profit[1], data$profit[1+1]))
regs[1] = lm(profit ~ year, df)
plots[1] = ggplot(df, aes(x=year, y=profit)) + 
  geom_line() + geom_point()
  stat_smooth(method = "lm", col = "red")

min_year = 1000
min_i = 0
max_year = -1000
max_i = 0
for (i in 1:length(regs)){
  if (regs[[i]]["year"] < min_year){
    min_year = regs[[i]]["year"]
    min_i = i
  }
  else{
    if (regs[[i]]["year"] > max_year){
      max_year = regs[[i]]["year"]
      max_i = i
    }
  }
}

