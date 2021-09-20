library(ggplot2)
library(rpart)
library(mlbench)
library(adabag)

data(Vehicle)
l = length(Vehicle[,1])
sub = sample(1:l,7*l/10)  
mfinal = seq(1, 301, 10)
maxdepth = 5
res = rep(0, length(mfinal))
for(i in 1:length(mfinal)){
  Vehicle.adaboost = boosting(Class ~.,data=Vehicle[sub,], mfinal=mfinal[i], maxdepth=maxdepth)
  Vehicle.adaboost.pred = predict.boosting(Vehicle.adaboost, newdata=Vehicle[-sub, ])
  res[i] = Vehicle.adaboost.pred$error
}
df = data.frame(mfinal = mfinal, error = res)
ggplot(df, aes(x = mfinal, y = error)) +
  geom_line()
