library(ggplot2)
library(rpart)
library(mlbench)
library(adabag)

data(Glass)
l = length(Glass[,1])
sub = sample(1:l,7*l/10)  
mfinal = seq(1, 201, 10)
maxdepth = 5
res = rep(0, length(mfinal))
for(i in 1:length(mfinal)){
  Glass.adaboost = bagging(Type ~.,data=Glass[sub,], mfinal=mfinal[i], maxdepth=maxdepth)
  Glass.adaboost.pred = predict.bagging(Glass.adaboost, newdata=Glass[-sub, ])
  res[i] = Glass.adaboost.pred$error
}
df = data.frame(mfinal = mfinal, error = res)
ggplot(df, aes(x = mfinal, y = error)) +
  geom_line()
