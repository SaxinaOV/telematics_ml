library(tree)
library(maptree)
library(ggplot2)
library(dplyr)

train = read.csv("Titanic_train2.csv", sep=",", quote ="\"", stringsAsFactors = FALSE)
test = read.csv("Titanic_test2.csv", sep=",", quote ="\"", stringsAsFactors = TRUE)

train = na.omit(train)
train$Survived = factor(train$Survived)
train$Sex = factor(train$Sex)
#train$Pclass = factor(train$Pclass, ordered=TRUE, levels = c(3, 2, 1))
train = select(train, Survived, Pclass, Age, Sex, SibSp, Parch, Fare)
test = select(test, Pclass, Age, Sex, SibSp, Parch, Fare)
#test$Pclass = factor(test$Pclass, ordered=TRUE, levels = c(3, 2, 1))
test = na.omit(test)
tree = tree(Survived~., train)
draw.tree(tree, print.levels=TRUE)

s = round(seq(dim(train)[1]*0.2))
valid = train[s,]
train = train[-s,]
tree = tree(Survived~., train)
pr = predict(tree, valid)
prediction = c()
for (i in 1:dim(pr)[1]){
  if (pr[i,1] > pr[i,2]){
    prediction[i] = 0
  }
  else{
    prediction[i] = 1
  }
}
tab = table(valid$Survived, prediction)
error = sum(diag(tab))/sum(tab)

