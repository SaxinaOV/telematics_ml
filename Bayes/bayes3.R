library(dplyr)
library(ggplot2)
library(rpivotTable)
library(dummies)
setwd("/home/olga/MyProjects/Polikek/ML/Bayes/datasets")

data.train = read.csv("Titanic_train2.csv", sep=",", quote ="\"", stringsAsFactors = TRUE)
data.train = select(data.train, Survived, Pclass, Age, Sex, SibSp, Parch)
#data.train$Fare[data.train$Fare==0] <- median(data.train$Fare, na.rm = TRUE)
#data.train$Age[is.na(data.train$Age)] <- median(data.train$Age, na.rm = TRUE)
data.train = na.omit(data.train)
data.train$Survived = factor(data.train$Survived)
data.train$Pclass = factor(data.train$Pclass, ordered=TRUE, levels = c(3, 2, 1))
#colSums(is.na(data.train))

data.test = read.csv("Titanic_test2.csv", sep=",", quote ="\"", stringsAsFactors = TRUE)
data.test = select(data.test, Pclass, Age, Sex, SibSp, Parch)
data.test$Pclass = factor(data.test$Pclass, ordered=TRUE, levels = c(3, 2, 1))
#data.train$Age[is.na(data.train$Age)] <- median(data.train$Age, na.rm = TRUE)
#data.test$Age[is.na(data.test$Age)] <- mean(data.test$Age, na.rm = TRUE)
#data.test$Fare[is.na(data.test$Fare)] <- mean(data.test$Fare, na.rm = TRUE)
data.test = na.omit(data.test)

bayes = naiveBayes(Survived ~ ., data = data.train)
prediction = predict(bayes, data.test)

s = round(seq(dim(data.train)[1]*0.2))
data.valid = data.train[s,]
data.train = data.train[-s,]
bayes = naiveBayes(Survived ~ ., data = data.train)
prediction = predict(bayes, data.valid)
t = table(prediction, data.valid$Survived)
accuracy = sum(diag(t)) / sum(t)

#rpivotTable(data.train,rows="Survived", cols="Sex")
#rpivotTable(data.train,rows="Survived", cols="SibSp")
#rpivotTable(data.train,rows="Survived", cols="Pclass")
#rpivotTable(data.train,rows="Survived", cols="Age")
#rpivotTable(data.train,rows="Survived", cols="Parch")
