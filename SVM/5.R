library(e1071)
library(rpivotTable)
library(ggplot2)


setwd("/home/olga/MyProjects/Polikek/ML/SVM/datasets")
dataSVM = read.delim("svmdata5.txt", stringsAsFactors=TRUE)
dataTest = read.delim("svmdata5test.txt", stringsAsFactors=TRUE)
g = 1
#my_seq = seq(1, 30, 2)
my_seq = c(seq(0.1, 5, 0.1), seq(6, 20, 2))
er_p = list("train" = c(my_seq), test = c(my_seq))
er_s = list("train" = c(my_seq), test = c(my_seq))
er_r = list("train" = c(my_seq), test = c(my_seq))
j = 1

for (i in my_seq){
  svm = svm(Colors~., data=dataSVM, cost=1, kernel="polynomial", gamma=i)
  predictionsTest=predict(svm, dataSVM)
  #plot(svm, dataSVM, X1~X2, svSymbol = 1, dataSymbol = 4, grid = 250, symbolPalette=c("Green", "Red"))
  ac_table = table(dataSVM$"Color", predictionsTest)
  er_p$train[j] = 1 - sum(diag(ac_table)) / sum(ac_table)
  
  svm = svm(Colors~., data=dataTest, cost=1, kernel="polynomial", gamma=i)
  predictionsTest=predict(svm, dataTest)
  ac_table = table(dataTest$"Color", predictionsTest)
  er_p$test[j] = 1 - sum(diag(ac_table)) / sum(ac_table)
  
  svm = svm(Colors~., data=dataSVM, cost=1, kernel="radial", gamma=i)
  predictionsTest=predict(svm, dataSVM)
  ac_table = table(dataSVM$"Color", predictionsTest)
  er_r$train[j] = 1 - sum(diag(ac_table)) / sum(ac_table)
  
  svm = svm(Colors~., data=dataTest, cost=1, kernel="radial", gamma=i)
  predictionsTest=predict(svm, dataTest)
  ac_table = table(dataTest$"Color", predictionsTest)
  er_r$test[j] = 1 - sum(diag(ac_table)) / sum(ac_table)
  
  svm = svm(Colors~., data=dataSVM, cost=1, kernel="sigmoid", gamma=i)
  predictionsTest=predict(svm, dataSVM)
  ac_table = table(dataSVM$"Color", predictionsTest)
  er_s$train[j] = 1 - sum(diag(ac_table)) / sum(ac_table)
  
  svm = svm(Colors~., data=dataTest, cost=1, kernel="sigmoid", gamma=i)
  predictionsTest=predict(svm, dataTest)
  ac_table = table(dataTest$"Color", predictionsTest)
  er_s$test[j] = 1 - sum(diag(ac_table)) / sum(ac_table)
  j = j + 1
}

df.pol = data.frame("train"=er_p$train, "test"=er_p$test, s=my_seq)
df.rad = data.frame("train"=er_r$train, "test"=er_r$test, s=my_seq)
df.sig = data.frame("train"=er_s$train, "test"=er_s$test, s=my_seq)

  
ggplot(df.pol, aes(x = s)) + 
  geom_line(aes(y = train, color = "train")) +
  geom_line(aes(y = test, color = "test")) +
  labs(x = "gamma", y = "error")

ggplot(df.rad, aes(x = s)) + 
  geom_line(aes(y = train, color = "train")) +
  geom_line(aes(y = test, color = "test")) +
  labs(x = "gamma", y = "error")

ggplot(df.sig, aes(x = s)) + 
  geom_line(aes(y = train, color = "train")) +
  geom_line(aes(y = test, color = "test")) +
  labs(x = "gamma", y = "error")
  

ggplot(df.test, aes(x = my_seq, y = seq(0.1, 1, 0.1))) +
  geom_line(aes(y=pol)) +
  geom_line(aes(y=rad)) +
  geom_line(aes(y=sig))

library(babynames)