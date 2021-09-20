library(e1071)
library(rpivotTable)
library(ggplot2)

mse=function(l1, l2){
  s = 0
  for (i in 1:length(l1)){
    s = s + (l1[i] - l2[i])^2
  }
  s = s / length(l1)
  return(s)
}

setwd("/home/olga/MyProjects/Polikek/ML/SVM/datasets")
data = read.delim("svmdata6.txt")
plot(data$X, data$Y)
trainIdx = sample(nrow(data), nrow(data) / 2, replace = FALSE)
dataTrain = data[trainIdx, ]
dataTest = data[-trainIdx, ]

mse_c = c()
epsilon=seq(0.01, 1, 0.05)
j = 1
for (i in epsilon){
  svm = svm(Y~., data=dataTrain, type="eps-regression", epsilon=i, cost=1, kernel="radial")  
  predictionsTest=predict(svm, dataTest)
  mse_c[j] = mse(dataTest$Y, predictionsTest)
  j = j + 1
}
MSE=mse_c
plot(epsilon, MSE)

# svm$tot.nSV
svm = svm(Y~., data=dataTrain, type="eps-regression", epsilon=0.0001, cost=1, kernel="radial")  
predictionsTest=predict(svm, dataTest)
plot(dataTest$X, predictionsTest)


