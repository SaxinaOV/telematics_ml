library(e1071)
library(rpivotTable)
library(ggplot2)

setwd("/home/olga/MyProjects/Polikek/ML/SVM/datasets")
dataSVM = read.delim("svmdata1.txt", stringsAsFactors=TRUE)

# trainIdx = sample(nrow(dataSVM), nrow(dataSVM) / 2, replace = FALSE)
# dataTrain = dataSVM[trainIdx, ]
# dataValid = dataSVM[-trainIdx, ]
svm = svm(Colors~., data=dataTrain, type="C-classification", cost=1, kernel="linear")
# predictionsTest=predict(svm, dataValid)
# plot(svm, dataValid, X1~X2, svSymbol = 1, dataSymbol = 4, grid = 250, symbolPalette=c("Green", "Red"))
svm$tot.nSV
# ac_table = table(dataValid$"Color", predictionsTest)
# ac_table
# sum(diag(ac_table)) / sum(ac_table)
# 

dataTest = read.delim("svmdata1test.txt", stringsAsFactors=TRUE)
# 
# #svm = svm(Colors~., data=dataTrain, type="C-classification", cost=1, kernel="linear")
# predictionsTest=predict(svm, dataTest)
# plot(svm, dataTest, X1~X2, svSymbol = 1, dataSymbol = 4, grid = 250, symbolPalette=c("Green", "Red"))
# svm$tot.nSV
# ac_table = table(dataTest$"Color", predictionsTest)
# ac_table
# sum(diag(ac_table)) / sum(ac_table)

testIdx = sample(nrow(dataSVM), nrow(dataSVM) / 2, replace = FALSE)
dataTest = dataSVM[testIdx, ]
dataSVM = dataSVM[-testIdx, ]

g = 1
svm = svm(Colors~., data=dataTest, cost=1, kernel="polynomial", gamma=g)
predictionsTest=predict(svm, dataTest)
# plot(svm, dataTest, X1~X2, svSymbol = 1, dataSymbol = 4, grid = 250, symbolPalette=c("Green", "Red"))
ac_table = table(dataTest$"Color", predictionsTest)
ac_table
error_pol = 1 - sum(diag(ac_table)) / sum(ac_table)
error_pol
# 
# svm = svm(Colors~., data=dataSVM, type="C-classification", cost=1, kernel="radial", gamma=g)
# predictionsTest=predict(svm, dataTest)
# plot(svm, dataTest, X1~X2, svSymbol = 1, dataSymbol = 4, grid = 250, symbolPalette=c("Green", "Red"))
# ac_table = table(dataTest$"Color", predictionsTest)
# error_rad = 1 - sum(diag(ac_table)) / sum(ac_table)
# 
# svm = svm(Colors~., data=dataSVM, type="C-classification", cost=1, kernel="sigmoid", gamma=g)
# predictionsTest=predict(svm, dataTest)
# plot(svm, dataTest, X1~X2, svSymbol = 1, dataSymbol = 4, grid = 250, symbolPalette=c("Green", "Red"))
# ac_table = table(dataTest$"Color", predictionsTest)
# error_sigm = 1 - sum(diag(ac_table)) / sum(ac_table)
# 
# er_table = data.frame(kernel = c("polynomial", "radial", "sigmoid"), classification_error = c(error_pol, error_rad, error_sigm))
# ggplot(er_table, aes(x=kernel, y=classification_error)) + 
#    geom_bar(stat = "identity")
# # 
#  print(er_table)
#  