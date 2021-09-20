library(e1071)
library(ggplot2)
library(kknn)
library(Rtsne)
library(tree)
library(maptree)
library(cluster)
library(factoextra)
library(dendextend)
library(glmnet)
library(pvclust)

setwd("/home/olga/MyProjects/Polikek/ML/Kurs")
data = read.delim("wine.data", sep = ",")

d = dim(data)[1]
target = data$class
data$class = as.factor(target)
#####################################
#t-sne
####################################
data = subset(data, select = -c(class))
tsne = Rtsne(data, dims=1)
plot(tsne$Y , t='n')
text(tsne$Y,labels=target,col = target)

names(target) = seq(1:d)
data$class = as.factor(target)
#####################################
#KNN
####################################
set.seed(1234)
s = sample(d, size = 0.8 * d)
train = data[s,]
test = data[-s,]

kernels <- c("rectangular","triangular", "epanechnikov", "biweight", "cos", "inv", "gaussian", "rank", "optimal" )
result <- train.kknn(class~., train, kmax=20, distance = 1,
                     kernel=kernels)
plot(result)
knn = kknn(class~., train, test, k=7, kernel="triangular", distance = 1)
table_knn = table(test$class, knn$fitted.values)
table_knn
accuracy_knn = sum(diag(table_knn))/sum(table_knn)
accuracy_knn
prediction = predict(result, test[,-1])
t = table(prediction, as.numeric(test$class))
sum(diag(t))/sum(t)
#####################################
#SVM
####################################
s = sample(d)
data = data[s,]
m = round(d/10)
accuracy_svm = 0
kernel = "polynomial"
cost = 100
for (i in 1:10){
  if (i == 1){
    test = data[1:m,]
    train = data[(m+1):d,]
    test = na.omit(test)
    svm = svm(class~., data=train)
    prediction = predict(svm, test)
    table_svm = table(test$class, prediction)
    accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
  }
  else{
    if (i == 10){
      test = data[(m*10):d,]
      train = data[1:(m*10-1),]
      test = na.omit(test)
      svm = svm(class~., data=train)
      prediction = predict(svm, test)
      table_svm = table(test$class, prediction)
      accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
    }
    else{
      test = data[(m*i):(m*i + m),]
      train = rbind(data[1:(m*i - 1),], data[(m*i + m + 1):d,])
      test = na.omit(test)
      svm = svm(class~., data=train)
      prediction = predict(svm, test)
      table_svm = table(test$class, prediction)
      accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
    }
  }
}
accuracy_svm = accuracy_svm/10
accuracy_svm
#####################################
#tree
####################################

accuracy_tree = 0
for (i in 1:10){
  if (i == 1){
    test = data[1:m,]
    train = data[(m+1):d,]
    test = na.omit(test)
    tree = tree(class~., train)
    prediction = predict(tree, test)
    error = 0
    for (i in 1:length(test$class)){
      if (prediction[i, test$class[i]] != 1){
        error = error + 1
      }
    }
    accuracy_tree = accuracy_tree + 1 - error/dim(test)[1]
  }
  else{
    if (i == 10){
      test = data[(m*10):d,]
      train = data[1:(m*10-1),]
      test = na.omit(test)
      tree = tree(class~., train)
      prediction = predict(tree, test)
      error = 0
      for (i in 1:length(test$class)){
        if (prediction[i, test$class[i]] != 1){
          error = error + 1
        }
      }
      accuracy_tree = accuracy_tree + 1 - error/dim(test)[1]
    }
    else{
      test = data[(m*i):(m*i + m),]
      train = rbind(data[1:(m*i - 1),], data[(m*i + m + 1):d,])
      test = na.omit(test)
      tree = tree(class~., train)
      prediction = predict(tree, test)
      error = 0
      for (i in 1:length(test$class)){
        if (prediction[i, test$class[i]] != 1){
          error = error + 1
        }
      }
      accuracy_tree = accuracy_tree + 1 - error/dim(test)[1]
    }
  }
}
accuracy_tree/10
draw.tree(tree)

tree = tree(class~., train)
prediction = predict(tree, test)
error = 0
for (i in 1:length(test$class)){
  if (prediction[i, test$class[i]] != 1){
    error = error + 1
  }
}
1 - error/dim(test)[1]
#####################################
#cluster
####################################
data = read.delim("wine.data", sep = ",")
target = data$class
data = subset(data, select = -c(class))
d = dist(data)
dend = hclust(d, method = "centroid")
plot(dend, cex=0.4)
groups = cutree(dend, k=3)
rect.hclust(dend, k = 3, border = "green")
t = table(groups, target)
1 - (sum(diag(t)) / sum(t))

#####################################
#lasso
####################################
x = as.matrix(data[,-1])
y = as.matrix(data[,1])
lambda_seq <- 10^seq(2, -2, by = -.1)
cv_output <- cv.glmnet(x, y, alpha = 1, 
                       lambda = lambda_seq, nfolds = 5)
plot(cv_output)
best_lam <- cv_output$lambda.min
best_lam
lasso_best <- glmnet(x, y, alpha = 1, lambda = best_lam, label=TRUE)
coef(lasso_best)


