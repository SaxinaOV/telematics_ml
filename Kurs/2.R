setwd("/home/olga/MyProjects/Polikek/ML/Kurs")
data = read.delim("wine.data", sep = ",")
data$class = as.factor(data$class)
d = dim(data)[1]
target = data$class
s = sample(d)
data = data[s,]
m = round(d/10)
###############################
kernel = "radial"
ac_rad = rep(0,7)
accuracy_svm = 0
for (cost in seq(1,7)){
  for (i in 1:10){
    if (i == 1){
      test = data[1:m,]
      train = data[(m+1):d,]
      test = na.omit(test)
      svm = svm(class~., data=train, cost=cost, kernel=kernel)
      prediction = predict(svm, test)
      table_svm = table(test$class, prediction)
      accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
    }
    else{
      if (i == 10){
        test = data[(m*10):d,]
        train = data[1:(m*10-1),]
        test = na.omit(test)
        svm = svm(class~., data=train, cost=cost, kernel=kernel)
        prediction = predict(svm, test)
        table_svm = table(test$class, prediction)
        accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
      }
      else{
        test = data[(m*i):(m*i + m),]
        train = rbind(data[1:(m*i - 1),], data[(m*i + m + 1):d,])
        test = na.omit(test)
        svm = svm(class~., data=train, cost=cost, kernel=kernel)
        prediction = predict(svm, test)
        table_svm = table(test$class, prediction)
        accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
      }
    }
  }
  ac_rad[cost] = accuracy_svm/10
  accuracy_svm = 0
}
plot(x=seq(1,7), y=ac_rad)
############################
kernel = "polynomial"
ac_pol = rep(0,7)
accuracy_svm = 0
for (cost in seq(1,7)){
  for (i in 1:10){
    if (i == 1){
      test = data[1:m,]
      train = data[(m+1):d,]
      test = na.omit(test)
      svm = svm(class~., data=train, cost=cost, kernel=kernel)
      prediction = predict(svm, test)
      table_svm = table(test$class, prediction)
      accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
    }
    else{
      if (i == 10){
        test = data[(m*10):d,]
        train = data[1:(m*10-1),]
        test = na.omit(test)
        svm = svm(class~., data=train, cost=cost, kernel=kernel)
        prediction = predict(svm, test)
        table_svm = table(test$class, prediction)
        accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
      }
      else{
        test = data[(m*i):(m*i + m),]
        train = rbind(data[1:(m*i - 1),], data[(m*i + m + 1):d,])
        test = na.omit(test)
        svm = svm(class~., data=train, cost=cost, kernel=kernel)
        prediction = predict(svm, test)
        table_svm = table(test$class, prediction)
        accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
      }
    }
  }
  ac_pol[cost] = accuracy_svm/10
  accuracy_svm = 0
}
plot(x=seq(1,7), y=ac_pol)
##########################
kernel = "sigmoid"
ac_sig = rep(0,7)
accuracy_svm = 0
for (cost in seq(1,7)){
  for (i in 1:10){
    if (i == 1){
      test = data[1:m,]
      train = data[(m+1):d,]
      test = na.omit(test)
      svm = svm(class~., data=train, cost=cost, kernel=kernel)
      prediction = predict(svm, test)
      table_svm = table(test$class, prediction)
      accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
    }
    else{
      if (i == 10){
        test = data[(m*10):d,]
        train = data[1:(m*10-1),]
        test = na.omit(test)
        svm = svm(class~., data=train, cost=cost, kernel=kernel)
        prediction = predict(svm, test)
        table_svm = table(test$class, prediction)
        accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
      }
      else{
        test = data[(m*i):(m*i + m),]
        train = rbind(data[1:(m*i - 1),], data[(m*i + m + 1):d,])
        test = na.omit(test)
        svm = svm(class~., data=train, cost=cost, kernel=kernel)
        prediction = predict(svm, test)
        table_svm = table(test$class, prediction)
        accuracy_svm = accuracy_svm + sum(diag(table_svm))/sum(table_svm)
      }
    }
  }
  ac_sig[cost] = accuracy_svm/10
  accuracy_svm = 0
}
plot(x=seq(1,7), y=ac_sig)
#################
library(ggplot2)
df = data.frame(cost = seq(1,7),
      radial = ac_rad,
      polynomial = ac_pol,
      sigmoid = ac_sig)
ggplot(df, aes(x = cost)) +
  geom_line(aes(y=radial, color="radial")) +
  geom_line(aes(y=polynomial, color="polynomial")) +
  geom_line(aes(y=sigmoid, color="sigmoid")) +
  labs(y = "accuracy")
