library(kknn)
library(ggplot2)
setwd("/home/olga/MyProjects/Polikek/ML/KNN/datasets")
mydata.learn <- read.delim("svmdata4.txt", sep="\t", stringsAsFactors=TRUE)
mydata.test <- read.delim("svmdata4test.txt", sep="\t", stringsAsFactors=TRUE)
mydata.train <- train.kknn(Colors~., mydata.learn, kmax=10)
plot(mydata.train)
plot(mydata.learn$X1, mydata.learn$X2, pch=21,
     bg=c("green","red") [unclass(mydata.learn$Colors)],  main="My train data")
