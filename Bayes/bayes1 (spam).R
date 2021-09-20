library(e1071)
library(ggplot2)
library(kernlab)

data(spam)
size <- seq(0.1, 0.5, by=0.1)
n = dim(spam)[1]
mean_accuracy <- c()
for (j in 1:length(size)){
  accuracy <- c()
  s = n * size[j]
  for (k in 1:10){
    idx <- sample(1:dim(spam)[1], s)
    spamtrain <- spam[-idx, ][1:2000,]
    spamtest <- spam[idx, ]
    model <- naiveBayes(type ~ ., data = spamtrain)
    accuracy_table <- table(predict(model, spamtest), spamtest$type)
    #predict(model, spamtest, type = "raw")
    accuracy[k] <- sum(diag(accuracy_table)) / sum(accuracy_table)
  }
  mean_accuracy[j] <- mean(accuracy)
}

new_data <- data.frame(
  size_of_testing_dataset=size * n,  
  accuracy=mean_accuracy)
ggplot(new_data, aes(x=size_of_testing_dataset, y=accuracy)) + 
  geom_line() +
  geom_point()

