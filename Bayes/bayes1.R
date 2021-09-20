library(e1071)
library(ggplot2)

setwd("/home/olga/MyProjects/Polikek/ML/Bayes/datasets")
A_raw <- read.table("Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
m <- dim(A_raw)[1]
size <- seq(0.1, 0.5, by=0.1)
set.seed(12345)
mean_accuracy <- c()
for (j in 1:length(size)){
  A_rand <- A_raw[ order(runif(m)), ]
  nt <- as.integer(m*size[j])
  accuracy <- c()
  for (k in 1:10){
    A_test <- A_rand[1:nt, ]
    #A_test <- A_rand[(nt+1):n, ]
    A_train <- A_rand[m-400:m, ]
    A_classifier <- naiveBayes(V10 ~ ., data = A_train)
    A_predicted <- predict(A_classifier, A_test)
    accuracy_table <- table(A_predicted, A_test$V10)
    accuracy[k] <- sum(diag(accuracy_table)) / sum(accuracy_table)
  }
  mean_accuracy[j] <- mean(accuracy)
}

new_data <- data.frame(
  size_of_testing_dataset=size * m,  
  accuracy=mean_accuracy)
ggplot(new_data, aes(x=size_of_testing_dataset, y=accuracy)) + 
  geom_line() +
  geom_point()

