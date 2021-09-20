library(kknn)
library(ggplot2)
library(dplyr)
setwd("/home/olga/MyProjects/Polikek/ML/KNN/datasets")

data <- read.delim("Tic_tac_toe.txt", sep=",", head=FALSE)
data$V10 <- as.factor(data$V10)
m <- dim(data)[1]
mean_accuracy <- c()
#size_list <- seq(0.1, 0.8, by=0.1) * m
size_list <- c(100, 200, 300, 400, 500)
for (j in 1:length(size_list)){
  sample_size <- size_list[j]
  accuracy <- c()
  for (k in 1:20){
    val <- sample(1:m, size = 400)
    data.learn <- data[val,]
    data2 <- data[-val,]
    val2 <- sample(m-400, size = sample_size)
    data.valid <- data2[val2,]
    data.kknn <- kknn(V10~., data.learn, data.valid)
    accuracy_table <- table(data.kknn$fitted.values, select(data.valid, V10)[,1])
    accuracy[k] <- sum(diag(accuracy_table)) / sum(accuracy_table)
  }
  mean_accuracy[j] <- mean(accuracy)
}


new_data <- data.frame(
  size_of_learning_dataset=size_list,  
  accuracy=mean_accuracy
)
ggplot(new_data, aes(x=size_of_learning_dataset, y=accuracy)) + 
  geom_line() +
  geom_point()
#plot(unlist(size_list), unlist(mean_accuracy), "o")
