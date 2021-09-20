library(kernlab)
library(kknn)

data(spam)
m <- dim(spam)[1]
mean_accuracy <- c()
#size_list <- seq(0.1, 0.9, by=0.1) * m
size_list <- c(200, 400, 500, 600, 700, 800, 900, 1000)
for (j in 1:length(size_list)){
  sample_size <- size_list[j]
  accuracy <- c()
  for (k in 1:20){
    val <- sample(1:m, size = 2500)
    spam.learn <- spam[val,]
    data.valid <- spam[-val,]
    val2 <- sample(m-2500, size = sample_size)
    spam.valid <- data.valid[val2,]
    spam.kknn <- kknn(type~., spam.learn, spam.valid)
    accuracy_table <- table(spam.kknn$fitted.values, select(spam.valid, type)[,1])
    accuracy[k] <- sum(diag(accuracy_table)) / sum(accuracy_table)
  }
  mean_accuracy[j] <- mean(accuracy)
}

library(ggplot2)
new_data <- data.frame(
  size_of_learning_dataset=size_list,  
  accuracy=mean_accuracy)
dev.off()
ggplot(new_data, aes(x=size_of_learning_dataset, y=accuracy)) + 
  geom_line() +
  geom_point()












