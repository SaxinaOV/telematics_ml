library(kknn)
library(mlbench)
library(ggplot2)
data(Glass)
Glass <- Glass[-1]
m <- dim(Glass)[1]
k_list <- c(2, 5, 7, 10, 15)
wrong_list <- list()
kernels <- c("rectangular","triangular", "epanechnikov", "biweight", 
             "triweight", "cos", "inv", "gaussian", "rank", "optimal" )
for (i in 1:length(k_list)){
  wrong_list[i] <- 0
  for (t in 1:10){
    val <- sample(1:m, size = 150, replace = FALSE, prob = rep(1/m, m))
    learn <- Glass[val,]
    valid <- Glass[-val,]
    result <- kknn(Type~., learn, valid, k=k_list[i], kernel="triangular")
    fit <- fitted(result)
    wrong <- 0
    for (j in 1:nrow(valid)){
      if (valid$Type[j] != fit[j]){
        wrong <- wrong + 1
      }
    }  
    wrong_list[i] <- unlist(wrong_list[i]) + wrong
  }
  wrong_list[i] <- unlist(wrong_list[i])/10
}
  
  
data <- data.frame(
  k= unlist(k_list),  
  errors=unlist(wrong_list)
)
ggplot(data, aes(x=k, y=errors)) + 
  geom_bar(stat = "identity")

  
for (i in 1:length(kernels)){
  wrong_list[i] <- 0
  for (t in 1:10){
    val <- sample(1:m, size = 150, replace = FALSE, prob = rep(1/m, m))
    learn <- Glass[val,]
    valid <- Glass[-val,]
    result <- kknn(Type~., learn, valid, k=5, kernel=kernels[i])
    fit <- fitted(result)
    wrong <- 0
    for (j in 1:nrow(valid)){
      if (valid$Type[j] != fit[j]){
        wrong <- wrong + 1
      }
    }  
    wrong_list[i] <- unlist(wrong_list[i]) + wrong
  }
  wrong_list[i] <- unlist(wrong_list[i])/10
}


data <- data.frame(
  k=kernels,  
  errors=unlist(wrong_list)
)
ggplot(data, aes(x=k, y=errors)) + 
  geom_col()  
  
  
  
  
  
  


























