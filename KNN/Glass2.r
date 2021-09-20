library(kknn)
library(mlbench)
library(ggplot2)
data(Glass)
kernels <- c("rectangular","triangular", "epanechnikov", "biweight", 
             "triweight", "cos", "inv", "gaussian", "rank", "optimal" )
result <- train.kknn(Type~., Glass, kmax=10, 
          kernel=c("rectangular","triangular", "epanechnikov", "biweight", 
          "triweight", "cos", "inv", "gaussian", "rank", "optimal" ))
plot(result)

misclassification <- c()
for (i in 1:10){
  result <- train.kknn(Type~., Glass, distance=i, ks=c(8), kernel="biweight")
  misclassification[i] <- result$MISCLASS
}
plot(x=seq(1,10), y=misclassification)

example <- list(RI =1.516, Na =11.7, Mg =1.01, Al =1.19, Si =72.59,
                K=0.43, Ca =11.44, Ba =0.02, Fe =0.1 )

example_kknn <- kknn(Type~., Glass, example)
print(example_kknn$fitted.values, max.levels = 0)
print(example_kknn$prob[,"5"])
for (i in 1:length(example)){
  print(names(example)[i])
  new_ex <- example[-i]
  new_Glass <- Glass[-i]
  example_kknn <- kknn(Type~., new_Glass, new_ex)
  print(example_kknn$fitted.values, max.levels = 0)
  print(example_kknn$prob[,example_kknn$fitted.values])
  new_ex <- example
  new_Glass <- Glass
}
