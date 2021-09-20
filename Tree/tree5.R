library(tree)
library(maptree)
library(ggplot2)

setwd("/home/olga/MyProjects/Polikek/ML/Tree/datasets")
train = read.delim("svmdata4.txt", sep="", stringsAsFactors = TRUE)
test = read.delim("svmdata4test.txt", sep="", stringsAsFactors = TRUE)
tr = tree(Colors~., train)
draw.tree(tr)
ggplot(test, aes(x=X1, y=X2, color=Colors)) + 
  geom_point(size=6, alpha=0.7)
pr = predict(tr, test)
error = 0
for (i in 1:length(test$Colors)){
  if (pr[i, test$Colors[i]] != 1){
    error = error + 1
  }
}
print(error/length(test$Colors))
