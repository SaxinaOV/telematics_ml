library(tree)
library(maptree)
library(ggplot2)

setwd("/home/olga/MyProjects/Polikek/ML/Tree/datasets")
data = read.delim("Lenses.txt", sep="")
data$type = factor(data$type)
data = data[,-1]
tree = tree(type~., data)
draw.tree(tree)
