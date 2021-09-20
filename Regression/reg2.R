library(qpcR)

setwd("/home/olga/MyProjects/Polikek/ML/Regression/datasets")

data = read.delim("reglab2.txt")
x = colnames(data)[-1]
min_RSS = 10000
rss = c()
formulas = c()
k = 1
set = c()
for (i in 1:(dim(data)[2]-1)){
  c = combn(x, i)
  d = dim(c)[2]
  for (j in 1:d){
    formula = as.formula(paste("y ~", paste(c[,j], collapse = "+")))
    fit = lm(formula, data)
    rss[k] = RSS(fit)
    formulas[k] = paste(c[,j], collapse = "+")
    k = k + 1
    if (RSS(fit) < min_RSS){
      min_RSS = RSS(fit)
      set = c[,d]
    }
  }
}
