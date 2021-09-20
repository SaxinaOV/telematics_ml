library(mlbench)
library(tree)
library(maptree)

data(Glass)
t = tree(Type~., Glass)
draw.tree(t, cex=0.7)
t = snip.tree(t, c(26,31,108))
draw.tree(t, cex=0.7)
prune.tree(t)
#RI =1.516 Na =11.7 Mg =1.01 Al =1.19 Si =72.59 K=0.43 Ca =11.44 Ba =0.02 Fe =0.1
test = data.frame("RI"=1.516, "Na"=11.7, "Mg"=1.01, "Al" =1.19,
         "Si" =72.59, "K"=0.43, "Ca" =11.44, "Ba" =0.02, "Fe" =0.1)
tree.prediction = predict(t, test)
