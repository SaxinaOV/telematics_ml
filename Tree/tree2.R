library(tree)
library(maptree)
library(DAAG)


# 2) Загрузите набор данных spam7 из пакета DAAG. Постройте дерево классификации 
# для модели, задаваемой следующей формулой: yesno ~., дайте интерпретацию 
# полученным результатам. Запустите процедуру “cost-complexity prunning” с выбором
# параметра k по умолчанию, method = ’misclass’, выведите полученную 
# последовательность деревьев. Какое из полученных деревьев, на Ваш взгляд, 
# является оптимальным? Объясните свой выбор.

data(spam7)
t = tree(yesno~., spam7)
draw.tree(t)
t_snp = snip.tree(t, c(11))
draw.tree(t_snp)
t2 = prune.tree(t, method = "misclass")
t3 = prune.tree(t, k = 864)
draw.tree(t3)

rows = sample(nrow(spam7))
spam7 = spam7[rows, ]
train = spam7[1:4000,]
test = spam7[4001:4601,]
tr = tree(yesno~., train)
pr = predict(tr, test)
prediction = c()
for (i in 1:dim(pr)[1]){
  if (pr[i,1] > pr[i,2]){
    prediction[i] = "n"
  }
  else{
    prediction[i] = "y"
  }
}
tab = table(test$yesno, prediction)
error = sum(diag(tab))/sum(tab)

