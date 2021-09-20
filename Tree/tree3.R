library(tree)
library(maptree)
library(DAAG)
library(e1071)
library(ggplot2)
# Загрузите набор данных nsw74psid1 из пакета DAAG. Постройте регрессионное дерево
# для модели, задаваемой следующей формулой: re78 ~.. Постройте регрессионную
# модель и SVM-регрессию для данной формулы. Сравните качество построенных 
# моделей, выберите оптимальную модель и объясните свой выбор.

data(nsw74psid1)
testIdx = sample(nrow(nsw74psid1), round(nrow(nsw74psid1) / 3), replace = FALSE)
train = nsw74psid1[-testIdx,]
test = nsw74psid1[testIdx,]

t = tree(re78 ~., train)
draw.tree(t, cex=0.7)
svm = svm(re78 ~., train, type="eps-regression")  
svm.prediction = predict(svm, test)
tree.prediction = predict(t, test)

df = data.frame(id=seq(1,dim(test)[1]), test=test$re78, svm=svm.prediction, tree=tree.prediction)
earn = seq(min(test$re78), max(test$re78))

ggplot(df, aes(x=id )) +
  geom_point(aes(y=test), colour="red") +
  geom_point(aes(y=svm), colour="green")

ggplot(df, aes(x=id )) +
  geom_point(aes(y=test), colour="red") +
  geom_point(aes(y=tree), colour="yellow")
  

svm.error = mean(sqrt((svm.prediction - test$re78)^2))
tree.error = mean(sqrt((tree.prediction - test$re78)^2))
paste("svm error: ", svm.error)
paste("tree error: ", tree.error)
