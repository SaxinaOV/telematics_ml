#2. Сгенерируйте 100 точек с двумя признаками X1 и X2 в соответствии 
#с нормальным распределением так, что первые 50 точек (class -1) имеют 
#параметры: мат. ожидание X1  равно 10, мат. ожидание X2 равно 14, 
#среднеквадратические отклонения для обеих переменных равны 4. 
#Вторые 50 точек (class +1) имеют параметры: мат. ожидание X1 равно 20, 
#мат. ожидание X2 равно 18, среднеквадратические отклонения для обеих 
#переменных равны 3. Построить соответствующие диаграммы, 
#иллюстрирующие данные. Построить байесовский классификатор и оценить 
#качество классификации. 
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(e1071)

data = data.frame(X1 = c(rnorm(50, mean=10, 4), rnorm(50, mean=20, 3)),
                  X2 = c(rnorm(50, mean=14, 4), rnorm(50, mean=18, 3)),
                  class = c(rep("-1",50), rep("1",50)))

ggplot(data, aes(x=X2, fill=class)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

ggplot(data, aes(x=X1, y=X2, color=class)) + 
  geom_point(size=6)

rows = sample(nrow(data))
data = data[rows, ]
train = data[1:60,]
test = data[61:100,]
bayes = naiveBayes(class~., train)
prediction = predict(bayes, test)
ac_tab = table(prediction, test$class)
correct = sum(diag(ac_tab))/sum(ac_tab)


data <- data.frame(
  type = c( rep("variable 1", 1000), rep("variable 2", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=4) )
)

# Represent it
p <- data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
