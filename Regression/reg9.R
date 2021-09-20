data = cars
plot(data)
reg = lm(dist ~ speed, data)
summary(reg)
lines(data$speed, predict(reg), col = 'red')

predict(reg, list(speed=40))
        