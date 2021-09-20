library(datasets)
library(tidyr)

data = data.frame(year=as.numeric(time(EuStockMarkets)), 
                  price=as.matrix(EuStockMarkets))
df = gather(data, key=measure, value=Rate, c("price.DAX", "price.SMI", "price.CAC", "price.FTSE"))

ggplot(df, aes(x=year, y = Rate, group = measure, colour = measure)) + 
  geom_line() +
  labs(x = "Year", y = "Price", color = "Market") 

all = data.frame(Year=rep(data$year, 4), 
                 Price=c(data$price.CAC, data$price.DAX, data$price.FTSE, data$price.SMI))

ggplot(data, aes(x = year)) +
  geom_line(aes(y = price.DAX)) +
  geom_line(aes(y = price.SMI)) +
  stat_smooth(method = "lm", col = "red")

ggplot(all, aes(x=Year, y=Price)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

reg.CAC = lm(price.CAC ~ year, data)
reg.DAX = lm(price.DAX ~ year, data)
reg.FTSE = lm(price.FTSE ~ year, data)
reg.SMI = lm(price.SMI ~ year, data)
reg = lm(price ~ year, all)

summary(reg.CAC)
summary(reg.DAX)
summary(reg.FTSE)
summary(reg.SMI)
summary(reg)
reg.CAC$coefficients
reg.DAX$coefficients
reg.FTSE$coefficients
reg.SMI$coefficients
