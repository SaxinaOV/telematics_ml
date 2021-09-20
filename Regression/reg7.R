library(datasets)
library(forecast)
library(TSstudio)

data = sunspot.year
df = data.frame(year=time(data), sunspots=as.matrix(data))
reg = lm(sunspots ~., df)
ggplot(df, aes(x=year, y=sunspots)) +
  geom_line() +
  stat_smooth(method = "lm", col = "red")
summary(reg)