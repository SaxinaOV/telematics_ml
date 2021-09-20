library(cluster)

data = votes.repub
plot(agnes(votes.repub))
states = row.names(data)
means = c()
for (i in 1:length(states)){
  means[i] = mean(unlist(data[states[i],]), na.rm=TRUE)
}
rank = mapply(c, states, means , SIMPLIFY = FALSE )
df = data.frame(states=states, means=means)
df = df[order(df$means),]
