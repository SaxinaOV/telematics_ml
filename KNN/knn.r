data(spam)
m <- dim(spam)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m))
spam.learn <- spam[-val,]
spam.valid <- spam[val,]
spam.kknn <- kknn(type~., spam.learn, spam.valid, distance = 1, kernel = "triangular")
probs <- spam.kknn[["prob"]]
fits <- spam.kknn[["fitted.values"]]
sum <- 0
for (i in 1:length(val)){
  sum <- sum + probs[i, fits[i]]
}
print(sum/length(val))

