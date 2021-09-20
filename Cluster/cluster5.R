library(cluster)
library(factoextra)

setwd("/home/olga/MyProjects/Polikek/ML/Cluster/datasets")

data = read.delim("seeds_dataset.txt")
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07") 
pairs(data[,1:8], pch = 19,  cex = 0.5,
      col = my_cols[data$sort])

sorts = data[,ncol(data)] 
data = data[,-ncol(data)]

cl = clara(data, k = 3)
fviz_cluster(cl, data = data,
             geom = "point", 
             ggtheme = theme_bw()
)
plot(cl)

set.seed(12345)
data2 = subset(data, select = -c(A,P))
k2 = kmeans(data2, centers = 3)
k2$betweenss/k2$totss * 100
tab = table(k2$cluster, sorts)
tab
cl = clara(data, k = 3)
fviz_cluster(cl, data = data,
             geom = "point", 
             ggtheme = theme_bw()
)
pca <- prcomp(data, center = TRUE, scale = TRUE)
summary(pca)
pca$rotation
biplot(pca)
