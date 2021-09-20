library(cluster)
library(factoextra)

c1 = kmeans(pluton, centers = 3, iter.max = 1)
c2 = kmeans(pluton, centers = 3, iter.max = 2)
c3 = kmeans(pluton, centers = 3, iter.max = 3, 
            algorithm = "Hartigan-Wong",trace=TRUE )
summary(c3)
fviz_cluster(c2, data = pluton,
             geom = "point", 
             ggtheme = theme_bw()
)
plot(pluton, col = c2$cluster)
