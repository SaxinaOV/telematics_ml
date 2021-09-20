library(cluster)
library(scatterplot3d)
library("plot3D")
library(factoextra)

n = 50
mmax = 150
mmin = 0
sdmax = 50
sdmin = 5

data = data.frame(x = c(rnorm(n, mmax, sdmax), rnorm(n, mmin, sdmin), rnorm(n, mmin, sdmin)),
                  y = c(rnorm(n, mmin, sdmin), rnorm(n, mmax, sdmax), rnorm(n, mmin, sdmin)),
                  z = c(rnorm(n, mmin, sdmin), rnorm(n, mmin, sdmin), rnorm(n, mmax, sdmax)))
scatterplot3d(x = data$x, y = data$y, z = data$z)
scatter3D(x = data$x, y = data$y, z = data$z)

cl_ef = clara(data, k = 3, metric = "euclidean", stand = FALSE)
cl_mf = clara(data, k = 3, metric = "manhattan", stand = FALSE)
cl_et = clara(data, k = 3, metric = "euclidean", stand = TRUE)
cl_mt = clara(data, k = 3, metric = "manhattan", stand = TRUE)
cl_jf = clara(data, k = 3, metric = "jaccard", stand = FALSE)
cl_jt = clara(data, k = 3, metric = "jaccard", stand = TRUE)
fviz_cluster(cl_ef, data = data,
             geom = "point", 
             ggtheme = theme_bw()
)
plot(cl_ef)
plot(cl_mf)
plot(cl_et)
plot(cl_mt)
plot(cl_jf)
plot(cl_jt)
plot(data, col=cl_ef$clustering)
