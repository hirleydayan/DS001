install.packages("fpc")
install.packages("fclust")
install.packages("NbClust")
install.packages("ppclust")
install.packages("cluster")
install.packages("mlbench")
install.packages("kernlab")
install.packages("gridExtra")
install.packages("factoextra")

library(ggplot2)
library(datasets)
data(iris)
summary(iris)
d <- dist(iris[, 3:4], method="euclidean"); d
heatmap(as.matrix(d), symm=TRUE)
clusters <- hclust(d, method="complete"); clusters
clusters_cut <- cutree(clusters , k=3); clusters_cut
table(clusters_cut, iris$Species) 

ggplot(iris, aes(Petal.Length, Petal.Width,color=iris$Species)) +
    geom_point(alpha=0.4, size=3.5) +
    geom_point(col=clusters_cut) +
    scale_color_manual(values=c('black', 'red', 'green'))

clusters <- hclust(d, method='average')
plot(clusters)





cl2 <- kmeans(iris[,1:4], 3, nstart=20)
cl2
library(cluster)
dissE <- daisy(iris[,1:4])
sk <- silhouette(cl2$cl, dissE)
plot(sk)
library(factoextra)
fviz_silhouette(sk)


library(NbClust)
# remove coluna com ŕotulos das classes e escala os dados
iris.scaled <- scale(iris[, -5])
nb <- NbClust(iris.scaled , distance="euclidean",
              min.nc=2, max.nc=10, method="complete",index="all")

library(factoextra)
fviz_nbclust(nb) + theme_minimal()


#####

library(cluster)
library(factoextra)
library(NbClust)
data(USArrests)
df <- USArrests 
df <- na.omit(df) 
df <- scale(df)
head(df, n=3)
set.seed(123)
k2 <- kmeans(df, centers=2, nstart=25)
k2

dd <- cbind(USArrests , cluster=k2$cluster)
head(dd)
k2$size
k2$cluster
head(k2$cluster , 4)
k2$centers
fviz_cluster(k2, data=df)

k3 <- kmeans(df, centers=3, nstart=25)
k4 <- kmeans(df, centers=4, nstart=25)
k5 <- kmeans(df, centers=5, nstart=25)

p1 <- fviz_cluster(k2, geom="point", data=df) + ggtitle("k=2")
p2 <- fviz_cluster(k3, geom="point", data=df) + ggtitle("k=3")
p3 <- fviz_cluster(k4, geom="point", data=df) + ggtitle("k=4")
p4 <- fviz_cluster(k5, geom="point", data=df) + ggtitle("k=5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=2)

set.seed(123)
fviz_nbclust(df, kmeans, method="wss")
fviz_nbclust(df, kmeans, method="silhouette")

## Wine

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep= ",")
wine.stand <- scale(wine[-1])
km <- kmeans(wine.stand , 3)
km$center
km$cluster
km$size
library(cluster)
clusplot(wine.stand, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
table(wine[,1], km$cluster)

d <- dist(wine.stand, method="euclidean")
h <- hclust(d, method="ward") # Com media ponderada de distancia 
plot(h)

groups <- cutree(h, k=3)
table(wine[,1], groups)
