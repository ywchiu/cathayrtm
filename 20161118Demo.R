## iris 階層式分群

data(iris)
View(iris)
label <- iris[,5]
data  <- iris[,-5]
?hclust
fit <- hclust(dist(data), method = "ward.D2")
plot(fit)
cluster <- cutree(fit, 3)
cluster

par(mfrow=c(1,2))
plot(iris$Petal.Length, iris$Petal.Width, col = cluster)
plot(iris$Petal.Length, iris$Petal.Width, col = label)
