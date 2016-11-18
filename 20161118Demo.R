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


# 文字分群
## 讀取資料
download.file('https://raw.githubusercontent.com/ywchiu/rtibame/master/History/Class1/news.RData', 'new.RData')
load('new.RData')
View(news)

## Jieba 切詞, 轉詞頻矩陣
library(jiebaR)
str(news)
news$V2     <- as.character(news$V2)
news$V1     <- as.character(news$V1)
mixseg      <- worker(user = '/home/trainee/user50/user.dict.utf8')
news.seg    <- lapply(news$V2, function(e) mixseg <= e)
news.corpus <- Corpus(VectorSource(news.seg))
news.dtm    <- DocumentTermMatrix(news.corpus, control=list(wordLengths=c(2,Inf)))
news.dtm

## 計算文字距離
library(proxy)
news.dist <- proxy::dist(as.matrix(news.dtm), method = 'cosine')
news.mat  <- as.matrix(news.dist)
dim(news.mat)

## 文章分群
fit <- hclust(news.dist, method = "ward.D2")
par(mfrow=c(1,1))
plot(fit)
cluster <- cutree(fit, 6)
news[cluster == 1, 'V1']
