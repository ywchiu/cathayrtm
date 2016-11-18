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
library(tm)
str(news)
news$V2     <- as.character(news$V2)
news$V1     <- as.character(news$V1)
mixseg      <- worker(user = '/home/trainee/user50/user.dict.utf8')
news.seg    <- lapply(news$V2, function(e) mixseg <= e)
news.corpus <- Corpus(VectorSource(news.seg))


## 或可以自製Transformer
removeen <- content_transformer(
  function(x, pattern){
    return(x[grepl('^[\u4e00-\u9fa5]+$',x)])
  }
)

doc         <- tm_map(news.corpus, removeen)
news.dtm    <- DocumentTermMatrix(doc, control=list(wordLengths=c(2,Inf)))
news.dtm

## 計算文字距離
library(proxy)
news.dist <- proxy::dist(as.matrix(news.dtm), method = 'cosine')
news.mat  <- as.matrix(news.dist)
dim(news.mat)

## 文章分群(階層式)
fit <- hclust(news.dist, method = "ward.D2")
par(mfrow=c(1,1))
plot(fit)
cluster <- cutree(fit, 6)
news[cluster == 1, 'V1']

table(cluster)

## 文章分群(KMEANS)
fit2 <- kmeans(news.dist, 6)
fit2$cluster
news[fit2$cluster ==6,  'V1']

## 繪製分群文字雲
library(wordcloud2)
colsum.dtm <- colSums(as.matrix(news.dtm[cluster == 1, ]) )
tb <- colsum.dtm[order(colsum.dtm, decreasing = TRUE)][1:100]
wordcloud2(as.table(tb))




## 繪製分群結果
rect.hclust(fit, 6)


## 找出相似文章
news[order(news.mat[7,])[1:10], 'V1']
article.query=function(idx){
  news[news.mat[idx,] < 0.8, 'V1']
}

article.query(5)[1:10]

# 使用naive bayes 做文章分類

## 產生1,500篇文章詞頻矩陣
download.file('https://raw.githubusercontent.com/ywchiu/rtibame/master/data/applenews.RData', destfile = 'appledaily.RData')
load('appledaily.RData')
View(applenews)
str(applenews)

## 挑選娛樂與財經　
apple.subset <- applenews[applenews$category%in%c('財經', '娛樂', '社會'),]


## 或可以自製Transformer
removeen <- content_transformer(
  function(x, pattern){
    return(x[grepl('^[\u4e00-\u9fa5]+$',x)])
  }
)

library(jiebaR)
mixseg    <- worker(user = '/home/trainee/user50/user.dict.utf8')
apple.seg <- lapply(apple.subset$content, function(e) mixseg <= e)
s.corpus  <- Corpus(VectorSource(apple.seg))

## 字詞的清理
doc       <- tm_map(s.corpus, removeNumbers)
doc       <- tm_map(doc, removeen)
dtm       <- DocumentTermMatrix(doc, control=list(wordLengths=c(2,Inf)))
dtm

## 挑選詞頻大於五的
ft<-findFreqTerms(dtm, 5)
control.list <- list(wordLengths=c(2,Inf),dictionary=ft)
new.dtm <- DocumentTermMatrix(Corpus(VectorSource(doc)),control=control.list)
new.dtm

## 只列出是否有對到該詞的
dtm.count<-apply(new.dtm, MARGIN =2, convert_counts)
dim(dtm.count)

## 將資料列為訓練跟測試資料集
set.seed(123)
idx <- sample.int(2, nrow(dtm.count),replace=TRUE, prob = c(0.7, 0.3))
table(idx)

### 訓練跟測試資料集Feature
m <- as.data.frame(dtm.count)
trainset <- m[idx == 1, ]
testset  <- m[idx == 2, ]

### 訓練跟測試資料集Label
traintag <- as.factor(apple.subset[idx == 1, 'category'])
testtag  <- as.factor(apple.subset[idx == 2, 'category'])


## 做機器學習
library(e1071)
fit  <- naiveBayes(trainset, traintag)
pred <- predict(fit, testset)

dim(dtm.count)

tb <- table(pred, testtag)
(29 + 31) / (29 + 31 + 3)

library(caret)
confusionMatrix(tb)

apple.subset[which((pred== '娛樂') & (testtag =='財經')),'title']

## 建立詞頻矩陣
e1 ='this is a book'
e2 ='this is my car'
e.vec    <- strsplit(c(e1, e2), ' ')
e.corpus <- Corpus(VectorSource(e.vec))
e.dtm    <- DocumentTermMatrix(e.corpus)
inspect(e.dtm)

convert_counts <- function(x){
  x <-ifelse(x > 0, 1, 0)
  x <-factor(x, levels=c(0, 1), labels=c("No", "Yes"))
  return(x)
}

apply(e.dtm, MARGIN =2, convert_counts)
