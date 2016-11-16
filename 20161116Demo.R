## 使用Jieba 斷詞並計算詞頻

article <- '川普當選美國總統，但他與家族的商業王國，要如何做好利益迴避成為敏感話題。
《每日郵報》報導，準第一千金伊凡卡（Ivanka Trump）在川普當選後，與老爸一起上新聞節目「60分鐘」(60 Minutes)接受採訪，穿戴價值1萬美元（約32萬台幣）的高檔手鐲，事後竟不避嫌的透過公司員工，發電郵通知時尚記者手鐲的細節、購買方式等，引發各界砲轟。
有網友在推特上怒批：「川普的珠寶公司，根本就在利用節目宣傳，貪腐和利益衝突要開始啦」、「這家族的貪婪真是讓我感到噁心，根本有病！」，也有網友寫道：「意外嗎？我猜他們幾年前就想這樣搞，而我只是一介平民」。
對此，伊凡卡川普的品牌總監克蘭（Abigail Klem）表示，該則媒體通知是由專業的行銷雇員所發出，遵循業界協議規範，也坦承公司在選後正在做政策調整：「我們積極討論新的政策程序，與所有夥伴一同向前」。
其實伊凡卡曾在川普競選總統期間，利用共和黨黨內大會的場合，推銷自己身穿的套裝，但這次的手鐲爭議，可說是在川普正式當選後第一次發生。目前伊凡卡本人並沒有對事件作出回應。（林孝儒／綜合外電報導）'

library(jiebaR)
mixseg <- worker(user = '/home/trainee/user50/user.dict.utf8')
word   <- mixseg <= article
tb     <- table(word)

## 繪製文字雲
library(wordcloud2)
tb2 <- tb[(nchar(names(tb)) >= 2) &  tb >=2]
tb3 <- tb2[grepl('[\u4e00-\u9fa5]+', names(tb2))] 
wordcloud2(as.table(tb3), shape = 'star')



## 計算 tf-idf
a   <- c("a")
abb <- c("a", "b", "b")
abc <- c("a", "b", "c")
D   <- list(a, abb, abc)

### tfidf('a',a,D)
tf  <- 1 /1
idf <- log(3/3)
tf * idf 

### tfidf('b',abb,D)
tf  <- 2/3
idf <- log(3/2)
tf * idf 

### tfidf('b',abc,D)
tf  <- 1/3
idf <- log(3/2)
tf * idf

### tfidf('c',abc,D)
tf  <- 1/3
idf <- log(3/1)
tf * idf


### tfidf('a',abc,D)
tf  <- 1/3
idf <- log(3/3)
tf * idf


### 建立 tf-idf 計算函式
a   <- c("a")
abb <- c("a", "b", "b")
abc <- c("a", "b", "c")
D   <- list(a, abb, abc)

t   <- 'a'
d   <-  abc
tfidf<-function(t,d, D){
  tf  <- length(which(d == t)) / length(d)
  idf <- log(length(D) / sum(sapply(D, function(d) t %in% d)))
  tf * idf
}

tfidf('a',a,D)
tfidf('a',abc,D)
tfidf('b',abb,D)
tfidf('b',abc,D)
tfidf('c',abc,D)



## tf-idf 實作

library(jiebaR)
mixseg <- worker(user = '/home/trainee/user50/user.dict.utf8')
a <- '樂見川普實踐反恐政見　敘總統稱美「天然盟友」'
b <- '川普當選美國總統，杜特蒂：不用再和美國吵架了'
c <- '靠爸太明顯！川普女上節目曝自家手鐲挨轟'
a1 <- mixseg <= a
b1 <- mixseg <= b
c1 <- mixseg <= c
D <- list(a1,b1,c1)
tfidf('川普', a1, D)

sort(sapply(b1, function(t) tfidf(t, b1, D)), decreasing = TRUE)


## 建立詞頻向量
e3     <- 'Hello, I am David. I have taken over 100 courses ~~~' 
e3.vec <- strsplit(e3, ' ')
e3.corpus =Corpus(VectorSource(e3.vec))
e3.dtm =DocumentTermMatrix(e3.corpus)
inspect(e3.dtm)


dtm=DocumentTermMatrix(e3.corpus, control=list(wordLengths=c(1, 20)))
inspect(dtm)

getTransformations()


doc <- tm_map(e3.corpus, removeNumbers)
doc <- tm_map(doc, removePunctuation)
dtm <- DocumentTermMatrix(doc)
inspect(dtm)


## 或可以自製Transformer
removetilde <- content_transformer(
  function(x, pattern){
    return(gsub("~", "", x))
  }
)


doc <- tm_map(e3.corpus, removetilde)
dtm <- DocumentTermMatrix(doc)
inspect(dtm)

## 建立詞頻矩陣
e1 ='this is a book'
e2 ='this is my car'
e.vec    <- strsplit(c(e1, e2), ' ')
e.corpus <- Corpus(VectorSource(e.vec))
e.dtm    <- DocumentTermMatrix(e.corpus)
inspect(e.dtm)


##建立中文的詞頻矩陣
library(jiebaR)
mixseg=worker(user = '/home/trainee/user50/user.dict.utf8')
s ="大巨蛋案對市府同仁下封口令？柯P否認"
s1 ="柯P市府近來飽受大巨蛋爭議"
s.vec    <- list(mixseg <= s, mixseg<= s1)
s.corpus <- Corpus(VectorSource(s.vec))
s.dtm    <- DocumentTermMatrix(s.corpus, control=list(wordLengths=c(1, 20)))
inspect(s.dtm)

## 產生1,500篇文章詞頻矩陣
download.file('https://raw.githubusercontent.com/ywchiu/rtibame/master/data/applenews.RData', destfile = 'appledaily.RData')
load('appledaily.RData')
View(applenews)
str(applenews)


## 或可以自製Transformer
removeen <- content_transformer(
  function(x, pattern){
    return(x[grepl('^[\u4e00-\u9fa5]+$',x)])
  }
)

library(jiebaR)
mixseg    <- worker(user = '/home/trainee/user50/user.dict.utf8')
apple.seg <- lapply(applenews$content, function(e) mixseg <= e)
s.corpus  <- Corpus(VectorSource(apple.seg))
doc       <- tm_map(s.corpus, removeNumbers)
doc       <- tm_map(doc, removeen)
s.dtm     <- DocumentTermMatrix(doc, control=list(wordLengths=c(2,Inf)))
#inspect(s.dtm[1,1])
dim(s.dtm)
s.dtm$dimnames$Terms

## 詞頻矩陣操作
findFreqTerms(s.dtm, lowfreq = 200 )
findAssocs(s.dtm, "坎城", 0.9)


dtm.remove <- removeSparseTerms(s.dtm, 0.99)
dtm.remove$dimnames$Terms


## 計算文章之間的相似程度
a <- c(1, 2, 2, 1, 1, 1, 0)
b <- c(1, 2, 2, 1, 1, 2, 1)

cosine_distance <- sum(a * b) / (sqrt(sum(a ^ 2 )) * sqrt(sum(b ^ 2 )))

library(proxy)
proxy::dist(rbind(a,b), method = 'cosine')
dtm.dist <- proxy::dist(as.matrix(dtm.remove), method = 'cosine')
dtm.mat  <- as.matrix(dtm.dist)
dim(dtm.mat)

cbind(
applenews[dtm.mat[20,]< 0.5, 'title'],
dtm.mat[dtm.mat[20,]< 0.5, 20]
)
