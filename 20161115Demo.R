library(jiebaR)
s="那我們酸民婉君也可以報名嗎"

## 使用Segment
getwd()
mixseg = worker(user = '/home/trainee/user50/user.dict.utf8')
segment(code=s , jiebar=mixseg)

## 使用<=
mixseg <= s

## 抓出詞性
tagseg <- worker('tag', user = '/home/trainee/user50/user.dict.utf8')
tagseg <= s

## 抓取關鍵詞 (除非有去維護IDF 字典，否則我不建議你去使用他)
key =worker('keywords',user = '/home/trainee/user50/user.dict.utf8', topn=3)
key <=s
IDFPATH

## 自由時報爬蟲
library(rvest)
url <- 'http://news.ltn.com.tw/news/world/breakingnews/1863650'
keywords <- read_html(url) %>%
  html_nodes('.con_keyword a') %>%
  html_text()
keywords

## 使用NLP 做n-gram
library(NLP)
s <-strsplit(x="那我們酸民婉君也可以報名嗎", split='')
class(s)
bigram <- ngrams(unlist(s), 2)
vapply(bigram, paste, "", collapse ="")
sapply(bigram, function(e) paste(e, collapse=''))

# Tri-gram
trigram <- ngrams(unlist(s), 3)
sapply(trigram, function(e) paste(e, collapse=''))

# 4-gram
quadgram <- ngrams(unlist(s), 4)
sapply(quadgram, function(e) paste(e, collapse=''))


a <- list(c(1,3), c(2,4))
vapply(a, sum, 1)


## Bigram 斷詞實戰
article <- "搶著註冊...中國出現各種「藍瘦香菇公司」
〔即時新聞／綜合報導〕中國南寧一名男子因為女友去旅遊不在身邊，錄下自己寂寞的心境，卻因為口音問題把「難受想哭」說成「藍瘦香菇」，瞬間成為中國的流行語。而中國企業紛紛嗅到商機，集體搶著註冊以「藍瘦香菇」為名的公司名稱。
中國南寧一名男子把「難受想哭」說成「藍瘦香菇」，瞬間成為中國的流行語。（圖擷自直播網站）
綜合媒體報導，目前在深圳市已註冊了「藍瘦香菇實業有限公司」與「藍瘦香菇投資有限公司」，前者登記的經營範圍包括了電子電器產品、汽車配件、日用百貨、建築裝飾等等的銷售；後者則提供創業諮詢與投資服務。
另外在深圳還有一家「藍瘦香菇貿易有限公司」，但經營範圍並不清楚；在湖北宜昌也有「遠安縣藍瘦香菇銷售有限公司」跟風成立，但和其他以「藍瘦香菇」為名的企業不同，這間公司是真的打算要販賣香菇。
其實中國企業瘋搶流行與充當公司名稱的狀況早就存在，例如在今年的巴西奧運，中國女泳將傅園慧因「洪荒之力」一句話爆紅，到現在就有50多家以「洪荒之力」冠名的公司成立。"
w <-strsplit(x=article, split='')
bigram <- ngrams(unlist(w), 2)
bigram.str <- sapply(bigram, function(e) paste(e, collapse=''))
tb <- table(bigram.str)
tb[tb > 2]

strsplit(article, '〔|〕|「|」|。|，|；|、|（|）|\\.+|／')


### 根據標點符號做斷句
a.split <- strsplit(article, '〔|〕|「|」|。|，|；|、|（|）|\\.+|／')

### 拆成單字為Vector 中的每個元素
w.split <- strsplit(x=unlist(a.split), split='')

### 建立一個bigram 函式
bigram <-function(w){
  bigram <-ngrams(unlist(w), 4)
  bigram.str<-sapply(bigram, function(e) paste(e, collapse=''))
  bigram.str
}

### 套用bigram 函式到每個list 之中
bigram.all <- sapply(w.split, bigram)

### 統計bigram 出現的次數
tb <- table(unlist(bigram.all))

tb[tb > 2]

## 移除關鍵字

s ="當初中央政府拿台北市的精華地跟北市府交換"
s.split <- strsplit(s, '台北市')
unlist(s.split)
paste(unlist(s.split), collapse='')


## 建立移除關鍵字函式
removekey <-function(s, keys){
  for(key in keys){
    s.split=strsplit(s, key)
    s =paste(unlist(s.split), collapse ="")
  }
  s
}

removekey("當初中央政府拿台北市的精華地跟北市府交換", c("台北市", "中央", "政府"))
