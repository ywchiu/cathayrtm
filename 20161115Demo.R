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


## 建立ngram斷詞函式
ngram.func<-function(w, n){
  n.gram<-ngrams(unlist(w), n)
  n.gram.str<-sapply(n.gram, function(e) paste(e, collapse =""))
  n.gram.str
}

s <- "當初中央政府拿台北市的精華地跟北市府交換"
ngram.func(strsplit(s, ''), 4)


## 實作長詞優先斷詞
seq(4,2,-1)

longTermFirst <-function(article, keywords, threshold){
  for(i in seq(4,2,-1)){
    ### 移除關鍵字
    article =removekey(article, keywords)
    ### 斷句
    a.split<-strsplit(article, '〔|〕|「|」|。|，|；|、|（|）|\\.+|／')
    
    ### n-gram
    w.split<-strsplit(x=unlist(a.split), split='')
    n.gram.all<-sapply(w.split, function(e) ngram.func(e,i))
    
    ### 統計n-gram 次數
    tb<-table(unlist(n.gram.all))
    
    ### 取出超過閥值的字詞
    candidate <-names(tb[tb>=threshold])
    keywords =c(keywords, candidate)
  }
  keywords
}


article = "金曲歌后張惠妹（阿妹）去年4月在台北小巨蛋舉辦演唱會，全場萬名觀眾隨音樂興奮跳躍，附近居民因震動而抗議，相關單位隨後祭出「阿妹條款」約束攻蛋歌手，但阿妹萬萬想不到自此竟成小巨蛋黑名單，《蘋果》接獲爆料，指她去年申請今年檔期辦純粹音樂型態的音樂會，小巨蛋就不批准，今年再度申請明年年底的檔期要讓「烏托邦」巡演唱回小巨蛋，小巨蛋審查委員再度駁回，而且沒有給予任何理由及討論機會，此舉形同封殺阿妹。
記者詢問阿妹經紀人陳鎮川此事，他表示要先去了解狀況，稍後再與記者通話時，他承認透過內線消息，獲知阿妹明年小巨蛋演出申請再度吃閉門羹，難掩激動地說：「對於這樣的結果我覺得非常不公平，這幾年來阿妹也算是小巨蛋的代表性歌手，光是付給小巨蛋的幾千萬場租難道不夠讓小巨蛋針對震動的問題做出改善的方案？怎麼會現在感覺上要阿妹扛下這個問題？」
接著，陳鎮川再表示：「我們從來沒有不願意遵守規定，去年演出造成震動，我們也馬上更改節目內容。現在的問題是我們無法可循！」
他也質疑既然有這樣的問題，為何不能有明確規定讓阿妹及製作團隊去遵守？況且連溝通的機會都不給，實在太不合理。也透露阿妹聽到以後非常失望，還天真的回應：「我可以不唱《三天三夜》啊⋯⋯」她希望小巨蛋能夠針對這個事情有更好解決方案，「我對於在小巨蛋開演唱會有很深厚的感情⋯⋯」
他也沉重表示，小巨蛋此舉的結論就是台北的歌迷勢必無緣看到阿妹針對出道20年全新設計的烏托邦2.0演出，「我們真的很遺憾。也對小巨蛋的管理徹底失望和憤怒」。
而負責申請場地、阿妹演唱會的主辦單位上引娛樂表示 沒有明確的理由 就拒絕國寶級歌手的演出申請二次 實是台灣數十萬歌迷的損失 且在大巨蛋懸而未決之際 更是讓人遺憾，歌手努力讓台灣流行音樂的領先 卻連政府也不相挺。
對此，北市捷運局公關凌啟堯表示：「通常表演單位提交檔期申請，會交由台北市政府文化局檔期審議委員會，由委員會決定通過或駁回，一旦檔期確認，捷運局只是執行單位，負責跟演出單位簽約與執行。」
北市文化局回應：「張惠妹申請檔期為106年12月檔期，總計有14件提出申請，均有檔期衝突情形，考量活動多元性、公共安全及檔期衝突等因素，由委員共識決議。」文化局並重申：「市府絕未封殺任何表演者或團體，但同時段有14組人申請，勢必會造成遺珠之憾。未來北流（台北流行音樂中心）完成，盼能提供更多場地。震盪一事適用於所有申請者，周邊民眾之權益仍須兼顧。市府一定在兼顧表演團體以及周遭民眾權益間努力取得平衡，盼各界諒解。」（李志展、林丞偉／台北報導）"
keywords = c()
longTermFirst(article, keywords,4)
