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
