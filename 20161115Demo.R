library(jiebaR)
s="那我們酸民婉君也可以報名嗎"

## 使用Segment
getwd()
mixseg = worker(user = '/home/trainee/user50/user.dict.utf8')
segment(code=s , jiebar=mixseg)

## 使用<=
mixseg <= s

