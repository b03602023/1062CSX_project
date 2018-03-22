#---------參考的code (可以解決載入較舊頁面的問題)-------
rm(list=ls(all.names=TRUE))
library(httr)
prefex <- "https://graph.facebook.com/v2.10/"
token <- "EAACEdEose0cBAN13GZCJLOWwnxOoVzoxe69aPalyJtbmIlWVmZCgKxhms1bCaFzRxbdc3eOgYQPscSmGTPToGRZBkL4SrOZAc9ZCCdijqXm2njvXwoNfMeS7zHTWWwVJQA1S8nEZAbZBk5ZAZCuNoYgTfx8qTR3AErC0X4bc77dBmF3QeBINmzxyP277R1SDCBGAZD"
number <- 1      #只爬入一篇貼文post

# 271111393019477為TOEFL Taiwan的id
# 限定從2018-03-11到2018-03-22 時間內最近的一篇貼文
target <- "271111393019477/posts?limit="
control <- "&until=2018-03-22&since=2018-03-11"
# "271111393019477/posts?limit=1&until=2018-03-22&since=2018-03-11&access_token="
attrs <- paste0(target, number, control,"&access_token=")  
url <- paste0(prefex, attrs, token)

# 限定content是屬於httr這個library的
res <- httr::GET(url)
data <-  httr::content(res)
#把data先解開他list的結構，再用matrix方式存下來
#groups= matrix(unlist(data$data))
groups <- data.frame(unlist(data$data))

#存成檔案(因為要分梯次存，所以藉由count這個變數來存取每一篇文章)
filename = paste0(1, ".txt")
write.table(groups,filename)
#要跳到下一頁
after  = data$paging$cursors$after

nextflg= data$paging$`next`    # == nextflg= data$paging[[2]]

count=1
while(nextflg!= "NULL"){
  
  count=count+1
  attrs  = paste0(target, number, control, "&after=", after,"&access_token=")
  #看不懂?? nexturl= paste0(prefex,attrs,"&after=",after)
  
  url = paste0(prefex,attrs,token)
  
  nres= httr::GET(url)
  ndata  = httr::content(nres)
  ngroups= data.frame(unlist(ndata$data))
  #p1=ndata[["data"]][[1]]$message
  #p1=ndata$data[[1]]$message
  ##可用try_catch來測試，while loop停在哪一段 可以記錄走到哪一段停止
  
  after  = ndata$paging$cursors$after
  nextflg = ndata$paging$`next`    # ndata$paging[[2]]
  
  filename = paste0(count, ".txt")
  
  write.table(ngroups,filename)
}
#要做文字雲 抓一些需要用的套件
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
#進行文本清理
par(family='STKaiti')#讓文字顯示成中文
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))

#要清洗掉的東西

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
#定義清洗：清洗就是把你找到的符號用空白取代
)
docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "的")
docs <- tm_map(docs,toSpace, "及")
docs <- tm_map(docs,toSpace, "為")
docs <- tm_map(docs,toSpace, "是")
docs <- tm_map(docs,toSpace, "在")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

mixseg = worker()
segment <- c("陳菊","布里斯本","高雄","重劃區","合作會","後勁溪")
new_user_word(mixseg,segment)

#-------------github 老師上課的example
library(httr)
rm(list=ls(all.names=TRUE))
token <- "EAACEdEose0cBAN13GZCJLOWwnxOoVzoxe69aPalyJtbmIlWVmZCgKxhms1bCaFzRxbdc3eOgYQPscSmGTPToGRZBkL4SrOZAc9ZCCdijqXm2njvXwoNfMeS7zHTWWwVJQA1S8nEZAbZBk5ZAZCuNoYgTfx8qTR3AErC0X4bc77dBmF3QeBINmzxyP277R1SDCBGAZD"
prefex = "https://graph.facebook.com/v2.12/twTOEFL/?fields=posts&access_token="
url    = paste0(prefex, token)
res    = httr::GET(url)
posts  = content(res)


##------------載入較舊頁面(有問題)---------------
temp=list()

#遇到問題，利用append不斷疊加厚url的位置會改變
for(c in 2:5){
  #token  = "EAACEdEose0cBAIG9g05nYvrEZCqpqTEeYjTSFMANNoViGP3RtglpQOA2rxZAwUR8gyZBae1nLqZCfnwuIn8jkeluxmr87SrEIBkUQuheFN0YqIxnnP3MFz33igUcK6BGfNsPZBTPBKEJ1jbqd6B28JT55zMZAD0cNN6yvi0aLG44HWG1I0EXxNtZBXQOvUQUCYZD"
  #prefex = "https://graph.facebook.com/v2.12/twTOEFL/?fields=posts&access_token="
  #url    = paste0(prefex, token)
  url = posts$posts$paging$`next`
  # url = posts$paging$`next`
  res    = httr::GET(url)
  posts  = content(res)
  temp = append(temp,posts)
  
}


#--------------------------------
res = POST("https://graph.facebook.com/v2.12/me/feed",
           body=list(message=sprintf("[TEST Posting Message] %s At %s","httr 測試",Sys.time()),
                     access_token=token))
postId = content(res)$id


url = sprintf("https://graph.facebook.com/v2.12/%s?access_token=%s", postId, token)
res = DELETE(url)
content(res)
