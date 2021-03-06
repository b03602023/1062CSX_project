#---------Rfacebook--------
token = "EAACEdEose0cBAPWqw5JDTSaYbWgWYcCsGJ11ZAuZBbFR6ZAZBlahtxdas6Sk2ERLKRBW1V3rZCJNjXo1Fp26ABZA85gyiLgi85fDhUt4CbwTIZBcLTUmsZCZCJNUwKvpCW2o5m2DxSWvby8TS5v0ETvk2srZB7UvU43TvcVPlR6pcz4CaFMvLYQxnZB92efbGzrZAX8ZD"
library(Rfacebook)
me <- getUsers("me", token, private_info = TRUE)
me$name

require("Rfacebook")
fb.oauth <- fbOAuth(
  app_id="599990343682885",
  app_secret="5162044ac94f64461ed587312d424521",
  extended_permissions = TRUE)

me <- getUsers("me",token=fb.oauth)
me$name



#---------參考的code (可以解決載入較舊頁面的問題)-------
#--------限定時間期限的蒐集貼文方法------
#-----------defining the time------------
#20180330發現問題: 對於抓取橫跨不同年份的貼文，在while迴圈中會出現error
#就是雖然有nextflg，但是會造成ndata為空的list，而出現error。
rm(list=ls(all.names=TRUE))
library(httr)
prefex <- "https://graph.facebook.com/v2.10/"
token <- "EAACEdEose0cBAA7yCOCQG6qf4binKSpLjHEYerAZAKObQao5OBgc6aAk4oihgXdrJRuJUgJdxCOYRH6sxp8iQBeNGY1iZAUXdXkPgLsrQpwsuecrFN9j3kzJh0INXmjXgTZBGgJikpKtcXkUs9DkmQRGJHr5uEZAuorNj3ZCHD5R0JJ8ZAzCu4StfJk1ZBkCL4ZD"
number <- 5      #只爬入一篇貼文post

# 271111393019477為TOEFL Taiwan的id
# 136845026417486 為柯文哲的id
# 175549807657 為低碳生活部落客的id
# 限定從2017-03-11到2018-03-22 時間內最近的一篇貼文
target <- "175549807657/posts?limit="
control <- "&until=2018-03-22&since=2018-01-01"
# "271111393019477/posts?limit=1&until=2018-03-22&since=2017-01-12&access_token="
attrs <- paste0(target, number, control,"&access_token=")  
url <- paste0(prefex, attrs, token)

# 限定content是屬於httr這個library的
res <- httr::GET(url)
data <-  httr::content(res)
#把data先解開他list的結構，再用matrix方式存下來
#groups= matrix(unlist(data$data))
groups <- matrix(unlist(data$data))  #groups到變成文字雲的時候就是變成文件doc了

#存成檔案(因為要分梯次存，所以藉由count這個變數來存取每一篇文章)
filename = paste0(1, ".txt")
write.table(groups,filename)
#要跳到下一頁
after  = data$paging$cursors$after  # after : This is the cursor that points to the end of the page of data that has been returned.

nextflg= data$paging$`next`    # == nextflg= data$paging[[2]]
                              #nextflg是要用來判斷是否到動態頁的最底端了( the last page of data)

count=1
while(class(nextflg) != "NULL"){
  
  count=count+1
  #attrs  = paste0(target, number, control, "&after=", after,"&access_token=")
  #url = paste0(prefex,attrs,token)
  #上面的code和nextflg的意思是一樣的
  nurl = nextflg
  nres= httr::GET(nurl)
  ndata  = httr::content(nres)
  ngroups= matrix(unlist(ndata$data))
  #p1=ndata[["data"]][[1]]$message
  #p1=ndata$data[[1]]$message
  ##可用try_catch來測試，while loop停在哪一段 可以記錄走到哪一段停止
  
  after  = ndata$paging$cursors$after
  nextflg = ndata$paging$`next`    # ndata$paging[[2]]
  
  filename = paste0(count, ".txt")  #檔名
  
  write.table(ngroups,filename)
}

#--------文字雲---------
#要做文字雲 抓一些需要用的套件
#從電腦讀檔（仰賴 NLP/tm 這兩個函式庫）
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)      #斷詞用
library(RColorBrewer)
library(wordcloud)

#進行文本清理
par(family='STKaiti')  #字體設定；讓文字顯示成中文
#將R環境設定成中文
#Sys.setlocale(category = "LC_ALL", locale = "cht") 
#par(family=("Heiti TC Light"))
# 讀入wd內的資料夾中所有 *.txt 文章
# 變成docs作為被分析的語料庫
filenames <- list.files(getwd(), pattern="*.txt")  #pattern: an optional regular expression. Only file names which match the regular expression will be returned.
files <- lapply(filenames, readLines)  #Read some or all text lines from a connection.
docs <- Corpus(VectorSource(files))  #Representing and computing on corpora(語料庫).


#VectorSource: create a vector source
 # A vector source interprets each element of the vector x 
 # as a document.

#Corpora: Representing and computing on corpora.
#Corpus是一種文字檔資料格式--文本
# 移除可能有問題的符號


#要清洗掉的東西
## 進行清除停用字符，停用字符指的是一些文章中常見的單字，
## 但卻無法提供我們資訊的冗字。例如有些、以及、因此…等等字詞。
### 20180401發現要先清洗掉停用字，再清洗標點符號，不然有些文字會變成亂碼
### 例如「節能標竿」會變成「葛鉏衧\xf1 」
### 但仍不知道為什麼會這樣

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}     # Create content transformers, i.e., functions which modify the content of an R objec
      # gsub performs replacement of all matches.(以空白取代)
#定義清洗：清洗就是把你找到的符號用空白取代
)
# content_transformer 為內文取代 function
# tm_map(x, FUN, ...)
# Interface to apply transformation functions (also denoted as mappings) to corpora.
# x: A corpus. 語料庫("Corpus" is a collection of text documents.)
# FUN: a transformation function taking a text document (a character vector when x is a SimpleCorpus) as input and returning a text document (a character vector of the same length as the input vector for SimpleCorpus). The function content_transformer can be used to create a wrapper to get and set the content of text documents.
# 複製文本中你不要的符號刪除，可寫多行刪除多種符號
#docs <- tm_map(docs, toSpace, "文本中你不要的符號貼於此")
docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "的")
docs <- tm_map(docs,toSpace, "及")
docs <- tm_map(docs,toSpace, "為")
docs <- tm_map(docs,toSpace, "是")
docs <- tm_map(docs,toSpace, "在")
docs <- tm_map(docs,toSpace, "我")
docs <- tm_map(docs,toSpace, "沒有")
docs <- tm_map(docs,toSpace, "不要")
docs <- tm_map(docs,toSpace, "只")
docs <- tm_map(docs,toSpace, "正要")
docs <- tm_map(docs,toSpace, "尤其")
docs <- tm_map(docs,toSpace, "做")
docs <- tm_map(docs,toSpace, "活動")
docs <- tm_map(docs,toSpace, "停止")
docs <- tm_map(docs,toSpace, "一下")
docs <- tm_map(docs,toSpace, "進行")
docs <- tm_map(docs,toSpace, "要")
docs <- tm_map(docs,toSpace, "們")
docs <- tm_map(docs,toSpace, "有")
docs <- tm_map(docs,toSpace, "要")
docs <- tm_map(docs,toSpace, "就")
docs <- tm_map(docs,toSpace, "也")
docs <- tm_map(docs,toSpace, "會")
docs <- tm_map(docs,toSpace, "台灣")
docs <- tm_map(docs,toSpace, "台北")
docs <- tm_map(docs,toSpace, "就")
docs <- tm_map(docs,toSpace, "生活")
docs <- tm_map(docs,toSpace, "更")
docs <- tm_map(docs,toSpace, "不")
docs <- tm_map(docs,toSpace, "讓")
docs <- tm_map(docs,toSpace, "都")
docs <- tm_map(docs,toSpace, "與")
docs <- tm_map(docs,toSpace, "小編")
docs <- tm_map(docs,toSpace, "就")
docs <- tm_map(docs,toSpace, "喔")
docs <- tm_map(docs,toSpace, "新")
docs <- tm_map(docs,toSpace, "年")
docs <- tm_map(docs,toSpace, "可以")
docs <- tm_map(docs,toSpace, "啊")
docs <- tm_map(docs,toSpace, "文章")
docs <- tm_map(docs,toSpace, "到")
docs <- tm_map(docs,toSpace, "將")
docs <- tm_map(docs,toSpace, "演講")
docs <- tm_map(docs,toSpace, "格")
docs <- tm_map(docs,toSpace, "一個")
docs <- tm_map(docs,toSpace, "以")
docs <- tm_map(docs,toSpace, "中")
docs <- tm_map(docs,toSpace, "下午")
docs <- tm_map(docs,toSpace, "但")
docs <- tm_map(docs,toSpace, "吧")
docs <- tm_map(docs,toSpace, "還")
docs <- tm_map(docs,toSpace, "如何")
docs <- tm_map(docs,toSpace, "將")
docs <- tm_map(docs,toSpace, "﹍")
# 清除大小寫英文與數字
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)



# 語詞詞幹化(stemmization)
# 以英文為例
#https://zh.wikipedia.org/wiki/词干提取
#library(SnowballC)
#確保任何形式的單字只會轉換成相同詞性出現一次
#docs <- tm_map(docs, stemDocument)
##Stem words in a text document using Porter's stemming algorithm

#在 worker() 內可以設定各種不同的全切分法模型與引用外部詞庫，在這裡直接使用
#預設的全切分法的混合模型，與 jieba 自帶的詞庫。
# 一般斷詞
mixseg = worker()
#適時的增加詞庫
# segment <- c("陳菊","布里斯本","高雄","重劃區","合作會","後勁溪")
segment <- c("低碳生活部落格","綠建築","節能","氣候變遷","電動車","台達電子","台達電子文教基金會","綠色能源")
new_user_word(mixseg,segment)   #Add user word


#斷詞  mixseg[groups]
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
#轉成文件
#詞頻結果:
freqFrame = as.data.frame(table(unlist(seg)))
# 觀察出現由多到寡的文字
View(freqFrame[order(freqFrame$Freq, decreasing = TRUE),])
freqFrame[freqFrame$Var1=="雙城記",]
#有詞頻之後就可以去畫文字雲
#畫出文字雲
library(extrafont)
#======fail to debug=====
#extrafont::loadfonts(device="win")
#extrafont::fonttable()
#extrafont::font_import("C:/Windows/Fonts", pattern = "Arial")
#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = Arial)
#par(family=("Arial"))
#======fail to debug=====
#20180331 解決問題 Font family not found in Windows font database
#需要先網路下載字型後，放在C:\Windows\Fonts資料夾中
#在用windowsFonts指定字型
windowsFonts(TC=windowsFont("Heiti TC Light"))
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),
          min.freq=5,max.words=50,
          random.order=FALSE,random.color=FALSE, 
          rot.per=.2, colors=brewer.pal(11, "Paired")[c(1:7)],
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE,family="TC")

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),
          min.freq=5,max.words=50,
          random.order=FALSE,random.color=FALSE, 
          rot.per=.2, colors=brewer.pal(11, "Paired")[c(1:17)],
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE,family="TC")
# wordcloud2生成文字雲
# https://www.r-graph-gallery.com/the-wordcloud2-library/
# https://kknews.cc/zh-tw/other/5gvgba6.html
library(wordcloud2)
figPath = system.file("factory.png",package = "wordcloud2")
wordcloud2(freqFrame[freqFrame$Freq>10,], size = 0.8, fontFamily = "微軟雅黑",
           color = "random-light", backgroundColor = "white",
           shape = "star"
           )


#文字雲解說：
#min.freq=50：最小頻率為50
#max.words=150 : 最多150個字
#random.order=TRUE：順序隨機
#colors=brewer.pal(8, "Dark2") : library(RColorBrewer)提供的風格之一
#另一種資料取代方法：
#files[[1]] = gsub(“的”,””,files[[1]])
#files[[1]] = gsub(“作者”,””,files[[1]])








#-------------github 老師上課的example-----
library(httr)
rm(list=ls(all.names=TRUE))
token = "EAACEdEose0cBAN13GZCJLOWwnxOoVzoxe69aPalyJtbmIlWVmZCgKxhms1bCaFzRxbdc3eOgYQPscSmGTPToGRZBkL4SrOZAc9ZCCdijqXm2njvXwoNfMeS7zHTWWwVJQA1S8nEZAbZBk5ZAZCuNoYgTfx8qTR3AErC0X4bc77dBmF3QeBINmzxyP277R1SDCBGAZD"
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
