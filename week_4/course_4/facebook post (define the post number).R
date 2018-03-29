#------facebook posts-----------
#-------define the post number--------
#------定義蒐集貼文的數量------
rm(list=ls())

library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(Rfacebook)

page.id <- "175549807657" 
token <-"EAACEdEose0cBAIKoyKcxotsUNG8iOJCx7YsKxpDiRQZA3MBhuZAVXHZCqgwjtPIxR0J24WYmWFg7LIqoE9QnZCS6IwOkom5PuE2czZCZAVPBwPtN23khnhdfFrE4oqRnDNO6IDtaDq0XvcfurUm6am6DLWbO1Eb6u0EUiDZAn1VouAuJpis5kA5V5lgLmEw990ZD"
page <- getPage(page.id, token, n = 30)

docs <- Corpus(VectorSource(page$message))

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))


docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "啊")
docs <- tm_map(docs, toSpace, "站")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "很")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "這")
docs <- tm_map(docs, toSpace, "讓")
docs <- tm_map(docs, toSpace, "會")
docs <- tm_map(docs, toSpace, "跟")
docs <- tm_map(docs, toSpace, "多")
docs <- tm_map(docs, toSpace, "月")
docs <- tm_map(docs, toSpace, "增加")
docs <- tm_map(docs, toSpace, "會")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "真")
docs <- tm_map(docs, toSpace, "上")
docs <- tm_map(docs, toSpace, "但")
docs <- tm_map(docs, toSpace, "又")
docs <- tm_map(docs, toSpace, "各")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}

seg = lapply(docs, jieba_tokenizer)

freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:34),]
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)


