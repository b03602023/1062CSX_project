# project_1

# http://elections.olc.tw/candidates/view/53d79150-f188-46e7-8f6d-5fc6cb455a61
# 朱立倫選舉黃頁 2014新北市市長
### 蒐集朱立倫臉書專頁從2012-01-01到2018-04-07 時間的貼文
### 朱立倫2014年競選新北市市長的選舉黃頁
# file <- read.table("campaign page.txt")
#-------clean data--------
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)      #斷詞用
library(RColorBrewer)
library(wordcloud)
library(tmcn)   #segmentCN
filenames <- list.files(getwd(), pattern="*.txt")  #pattern: an optional regular expression. Only file names which match the regular expression will be returned.
files <- lapply(filenames, readLines)  #Read some or all text lines from a connection.
docs <- Corpus(VectorSource(files))  #Representing and computing on corpora(語料庫).

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}     

)

docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
# 清除大小寫英文與數字
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

#-------斷詞--------
# 加入新字詞
mixseg = worker()
keyword <- read.csv("keyword.csv", header=FALSE)
keyword <- apply(keyword, 1, paste0)
new_user_word(mixseg, keyword)   #Add user word


#斷詞  mixseg[groups]
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)

#轉成文件
#詞頻結果:
library(Matrix)   #nnzero
freqFrame = as.data.frame(table(unlist(seg)))
# View(freqFrame[order(freqFrame$Freq),])
seg <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(seg, control = list(wordLengths = c(1,10)))

#termfreq <- as.matrix(tdm)  #詞頻向量

# View(head(as.matrix(tdm))[,1:14])
tf <- as.matrix(tdm)/apply(tdm, 2, sum)   #term frequency: the number of words in every document
# View(head(tf)[,1:20])
idf <- log10(ncol(tdm)/apply(tdm, 1, nnzero))
tfidf <- tf*idf       #TF-IDF
# View(head(tfidf)[,1:20])       #TF-IDF
s_tfidf <- apply(tfidf, 1, sum)
s_tfidf <- s_tfidf[order(s_tfidf, decreasing = TRUE)]
#View(s_tfidf[s_tfidf>0.23][1:10])
tfidf <- as.data.frame(tfidf)

#------cos similarity ranking-------
# View(head(tfidf)[,1:10])


#------visualize----
library(ggplot2)
t <- 30
p <- ggplot(tfidf[s_tfidf>t,], aes(names(s_tfidf[s_tfidf>t]), s_tfidf[s_tfidf>t]))
p + geom_bar(stat = "identity")+ 
  theme(axis.text.y = element_text(angle = 60, hjust = 1)
  )+
  coord_flip()+
  xlab("字詞") + ylab("TF-IDF")

#

### 待解決問題：將seg轉換為Corpus後進行TermDocumentMatrix，
### 但會有編碼的情形出現，且其TF-IDF有些還蠻高的

