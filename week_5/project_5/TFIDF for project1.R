#------getpost-------
rm(list=ls(all.names=TRUE))
library(httr)
prefex <- "https://graph.facebook.com/v2.10/"
token <- "EAACEdEose0cBAK7w4hguBSuTaxvO60aPgXJGZBS4DIWmExvNqPRBP2goH0XjXtCZBJIiX4VGwJvb8QDL806D4owmJJs83FPzNE8ZBP9DR79IRhsG1HTZBQICRaC8OL8ZBGj3P4Yf0To1w4wmFdM5lEyas2vKdTHwE9UmD4G5QVhIModf91JCcjzMX9EfqOXsZD"
number <- 1      #只爬入一篇貼文post

# 101501458062251287為朱立倫的id
# 136845026417486 為柯文哲的id
# 175549807657 為低碳生活部落客的id
# 限定從2017-03-11到2018-03-22 時間內最近的一篇貼文
target <- "10150145806225128/posts?limit="
control <- "&until=2018-04-07&since=2018-01-01"
control <- c(control, paste0("&until=",2017:2012,"-12-31","&since=",2017:2012,"-01-01"))
count=1
getpost <- function(control, count){

  attrs <- paste0(target, number, control,"&access_token=")  
  url <- paste0(prefex, attrs, token)
  
  # 限定content是屬於httr這個library的
  res <- httr::GET(url)
  data <-  httr::content(res)
  #把data先解開他list的結構，再用matrix方式存下來
  #groups= matrix(unlist(data$data))
  groups <- matrix(unlist(data$data))  #groups到變成文字雲的時候就是變成文件doc了
  
  #存成檔案(因為要分梯次存，所以藉由count這個變數來存取每一篇文章)
  filename = paste0(count, ".txt")
  write.table(groups,filename)
  #要跳到下一頁
  after  = data$paging$cursors$after  # after : This is the cursor that points to the end of the page of data that has been returned.
  
  nextflg= data$paging$`next`    # == nextflg= data$paging[[2]]
  #nextflg是要用來判斷是否到動態頁的最底端了( the last page of data)
  
  
  while(class(nextflg) != "NULL"){
    
    count=count+1
    #print(count)
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
  return(count+1)
}

for(i in 1:length(control)){
  count <- getpost(control[i], count)
  #print(paste0(count,'!!!'))
}

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
mixseg = worker()
segment <- c("新北")
new_user_word(mixseg,segment)   #Add user word


#斷詞  mixseg[groups]
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)

#轉成文件
#詞頻結果:
library(Matrix)   #nnzero

freqFrame = as.data.frame(table(unlist(seg)))

seg <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(seg, control = list(wordLengths = c(1,10)))

# 可用matrix呈現: as.matrix(tdm)
#-------caculate  TF-IDF ----------
tf <- as.matrix(tdm)/apply(tdm, 2, sum)   #term frequency: the number of words in every document
idf <- log10(ncol(tdm)/apply(tdm, 1, nnzero))
tfidf <- tf*idf       #TF-IDF
# View(head(tfidf))       #TF-IDF

s_tfidf <- apply(tfidf, 1, sum)
s_tfidf <- s_tfidf[order(s_tfidf, decreasing = TRUE)]
#s_tfidf[s_tfidf>0.23]
tfidf <- as.data.frame(tfidf)
s_tfidf[s_tfidf>0.23][1:10]
