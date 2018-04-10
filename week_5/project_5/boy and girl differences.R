# pchome
# girl and boy difference
library(httr)
pageno <- 1:10    #搜尋頁數
g_url <- paste0("https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=%E5%A5%B3%E8%A3%9D&page=", pageno, "&sort=rnk/dc")
b_url <- paste0("https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=%E7%94%B7%E8%A3%9D&page=", pageno, "&sort=rnk/dc")
#url = "https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=%E5%A5%B3&page=1&sort=rnk/dc"
g_prods = character()
b_prods = character()
getcontent <- function(url, prods){
  for(i in 1:length(pageno)){
    res = GET(url[i])
    res_json = httr::content(res)
    results <- data.frame(do.call(rbind,res_json$prods))
    prods[((pageno[i]-1)*20+1):(pageno[i]*20)] <- unlist(results$name)
  }
  return(prods)
}
g_prods <- c(g_prods, getcontent(g_url, g_prods))

Sys.sleep(60)
b_prods <- c(b_prods, getcontent(b_url, b_prods))

write.csv(g_prods, "g_prods.csv")
write.csv(b_prods, "b_prods.csv")


#--------------
g_prods <- read.csv("g_prods.csv", header = FALSE)
g_prods <- as.character(mapply(paste0, g_prods))   #從dataframe轉換成character
b_prods <- read.csv("b_prods.csv", header = FALSE)
b_prods <- as.character(mapply(paste0, b_prods))   #從dataframe轉換成character

#-------clean data--------
processdata <- function(prods){
  library(NLP)
  library(tm)
  library(jiebaRD)
  library(jiebaR)      #斷詞用
  library(RColorBrewer)
  library(wordcloud)
  library(tmcn)   #segmentCN
  docs <- Corpus(VectorSource(prods))
  
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
  segment <- c("女裝")
  #new_user_word(mixseg,segment)   #Add user word
  
  
  #斷詞  mixseg[groups]
  jieba_tokenizer=function(d){
    unlist(segment(d[[1]],mixseg))
  }
  #1
  seg = lapply(docs, jieba_tokenizer)
  
  return(seg)
}
g_seg <- processdata(g_prods)
b_seg <- processdata(b_prods)

##------詞頻分析------
plotcloud <- function(seg, tres){
  library(Matrix)   #nnzero
  freqFrame = as.data.frame(table(unlist(seg)))
  library(wordcloud2)

  # 將女裝關鍵字去除
  
  wordcloud2(freqFrame[(freqFrame$Freq>tres)&(freqFrame$Freq!=max(freqFrame$Freq)),], fontFamily = "微軟雅黑",
             color = "random-light", backgroundColor = "grey", size = 0.5
  )
}
plotcloud(g_seg, 10)
plotcloud(b_seg, 10)


#-----UNIQULO-------
library(rvest)
page <- 0:12
p <- 48*(page)

u_getrvest <- function(url){
  res <- read_html(url)
  raw.titles <- res %>% html_nodes("div.unit .info .name")
  u_prods <- raw.titles %>% html_node("a") %>% html_attr('title')
  
  return(u_prods)
}
# -----UNIQULO女裝----------
ug_url <- paste0("http://www.uniqlo.com/tw/store/search?qtext=%E5%A5%B3%E8%A3%9D&qbrand=10&qclv1=&qclv25=&qclv2=&qrange=&qcolor=&qsize=&qnew=&qdiscount=&qlimit=&qmulti=&qonline=&qspsize=&qstart=", p,"&sort=goods_disp_priority")

ug_prods <- mapply(u_getrvest, ug_url)
colnames(ug_prods) <- page+1
#raw.prices <- res %>% html_nodes("dd.price") %>% html_text
#raw.prices <- as.character(raw.prices)
#gsub("NT$", "", raw.prices)

ug_seg <- processdata(as.character(ug_prods))
ug_freqFrame = as.data.frame(table(unlist(ug_seg)))
tres <-10
wordcloud2(ug_freqFrame[(ug_freqFrame$Freq>tres)&(ug_freqFrame$Freq!=max(ug_freqFrame$Freq)),], fontFamily = "微軟雅黑",color = "random-light", backgroundColor = "grey", size = 1
)
# -----UNIQULO男裝----------
ub_url <- paste0("http://www.uniqlo.com/tw/store/search?qtext=%E7%94%B7%E8%A3%9D&qbrand=10&qclv1=&qclv25=&qclv2=&qrange=&qcolor=&qsize=&qnew=&qdiscount=&qlimit=&qmulti=&qonline=&qspsize=&qstart=", p,"&sort=goods_disp_priority")

ub_prods <- mapply(u_getrvest, ub_url)
colnames(ub_prods) <- page+1
#raw.prices <- res %>% html_nodes("dd.price") %>% html_text
#raw.prices <- as.character(raw.prices)
#gsub("NT$", "", raw.prices)

ub_seg <- processdata(as.character(ub_prods))
ub_freqFrame = as.data.frame(table(unlist(ub_seg)))
tres <-10
wordcloud2(ub_freqFrame[(ub_freqFrame$Freq>tres)&(ub_freqFrame$Freq!=max(ub_freqFrame$Freq)),], fontFamily = "微軟雅黑",color = "random-light", backgroundColor = "grey", size = 1
)

#-------詞頻-------
g_seg <- Corpus(VectorSource(g_seg))
g_tdm <- TermDocumentMatrix(g_seg, control = list(wordLengths = c(1,10)))


# 可用matrix呈現: as.matrix(tdm)
#-------caculate  TF-IDF ----------
g_seg <- Corpus(VectorSource(g_seg))
g_tdm <- TermDocumentMatrix(g_seg, control = list(wordLengths = c(1,10)))

g_tf <- as.matrix(g_tdm)/apply(g_tdm, 2, sum)   #term frequency: the number of words in every document
g_idf <- log10(ncol(g_tdm)/apply(g_tdm, 1, nnzero))
g_tfidf <- g_tf*g_idf       #TF-IDF
g_tfidf <- apply(g_tfidf, 1, sum)
View(head(g_tfidf[order(g_tfidf, decreasing = TRUE)], n = 15L))       #TF-IDF

b_seg <- Corpus(VectorSource(b_seg))
b_tdm <- TermDocumentMatrix(b_seg, control = list(wordLengths = c(1,10)))

b_tf <- as.matrix(b_tdm)/apply(b_tdm, 2, sum)   #term frequency: the number of words in every document
b_idf <- log10(ncol(b_tdm)/apply(b_tdm, 1, nnzero))
b_tfidf <- b_tf*b_idf       #TF-IDF
b_tfidf <- apply(b_tfidf, 1, sum)
View(head(b_tfidf[order(b_tfidf, decreasing = TRUE)], n = 15L))       #TF-IDF

ug_seg <- Corpus(VectorSource(ug_seg))
ug_tdm <- TermDocumentMatrix(ug_seg, control = list(wordLengths = c(1,10)))


ug_tf <- as.matrix(ug_tdm)/apply(ug_tdm, 2, sum)   #term frequency: the number of words in every document
ug_idf <- log10(ncol(ug_tdm)/apply(ug_tdm, 1, nnzero))
ug_tfidf <- ug_tf*ug_idf       #TF-IDF
ug_tfidf <- apply(ug_tfidf, 1, sum)
View(head(ug_tfidf[order(ug_tfidf, decreasing = TRUE)], n = 15L))       #TF-IDF

ub_seg <- Corpus(VectorSource(ub_seg))
ub_tdm <- TermDocumentMatrix(ub_seg, control = list(wordLengths = c(1,10)))


ub_tf <- as.matrix(ub_tdm)/apply(ub_tdm, 2, sum)   #term frequency: the number of words in every document
ub_idf <- log10(ncol(ub_tdm)/apply(ub_tdm, 1, nnzero))
ub_tfidf <- ub_tf*ub_idf       #TF-IDF
ub_tfidf <- apply(ub_tfidf, 1, sum)
View(head(ub_tfidf[order(ub_tfidf, decreasing = TRUE)], n = 15L))       #TF-IDF





#-------淘寶--------
url <- "https://s.taobao.com/search?spm=a21wu.241046-tw.6977698868.5.239460c62b8QtV&q=%E5%A5%B3%E8%A3%85&acm=lb-zebra-241046-2058600.1003.4.1797247&scm=1003.4.lb-zebra-241046-2058600.OTHER_14950676920071_1797247"
u_getrvest <- function(url){
  res <- read_html(url)
  raw.titles <- res %>% html_nodes(".J_ClickStat")
  u_prods <- raw.titles %>% html_node("a") %>% html_attr('title')
  
  return(u_prods)
}






seg <- Corpus(VectorSource(seg))
Sys.setlocale()
tdm <- TermDocumentMatrix(seg, control = list(wordLengths = c(1,10)))


View(as.matrix(tdm))
View(freqFrame)
View(unlist(seg))
View(data.frame(do.call(rbind,res_json$prods)))