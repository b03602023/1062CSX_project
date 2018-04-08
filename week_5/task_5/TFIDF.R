# week_5 task_5 TF-IDF for week_4 低碳生活部落格
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)      #斷詞用
library(RColorBrewer)
library(wordcloud)
library(readtext)
library(tmcn)   #segmentCN
#filenames <- list.files(getwd(), pattern="*.txt")  #pattern: an optional regular expression. Only file names which match the regular expression will be returned.
#files <- lapply(filenames, readLines)  #Read some or all text lines from a connection.
files <- readtext("*.txt", encoding = "big5")
docs <- Corpus(VectorSource(files$text))  #Representing and computing on corpora(語料庫).

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}     # Create content transformers, i.e., functions which modify the content of an R objec
# gsub performs replacement of all matches.(以空白取代)
#定義清洗：清洗就是把你找到的符號用空白取代
)

docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
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
library(Matrix)   #nnzero

freqFrame = as.data.frame(table(unlist(seg)))
#seg <- as.data.frame(unlist(seg))
seg <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(seg, control = list(wordLengths = c(1,10)))
# 可用matrix呈現: as.matrix(tdm)
#-------caculate  TF-IDF ----------
tf <- as.matrix(tdm)/apply(tdm, 2, sum)   #term frequency: the number of words in every document
s_tf <- apply(tf, 1, sum)
idf <- log10(ncol(tdm)/apply(tdm, 1, nnzero))
tfidf <- tf*idf       #TF-IDF
#write.csv(tfidf, "show.csv")
# View(head(tfidf))       #TF-IDF

s_tfidf <- apply(tfidf, 1, sum)
all <- cbind(s_tf, idf, s_tfidf)
colnames(all) <- c("TF", "IDF", "TF-IDF")

s_tfidf <- s_tfidf[order(s_tfidf, decreasing = TRUE)]
#s_tfidf[s_tfidf>0.23]
tfidf <- as.data.frame(tfidf)
#View(tm::findFreqTerms(tdm, 22))
#inspect(tdm)

#---------tidytext package to calculate TF-IDF (not sure how to use)----------
library(psych)
library(tidytext)



#------visualize----
library(ggplot2)
t <- 0.28
reorder_size <- function(x) { factor(x, levels = names(sort(table(x)))) }
names(s_tfidf) <- reorder_size(names(s_tfidf))

p <- ggplot(tfidf[s_tfidf>t,], aes(reorder_size(names(s_tfidf[s_tfidf>t])), s_tfidf[s_tfidf>t]))
p + geom_bar(stat = "identity")+ 
  theme(axis.text.y = element_text(angle = 60, hjust = 1)
        )+
  coord_flip()+
  xlab("TF-IDF") + ylab("字詞")

#----------cos similarity ranking in r------------



#------------錯-----------
p <- ggplot(tfidf[s_tfidf>t,], aes(names(s_tfidf[s_tfidf>t][order(s_tfidf[s_tfidf>t],decreasing = T)]),
                                   s_tfidf[s_tfidf>t][order(s_tfidf[s_tfidf>t],decreasing = T)]))
p + geom_bar(stat = "identity")+ 
  theme(axis.text.y = element_text(angle = 60, hjust = 1))+
  coord_flip()


plot(rownames(tfidf[s_tfidf>0.2,]),s_tfidf[s_tfidf>0.2])



# 觀察出現由多到寡的文字
#View(freqFrame[order(freqFrame$Freq, decreasing = TRUE),])
#freqFrame[freqFrame$Var1=="雙城記",]


#tm_map(docs, segmentCN, nature = TRUE) 斷詞?
docs <- Corpus(VectorSource(docs))
tdm <- TermDocumentMatrix(docs, control = list(wordLengths = c(1,4)))
inspect(tdm)

tdm <- TermDocumentMatrix(docs)
inspect(tdm[1:10, 1:10])






#----------------------------------------------
tdm = seg[[1]]
for(i in 2:length(seg)){
  tdm = merge(tdm, seg[[i]], by="Var1", all=TRUE)
}

count_token = function(d)
{
  as.data.frame(table(d))
}

names(tokens)

