library(rvest)
url <- "https://zh.wikipedia.org/zh-tw/%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80"
# Get response
res <- read_html(url)

# Parse the content and extract the titles
# . for class; # for ID 
raw.titles <- html_nodes(res,"table.wikitable")
text <- html_text(html_nodes(x =res, css = "table.wikitable tbody tr td"))
text <- html_text(raw.titles)
#View(text)
#2017-2018
text <- text[21:24]    #第991~1183集 (2017/0703~2018/06/07)
#text <- matrix(text, nrow=length(text))
text <- lapply(X = text, FUN = strsplit, split="\n", fixed=T)
#text <- lapply(text, unlist)
text <- unlist(text)
title <- text[1:5]
n = 5000


#---------清理函數-------
clean <- function(text){
  
  #----------------the final version----------------------
  #f <- function(t1){
  #tda = matrix(tok, ncol=length(title),byrow=TRUE)
  #t1 <- text[[2]]
  #t1 <- unlist(strsplit(t1,split="\n", fixed=T))
  #t1 <- unlist(strsplit(t1,split="\n", fixed=T))
  t1 <- text
  t1 <- t1[-(1:5)]
  #-------重播位置-----------
  p <- numeric()     #重播位置
  for(i in 1:length(t1)){
    if(gregexpr("年",t1[i])[[1]][1]==5){
      p <- c(p,i)
    }
  }
  t1 <- t1[-p]
  
  
  #--------嘉賓佔多格--guest-----
  
  #處理嘉賓佔多格的情況
  a=1
  while(a<10){
    start <- as.numeric(t1[1])
    ep <- seq(start, start+length(t1),by=1)
    j<-1        #ep位置
    rplace<-1   #r 位置
    r=numeric()
    
    for(i in 1:n){
      if(is.na(t1[i+5])){break}
      if(t1[i]=="集數"){
        t1 <-c(t1[1:(i-1)], t1[(i+5):length(t1)])
        print(i)
      }
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+6]){     #嘉賓佔2格
        t1[i+3] <- paste0(t1[i+3],",",t1[i+4])
        r[rplace] <- (i+4)                        #嘉賓第二格的位置(之後要移除的)
        j=j+1
        rplace=rplace+1
      }
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+7]){     #嘉賓佔3格
        t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5])
        r[rplace:(rplace+1)] <- c(i+4,i+5)        #嘉賓第二格的位置(之後要移除的)
        j=j+1
        rplace=rplace+2
      }
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+8]){     #嘉賓佔4格
        t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5],",",t1[i+6])
        r[rplace:(rplace+2)] <- c(i+4,i+5,i+6)        #嘉賓第二格的位置(之後要移除的)
        j=j+1
        rplace=rplace+3
      }
      if((ep[j]==t1[i]&&ep[j+1]==t1[i+5])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+4])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+3])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+2])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+1])
      ){     #嘉賓佔一格
        j=j+1
      }
      
    }
    if(c(r,2) == 2){break}
    t1 <- t1[-r]
    a=a+1
  }
  
  #--------上下分集----------
  i=1
  for(i in 1:n){
    if(is.na(t1[i+5])){break}
    #1. (下) 接 (上) 
    if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(下)") && (substr(t1[i+5], 1, 5) == substr(t1[i+1], 1, 5))){
      act2 <- substr(t1[i+1], gregexpr(")",t1[i+1])[[1]][1]+2, nchar(t1[i+1]))  #演員暫時 ()
      act1 <- t1[i-4]
      t1[i] <- paste0(t1[i], ",", substr(t1[i+1], 1, gregexpr(")",t1[i+1])[[1]][1])) #雙重主題
      t1[i+1] <- paste0(act1, ",", act2)    #雙重演員
      t1 <-c(t1[1:(i+5)], act2, "", t1[(i+6):length(t1)])   #接續的演員
      print(i)
    }
    #2.  處理上下不同天的情況
    if(substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5)&&t1[i]!=""&&t1[i+1]!=""&&nchar(t1[i+6])<5&&t1[i+6]!=""){
      act <- t1[i+1]
      t1 <-c(t1[1:(i+5)], act, "", t1[(i+6):length(t1)])   #接續的演員
      print(i)
    }
    #1018 第二回的bug
    if(substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5)&&t1[i]!=""&&t1[i+1]!=""&&nchar(t1[i+6])<5){
      act <- t1[i+1]
      t1 <-c(t1[1:(i+5)], act, t1[(i+6):length(t1)])   #接續的演員
      print(i)
    }
    #3.  處理上下不同天的情況，但沒有上下分集
    #1001
    if(nchar(t1[i])<=4&&nchar(t1[i+2])<=4&&t1[i]!=""&&t1[i+2]!=""){
      if(t1[i]==(as.numeric(t1[i+2])-1)){
        t1 = c(t1[1:(i+1)],t1[(i-3):(i-1)],t1[(i+2):length(t1)])
      }
    }
    if(t1[i]==1182){break}
    if(nchar(t1[i])<=4&&nchar(t1[i+2])<=4&&t1[i]!=""){
      if(t1[i]==(as.numeric(t1[i+3])-1)){
        t1 = c(t1[1:(i+1)],t1[(i-3):(i-2)],t1[(i+2):length(t1)])
      }
    }
    
  }

  t1 <- data.frame(matrix(t1, ncol=length(title), byrow=TRUE))
  names(t1) <- title
  return(t1)

  
}
c_text <- clean(text)   #處理好的data.frame
#清理空白的欄位(row)
for(i in 1:nrow(c_text)){
  if(c_text[i,3]==""){
    c_text <- c_text[1:(i-1),]
    return(c_text)
  } 
}

#整理格式
c_text$"主題" <- as.character(c_text$"主題")
c_text$"來賓" <- as.character(c_text$"來賓")
c_text$"集數" <- as.numeric(as.character(c_text$"集數"))
#來賓
guest <- c_text$"來賓"
strsplit(guest[1],"、")

library(magrittr)
guest <- guest %>% lapply(.,strsplit, "、") %>%
  unlist %>%
  lapply(.,strsplit,":") %>%
  unlist %>%
  lapply(.,strsplit,"&") %>%
  unlist %>%
  lapply(.,strsplit,",") %>% unlist %>%
  lapply(.,strsplit,"：") %>% unlist

f_guest <- as.factor(guest)
View(table(f_guest))

View(c_text)



