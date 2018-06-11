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
text <- text[22:24]    #23 has a problem (with two topic in one day 1/30)
#text <- matrix(text, nrow=length(text))
text <- lapply(X = text, FUN = strsplit, split="\n", fixed=T)
#text <- lapply(text, unlist)
text <- unlist(text)
title <- text[1:5]
n = 5000



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
  while(a<=2){
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
      if((ep[j]==t1[i]&&ep[j+1]==t1[i+5])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+4])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+3])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+2])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+1])
      ){     #嘉賓佔一格
        j=j+1
      }
      
    }
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
    if(substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5)&&t1[i]!=""&&t1[i+1]!=""&&nchar(t1[i+6])<5){
      act <- t1[i+1]
      t1 <-c(t1[1:(i+5)], act, "", t1[(i+6):length(t1)])   #接續的演員
      print(i)
    }
    
  }
  
  #print(j)
  #print(ep)
  #return(t1)
  #}
  #t1 <- f(text)
  
  #View(t1)
  #View(t1[76:length(t1)])
  t1 <- data.frame(matrix(t1, ncol=length(title), byrow=TRUE))
  names(t1) <- title
  return(t1)
  #t1 <- matrix(t1, ncol=length(title), byrow=TRUE)
  
}
c_text <- clean(text)   #處理好的data.frame








#----------the first one version---------------
#去除周五的重播、備註欄
sortout <- function(t){
  #去除周五的重播以及備註欄
  p <- numeric()
  q <- numeric()
  d1 <- 0     #d1和d2的作用就是避免嘉賓有空格而占兩個位置情況產生的bug
  d2 <- 1 
  for(i in 1:length(t)){
    if(t[i]==""){
      p=c(p,i)                          #p記憶""的位置
      if(length(p)%%4==0){d1=d1+1}
    }
    if(d1==d2){q=c(q,i+1);d2=d2+1}      #q記憶重播的位置
  }
  t <- t[-c(p,q)]
  #合併嘉賓的字串
 return(t)
}

clean <- function(t1){

  t1 <- unlist(strsplit(t1,split="\n", fixed=T))
  title <- t1[1:4]     #欄位名稱
  t1 <- t1[-(1:5)]
  t1 <- sortout(t1)
  #合併嘉賓字串
  start <- as.numeric(t1[1])
  ep <- seq(start, start+100,by=1)
  j<-1
  rplace<-1
  r=numeric()
  for(i in 1:length(t1)){
    if(is.na(t1[i+5])){break}
    if(ep[j]==t1[i]&&ep[j+1]==t1[i+5]){     #嘉賓佔2格
      t1[i+3] <- paste0(t1[i+3],",",t1[i+4])
      r[rplace] <- (i+4)                        #嘉賓第二格的位置(之後要移除的)
      j=j+1
      rplace=rplace+1
    }
    if(ep[j]==t1[i]&&ep[j+1]==t1[i+6]){     #嘉賓佔3格
      t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5])
      r[rplace:(rplace+1)] <- c(i+4,i+5)        #嘉賓第二格的位置(之後要移除的)
      j=j+1
      rplace=rplace+2
    }
    if(ep[j]==t1[i]&&ep[j+1]==t1[i+4]){     #嘉賓佔一格
      j=j+1
    }
  }
  t1 <- t1[-r]
  
  t1 <- data.frame(matrix(t1, ncol=4, byrow=T))
  names(t1) <- title
  return(t1)
}

text <- apply(text, 1,clean)

View(t1)




#--------original method-------------
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
text <- text[22:24]    #23 has a problem (with two topic in one day 1/30)
#text <- matrix(text, nrow=length(text))
text <- lapply(X = text, FUN = strsplit, split="\n", fixed=T)
#text <- lapply(text, unlist)
text <- unlist(text)
title <- text[1:5]
tda = matrix(tok, ncol=length(title),byrow=TRUE)

t1 <- text[[2]]
t1 <- unlist(strsplit(t1,split="\n", fixed=T))
#t1 <- unlist(strsplit(t1,split="\n", fixed=T))
title <- t1[1:5]     #欄位名稱
t1 <- t1[-(1:5)]

p <- numeric()     #重播位置
for(i in 1:length(t1)){
  if(gregexpr("年",t1[i])[[1]][1]==5){
    p <- c(p,i)
  }
}
t1 <- t1[-p]

#處理嘉賓佔多格的情況
start <- as.numeric(t1[1])
ep <- seq(start, start+100,by=1)
j<-1        #ep位置
rplace<-1   #r 位置
r=numeric()
for(i in 1:length(t1)){
  if(is.na(t1[i+5])){break}
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
  if((ep[j]==t1[i]&&ep[j+1]==t1[i+5])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+4])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+3])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+2])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+1])
  ){     #嘉賓佔一格
    j=j+1
  }
}
t1 <- t1[-r]

i=1
for(i in 1:85){
  #1. (下) 接 (上) 
  if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(下)") && (substr(t1[i+5], 1, 5) == substr(t1[i+1], 1, 5))){
    act2 <- substr(t1[i+1], gregexpr(")",t1[i+1])[[1]][1]+2, nchar(t1[i+1]))  #演員暫時 ()
    act1 <- t1[i-4]
    t1[i] <- paste0(t1[i], ",", substr(t1[i+1], 1, gregexpr(")",t1[i+1])[[1]][1])) #雙重主題
    t1[i+1] <- paste0(act1, ",", act2)    #雙重演員
    t1 <-c(t1[1:(i+5)], act2, "", t1[(i+6):length(t1)])   #接續的演員
  }
  #2.  處理上下不同天的情況
  if(substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5)&&t1[i]!=""){
    act <- t1[i+1]
    t1 <-c(t1[1:(i+5)], act, "", t1[(i+6):length(t1)])   #接續的演員
  }
  
}
View(t1)
View(matrix(t1, ncol=length(title), byrow=TRUE))
t1 <- matrix(t1, ncol=length(title), byrow=TRUE)



x=0
p <- numeric()

for(i in 1:length(t1))
  
  
  
  f <- function(t1){
    if(gregexpr("年",t1)[[1]][1]==5){
      x=x+1
      #p <- c(p, 21)
      return(x)
    }
  }

#去除周五的重播、備註欄
sortout <- function(t){
  #去除周五的重播以及備註欄
  p <- numeric()
  q <- numeric()
  d1 <- 0     #d1和d2的作用就是避免嘉賓有空格而占兩個位置情況產生的bug
  d2 <- 1 
  for(i in 1:length(t)){
    if(t[i]==""){
      p=c(p,i)                          #p記憶""的位置
      
    }
    if(as.numeric(t[i-4])%%4==2){d1=d1+1}           #
    if(d1==d2){q=c(q,i+1);d2=d2+1}      #q記憶重播的位置
  }
  t <- t[-c(p,q)]
  return(t)
}
t1 <- text[2]
t1 <- unlist(strsplit(t1,split="\n", fixed=T))
title <- t1[1:4]     #欄位名稱
t1 <- t1[-(1:5)]
t1 <- sortout(t1)
#合併嘉賓字串 & 解決主題分上下被攔腰的狀況

start <- as.numeric(t1[1])
ep <- seq(start, start+100,by=1)
j<-1
rplace<-1
r=numeric()
for(i in 1:length(t1)){
  if(is.na(t1[i+5])){break}
  if(ep[j]==t1[i]&&ep[j+1]==t1[i+5]){     #嘉賓佔2格
    t1[i+3] <- paste0(t1[i+3],",",t1[i+4])
    r[rplace] <- (i+4)                        #嘉賓第二格的位置(之後要移除的)
    j=j+1
    rplace=rplace+1
  }
  if(ep[j]==t1[i]&&ep[j+1]==t1[i+6]){     #嘉賓佔3格
    t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5])
    r[rplace:(rplace+1)] <- c(i+4,i+5)        #嘉賓第二格的位置(之後要移除的)
    j=j+1
    rplace=rplace+2
  }
  if(ep[j]==t1[i]&&ep[j+1]==t1[i+4]){     #嘉賓佔一格
    j=j+1
  }
}



t1 <- t1[-r]

t1 <- data.frame(matrix(t1, ncol=4, byrow=T))
names(t1) <- title



#---------other method-----------------
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
text <- text[22:24]    #23 has a problem (with two topic in one day 1/30)
#text <- matrix(text, nrow=length(text))
text <- lapply(X = text, FUN = strsplit, split="\n", fixed=T)
#text <- lapply(text, unlist)
text <- unlist(text)
title <- text[1:5]
tda = matrix(tok, ncol=length(title),byrow=TRUE)

t1 <- text[[2]]
t1 <- unlist(strsplit(t1,split="\n", fixed=T))
#t1 <- unlist(strsplit(t1,split="\n", fixed=T))
title <- t1[1:5]     #欄位名稱
t1 <- t1[-(1:5)]

p <- numeric()     #重播位置
for(i in 1:length(t1)){
  if(gregexpr("年",t1[i])[[1]][1]==5){
    p <- c(p,i)
  }
}
t1 <- t1[-p]

#處理嘉賓佔多格的情況
start <- as.numeric(t1[1])
ep <- seq(start, start+100,by=1)
j<-1        #ep位置
rplace<-1   #r 位置
r=numeric()
for(i in 1:length(t1)){
  if(is.na(t1[i+5])){break}
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
  if((ep[j]==t1[i]&&ep[j+1]==t1[i+5])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+4])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+3])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+2])||
     (ep[j]==t1[i]&&ep[j+1]==t1[i+1])
  ){     #嘉賓佔一格
    j=j+1
  }
}
t1 <- t1[-r]

i=1
actno = numeric()   #標示要更新的演員位置
act = character()
count_act = 1       

for(i in 1:88){
  #1. (下) 接 (上) 
  if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(下)") && (substr(t1[i+5], 1, 5) == substr(t1[i+1], 1, 5))){
    act2 <- substr(t1[i+1], gregexpr(")",t1[i+1])[[1]][1]+2, nchar(t1[i+1]))  #演員暫時 ()
    act1 <- t1[i-4]
    t1[i] <- paste0(t1[i], ",", substr(t1[i+1], 1, gregexpr(")",t1[i+1])[[1]][1])) #雙重主題
    #t1[i+1] <- paste0(act1, ",", act2)    #雙重演員
    actno[count_act] <- i+1
    act[count_act] <- paste0(act1, ",", act2)    #雙重演員
    count_act =count_act+1
    
    
    
    #t1 <-c(t1[1:(i+5)], act2, "", t1[(i+6):length(t1)])   #接續的演員
  }
  #2.  處理上下不同天的情況
  if((substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5))&&(t1[i]!="")){
    
    #t1 <-c(t1[1:(i+5)], act, "", t1[(i+6):length(t1)])   #接續的演員
    actno[count_act] <- i+1
    act[count_act] <- t1[i+1]          #接續的演員
    count_act =count_act+1
  }
  
}
View(t1)
View(matrix(t1, ncol=length(title), byrow=TRUE))
t1 <- matrix(t1, ncol=length(title), byrow=TRUE)



x=0
p <- numeric()

for(i in 1:length(t1))
  
  
  
  f <- function(t1){
    if(gregexpr("年",t1)[[1]][1]==5){
      x=x+1
      #p <- c(p, 21)
      return(x)
    }
  }

#去除周五的重播、備註欄
sortout <- function(t){
  #去除周五的重播以及備註欄
  p <- numeric()
  q <- numeric()
  d1 <- 0     #d1和d2的作用就是避免嘉賓有空格而占兩個位置情況產生的bug
  d2 <- 1 
  for(i in 1:length(t)){
    if(t[i]==""){
      p=c(p,i)                          #p記憶""的位置
      
    }
    if(as.numeric(t[i-4])%%4==2){d1=d1+1}           #
    if(d1==d2){q=c(q,i+1);d2=d2+1}      #q記憶重播的位置
  }
  t <- t[-c(p,q)]
  return(t)
}
t1 <- text[2]
t1 <- unlist(strsplit(t1,split="\n", fixed=T))
title <- t1[1:4]     #欄位名稱
t1 <- t1[-(1:5)]
t1 <- sortout(t1)
#合併嘉賓字串 & 解決主題分上下被攔腰的狀況

start <- as.numeric(t1[1])
ep <- seq(start, start+100,by=1)
j<-1
rplace<-1
r=numeric()
for(i in 1:length(t1)){
  if(is.na(t1[i+5])){break}
  if(ep[j]==t1[i]&&ep[j+1]==t1[i+5]){     #嘉賓佔2格
    t1[i+3] <- paste0(t1[i+3],",",t1[i+4])
    r[rplace] <- (i+4)                        #嘉賓第二格的位置(之後要移除的)
    j=j+1
    rplace=rplace+1
  }
  if(ep[j]==t1[i]&&ep[j+1]==t1[i+6]){     #嘉賓佔3格
    t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5])
    r[rplace:(rplace+1)] <- c(i+4,i+5)        #嘉賓第二格的位置(之後要移除的)
    j=j+1
    rplace=rplace+2
  }
  if(ep[j]==t1[i]&&ep[j+1]==t1[i+4]){     #嘉賓佔一格
    j=j+1
  }
}



t1 <- t1[-r]

t1 <- data.frame(matrix(t1, ncol=4, byrow=T))
names(t1) <- title






t1[,1] <- as.numeric(t1[,1])
