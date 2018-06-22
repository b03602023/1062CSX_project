

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
for(i in 1:length(t1)){
  #1. (下) 接 (上) 
  if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(下)") && (substr(t1[i+5], 1, 5) == substr(t1[i+1], 1, 5))){
    act2 <- substr(t1[i+1], gregexpr(")",t1[i+1])[[1]][1]+2, nchar(t1[i+1]))  #演員暫時 ()
    act1 <- t1[i-4]
    t1[i] <- paste0(t1[i], ",", substr(t1[i+1], 1, gregexpr(")",t1[i+1])[[1]][1])) #雙重主題
    t1[i+1] <- paste0(act1, ",", act2)    #雙重演員
    t1 <-c(t1[1:(i+5)], act2, "", t1[(i+6):length(t1)])   #接續的演員
  }
  #2.  處理上下不同天的情況
  if(substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5)&&t1[i]!=""&&t1[i+1]!=""&&nchar(t1[i+6])<5){
    act <- t1[i+1]
    t1 <-c(t1[1:(i+5)], act, "", t1[(i+6):length(t1)])   #接續的演員
  }

}

#View(t1)
#View(t1[76:length(t1)])
View(matrix(t1, ncol=length(title), byrow=TRUE))
#t1 <- matrix(t1, ncol=length(title), byrow=TRUE)

