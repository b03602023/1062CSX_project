library(rvest)
url <- "https://zh.wikipedia.org/zh-tw/%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80"
# Get response
res <- read_html(url)

# Parse the content and extract the titles
# . for class; # for ID 
raw.titles <- html_nodes(res,"table.wikitable")
text <- html_text(html_nodes(x =res, css = "table.wikitable tbody tr td"))
text <- html_text(raw.titles)
#�?91~1183??(2017/07/03~2018/06/07)
text <- text[20:24]   
text <- lapply(X = text, FUN = strsplit, split="\n", fixed=T)
text <- unlist(text)
title <- text[1:5]

n = 5000

#--------?��? 967~1175 (20170522�?0180524)---------
for(i in 1:length(text)){
  if(text[i]==967){start = i}
  if(text[i]==1175){over = i + 4}
  #text <- text[((text[i]==1175)+4)]
}

text <- text[start:over]
for(i in 1:length(text)){
  if(text[i]=="?�數"){
    text <-c(text[1:(i-1)], text[(i+5):length(text)])
    print(i)
  }
}

#---------?��??�數-------
clean <- function(text){
  
 
  t1 <- text
  #-------?�播位置-----------
  p <- numeric()     #?�播位置
  for(i in 1:length(t1)){
    if(gregexpr("�?,t1[i])[[1]][1]==5){
      p <- c(p,i)
    }
  }
  t1 <- t1[-p]
  
  
  #--------?��?佔�???-guest-----
  #?��??��?佔�??��??��?
  a=1
  while(a<10){
    start <- as.numeric(t1[1])
    ep <- seq(start, start+length(t1),by=1)
    j<-1        #ep位置
    rplace<-1   #r 位置
    r=numeric()
    
    
    for(i in 1:n){
      if(is.na(t1[i+5])){break}
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+6]&&substr(t1[i+3], 1, 5) == substr(t1[i+8], 1, 5)){     #?��?�??��?但�?上�???
        
        j=j+1
        
      }
      
      
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+6]&&substr(t1[i+3], 1, 5) != substr(t1[i+8], 1, 5)){     #?��?�???
        t1[i+3] <- paste0(t1[i+3],",",t1[i+4])
        r[rplace] <- (i+4)                        #?��?第�??��?位置(之�?要移?��?)
        j=j+1
        rplace=rplace+1
      }
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+7]){     #?��?�???
        t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5])
        r[rplace:(rplace+1)] <- c(i+4,i+5)        #?��?第�??��?位置(之�?要移?��?)
        j=j+1
        rplace=rplace+2
      }
      if(ep[j]==t1[i]&&ep[j+1]==t1[i+8]){     #?��?�???
        t1[i+3] <- paste0(t1[i+3],",",t1[i+4],",",t1[i+5],",",t1[i+6])
        r[rplace:(rplace+2)] <- c(i+4,i+5,i+6)        #?��?第�??��?位置(之�?要移?��?)
        j=j+1
        rplace=rplace+3
      }
      if((ep[j]==t1[i]&&ep[j+1]==t1[i+5])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+4])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+3])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+2])||
         (ep[j]==t1[i]&&ep[j+1]==t1[i+1])
      ){     #?��?佔�???
        j=j+1
      }
      
    }
    if(c(r,2) == 2){break}
    t1 <- t1[-r]
    a=a+1
  }
  
  #--------上�??��?----------
  i=1
  for(i in 1:n){
    if(is.na(t1[i+5])){break}
    #1. (�? ??(�? 
    if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(�?") && (substr(t1[i+5], 1, 5) == substr(t1[i+1], 1, 5))){
      act2 <- substr(t1[i+1], gregexpr(")",t1[i+1])[[1]][1]+2, nchar(t1[i+1]))  #演員?��? ()
      act1 <- t1[i-4]
      t1[i] <- paste0(t1[i], ",", substr(t1[i+1], 1, gregexpr(")",t1[i+1])[[1]][1])) #?��?主�?
      t1[i+1] <- paste0(act1, ",", act2)    #?��?演員
      t1 <-c(t1[1:(i+5)], act2, "", t1[(i+6):length(t1)])   #?��??��???
      print("1")
    }
    #2.  ?��?上�?不�?天�??��?  976(48)  
    if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="（�?�?)&&
       (substr(t1[i+1],(nchar(t1[i+1])-2),nchar(t1[i+1]))=="（�?�?)&&
       (substr(t1[i+6],(nchar(t1[i+6])-2),nchar(t1[i+6]))=="（�?�?)){
      print(t1[i])
      act <- paste0(t1[i-4],",",t1[i+2])
      t1 <-c(t1[1:(i-1)], paste0(t1[(i)],t1[i+1]), act, "", 
             t1[(i+4):(i+6)],t1[i+2],"",t1[(i+7):length(t1)])   #?��??��???
      print("2")
    }
    #2.  ?��?上�?不�?天�??��? 1112(719)
    if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(�?")&&
       (substr(t1[i+1],(nchar(t1[i+1])-2),nchar(t1[i+1]))=="(�?")&&
       (substr(t1[i+6],(nchar(t1[i+6])-2),nchar(t1[i+6]))=="(�?")){
      print(t1[i])
      act <- paste0(t1[i-4],",",t1[i+2])
      t1 <-c(t1[1:(i-1)], paste0(t1[(i)],t1[i+1]), act, "", 
             t1[(i+4):(i+6)],t1[i+2],"",t1[(i+7):length(t1)])   #?��??��???
      print("2-2")
    }
    #?��?上�?不�?天�??��?&????�天?�這種?��? 976
    #1030 第�??��?bug (1040)
    if(nchar(t1[i])==4&&substr(t1[i],2,2)!="??&&t1[i+2]==""){
      print(t1[i])
      t1 <- c(t1[1:(i+1)],t1[(i-3):(i-2)],t1[(i+2):length(t1)])
      print("4")
    }
    #1018 第�??��?bug
    if(substr(t1[i], 1, 5) == substr(t1[i+5], 1, 5)&&
       t1[i]!=""&&t1[i+1]!=""&&nchar(t1[i+6])<5 && nchar(t1[i])>4 &&
       (substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))!="(�?")){
      print(t1[i])
      act <- t1[i+1]
      t1 <-c(t1[1:(i+5)], act, t1[(i+6):length(t1)])   #?��??��???
      print("5")
      
    }
    #1120 bug
    if((substr(t1[i],(nchar(t1[i])-2),nchar(t1[i]))=="(�?")&&
       (substr(t1[i+5],(nchar(t1[i+5])-2),nchar(t1[i+5]))=="(�?") && t1[i+2]=="" &&
       (substr(t1[i-1],(nchar(t1[i-1])-2),nchar(t1[i-1]))!="(�?")&&
       (substr(t1[i+6],(nchar(t1[i+6])-2),nchar(t1[i+6]))!="(�?") && 
       nchar(t1[i+6])==4){
      print(t1[i])
      act <- t1[i+1]
      t1 <-c(t1[1:(i+5)], act,"", t1[(i+6):length(t1)])   #?��??��???
      print("6")
    }
    #3.  ?��?上�?不�?天�??��?，�?沒�?上�??��?
    #1001
    if(nchar(t1[i])<=4&&nchar(t1[i+2])<=4&&t1[i]!=""&&t1[i+2]!=""){
      if(t1[i]==(as.numeric(t1[i+2])-1)){
        print(t1[i])
        t1 = c(t1[1:(i+1)],t1[(i-3):(i-1)],t1[(i+2):length(t1)])
        print("7")
      }
    }
    if(t1[i]==1182){break}
    if(nchar(t1[i])<=4&&nchar(t1[i+2])<=4&&t1[i]!=""&&nchar(t1[i+4])<=4){
      if(t1[i]==(as.numeric(t1[i+3])-1)){
        t1 = c(t1[1:(i+1)],t1[(i-3):(i-2)],t1[(i+2):length(t1)])
        print("8")
      }
    }
    
  }
  
  t1 <- data.frame(matrix(t1, ncol=length(title), byrow=TRUE))
  names(t1) <- title
  return(t1)
  
  
}
c_text <- clean(text)   #?��?好�?data.frame

#----?��??��?--------
c_text$"主�?" <- as.character(c_text$"主�?")
c_text$"來�?" <- as.character(c_text$"來�?")
c_text$"?�數" <- as.numeric(as.character(c_text$"?�數"))

#------?��?�?-------
guest <- c_text$"來�?"

library(magrittr)
cleanguest <- function(guest){
  guest <- guest %>% lapply(.,strsplit, "??) %>%
    unlist %>%
    lapply(.,strsplit,":") %>%
    unlist %>%
    lapply(.,strsplit,"&") %>%
    unlist %>%
    lapply(.,strsplit,",") %>% unlist %>%
    lapply(.,strsplit,"�?) %>% unlist 
  #%>%
    #lapply(.,strsplit,"(") %>% unlist %>%
    #lapply(.,strsplit,")") %>% unlist
  return(guest)
}
guest_list <- lapply(X = guest, FUN = cleanguest)
guest_vector <- cleanguest(guest)

f_guest <- as.factor(guest_vector)
#View(table(f_guest))


# ---- represent guest with numbers-----
l_guest <- levels(f_guest)
#l_guest
indic <- 1:length(l_guest)

i_guest <- matrix(nrow=length(guest_list),ncol=max(sapply(guest_list, length)))
#sapply(guest_list, length)   ?��??��??��?賓數

for(i in 1:length(guest_list)){
  temp <- as.numeric()
  for(j in 1:length(guest_list[[i]])){
    temp[j] <- indic[(guest_list[[i]][j]==l_guest)]
  }
  i_guest[i,1:(sapply(guest_list, length)[i])] <- temp
}
for(i in 1:nrow(i_guest)){
  i_guest[i,][is.na(i_guest[i,])] <- 0
  
}
Encoding(l_guest) <- "UTF-8"
write.csv(i_guest,"guest_number.csv")
write.csv(cbind(data.frame(indic),data.frame(l_guest)),"guest_indic.csv")
rr = read.csv("guest_indic.csv")


barplot(table(f_guest)[order(table(f_guest), decreasing = TRUE)][1:25]
        , las = 2, ylim = c(0,35))
grid()
length(levels(f_guest))
length(table(f_guest)[table(f_guest)==1])
#levels(f_guest)





#-------combine wiki content with youtube view------
view <- read.csv("youtubeview3.csv")
#View(view)
view <- as.character(unlist(view))
viewp <- as.character()
view_clean <- as.character()
for(i in 1:length(view)){
  viewp[i] <- substr(view[i],gregexpr("�?,view[i])[[1]]+1, nchar(view[i]))
  view_clean[i] <- paste0(substr(viewp[i],1,gregexpr(",",viewp[i])[[1]]-1),
         substr(viewp[i],gregexpr(",",viewp[i])[[1]]+1,nchar(viewp[i]))
  )
  if(nchar(viewp[i])>=9){
    part1 <- substr(viewp[i],1,gregexpr(",",viewp[i])[[1]]-1)
    part2 <- substr(viewp[i],gregexpr(",",viewp[i])[[1]]+1,nchar(viewp[i]))
    view_clean[i] <- paste0(part1, substr(part2,1,gregexpr(",",part2)[[1]]-1),
                            substr(part2,gregexpr(",",part2)[[1]]+1,nchar(part2))
                            )
  }
}
c_view <- as.numeric(view_clean)
all <- cbind(c_text, c_view)
#write.csv(all, "all.csv")


#---------group the view------------
viewgroup <- c(10^6, 8*10^5, 5*10^5, 3*10^5, 10^5)
g_view <- numeric()
for(i in 1:length(c_view)){
  a <- 1             # assignment
  if(c_view[i]>viewgroup[a]){
    g_view[i] <- a
    
  }
  a <- a+1
  if(a<6 && a>1){
    for(j in 1:4){
      if(c_view[i]>viewgroup[a] && c_view[i]<=viewgroup[a-1]){
        g_view[i] <- a
      }
      a <- a+1
    }
  }else{a <- a+4}
  
  if(c_view[i]<=viewgroup[a-1]){
    g_view[i] <- a
  }
}

plot(c_view, col=g_view, cex = 1.2, lwd = 4, ylab = "view")
for(i in 1:length(viewgroup)){
  abline(h = viewgroup[i], lty = 2)
}
#f_view <- factor(c_view, labels = g_view)
hist(g_view)

all <- cbind(all, g_view)
write.csv(all, "all.csv")
write.csv(levels(f_guest),"f_guest.csv")


#------- ?��?來�??��?�?-----------


colnames(i_guest) <- paste0("guest",1:28)
all_c_guest <- cbind(all, i_guest)
input <- all_c_guest[,7:35]
View(input)
mode(input)
class(input)

