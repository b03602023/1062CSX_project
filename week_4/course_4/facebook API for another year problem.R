
#---------參考的code (可以解決載入較舊頁面的問題)-------
#--------限定時間期限的蒐集貼文方法------
#-----------defining the time------------
#20180330發現問題: 對於抓取橫跨不同年份的貼文，在while迴圈中會出現error
#就是雖然有nextflg，但是會造成ndata為空的list，而出現error。
rm(list=ls(all.names=TRUE))
library(httr)
prefex <- "https://graph.facebook.com/v2.10/"
token <- "EAACEdEose0cBAOLQrLZBZBtZBkEZBECAYXSt6RyEXNFxdT9TjqyHavhb0QZAvZBEhju3MxZCfkAVKj5ztRq5g0DhZCcmEoYNd7y6Ve8I0IHlHrZBSaZCZBOHLvYIZBcZBeP62BqnZBksgKsIwfAgeXPCPCIrP0UxTIwptkEWOSgwZBVcDyvyDRLPd4qVdfQPLy2HGT11I4ZD"
number <- 1      #只爬入一篇貼文post

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
#------another year------
# 271111393019477為TOEFL Taiwan的id
# 136845026417486 為柯文哲的id
# 175549807657 為低碳生活部落客的id
# 限定從2017-03-11到2018-03-22 時間內最近的一篇貼文
target <- "175549807657/posts?limit="
control <- "&until=2017-12-31&since=2017-01-01"
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
filename = paste0(count+1, ".txt")
write.table(groups,filename)
#要跳到下一頁
after  = data$paging$cursors$after  # after : This is the cursor that points to the end of the page of data that has been returned.

nextflg= data$paging$`next`    # == nextflg= data$paging[[2]]
#nextflg是要用來判斷是否到動態頁的最底端了( the last page of data)


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