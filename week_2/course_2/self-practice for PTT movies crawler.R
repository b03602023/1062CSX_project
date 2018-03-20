rm(list=ls(all.names=TRUE))

### practice_3
### Crawler Example

### Crawler_Example with rvest    #####################################################################
# 參考：https://blog.gtwang.org/r/rvest-web-scraping-with-r/
rm(list = ls())
library(rvest)

# Set url
url <- "https://www.ptt.cc/bbs/movie/index.html"
# Get response
res <- read_html(url)

# Parse the content and extract the titles
raw.titles <- res %>% html_nodes("div.title")

# Extract link
movie.article.link <- raw.titles %>% html_node("a") %>% html_attr('href')

# Extract article
movie.article.title <- raw.titles %>% html_node("a") %>% html_text() 

# Create dataframe
movie.df <- data.frame(movie.article.title, movie.article.link)

# Set df's colnames
colnames(movie.df) <- c("title", "link")
View(movie.df)

#------------------------------------------------------
library(XML)
library(RCurl)
library(httr)
#改變R目前使用的語系（暫時有效；重新啟動R之後此設定就會消失）
#中文換為英文： Sys.setlocale(category = "LC_ALL", locale = “UTF-8")
#英文換中文
Sys.setlocale(category = "LC_ALL", locale = "cht")
urlPath <- "https://www.ptt.cc/bbs/movie/index.html"

# Download a URI {RCurl}
# 獲得網頁的原始碼
temp    <- getURL(urlPath, encoding = "utf-8")
##下載後的內容是HTML格式，無法直接分析。所以利用XML套件的readHTMLTable和XPath的功能來將需要的資訊從文件中萃取出來。
xmldoc  <- htmlParse(temp)
title   <- xpathSApply(xmldoc, "//div[@class=\"title\"]", xmlValue)
title   <- gsub("\n", "", title)
title   <- gsub("\t", "", title)
author  <- xpathSApply(xmldoc, "//div[@class='author']", xmlValue)
path    <- xpathSApply(xmldoc, "//div[@class='title']/a//@href")
date    <- xpathSApply(xmldoc, "//div[@class='date']", xmlValue)
response<- xpathSApply(xmldoc, "//div[@class='nrec']", xmlValue)
alldata <- data.frame(title, author, path, date, response)
write.table(alldata, file = "movie.csv")