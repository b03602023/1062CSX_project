rm(list=ls(all.names=TRUE))

### practice_3
### Crawler Example

### Crawler_Example with rvest    #####################################################################
# ?x??https://blog.gtwang.org/r/rvest-web-scraping-with-r/
rm(list = ls())
library(rvest)
library(magrittr)

# Set url
url <- "https://www.imdb.com/chart/top?ref_=nv_wl_img_3"
# Get response
res <- read_html(url)

# Parse the content and extract the titles
raw.titles <- res %>% html_nodes("td.titleColumn")
raw.rate <- res %>% html_nodes(x = ., css = "td.ratingColumn.imdbRating")
# Extract link
movie.article.link <- raw.titles %>% html_node("a") %>% html_attr('href')

# Extract article
movie.article.title <- raw.titles %>% html_node("a") %>% html_text() 

# Extract rating
movie.rating<- raw.rate %>% html_node("strong") %>% html_text()

# !!!!!!!!!!!Extract rating people numbers
movie.ratepeople<- raw.rate %>% html_node("strong") %>% html_attr('title')

spli <- strsplit(movie.ratepeople," ") 
dd <- data.frame(spli)
names(dd) <- as.character(1:length(movie.rating))
dd=t(dd)

as.factor(dd[,4])


# Create dataframe
movie.df <- data.frame(movie.article.title, movie.article.link,movie.rating,dd[,4],movie.ratepeople)

# Set df's colnames
colnames(movie.df) <- c("title", "link","rating","the_number_of_rating_people","ps")
View(movie.df)

# Use ggplot
ggplot(data = movie.df, aes(x=rating, y=the_number_of_rating_people)) + 
  geom_area(stat = "bin")





#-------------------
rm(list=ls(all.names=TRUE))

### practice_3
### Crawler Example

### Crawler_Example with rvest    #####################################################################
# ?x??https://blog.gtwang.org/r/rvest-web-scraping-with-r/
rm(list = ls())
library(rvest)

# Set url
url <- "https://www.imdb.com/chart/top?ref_=nv_wl_img_3"
# Get response
res <- read_html(url)

# Parse the content and extract the titles
raw.titles <- res %>% html_nodes("td")

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
#????R????使用????系??????????；????????R之??此設定就?v?失??
#中????為???????Sys.setlocale(category = "LC_ALL", locale = ??UTF-8")
#??????中??
Sys.setlocale(category = "LC_ALL", locale = "cht")
urlPath <- "https://www.ptt.cc/bbs/movie/index.html"

# Download a URI {RCurl}
# ????網??????始碼
temp    <- getURL(urlPath, encoding = "utf-8")
##下??後????容??HTML????，?I法直??????。??以利??XML套件?AeadHTMLTable??XPath????????將??要??資??從??件中?u???????
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