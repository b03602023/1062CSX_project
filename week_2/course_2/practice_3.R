### practice_3
### Crawler Example

### Crawler_Example with rvest    #####################################################################
# 參考：https://blog.gtwang.org/r/rvest-web-scraping-with-r/
rm(list = ls())
library(rvest)

# Set url
url <- "https://www.ptt.cc/bbs/NBA/index5720.html"
# Get response
res <- read_html(url)

# Parse the content and extract the titles
raw.titles <- res %>% html_nodes("div.title")

# Extract link
nba.article.link <- raw.titles %>% html_node("a") %>% html_attr('href')

# Extract article
nba.article.title <- raw.titles %>% html_text()

# Create dataframe
nba.df <- data.frame(nba.article.title, nba.article.link)

# Set df's colnames
nba.df.col.names <- c("title", "link")
colnames(nba.df) <- nba.df.col.names


### Crawler_Example with jsonlite #####################################################################\
rm(list = ls())
library(jsonlite)
url <- "https://www.dcard.tw/_api/posts?popular=true"
res <- fromJSON(url)

#---------文化部收錄獨立特色書店的資料。--------
library(jsonlite)
url <- "https://cloud.culture.tw/frontsite/trans/emapOpenDataAction.do?method=exportEmapJson&typeId=M"
my.data <- fromJSON(url)
View(my.data)
my.data <- my.data[order(my.data$hitRate, decreasing = TRUE), ]
#barplot(height = my.data$hitRate, names.arg = my.data$name)
library(ggplot2)
p <- ggplot(data = my.data, aes(x = name, y = hitRate), group = areaCode, colour = areaCode) + 
  geom_point(shape = 2, colour = my.data$areaCode)
p
library(choroplethr)

library(plotly)
library(ggmap)

my.data$longitude<-as.numeric(my.data$longitude)
my.data$latitude<-as.numeric(my.data$latitude)
twmap <- get_map(location = c(121.53,24.8), zoom = 12.8, maptype = 'roadmap')
twmap0 <- ggmap(twmap, extent = 'device')+    #extent = 'device' 滿版
  geom_point(data = my.data, 
             aes(x = longitude, y = latitude, colour = hitRate), 
             size = 9) +
  
  scale_color_continuous(low = "yellow", high = "black")+
  guides(size = FALSE)
twmap0

p <- ggplotly(twmap0)  
p
#--------plotly-----------
library(plotly)
library(ggmap)
p <- ggplotly(twmap0)  
p
