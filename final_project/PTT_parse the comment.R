# PTT 

rm(list = ls())
library(rvest)
start=seq(0, 190, 10)
url <- paste0("https://www.google.com.tw/search?q=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80+%E5%AF%A6%E6%B3%81+PTT+site:www.ptt.cc&rlz=1C1NHXL_zh-TWTW791TW791&tbs=cdr:1,cd_min:5/22/2017,cd_max:5/24/2018,sbd:1&ei=T8csW7mmGpTv-Qb7maS4CQ&start=", 
       start, "&sa=N&biw=1517&bih=705")

res <- read_html(url[1])
raw.titles <- res %>% html_nodes("h3")

# Extract link
ptt.link <- raw.titles %>% html_node("a") %>% html_attr('href')

# Extract article
#movie.article.title <- raw.titles %>% html_node("a") %>% html_text() 

