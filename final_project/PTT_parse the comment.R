# PTT 

rm(list = ls())
library(rvest)
library(magrittr)

index <- seq(379, 525, 1)
url <- paste0("https://www.ptt.cc/bbs/TW_Entertain/index", 
              index, ".html")

link_extra <- function(url){
  res <- read_html(url)
  raw.titles <- res %>% html_nodes("div.title")
  
  ptt.link <- raw.titles %>% html_nodes("a") %>% html_attr('href') %>%
    paste0("https://www.ptt.cc",.)
  
  titles <- raw.titles %>% html_nodes("a") %>% html_text(.)
  re_titles <- titles[grepl(pattern = "綜藝大熱門", titles)]
  re_ptt.link <- ptt.link[grepl(pattern = "綜藝大熱門", titles)]
  #print(paste0(length(re_titles), ", ", length(re_ptt.link)))
  #re_titles <- re_titles[grepl(pattern = "實況", re_titles)]
  #re_ptt.link <- re_ptt.link[grepl(pattern = "實況", re_titles)]
  #print(paste0("* ", length(re_titles), ", ", length(re_ptt.link)))

  return(re_ptt.link)
}

title_extra <- function(url){
  res <- read_html(url)
  raw.titles <- res %>% html_nodes("div.title")
  
  ptt.link <- raw.titles %>% html_nodes("a") %>% html_attr('href') %>%
    paste0("https://www.ptt.cc",.)
  
  titles <- raw.titles %>% html_nodes("a") %>% html_text(.)
  re_titles <- titles[grepl(pattern = "綜藝大熱門", titles)]
  re_ptt.link <- ptt.link[grepl(pattern = "綜藝大熱門", titles)]
  #print(paste0(length(re_titles), ", ", length(re_ptt.link)))
  #re_titles <- re_titles[grepl(pattern = "實況", re_titles)]
  #re_ptt.link <- re_ptt.link[grepl(pattern = "實況", re_titles)]
  #print(paste0("* ", length(re_titles), ", ", length(re_ptt.link)))
  
  return(re_titles)
}
ptt.link <- sapply(X = as.list(url), FUN = link_extra)
ptt.title <- sapply(X = as.list(url), FUN = title_extra)
links_V <- unlist(ptt.link)
titles_V <- unlist(ptt.title)
View(cbind(titles_V, links_V))

#--------- Extract the date--------
date_extra <- function(ptt.link){
  res <- read_html(ptt.link)
  raw.dates <- res %>% html_nodes(".push-ipdatetime")
  raw.dates <- as.character(raw.dates)
  start <- regexpr(pattern = "/", raw.dates)

  date <- substr(x = raw.dates, start-2, start+2)[1]
  return(date)
}

dates <- sapply(X = links_V, FUN = date_extra)


#--------- Extract comments in PTT articles------
ptt_art_extra <- function(ptt.link){
  res <- read_html(ptt.link)
  raw.titles <- res %>% html_nodes("span.push-content")
  
  comments <- html_text(raw.titles )
  comments <- gsub(pattern = ": ", replacement = "", x = comments)
}
comments <- lapply(X = links_V, FUN = ptt_art_extra)

tdl <- cbind(titles_V, dates, links_V)


recomments <- comments[grepl(pattern = "實況", tdl[,1])]
tdl <- tdl[grepl(pattern = "實況", tdl[,1]),]


recomments <- recomments[!grepl(pattern = "Re", tdl[,1])]
tdl <- tdl[!grepl(pattern = "Re", tdl[,1]),]
rownames(tdl) <- 1:nrow(tdl)

tdl <- tdl[1:207,]
recomments <- recomments[1:207]

for(i in 1:nrow(tdl)){
  
  paste0(grep(tdl[i,2], "/"))
  
}

# tdl contains the title, date and llink for the article (matrix)
# and comments is a class of list.
View(cbind(tdl[,2], as.character(all[,2])))

#-------------------------------------------------------------



#------------- Extract ptt link using GOOGLE------------------
start=seq(0, 190, 10)
url <- paste0("https://www.google.com.tw/search?q=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80+%E5%AF%A6%E6%B3%81+PTT+site:www.ptt.cc&rlz=1C1NHXL_zh-TWTW791TW791&tbs=cdr:1,cd_min:5/22/2017,cd_max:5/24/2018,sbd:1&ei=T8csW7mmGpTv-Qb7maS4CQ&start=", 
              start, "&sa=N&biw=1517&bih=705")


ptt_link_extra <- function(url){
  res <- read_html(url)
  raw.titles <- res %>% html_nodes("h3")
  
  ptt.link <- raw.titles %>% html_node("a") %>% html_attr('href')
  start <- regexpr(pattern = "http", ptt.link)
  end <- regexpr(pattern = "html", ptt.link)
  ptt.link <-  substr(ptt.link, start, end+3)
  return(ptt.link)
}



#--------- Extract comments in PTT articles------
ptt_art_extra <- function(ptt.link){
  res <- read_html(ptt.link)
  raw.titles <- res %>% html_nodes("span.push-content")
  
  comments <- html_text(raw.titles )
  comments <- gsub(pattern = ": ", replacement = "", x = comments)
}

#--------- Extract the date--------
date_extra <- function(ptt.link){
  res <- read_html(ptt.link)
  raw.dates <- res %>% html_nodes(".push-ipdatetime")
  raw.dates <- as.character(raw.dates)
  start <- regexpr(pattern = "\"> ", raw.dates)
  end <- regexpr(pattern = "\n", raw.dates)
  date <- substr(x = raw.dates, start+3, end-7)[1]
  return(date)
}
ptt.link <- sapply(X = as.list(url), FUN = ptt_link_extra)
comments <- lapply(X = ptt.link, FUN = ptt_art_extra)
dates <- vapply(X = ptt.link, FUN = date_extra, FUN.VALUE = )

