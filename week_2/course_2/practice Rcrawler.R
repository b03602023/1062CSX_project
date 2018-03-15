# in class practice
library(tmcn)
library(rvest)
url="https://movies.yahoo.com.tw/chart.html?cate=rating"
# 訪問網站
html=read_html(url)
title=html_node(x = html, css = "div.rank_txt")
data=data.frame(title=toUTF8(html_text(title)))

