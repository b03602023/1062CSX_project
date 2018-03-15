# in class practice
library(rvest)
html=read_html("https://www.imdb.com/chart/boxoffice?ref_=nv_ch_cht_1")
title = html_nodes(x = html, css = ".titleColumn a")
title = html_text(x = title)
gross = html_nodes(x = html, css = ".secondaryInfo")
gross = html_text(x = gross)
htitle = html_nodes(x = html, css = ".titleColumn a")
ref = html_attr(htitle,"href")
data=data.frame(title = title, gross = gross, link = ref)


#---------------------------------------------------------------
library(tmcn)
library(rvest)
url="https://movies.yahoo.com.tw/chart.html?cate=rating"
# 訪問網站
html=read_html(url)
#table.title = html_nodes(x = html, css = ".tr top")

title = html_nodes(x = html, css = ".rank_txt")

htitle = html_node(x = html, css = ".td a")
#無法解決電影連結和預告片連結的問題(因為其class 都有td)
ref = html_attr( htitle, "href" )

data=data.frame(title=toUTF8(html_text(title)), ref = ref)

# 將<div class="rank_txt">郵報：密戰</div>當中的文字擷取出來
# html_text()
# 可以用 toUTF8(html_text()) 轉成text檔案


#--------------------------------------------------------
# in class practice
library(tmcn)
library(rvest)
url="https://movies.yahoo.com.tw/chart.html?cate=rating"
# 訪問網站
html=read_html(url)
title=html_nodes(x = html, css = ".rank_txt")

data=data.frame(title=toUTF8(html_text(title)))
