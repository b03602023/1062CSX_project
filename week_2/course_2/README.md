## R_資料科學程式設計

### week_2
#### 遇到之問題
`#無法解決電影連結和預告片連結的問題(因為其class 都有td)`
```{r}
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
```