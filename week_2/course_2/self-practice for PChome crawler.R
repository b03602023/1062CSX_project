
library(httr)

url <- "https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=%E5%A5%B3&page=1&sort=rnk/dc"
# 傳送get的請求(request)，取得網頁資訊response
# get response
res = GET(url)
# 若成功獲得response則狀態碼會顯示200 
# status: 200表示正常,404表示找不到網頁
# 查看content-type

res_json = httr::content(res)
do.call(rbind,res_json$prods)
View(data.frame(do.call(rbind,res_json$prods)))

url="https://ecshweb.pchome.com.tw/search/v3.3/all/categories?q=sony"
res = GET(url)
res_json = content(res)
do.call(rbind,res_json$prods)
View(data.frame(do.call(rbind,res_json$prods)))
