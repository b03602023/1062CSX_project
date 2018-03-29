## R_資料科學程式設計

### week_4

- week_4任務

    - https://ntu-csx-datascience.github.io/106-2RSampleCode/week_4/task_4/WordCloud-1.html    
    - https://ntu-csx-datascience.github.io/106-2RSampleCode/week_4/task_4/WordCloud-2.html    
    - 建立一命名為 week_4(or task_4, hw_4)的資料夾。
    - 完成一份由透過社群網站 Open API 取得文本的文字雲上傳至資料夾中，須繳交兩份檔案[.Rmd, .html)]。

---
#### in-class
>Facebook: [Graph API Explorer](https://developers.facebook.com/tools/explorer/145634995501895/)
>
>向下搜尋更舊的貼文可以利用for迴圈以及`posts$posts$paging$`next``



```{r}
library(httr)

token  = "EAACEdEose0cBAEdvZCE1C2g4rWepyX1DwggxJRl4BPVUD7QeyuKLYiDMzuqeEbXEX59vW2cEvbOZAEmsQfAl084x2RJCFxDGu2UOtCJbsCRma54ao6hZAdPvYwkDLPvt65GmIm84vxWd3hJ10Wi3hPNfFDgeHHs9vEu33W8IgJGBr1yWfbIZAGZAZBZAETpND0ZD"
prefex = "https://graph.facebook.com/v2.12/twTOEFL/?fields=posts&access_token="
url    = paste0(prefex, token)
res    = httr::GET(url)
posts  = content(res)


posts=list()

for(c in 1:5){
  token  = "EAACEdEose0cBAEdvZCE1C2g4rWepyX1DwggxJRl4BPVUD7QeyuKLYiDMzuqeEbXEX59vW2cEvbOZAEmsQfAl084x2RJCFxDGu2UOtCJbsCRma54ao6hZAdPvYwkDLPvt65GmIm84vxWd3hJ10Wi3hPNfFDgeHHs9vEu33W8IgJGBr1yWfbIZAGZAZBZAETpND0ZD"
  prefex = "https://graph.facebook.com/v2.12/twTOEFL/?fields=posts&access_token="
  url    = paste0(prefex, token)
  res    = httr::GET(url)
  temp  = content(res)
  posts = append(temp,posts)
  url = posts$posts$paging$`next`
}
```


#### home task
##### 字串處理
>[字串處理函數](https://molecular-service-science.com/2015/01/18/text-processing-in-r-using-grep/)
>[Handling and processing string in R](http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf)

##### 文字雲
>[Introduction to data cleaning in R](https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf)
>中文斷字處理
>詞頻矩陣inspect()
>切詞器cutter=worker()
>將新詞彙加入詞庫new_user_word
>切割詞彙與提供詞彙的詞性cutter=worker("tag"
>
>文字雲套件package: [wordcloud()](https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf)
>
>
>[Text mining and word cloud fundamentals in R : 5 simple steps you should know](http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know)
>
>
>

###### 整理資料
>[整理資料食用R套件](https://dsp.im/2015/02/r-packages/)
>
>[Data Processing with dplyr & tidyr](https://rpubs.com/bradleyboehmke/data_wrangling)
>
>分析方法
>>[雲端資料分析導引系統](http://www.r-web.com.tw/guider/1/section_A.php)


