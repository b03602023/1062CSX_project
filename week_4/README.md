## R_資料科學程式設計

### week_4


---
- in-class
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


