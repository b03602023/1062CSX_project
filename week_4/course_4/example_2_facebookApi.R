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


res = POST("https://graph.facebook.com/v2.12/me/feed",
           body=list(message=sprintf("[TEST Posting Message] %s At %s","httr 測試",Sys.time()),
                     access_token=token))
postId = content(res)$id


url = sprintf("https://graph.facebook.com/v2.12/%s?access_token=%s", postId, token)
res = DELETE(url)
content(res)
