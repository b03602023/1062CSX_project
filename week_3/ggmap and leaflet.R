#---------文化部收錄獨立特色書店的資料。--------
library(jsonlite)
url <- "https://cloud.culture.tw/frontsite/trans/emapOpenDataAction.do?method=exportEmapJson&typeId=M"
my.data <- fromJSON(url)
View(my.data)
# 排序--由大到小
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
twmap0 <- ggmap(twmap)+ 
  geom_point(data = my.data, 
             aes(x = longitude, y = latitude, colour = hitRate), 
             size = 9) +
  
  scale_color_continuous(low = "yellow", high = "black")+
  guides(size = FALSE)
twmap0

p <- ggplotly(twmap0)  
p
#------leaflet---------
library(leaflet)
map1 <- leaflet() %>%
  addTiles() %>%  # 加上預設的地圖資料
  addMarkers(lng=my.data$longitude, lat=my.data$latitude, popup="訊息方塊的文字說明")
map1  # 繪製地圖

content <- paste(sep = "<br/>",
                 "書店名: ",my.data$name,
                 "hitRate: ",my.data$hitRate
)

map2 <- leaflet(my.data) %>%
  addTiles() %>%  # 加上預設的地圖資料
  addCircleMarkers(radius = rank(my.data$hitRate)/5, 
                   color = "red", 
                   popup = content,
                   clusterOptions = markerClusterOptions()
                   )
map2  # 繪製地圖



#--------plotly-----------
library(plotly)
library(ggmap)
p <- ggplotly(twmap0)  
p
