#-----parse the observation data from Weather Bureau----
# 蒐集氣象局資料
# http://e-service.cwb.gov.tw/HistoryDataQuery/index.jsp
library(rvest)
library(magrittr)

start="2016-11"  #蒐集資料的起始日期yyyy-mm
end="2017-11"    #蒐集資料的結束日期yyyy-mm
# 不同的測站會有不同的prefex
prefex <- "http://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=466920&stname=%25E8%2587%25BA%25E5%258C%2597&datepicker="

time_count=function(start,end){
  start_yr = as.numeric(substr(start, 1, 4))
  end_yr = as.numeric(substr(end, 1, 4))
  start_m = as.numeric(substr(start, 6, 7))
  end_m = as.numeric(substr(end, 6, 7))
  m = 1:12
  yr = start_yr:end_yr
  #yr_temp = mapply(rep,yr[2:(length(yr)-1)],length(m))
  #t_temp = paste0(yr_temp,"-",m)
  t_temp = numeric()
  if(length(yr)!=1 && length(yr)!=2){
    t_temp = mapply(rep,yr[2:(length(yr)-1)],length(m)) %>% 
      paste0(.,"-",m)
  }
  #start_yr_temp = mapply(rep,start_yr,length(start_m:12))
  #start_t_temp = paste0(start_yr_temp,"-",start_m:12)
  start_t_temp = mapply(rep,start_yr,length(start_m:12)) %>%
    paste0(.,"-",start_m:12)
  
  #end_yr_temp = mapply(rep,end_yr,length(1:end_m))
  #end_t_temp = paste0(end_yr_temp,"-",1:end_m)
  end_t_temp = mapply(rep,end_yr,length(1:end_m)) %>%
    paste0(.,"-",1:end_m)
  t = c(start_t_temp,t_temp,end_t_temp)
  for(i in 1:length(t)){
    if(substr(t[i],7,7)==""){
      t[i] = paste0(substr(t[i],1,5),"0",substr(t[i],6,6))
    }
  }
  return(t)
}

parse <- function(time,prefex){
  url <- paste0(prefex,time)
  temp_data <- data.frame()
  for(i in 1:length(url)){
    res <- read_html(url[i])
    title <- html_text(html_nodes(res,"tr.second_tr"))
    vv <- html_nodes(x = res, css = "table#MyTable tbody tr td") %>% 
      html_text(.)
    # 清洗資料
    value <- gsub('\u00a0','',vv) %>% matrix(.,ncol = 32, byrow = T)
    temp_data  <- data.frame(value)
    title <- strsplit(title,split="\r\n\t\t", fixed=T) %>% unlist(.)
    title[length(title)] <- gsub("\t\t\r\n\t", replacement = "", title[length(title)])
    
    colnames(temp_data) <- title  # 加上每個欄位的名稱
    if(i == 1){data <- temp_data
    }else{
      data=rbind(data,temp_data)
    }
  }
  return(data)
}
time <- time_count(start, end)
data <- parse(time, prefex)


#利用主成分抽取有用資訊
#Performing Variable Selection 主成分分析: 減少數據集的維數，保留對變異數貢獻最大的特徵
#change the variable type
t_data <- data
t_data <- apply(t_data, MARGIN = 2, FUN = as.character)
t_data <- apply(t_data, 2, as.numeric)
t_data <- data.frame(matrix(t_data, nrow = nrow(data)))
names(t_data) <- names(data)
#delete the NAs column
for(i in 1:ncol(data)){
  if(sum(is.na(t_data[,i]))==nrow(t_data)){
    #names(t_data) <- names(t_data)[-i]
    t_data <- t_data[,-i]}
}
t_data <- t_data[,-1]
t_data <- t_data[complete.cases(t_data),]
#--------- PCA -------
pca_data <- prcomp(t_data, scale = TRUE)
stdev_data <- summary(pca_data)$importance[2,]
tp_data <- t_data[, which(stdev_data >= .04)]
tp_data <- cbind(t_data[,15],tp_data)
names(tp_data)[1] <- names(t_data)[15]
names(tp_data) <- c("rain","pressure","SLP","MaxP","MinP","T")

# 手動抽取資訊
#th_data  <- data[,c(2,8,9,11,13,14,17,22)]
#name <- names(th_data)
#change the data type
#str(t_data)
#th_data <- apply(th_data, MARGIN = 2, FUN = as.character)

#Removing NA Values 
#t_data <- data[complete.cases(th_data), ]
#th_data <- apply(th_data, 2, as.numeric)
#th_data <- data.frame(matrix(th_data, nrow = nrow(th_data)))
#names(th_data) <- name



#--------- visualization ------------
library(ggplot2)
ggplot(data = tp_data, aes(x = 1:nrow(tp_data), y = tp_data[,6])) + geom_line() +
  xlab("time") + ylab("temperature(degree C)") + 
  geom_ma(ma_fun = SMA, n = 12, size = 1) 
ggplot(data = tp_data, aes(x = 1:nrow(tp_data), y = tp_data[,1])) + geom_line() +
  xlab("time") + ylab("rain(mm)")


ggplot(data = t_data, aes(x = 1:nrow(t_data), y = t_data[,1])) + geom_line() +
  geom_line(data = t_data, aes(x = 1:nrow(t_data), y = t_data[,3]), color="red") +
  geom_line(data = t_data, aes(x = 1:nrow(t_data), y = t_data[,4]), color="blue") +
  xlab("time") + ylab("atmospheric pressure(hPa)")
n=365       # daily data -> view it in year scale
ggplot(data = t_data[1:n,], aes(x = 1:n, y = t_data[1:n,1])) + geom_line() +
  geom_line(data = t_data[1:n,], aes(x = 1:n, y = t_data[1:n,3]), color="red") +
  geom_line(data = t_data[1:n,], aes(x = 1:n, y = t_data[1:n,4]), color="blue") +
  xlab("time") + ylab("atmospheric pressure(hPa)")



#--------- normalization ------------
tp_mean <- apply(X = tp_data, MARGIN = 2, FUN = mean)
tp_sd <- apply(X = tp_data, MARGIN = 2, FUN = sd)
tpn_data <- scale(x = tp_data, center = tp_mean, scale = tp_sd)
tpn_data <- data.frame(tpn_data)
ggplot(data = tpn_data, aes(x = 1:nrow(tpn_data), y = tpn_data[,5])) + geom_line() +
  xlab("time") + ylab("temperature(degree C)")
ggplot(data = tpn_data, aes(x = 1:nrow(tpn_data), y = tpn_data[,1])) + geom_line() +
  xlab("time") + ylab("rain(mm)") +
  geom_line(data = tpn_data, aes(x = 1:nrow(tpn_data), y = tpn_data[,5]), color = "blue")

#--------ML1------------
# 總共有八年的資料
# 將1-1300作為訓練資料，1300-2000作為validation，2000-2300作為test data
nnetM <- nnet(formula = rain ~ ., linout = T, size = 3, decay = 0.001, maxit = 1000, trace = T, data = tp_data)
prediction <- predict(nnetM, tp_data)
prediction <- prediction*tp_sd[1]+tp_mean[1]
ggplot(data = tp_data, aes(x = 1:nrow(tp_data), y = tp_data[,1])) + geom_line() +
  xlab("time") + ylab("rainfall(mm)") +
  geom_line(data = tp_data, aes(x = 1:nrow(tp_data), y = as.vector(prediction)), color = "red")

ggplot(data = tp_data[1:n,], aes(x = 1:n, y = tp_data[1:n,1])) + geom_line() +
  xlab("time") + ylab("rainfall(mm)") +
  geom_line(data = tp_data[1:n,], aes(x = 1:n, y = as.vector(prediction)[1:n]), color = "red")

# write.csv(x = data, file = paste0(start," to ", end,".csv"))
#--------ML2------------
#訓練模型
library(neuralnet)
f1 <- as.formula('rain~T/pressure')
bpn <- neuralnet(formula = f1, data = tp_data, hidden = c(2,4),learningrate = 0.01,linear.output = F)
print(bpn)

#圖解BP
plot(bpn)


#--------ML 3-------------
#nnfor package
library(nnfor)

x = ts(tp_data[,1])
model <- elm(x)
predict <- forecast(model)

m <- mlp(x)
pred <- forecast(m)
p <- predict(mlp)
