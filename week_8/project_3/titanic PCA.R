data <- read.csv("titanicTrain.csv")
# 去除NA
# 整理資料格式
data <- data[1:1000,]
sapply(data, function(x) {sum(is.na(x))})
data <- subset(data, select = -body)
data <- data[is.na(data$age)!=TRUE,]
data$fs <- as.integer(data$parch)+as.integer(data$sibsp)+1

data$boat <- as.character(data$boat)
data$survived <- as.factor(data$survived)
data$pclass <- as.factor(data$pclass)
data$sibsp <- as.factor(data$sibsp)
data$parch <- as.factor(data$parch)
data$fs <- as.factor(data$fs)
summary(data)

# EDA
library(ggplot2)
library(scales)
library(gridExtra)

ggplot(data, aes(x = survived, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'How many people died and survived on the Titanic?') +
  geom_label(stat='count',aes(label=..count..), size=4) +
  theme_grey(base_size = 15)



#------- gender/sex-----------
b1 <- ggplot(data, aes(x = pclass)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8) +
  facet_grid(.~sex) +
  theme_grey(base_size = 15)

b2 <-
  ggplot(data, aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8, position = position_dodge(0.9)) +
  labs(x = 'pclass', y= "count") + facet_grid(.~sex) +
  theme(legend.position="none") + theme_grey()



# -----------pclass and sex-------------
### ----------class1.2女性活下來的比例相當高，class2.3男性活下來的比例最低
b3 <-
  ggplot(data, aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'pclass', y= "Percent") + facet_grid(.~sex) +
  theme(legend.position="none") + theme_grey()



grid.arrange(b1,b2,nrow=1)
b3

ggplot(data, aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'pclass', y= "Percent") + facet_grid(.~sex) +
  theme(legend.position="none") + theme_grey()

#------ pclass ----------
ggplot(data, aes(x = survived, fill = pclass)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=4, position=position_dodge(0.9)) +
  labs(x = 'class') +
  theme_grey(base_size = 15)

b1 <- ggplot(data, aes(x = pclass, fill = pclass)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8) +
  theme_grey(base_size = 15)

b2 <-
  ggplot(data, aes(x = pclass, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8, position=position_dodge(0.9)) +
  theme_grey(base_size = 15)
grid.arrange(b1,b2, nrow=1)


#------embarked----------
b1 <- ggplot(data, aes(x = embarked, fill = embarked)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8) +
  theme_grey(base_size = 15)

b2 <-
  ggplot(data, aes(x = embarked, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8, position=position_dodge(0.9)) +
  theme_grey(base_size = 15)
grid.arrange(b1,b2, nrow=1)


#-----sibsp-----------
#------男性存活比率都偏低，
#----女性的中若手足和配偶數量加起來不超過3，存活機率很高，幾乎都有達到80%
#活下且有兄弟姊妹親人是女性的比例是
length(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0& data$sex=="female"])/length(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0])
length(data$age[data$survived==0 & data$sibsp!=0 & data$parch!=0& data$sex=="male"])/length(data$age[data$survived==0 & data$sibsp!=0 & data$parch!=0])

# total statistics
b1 <- ggplot(data, aes(x = sibsp, fill = sibsp)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=5) +
  theme_grey(base_size = 15)
# statistics showing survival with sex difference
b2 <-
  ggplot(data, aes(x = sibsp, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=5, position=position_dodge(0.9)) +
  facet_grid(.~sex) +
  theme_grey(base_size = 15)

# total statistics with sex difference
b3 <-
  ggplot(data, aes(x = sibsp, fill = sex)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_label(stat = 'count', aes(label =..count..), size = 5, 
             position=position_dodge(0.9))+
  theme_grey(base_size = 15)

# percent statistics showing survival with sex difference
b4 <- 
  ggplot(data, aes(x = sibsp, fill = survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'sibsp', y= "Percent") + facet_grid(.~sex) +
  theme(legend.position="none") + theme_grey()

grid.arrange(b1,b2,b3,b4, nrow=2)

#----- parch-------------
#-----男性生存比例偏低，但若在sibsp為1和2之間的生存率有接近0.5
#------女性的生存率在sibsp為5以前都很高。
# 注意: parch為parch為2以前的樣本量較多。
b1 <- ggplot(data, aes(x = parch, fill = parch)) +
  geom_bar(stat='count', position='dodge') +
  facet_grid(.~sex) +
  geom_label(stat='count',aes(label=..count..), size=8) +
  theme_grey(base_size = 15)

b2 <-
  ggplot(data, aes(x = parch, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=8, position=position_dodge(0.9)) +
  facet_grid(.~sex) +
  theme_grey(base_size = 15)

ggplot(data, aes(x = parch, fill = survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'parch', y= "Percent") + facet_grid(.~sex) +
  theme(legend.position="none") + theme_grey()


grid.arrange(b1,b2, nrow=1)

# ------- family size -------
# -----family size為4的男生存活率最高，大概都是接近0.5
# -----family size為4的女生存活率也最高，
# total statistics
b1 <- ggplot(data, aes(x = fs, fill = fs)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=5) +
  theme_grey(base_size = 15)
# statistics showing survival with sex difference
b2 <-
  ggplot(data, aes(x = fs, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count',aes(label=..count..), size=5, position=position_dodge(0.9)) +
  facet_grid(.~sex) +
  theme_grey(base_size = 15)

# total statistics with sex difference
b3 <-
  ggplot(data, aes(x = fs, fill = sex)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_label(stat = 'count', aes(label =..count..), size = 5, 
             position=position_dodge(0.9))+
  theme_grey(base_size = 15)

# percent statistics showing survival with sex difference
b4 <- 
  ggplot(data, aes(x = fs, fill = survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'sibsp', y= "Percent") + facet_grid(.~sex) +
  theme(legend.position="none") + theme_grey()

grid.arrange(b1,b2,b3,b4, nrow=2)


# ------------ age----------
# 男性存活率最高的在5歲以下
# 女性則是大部分都存活率極高
ggplot(data, aes(x = age, fill = survived)) +
  geom_histogram(aes(fill=factor(survived))) + labs(title="Survival density, known-ages, and sex") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme_grey() + facet_grid(.~sex)

#-------fare--------
# 
#
ggplot(data, aes(x = fare, fill = survived)) +
  geom_histogram(aes(fill=factor(survived))) + labs(title="Survival density, known-ages, and sex") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme_grey() + facet_grid(.~sex)


# sibsp的影響
par(mfrow=c(1,2))
boxplot(data$sibsp, range=2, main="sibsp")
boxplot(data_sur$sibsp, col="red", main="survived sibsp")
boxplot(data_un$sibsp, col="blue", main="unsurvived sibsp")
summary(data_sur$sibsp)
summary(data_un$sibsp)
table(data_sur$sibsp)/length(data_sur$sibsp)*100
table(data_un$sibsp)/length(data_un$sibsp)*100
table(data$sibsp)/length(data$sibsp)*100




par(pch=19)
plot(data$age, data$fare)
points(data$age[data$survived==1], data$fare[data$survived==1], col="red") #活下
points(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0], 
       data$fare[data$survived==1 & data$sibsp!=0 & data$parch!=0], col="yellow")
points(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0 & data$sex=="female"], data$fare[data$survived==1 & data$sibsp!=0 & data$parch!=0 & data$sex=="female"], 
       col="green")
#legend("topright", legend=c("活下來",paste0("活下來","\n","有sibsp&parch"),"活下來+有sibsp&parch+女性"),col=c("red","yellow","green"),pch=19)


#活下且有兄弟姊妹親人等等



# 推測: 買最貴的票價存活機率極高
#          

# age 越大，越有可能上船?



# 登船但不一定會存活
data$survived[data$boat != ""]
as.factor(data$survived[data$boat != ""])
summary(as.factor(data$survived[data$boat != ""])) #有登船
summary(as.factor(data$survived[data$boat != ""]))[1]/length(as.factor(data$survived[data$boat != ""]))
# 沒登船但不一定就會死
data$survived[data$boat == ""]
summary(as.factor(data$survived[data$boat == ""])) #沒登船
summary(as.factor(data$survived[data$boat == ""]))[2]/length(as.factor(data$survived[data$boat == ""]))

# changing the data type
data$pclass <- as.factor(data$pclass)
data$survived <- as.factor(data$survived)
data$sex <- as.factor(data$sex)



library(e1071)
?svm
dataas <- subset(data, select=c(pclass,age,sibsp,parch,survived))
dataa <- subset(dataa, select=c(pclass,age,sibsp,parch))
model <- svm(survived ~ ., data = dataas)
pred_result <- predict(model, dataa)
table(pred_result, data$survived)    #無法成功因為age有139個NA

plot(model, dataas, age ~ sibsp,
     slice = list(age = 3, sibsp= 4),color.palette = terrain.colors)


